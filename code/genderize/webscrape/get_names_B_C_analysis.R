library(tidyverse)
library(rvest) #for html/web scraping

B_C_gender <- read_csv("Files/Broderick_Casadevall_gender_analysis.csv") #read in Nichole's data
B_C_gender <- B_C_gender[-2639,]

replace_value <- function(x, col_name){ #function to replace 1/0 with gender order
  case_when(x == 1 ~ col_name)
}

add_comma <- function(x){
  
  str_split(x, "") %>% unlist() %>% paste(., collapse = ",")
  
}

#two_co_auth <- c("m,m", "m,f", "f,f", "f,m") #list of situations with two co-authors

B_C_gender_tidy <- B_C_gender %>% 
  mutate(mm = replace_value(mm, "m,m"), #
         mf = replace_value(mf, "m,f"),
         ff = replace_value(ff, "f,f"),
         fm = replace_value(fm, "f,m")) %>%
  select(-`2+`) %>% #drop 2+ column (1/0)
  mutate(`Order for 2+ au` = str_extract(`Order for 2+ au`, "[:alpha:]{2}")) %>% 
  mutate(`Order for 2+ au` = map(`Order for 2+ au`, add_comma) %>% unlist) %>% 
  gather(mm:`Order for 2+ au`, key = "toss", value = "auth.order") %>% #tidy gender order
  select(-toss) %>% #drop unneeded column
  filter(auth.order != "NA") %>% #drop links where author gender wasn't completely assigned
  unique()
  
#pubmed only has initials, functions to get publisher's html data for full names
read_pubmed <- function(x){
  
  pubmed_html <-  read_html(x)#read in pubmed html data
  
  orig_link <- tryCatch(pubmed_html %>% xml_find_all('//dl[@class="rprtid"]/dd/a') %>% #find article PMCID
    html_text() %>% paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/", .) %>% head(n = 1), #extract & convert to link
    
    error = function(e){paste("NA; read_pubmed failed")
    })
  
  publish_html <- read_html(orig_link) #read in source link & get publishers data
  
  journ_title <- pubmed_html %>% xml_find_all('//div[@class="cit"]') %>% html_nodes("a") %>% html_text()
  
  return(publish_html)
}

#function to pull author names from highwire published journals, since it requires a different scripting
get_highwire_authors <- function(journal, x){
  
  auth_names_nodes <- x %>% xml_find_all('//span[@class="highwire-citation-authors"]') #find nodes with author names
  
  auth_name_cond <- str_detect(auth_names_nodes, "nlm-given-names") == FALSE #condition for which nodes to keep (avoids duplicating names)
  
  auth_names <- auth_names_nodes[auth_name_cond] %>% html_text(trim = TRUE) #drop duplicates & pull authors names
  
  return(auth_names) #returns a vector of strings, each as a name
}

#journals where scripts are identical except for the xpath
get_some_authors <- function(x, html){
  
  author_path <- case_when(#paste xpath based on url
    str_detect(x, "cell") ~ paste('//a[contains(@title, "Correspondence information")]'),
    str_detect(x, "plos") ~ paste("//*[@data-author-id]"),
    str_detect(x, "nature") ~ paste('//li[@itemprop="author"]/a/span[@itemprop="name"]'),
    str_detect(x, "jbc") ~ paste('//a[@class="name-search"]'),
    str_detect(x, "sciencedirect") ~ paste('//div[@id="author-group"]/a/span/span[@class="text given-name"]'),
    str_detect(x, "jci|j clin invest") ~ paste('//h4[@class="author-list"]'),
    str_detect(x, "elife") ~ paste('//span[@class="content-header__author"]/a'),
    str_detect(x, "sciencemag") ~ paste('//span[@class="name"]'),
    str_detect(x, "pmc") ~ paste('//div[@class="contrib-group fm-author"]/a'),
    str_detect(x, "wiley") ~ paste('//div/a[@class="author-name accordion-tabbed__control"]')
  )
  
  auth_names <- html %>% xml_find_all(author_path) %>% #retrieve nodes with author data
    html_text(trim = TRUE)  %>% paste(., collapse = ", ") #pull names into vector
  
  return(auth_names) #returns a vector of strings, each as a name
}

#function to determine which function should be used to pull authors names, based on the journal (e.g., highwire or not)
get_author_function <- function(URL, html) {
  
  get_pubmed <- function(x){read_html(x) %>% xml_find_all('//div[@class="cit"]') %>% html_nodes("a") %>% html_text() %>% str_replace(., "\\.", "") %>% tolower()}
  
  pmc_test <- try(html %>% xml_find_all('//li[@class="accid"]') %>% str_detect(., "PMC"))
  
  journal <- if(length(pmc_test) == 1) {paste("pmc")} else{ 
    case_when(
    str_detect(URL, "pubmed") ~ paste("pubmed"),
    str_detect(URL, "plos") ~ paste("plos"),
    str_detect(URL, "sciencemag.org") ~ paste("sciencemag"),
    str_detect(URL, "wiley") ~ paste("wiley"),
    str_detect(URL, "www\\.") ~ paste("extract"),
    TRUE ~ paste("NA; journal failed")
    )}
  
  journal <- if(str_detect(journal, "pubmed") == TRUE){get_pubmed(URL)}else{paste(journal)}
  
  journal <- if(str_detect(journal, "extract") == TRUE){str_extract(URL, "(?<=www\\.)[:alnum:]+") %>% tolower()}else{paste(journal)}
  
  highwire_journals <- c("jimmunology", "mbio", "jcb", "jem", "j cell bio", "j exp med", "pnas", "proc natl", "jimmunol") #journals published by highwire (req diff workflow)
  
  some_journals <- c("cell", "nature", "jbc", "plos", "sciencedirect", "jci", "j clin invest", "elife", "pmc", "sciencemag", "wiley") #journals w similar workflow
  
  this_function <- case_when(#match journal name to a list to determine correct function
    journal %in% highwire_journals ~ paste("get_highwire_authors"),
    journal %in% some_journals ~ paste("get_some_authors"),
    TRUE ~ paste("NA; get_authors failed"))
  
  which_function_arg <- paste0(this_function, "('", journal, "', html)")
  
  return(which_function_arg)
}

get_authors <- function(URL){
  
  print(URL)
  
  URL <- if_else(str_detect(URL, "http://|https://") == TRUE, #check if url has http, otherwise read_html doesn't work
                  paste(URL), #if there, go ahead
                  paste0("http://", URL))  #if not, add
  
  which_html <- if_else(str_detect(URL, "pubmed") == TRUE, #check to see if it's a pubmed link
                  paste("read_pubmed"), #if so, use pubmed function to get source html
                  paste("read_html")) #if not read in html data as usual
  
  html <- tryCatch(
    invoke(which_html, list(URL)),
    error = function(e) {paste("NA; html failed")}
  )
  
  which_function <- get_author_function(URL, html) #determine which function needed based on the journal
  
  author_names <- tryCatch(
    if(str_detect(which_function, "failed")){paste(which_function)}else{
    eval(parse(text = which_function))}, #somehow run the specified function
    error = function(e) {paste("NA;", which_function, "author_names failed")}
  ) %>% head(n = 1)
  
  return(author_names)
}


#get_jstor_authors <- function(x){} #skip for now b/c only one & it requires clicking a button to access a pdf, not sure how #to code around that
#
#
#links <- B_C_gender_tidy %>% mutate(weblinks = str_extract(`PMID:`, "www\\.[:alnum:]+\\.[:alpha:]{3}/")) #create column #with common part of links
#link_sum <- links %>% group_by(weblinks) %>% summarise(n = n()) #look at types of links present
#links %>% filter(is.na(weblinks)) %>% View() #check out ones typing as "na"
#
#test_urls <- B_C_gender_tidy %>% tail(n = 3) %>% 
#  mutate(author_names = map_chr(.$`PMID:`, get_authors))

authors <- B_C_gender_tidy %>% pull(`PMID:`) %>% 
  map(., get_authors) %>% unlist() %>% as.tibble

print("authors complete")

save(list = ls(), file = paste0("Processed_Data/B_C_authors_", Sys.Date(), ".Rdata"))

print("Rdata saved")

author_list_df <- cbind(B_C_gender_tidy, authors)

print("cbind complete")

write_csv(author_list_df, "Processed_Data/B_C_author_list.csv")

print("csv saved")
