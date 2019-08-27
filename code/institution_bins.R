#carnegie classifications (R1, R2 research etc)----
carnegie_class <- read_csv("../data/carnegie_class.csv") %>% 
  select(name, city, state, Basic)

us_industries <- read_csv("../data/us_industries.csv") %>% 
  pull(institution.name) %>% 
  str_to_lower(.) %>% unique()

fed_labs <- read_csv("../data/fed_state_labs.csv") %>% 
  pull(institution.name) %>% 
  str_to_lower(.) %>% unique()

R1 <- carnegie_class %>% 
  filter(Basic == "Doctoral Universities: Very High Research Activity") %>% 
  pull(name) %>% str_to_lower(.)

R2 <- carnegie_class %>% 
  filter(Basic == "Doctoral Universities: High Research Activity") %>% 
  pull(name) %>% str_to_lower(.)

medical_inst <- carnegie_class %>% 
  filter(Basic == "Special Focus Four-Year: Medical Schools & Centers") %>% 
  pull(name) %>% str_to_lower(.)

low_research <- carnegie_class %>% 
  filter(str_detect(Basic, "High Research") == FALSE & 
           str_detect(Basic, "Medical Schools") == FALSE) %>% 
  pull(name) %>% str_to_lower(.)

industry <- c("abbott", "abbvie", "accelerate", "achaogen", "achillion", "amgen",
              "allergan", "amplyx", "pharmaceuticals", "astrazeneca", "avidbiotics",
              "biopharma", "diagnostics", "bd", "beckman coulter", "bio rad", "nibr",
              "biorad", "biomerieux", "myers squibb", "chembio", "cellex", "merial",
              "therapeutics", "dupont", "genentech", " inc", "pharma", "merck",
              "biomérieux", "new england biolabs", "corporation", "consultant", 
              "jmi laboratories", "novartis", "the medicines company", "pfizer",
              "glaxosmithkline", "genesis biotechnology group", "sanofi", "battelle",
              "laboratories", "profectus biosciences", "emergent biosolutions", "gates foundation",
              "integral molecular", "jackson laboratory", "roche", "meck", "path",
              "neomed", "bayer", "cerexa", "gsk", "medical affairs strategic solutions",
              "infectious disease specialists", "viiv healthcare", "micromyx", "jmi labs",
              "astellas", "carestream molecular imaging", "inc research", "data first consulting",
              "southern research", "micromyxcom", "jhonson and johnson", "eli lilly and co",
              "consulting", "technical services", "technical solution", "strategic solutions", 
              "european molecular biology laboratory", "cea", "sri international",
              "emerald bio", "bloodcenter", "blood center", "life technologies",
              "kaiser permanente", "american red cross", "janssen", "calimmune",
              "great basin scientific", "femeris women", "henry m jackson foundation") %>% 
  paste0(., collapse = "|")

govt <- c("usda", "agricultural research service", "air force", "nih", "edgewood",
          "regional healthcare", "state health", "armed forces", "nasa", "nichd",
          "department of public health", "fda", "cdc", "national cancer institute", 
          "food and drug administration", "cleveland clinic", "health department",
          "homeland security", "veterans affairs", "environmental protection agency",
          "epa", "national lab", "centers for disease control", " va ", "army", "llc",
          "state public health laboratory", "state public health lab", "veterans",
          "rocky mountain lab", "usamriid", "niaid", "national institute of",
          "va medical center", "uniformed services university", "nci ", "afrims",
          "national animal disease center", "national institute of allergy and",
          "united states geological survey", "naval medical research", "pnnl",
          "wadsworth center, new york state dept health", "niddk", "usamiird",
          "national oceanic and atmospheric administration", "united states", 
          "va ann arbor", "dept of health service", "wadsworth center", "nci",
          "center for infectious disease reserach", "county public health",
          "saic frederick fnlcr", "us government", "argonne naltional labs",
          "national center for toxicological research", "niehs", "piadc",
          "nbacc", "national wildlife research center", "naval", "oregon health authority",
          "state of conn", "colorado . wildlife") %>% 
  paste0(., collapse = "|")

medical <- c("mayo clinic", "scripps research", "research hospital", "musc",
             "fred hutchinson", "massachusetts general hospital", "howard hughes",
             "beth israel deaconess medical center", "hospital", "children's mercy",
             "memorial sloan kettering", "school of medicine", "cedars sinai",
             "medical school", "medicine at ucla", "arkansas for med", "scripps",
             "medical center", "medical uni", "college of medicine", "umdnj",
             "louisana state university health sciences", "cancer center",
             "center for infectious disease research", "primate research center",
             "rosalind franklin university", "icahn school of medicine", "omrf",
             "university of tennessee health science center", "research institute",
             "anschutz medical campus", "health system", "institute", "upmc",
             "texas health science", "utmb", "texas medical branch", "ohsu",
             "sch of medicine", "utmb", "texas medical branch", "uthscsa",
             "oklahoma health sci", "medical college", "national jewish health",
             "oklahoma hsc", "oklahoma-health sci", "massacussetts med",
             "oregon health and science university", "texas health science",
             "university of tennessee health science center", "lsuhsc", "kumc",
             "bigelow laboratory for ocean sciences", "scripps florida", "chop",
             "international aids vaccine initiative", "woods hole", "mgh",
             "aaron diamond aids research center", "school of med", "medical ctr",
             "oklahoma medical research foundation", "marine biological laboratory",
             "scripps institution", "university of medicine", "med ctr", "dental",
             "jordan valley innovation center", "children's", "childrens", "alden research",
             "northshore university healthsystem", "cold spring harbor", "seattle biomed",
             "joslin diabetes", "marshfield clinic", "jcvi", "usuhs", "state laboratory",
             "md anderson", "oregon health (?=an|sci)", "lsu health", "samuel roberts") %>% 
  paste0(., collapse = "|")

r1 <- c("harvard", "binghamton", "cornell", "ucsd", "uc davis", "tulane university",
        "george washington", "brigham and women's", "boston university", "uga",
        "cornell university", "cuny graduate", "dana farber", "dartmouth",
        "broad institute", "case western", "mit", "university of mississippi",
        "lousiana state university, baton rouge", "stanford", "uc berkeley", 
        "university of michigan", "clemson", "columbia university", "duke",
        "emory university", "auburn", "georgetown university", "wisconsin madison",
        "wisconsin–madison", "university of minnesota", "ohio state", "drexel",
        "university of california", "illinois at urbana champaign", "emory",
        "university of pittsburgh", "washington university", "indiana university",
        "pennsylvania state university", "johns hopkins", "colorado state",
        "university of maryland", "north carolina state", "chapel hill", "yale",
        "massachusetts amherst", "texas at dallas", "texas at el paso", "knoxville",
        "texas at arlington", "texas at austin", "texas tech", "virginia tech",
        "university of virginia", "vanderbilt", "tufts", "oregon state uni", 
        "universisty of kentucky", "brown medical", "brown university", "alabama",
        "florida state", "georgia institute", "georgia state uni", "west virginia",
        "iowa state unviersity", "iowa state university", "wayne state", "ucsb",
        "california institute of technology", "florida international", "temple",
        "louisiana state university", "michigan state", "u michigan", "rutgers",
        "michigan medicine", "mississippi state", "montana state", "rensselaer",
        "new york university", "northwestern university", "oklahoma state", "usc",
        "university of arizona", "ity of arkansas", "central florida", "ucsc",
        "ity of chicago", "cincinnati", "colorado boulder", "colorado denver",
        "connecticut", "delaware$", "university of florida", "university of georgia",
        "sity of hawaii", "illinois at chicago", "of iowa", "of kansas", "of kentucky",
        "louisville", "of miami", "missouri columbia", "nebraska", "nevada", "u penn",
        "ity of new hampshire", "of new mexico", "of oklahoma", "of pennsylvania",
        "pittsburgh", "of rochester", "of south carolina", "south florida$", 
        "ity of southern cali", "utah", "of virginia", "wisconsin milwaukee",
        "wisconsin–milwaukee", "virginia commonwealth", "washington state uni",
        "tennessee", "tenessee", "of mass", "^university of missouri", "uw madison",
        "university at buffalo", "georgia tech", "ucla", "upenn", "caltech",
        "uc irvine", "uab", "uc santa cruz", "nc state", "at albany", "tulane",
        "uc santa barbara", "uconn", "u conn", "colorado amc", "colorado ans",
        "colorado.denv", "colorado.boulder", "colorado.health") %>% 
  paste0(., collapse = "|")

r2 <- c("cuny city", "east tennessee state", "baylor", "eastern michigan",
        "university of california, merced", "indiana university – purdue",
        "baltimore county", "north carolina charlotte", "north carolina in wilmington",
        "massachusetts med", "massachusetts lowell", "texas at san antonio",
        "massachusetts boston", "texas aandm", "missouri kansas", "missouri-kansas",
        "texas rio grande valley", "texas aandm university corpus christi", 
        "indiana university purdue university indianapolis", "purdue univesrity",
        "indiana university - purdue university indianapolis", "arizona state",
        "^purdue university", "louisiana state university, new orleans", 
        "arkansas state", "bowling green state", "brigham young", "clark uni",
        "catholic university", "central michigan", "william and may", "east carolina",
        "florida atlantic", "hampton universtiy", "jefferson", "kent state", 
        "loyola univ", "marshall", "mercer", "miami uni", "missouri uni", "camden",
        "new mexico state", "carolina aand", "north dakota state", "northern arizona",
        "northern illinois", "ohio uni", "old dominion", "rowan uni", "newark",
        "saint louis", "san diego state", "carbondale", "university of montana",
        "university of texas health science center at san antonio", "akron",
        "merced", "of maine", "wilmington", "ity of north dakota", "puerto rico",
        "of rhode island", "uc san diego", "south alabama", "south dakota",
        "toledo", "vermont", "wake forest", "wright state", "des moines", "etsu",
        "university of tennessee at chattanooga", "missouri–st louis") %>% 
  paste0(., collapse = "|")

low <- c("baptist health", "california state", "university of washington", 
         "indiana university northwest", "wisconsin–green bay", "wisconsin–la crosse",
         "wisconsin parkside", "wisconsin la crosse", "albert einstein", 
         "texas women's university", "st john's university", "edwardsville",
         "california, sf", "california san francisco", "detroit mercy", "hofstra",
         "pacific univeristy", "pace university", "long island univeristy", 
         "creighton university", "augusta university", "maryland baltimore",
         "arizona state university west campus", "midwestern university", "rpi",
         "rush university", "suny downstate", "weill cornell", "texas southwestern",
         "arkansas for med", "ut southwestern", "utsouthwestern", "ucsf", "concordia",
         "suny upstate", "new york at buffalo", "hunter college", "tribal", "shreveport",
         "suny buffalo", "california polytechnic ", "xavier", "of new york") %>% 
  paste0(., collapse = "|")

inst_list <- c("R1 Univ", "R2 Univ", "Low Research Univ", "Med School & Institutes", 
               "Federal Research", "Industry Research", "Other", "Non-US Inst")

non_us_inst <- c("nanjing|china|huazhong|oxford|british columbia|toronto|queensland|alberta|melbourne|mcgill|fudan university|university of cambridge|shanghai jiao tong university|jilin university|jiangnan university|monash university|inserm|sichuan university|university of liverpool|university of nottingham|sichuan agricultural university|university of edinburgh|university of sydney|tsinghua university|university of birmingham|national taiwan university|university college london|mahidol university|helmholtz centre for infection research|chinese university of hong kong|wageningen university|university of otago|university of adelaide|london school of hygiene and tropical medicine|cardiff university|king's college london|commonwealth scientific and industrial research organisation|ufmg|tongji university|university of naples federico ii|university of vienna|university of oslo|university of warwick|australian national university|university of montreal|universidad|canada|munich|cambridge|xi'an|buenos aires|ryerson|moscow|epfl|james cook university|laval university|western university|riken|oxford university|la trobe university|university of pavia|king saud university|murdoch|canadian|cinvestav|european molecular biology laboratory|osnabrück|minas gerais|hannover|niigata|postech|bergen|borstel|tasmania|hangzhou|tzu chi |beirut|wollongong|rio grande do sul|innsbruck|peter maccallum|gustave roussy|hokkaido|western sydney|milano|chinese|cancer research uk|jinlin|charles darwin|wehi|austin health|al qura|witten/herdecke")

#bin US institutions----
binned_inst <- data %>% #deal w. multiple inst?
  select(institution, country) %>% 
  mutate(institution = str_to_lower(institution)) %>% 
  filter(country == "United States") %>% 
  filter(str_detect(institution, non_us_inst) == FALSE) %>% 
  select(institution) %>% distinct() %>% 
  mutate(US.inst.type = case_when(
    institution %in% us_industries | str_detect(institution, industry) ~ "Industry Research",
    institution %in% R1 | str_detect(institution, r1) ~ "R1 Univ",
    institution %in% medical_inst | str_detect(institution, medical) ~ "Med School & Institutes",
    institution %in% R2 | str_detect(institution, r2) ~ "R2 Univ",
    institution %in% low_research | str_detect(institution, low) ~ "Low Research Univ",
    institution %in% fed_labs | str_detect(institution, govt) ~ "Federal Research", 
    TRUE ~ "Other"))