# UofT_api_case_data <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases
# fwrite(UofT_api_case_data, "CaseDataTables/UofT_cases.csv")

# Case_Data <- data.table(province=character(), region=character(), date=numeric(), cases=numeric())
#
# BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") %>%
#     select(-c(Province, HA, Cases_Reported_Smoothed)) %>%
#     filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) %>%
#     rename(date=Date, HA=HSDA, cases=Cases_Reported) %>%
#     mutate(date=as.Date(date), province="British Columbia")
# fwrite(BC_cases, "CaseDataTables/BC_cases.csv")

# QC_cases <- fread("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv") %>%
#     select(Date, Nom, cas_quo_tot_n) %>%
#     filter(grepl("\\d", Date) & grepl("\\d - [a-zA-Z]", Nom)) %>%
#     mutate(Nom = Nom %>% replace_accents %>% trim_numbers) %>%
#     rename(date=Date, HA=Nom, cases=cas_quo_tot_n) %>%
#     data.table %>%
#     mutate(date=as.Date(date), province="Quebec")
# fwrite(QC_cases, "CaseDataTables/QC_cases.csv")

# MB_cases <- UofT_api_case_data %>%
#     filter(province == "Manitoba") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date))
# fwrite(MB_cases, "CaseDataTables/MB_cases.csv")

# SK_cases <- get_saskatchewan_case_data() %>%
#     select("Date", "Region", "New Cases") %>%
#     rename(date=Date, HA=Region, cases="New Cases") %>%
#     mutate(date=as.Date(date), province="Saskatchewan") %>%
#     data.table
# fwrite(SK_cases, "CaseDataTables/SK_cases.csv")

#-----------------------------------------------------------------------------------

# Cases <- rbind(
#     fread("CaseDataTables/BC_cases.csv"),
#     fread("CaseDataTables/QC_cases.csv"),
#     fread("CaseDataTables/MB_cases.csv"),
#     fread("CaseDataTables/SK_cases.csv")
# )

# Classes <- rbind(
#     fread("Classifications/SK_classification.csv") %>% select(province, HA, CSD, SAC),
#     fread("Classifications/MB_classification.csv") %>% select(province, HA, CSD, SAC)
# )

# Scores <- data.table(province=character(), zone=character(), score=numeric(), total=numeric())

# for(prov in c("Saskatchewan", "Manitoba")) # 
# {
#     Cases_Here <- Cases[province==prov]
#     Classes_Here <- Classes[province==prov]
#     
#     for(zone in unique(Classes_Here$HA))
#     {
#         cumul_sum <- Cases_Here[HA==zone, sum(cases)]
#         
#         temp_classes <- Classes_Here[HA==zone]
#         
#         # written this way so that duplicate CMAs and CAs can be separated and single-counted, rather than double counted
#         CMAs <- temp_classes[which(grepl("cma (*)", SAC)), SAC] # unique(temp_classes[which(grepl("cma (*)", temp))])
#         CAs  <- temp_classes[which(grepl("ca (*)",  SAC)), SAC] # unique(temp[which(grepl("ca (*)", temp))])
#         MIZs <- temp_classes[which(! SAC %in% c(CMAs,  CAs)), SAC]
#         
#         numerator <- length(CMAs)*zone_scores("cma") + length(CAs)*zone_scores("ca") + sum(zone_scores(MIZs))
#         denominator <- length(CMAs) + length(CAs) + length(MIZs)
# 
#         final_score <- numerator # /denominator
# 
#         Scores <- rbind(Scores, list(prov, zone, final_score, cumul_sum))
#     }
# }

########################################### JUNK ######################################

# get_CSD_name <- function(x) str_extract(x, "[a-zA-Z]+")
# get_CSD_names <- function(theList) unlist(lapply(as.character(theList), get_CSD_name))
# 
# separate_code_string <- function(x) paste( str_extract(x, "[0-9]+"), str_extract(x, "[a-zA-Z]+") )
# 

# all_cas <- read.csv("Classifications/all_CAs.csv", na.strings=c("", "NA")) %>%
#     select(-c(CA, X)) %>%
#     na.omit %>%
#     unique %>%
#     mutate(from="all_cas", HA_code=extract_codes(CSD)) %>%
#     select(-CSD) %>%
#     data.table
# 
# all_cmas <- read.csv("Classifications/all_CMAs.csv", na.strings=c("", "NA")) %>%
#     select(-c(CMA, X)) %>%
#     na.omit %>%
#     unique %>%
#     mutate(from="all_cmas", HA_code=extract_codes(CSD)) %>%
#     select(-CSD) %>%
#     data.table

# get_CSD_name <- function(x) strsplit(x, ' - ')[[1]][2]
# get_CSD_names <- function(theList) unlist(lapply(as.character(theList), get_CSD_name))
# 
# all_cmas <- read.csv("Classifications/all_CMAs.csv", na.strings=c("", "NA")) %>% 
#     select(-X, -province) %>% 
#     na.omit %>% 
#     mutate(SGC2016=extract_codes(CSD)) %>% 
#     mutate(CSD=get_CSD_names(CSD))
# 
# # HA_LUT <- rbind(all_cas, all_cmas) %>% unique(by="HA_code")

# string_to_integer <- function(x) if(is.character(x)) as.integer(paste(unlist(strsplit(x, ",")), collapse='')) else x

# 

# cat("\014")  
# nrow(MIZs[prospective_HA==""][order(HA_code)][, -c("prospective_HA")])
# MIZs[prospective_HA=="" & (HA_code<4700 | HA_code>4799)][order(HA_code)][, -c("prospective_HA")][1:100]


# source of the data tables giving the CMAs, CAs and MIZs
# https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/comprehensive.cfm

# 101 # Canada, provinces and territories - hign level
# 201 # census metropolitan areas and census agglomerates
# 301 # census subdivisions
# 601 # statistical area classification
# 701 # census divisions
# 1601 # census tracts
# 1404 # economic regions
# 1701 # aggregate disseminatiom areas
# 1901 # dissemination areas

# datasetNumber <- 601
# theTable <- fread(sprintf("T%iEN.CSV", datasetNumber))

# caseData <-  data.table()


# 101 - provincial populations
# 201 gives names and classifications
# 301 - CSD types
# 601 - MIZ and population numbers per province, no names
# 701 - Geographic type
# 1401 - has population density


# caseData <- data.table(jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases)
# UofTAreas <- x$cases$health_region %>% unique %>% sort


# source page: https://www150.statcan.gc.ca/n1/en/geo?geotext=Algoma,%20Unorganized,%20North%20Part,%20Unorganized%20%5BCensus%20subdivision%5D,%20Ontario&geocode=A00053557095https://www150.statcan.gc.ca/n1/en/geo?geotext=Algoma,%20Unorganized,%20North%20Part,%20Unorganized%20%5BCensus%20subdivision%5D,%20Ontario&geocode=A00053557095


# jurisDictionary <- list(
#     
#     "Alberta" = list("code" = "AB", "regions" =  c(4832, 4833, 4834, 4835, 4831)),
#     
#     "British Columbia" = list("code" = "BC", "regions" = c(591, 592, 593, 594, 595)),
#     
#     "Manitoba" = list("code" = "MB", "regions" = c(4603, 4604, 4602, 4605, 4601)),
#     
#     "New Brunswick" = list("code" = "NB", regions = c(1301, 1302, 1303, 1304, 1305, 1306, 1307)),
#     
#     "Newfoundland and Labrador" = list("code" = "NL", "regions" = c(1012, 1011, 1014,	1013)),
#     
#     "Northwest Territories" = list("code" = "NT", "regions" = c(6101)),
#     
#     "Nova Scotia" = list("code" = "NS",  "regions" = c(1201, 1202, 1203, 1204)),
#     
#     "Nunavut" = list("code" = "NU", "regions" = c(6201)),
#     
#     "Ontario" = list("code" = "ON", 
#                      "regions" = c(3526, 3527, 3540, 3530, 3558, 3533, 3534, 3535, 3536, 
#                                    3537, 3538, 3539, 3541, 3542, 3543, 3544, 3546, 3547,
#                                    3549, 3551, 3553, 3555, 3556, 3557, 3560, 3575, 3561, 
#                                    3562, 3563, 3595, 3565, 3566, 3568, 3570
#                      )),
#     
#     "Prince Edward Island" = list("code" = "PE", "regions" = c(1100)),
#     
#     "Quebec" = list("code" = "QC", "regions" = c(2408, 2401, 2403, 2412, 2409, 2405, 2411, 2414, 2415, 2413, 2404, 2416, 2406, 2410, 2417, 2407, 2402, 2418)),
#     
#     "Saskatchewan" = list("code" = "SK", "regions" = c(473, 471, 472, 475, 474, 476)),
#     
#     "Yukon" = list("code" = "YT", "regions" = c(6001))
# )
# 
# jsonlite::fromJSON("https://www12.statcan.gc.ca/rest/census-recensement/CR2016Geo.json?lang=E&topic=8&geos=CSD&notes=0&cpt=00")

# jsonlite::fromJSON("https://www12.statcan.gc.ca/rest/census-recensement/CPR2016.json?lang=E&topic=8&notes=0&stat=0")

# jsonlite::fromJSON("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page.cfm?Lang=E&Geo1=CSD&Code1=4718070&Geo2=PR&Code2=47&SearchText=Buffalo%20Narrows&SearchType=Begins&SearchPR=01&B1=All&GeoLevel=PR&GeoCode=4718070&TABID=1&type=0")

# read_html("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/page.cfm?Lang=E&Geo1=CSD&Code1=4701037&Geo2=PR&Code2=47&SearchText=Alameda&SearchType=Begins&SearchPR=01&B1=All&GeoLevel=PR&GeoCode=4701037&TABID=1&type=8")


# for(name in c("population", "commuting_population", "commute_1", "commute_2", "commute_3", "commute_4"))
# {
#     saskData[, (name):=unname(sapply(get(name), string_to_integer))]
# }
# 
# collection <- saskData[,
#    lapply(.SD, sum, na.rm=T), 
#    .SDcols=c("population", "commuting_population", "commute_1", "commute_2", "commute_3", "commute_4"), 
#    by=district
# ]

# jsonlite::fromJSON("https://api.covid19tracker.ca/cases")

