library(cancensus)
library(CanCovidData)

replaceAccents <- function(string)
{
    accentedCharacters <- list("é"="e", "è"="e", "ô"="o", "Î"="I")
    for(index in 1:length(accentedCharacters))
    {
        string <- gsub(names(accentedCharacters)[index], accentedCharacters[[index]], string)
    }
    return(string)
}

replace_accents <- function(strings) unlist(lapply(strings, replaceAccents))
trim_numbers <- function(strings) unlist(lapply(strings, function(x) trimws(tail(strsplit(x, " - ")[[1]], 1))))
underscore <- function(strings) unlist(lapply(strings, function(x) paste(strsplit(x, " ")[[1]], collapse="_")))

zone_scores <- function(vec)
{
    zones <- list("none"=0, "weak"=1, "moderate"=2, "strong"=3, "ca"=4, "cma"=5)
    unlist(lapply( vec, function(x) zones[[x]] ))
}

# https://www150.statcan.gc.ca/n1/pub/92-195-x/2011001/geo/prov/tbl/tbl8-eng.htm
province_LUT <- data.table(rbind(
    c("Newfoundland and Labrador", "N.L.","NL", "10", "Atlantic"),
    c("Prince Edward Island", "P.E.I.",	"PE", "11", "Atlantic"),
    c("Nova Scotia", "N.S.", "NS", "12", "Atlantic"),
    c("New Brunswick", "N.B.", "NB", "13", "Atlantic"),
    c("Quebec", "Que.", "QC", "24", "Quebec"),
    c("Ontario", "Ont.", "ON", "35", "Ontario"),
    c("Manitoba", "Man.", "MB", "46", "Prairies"),
    c("Saskatchewan", "Sask.", "SK", "47", "Prairies"),
    c("Alberta", "Alta.", "AB", "48", "Prairies"),
    c("British Columbia", "B.C.", "BC", "59", "British Columbia"),
    c("Yukon", "Y.T.", "YT", "60", "Territories"),
    c("Northwest Territories", "N.W.T.", "NT", "61", "Territories"),
    c("Nunavut", "Nvt.", "NU", "62", "Territories")
)) %>% rename(province=V1, abbreviations=V2, alpha=V3, SGC=V4, region=V5)

lookup_province <- function(x)
{
    province_number <- substr(as.character(x), 1, 2)
    if(province_number %in% province_LUT$SGC) return(province_LUT[SGC==province_number, province])
    return("")
}
lookup_provinces <- function(theList) unlist(lapply(theList, lookup_province))

HA_LUT <- fread("Classifications/health_regions_by_code.csv") %>% select(-notes) %>% na.omit %>% unique(by="HA_code")

lookup_HA <- function(x) if(x %in% HA_LUT$HA_code) HA_LUT[HA_code==x, as.character(HA)] else ""
lookup_HAs <- function(codes) unlist(lapply(codes, lookup_HA))

# extract the first 4 numbers in the geocode for matching CSD to health regions
extract_code <- function(x) as.numeric(paste0(head(strsplit(x, '')[[1]], 4), collapse=''))
extract_codes <- function(theList) unlist(lapply(as.character(theList), extract_code))

# Saskatchewan gives COVID-19 data by geo-region rather than health authority, but the website
# gives a list, so we parse that and correct entries
SK_corr <- fread("grep -v '^#' Classifications/TRUE_SK_classification.csv", na.strings=c("", "NA")) %>% na.omit
# making the lookup table, searching by the name of the town
lookup_SK_HA <- function(the_town) if(the_town %in% SK_corr$town) SK_corr[town==the_town, HA] else ""
lookup_SK_HAs <- function(theList) unlist(lapply(theList, lookup_SK_HA))

HA_crosswalk <- fread("Classifications/health_region_crosswalk.csv")

get_standard_name <- function(x) if(x %in% HA_crosswalk$case_data_name) HA_crosswalk[case_data_name==x, display_name] else x
get_standard_names <- function(theList) unlist(lapply(theList, get_standard_name))

remove_CA_name <- function(x)
{
    if(grepl("cma", x)) return("cma")
    if(grepl("ca", x)) return("ca")
    return(x)
}
remove_CA_names <- function(theList) unlist(lapply(theList, remove_CA_name))

strip_region_type <- function(x) trimws(strsplit(as.character(x), '\\(')[[1]][1])
strip_region_types <- function(theList) unlist(lapply(theList, strip_region_type))

########################################## DATA ACQUISITION

UofT_api_case_data <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases %>%
    mutate(date_report=as.Date(date_report, format="%d-%m-%Y")) %>%
   data.table()
fwrite(UofT_api_case_data, "CaseDataTables/UofT_cases.csv")

Case_Data <- data.table(province=character(), region=character(), date=numeric(), cases=numeric())

BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") %>%
    select(-c(Province, HA, Cases_Reported_Smoothed)) %>%
    filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) %>%
    rename(date=Date, HA=HSDA, cases=Cases_Reported) %>%
    mutate(date=as.Date(date), province="British Columbia")
fwrite(BC_cases, "CaseDataTables/BC_cases.csv")

QC_cases <- fread("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv") %>%
    select(Date, Nom, cas_quo_tot_n) %>%
    filter(grepl("\\d", Date) & grepl("\\d - [a-zA-Z]", Nom)) %>%
    mutate(Nom = Nom %>% replace_accents %>% trim_numbers) %>%
    rename(date=Date, HA=Nom, cases=cas_quo_tot_n) %>%
    data.table %>%
    mutate(date=as.Date(date), province="Quebec")
fwrite(QC_cases, "CaseDataTables/QC_cases.csv")

MB_cases <- UofT_api_case_data %>%
    filter(province == "Manitoba") %>%
    select(province, date_report, health_region, cases) %>%
    rename(date=date_report, HA=health_region) %>%
    mutate(date=as.Date(date), province="Manitoba")
fwrite(MB_cases, "CaseDataTables/MB_cases.csv")

SK_cases <- get_saskatchewan_case_data() %>%
    select("Date", "Region", "New Cases") %>%
    rename(date=Date, HA=Region, cases="New Cases") %>%
    mutate(date=as.Date(date), province="Saskatchewan") %>%
    data.table
fwrite(SK_cases, "CaseDataTables/SK_cases.csv")

province_rename_helper <- function(x)
{
    if(x=="PEI") return("Prince Edward Island")
    if(x=="NWT") return("Northwest Territories")
    return(x)
}
rename_PEI_and_NWT <- function(theList) unlist(lapply(theList, province_rename_helper))
Territories_cases <- UofT_api_case_data %>%
    filter(province %in% c("PEI", "Yukon", "NWT", "Nunavut")) %>%
    select(province, date_report, health_region, cases) %>%
    rename(date=date_report, HA=health_region) %>%
    mutate(province=rename_PEI_and_NWT(province), HA=rename_PEI_and_NWT(HA), date=as.Date(date))
fwrite(Territories_cases, "CaseDataTables/Territories_cases.csv")

# Ontario data for confirmed cases - useful for delay distribution, etc
# https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f
ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") %>%
    select(-Total) %>%
    melt(., id.vars="Date") %>%
    rename(date=Date, HA=variable, cases=value) %>%
    mutate(province="Ontario") %>%
    data.table
fwrite(ON_cases, "CaseDataTables/ON_cases.csv")

# Alberta data from https://www.alberta.ca/stats/covid-19-alberta-statistics.htm#data-export
AB_cases <- fread("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") %>%
    select(-V1) %>%
    rename(date="Date reported", HA="Alberta Health Services Zone", type="Case type", age="Age group", status="Case status") %>%
    mutate(date=as.Date(date)) %>%
    data.table
AB_cases <- AB_cases[HA!="Unknown", .N, by=c("date", "HA")][order(date)] %>%
    rename(cases=N) %>% mutate(province="Alberta")
fwrite(AB_cases, "CaseDataTables/AB_cases.csv")

NB_cases <- UofT_api_case_data %>%
    filter(province=="New Brunswick") %>%
    select(province, date_report, health_region, cases) %>%
    rename(date=date_report, HA=health_region) %>%
    mutate(date=as.Date(date))
fwrite(NB_cases, "CaseDataTables/NB_cases.csv")

NS_cases <- UofT_api_case_data %>%
    filter(province=="Nova Scotia") %>%
    select(province, date_report, health_region, cases) %>%
    rename(date=date_report, HA=health_region) %>%
    mutate(date=as.Date(date))
fwrite(NS_cases, "CaseDataTables/NS_cases.csv")

NL_cases <- UofT_api_case_data %>%
    filter(province=="NL") %>%
    select(province, date_report, health_region, cases) %>%
    rename(date=date_report, HA=health_region) %>%
    mutate(date=as.Date(date), province="Newfoundland and Labrador")
fwrite(NL_cases, "CaseDataTables/NL_cases.csv")

# CSD_info <-  tibble()
# for(csd_number in list_of_levels %>% filter(level=="CSD") %>% pull(region))
# {
#     CSD_info <- rbind(
#         CSD_info,
#         get_census(
#             dataset="CA16",
#             regions=list(CMA=csd_number),
#             vectors=sprintf("v_CA16_%i", 492:501),
#             level="CSD"
#         )
#     )
# }
# write.csv(CSD_info, "Classifications/csd_information.csv")

