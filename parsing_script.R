rm(list=ls())

library(data.table)
library(dplyr)
library(RcppAlgos)
library(CanCovidData)
library(ggplot2)
library(janitor)
library(stringr)

setwd('/home/bren/Documents/GitHub/MIZ_project/')

replaceAccents <- function(string)
{
    accentedCharacters <- list("é"="e", "è"="e", "ô"="o", "Î"="I")
    for(index in 1:length(accentedCharacters))
    {
        string <- gsub(names(accentedCharacters)[index],  accentedCharacters[[1]], string)
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

############################### DATA CLEANING AND FINDING HEALTH AUTHORRITY LOOKUPS ############################### 

# https://www150.statcan.gc.ca/n1/pub/92-195-x/2011001/geo/prov/tbl/tbl8-eng.htm
province_LUT <- data.table(rbind(
    c("Newfoundland and Labrador", "N.L.",	"NL", "10", "Atlantic"),
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

#-----------------------------------------------------------------------------------------------------------------

raw_CMAs <- data.table(names=readLines("Classifications/raw_CMAs.txt"))

CMAs <- data.table(SGC2016=numeric(), CSD=numeric(), SAC=character(), HA_code=numeric())

for(index in 1:nrow(raw_CMAs))
{
    the_number <- str_extract(raw_CMAs$names[index], "[0-9]+")
    the_name <- gsub(paste0(the_number, " - "), '', raw_CMAs$names[index])

    if(nchar(the_number) == 3)
    {
        cma_name <- the_name
    } else {
        CMAs <- rbind(
            CMAs,
            list(
                SGC2016 = as.integer(the_number),
                CSD = the_name,
                SAC = sprintf("cma (%s)", cma_name),
                HA_code = substr(the_number, 1 ,4)
            )
        )
    }
}

#-----------------------------------------------------------------------------------------------------------------

raw_CAs <- data.table(names=readLines("Classifications/raw_CAs.txt"))

CAs <- data.table(SGC2016=numeric(), CSD=numeric(), SAC=character(), HA_code=numeric())

for(index in 1:nrow(raw_CAs))
{
    the_number <- str_extract(raw_CAs$names[index], "[0-9]+")
    the_name <- gsub(paste0(the_number, " - "), '', raw_CAs$names[index])

    if(nchar(the_number) == 3)
    {
        ca_name <- the_name
    } else {
        CAs <- rbind(
            CAs,
            list(
                SGC2016 = as.integer(the_number),
                CSD = the_name,
                SAC = sprintf("ca (%s)", ca_name),
                HA_code = substr(the_number, 1 ,4)
            )
        )
    }
}

#-----------------------------------------------------------------------------------------------------------------

MIZ_strong <- fread("Classifications/raw_MIZ_strong.csv") %>% mutate(SAC="strong")
MIZ_moderate <- fread("Classifications/raw_MIZ_moderate.csv") %>% mutate(SAC="moderate")
MIZ_weak <- fread("Classifications/raw_MIZ_weak.csv") %>% mutate(SAC="weak")
MIZ_none <- fread("Classifications/raw_MIZ_none.csv") %>% mutate(SAC="none")

extract_code <- function(x) as.numeric(paste0(head(strsplit(x, '')[[1]], 4), collapse=''))
extract_codes <- function(theList) unlist(lapply(as.character(theList), extract_code))

MIZs <-  rbind(MIZ_strong, MIZ_moderate, MIZ_weak, MIZ_none)
setnames(MIZs, "Code", "SGC2016")
setnames(MIZs, "Census subdivision", "CSD")
MIZs[, SGC2016:=lapply(SGC2016, str_extract, "[0-9]+")]
MIZs[, HA_code:=extract_codes(SGC2016)]

All_CSDs <- rbind(CMAs, CAs, MIZs %>% select(-Type)) %>%
    mutate(HA=lookup_HAs(HA_code)) %>%
    mutate(province=lookup_provinces(HA_code)) %>%
    arrange(HA_code)

# Saskatchewan gives COVID-19 data by geo-region rather than health authority, but the website
# gives a list, so we parse that and correct entries
SK_corr <- fread("grep -v '^#' Classifications/TRUE_SK_classification.csv", na.strings=c("", "NA")) %>% na.omit
# making the lookup table, searching by the name of the town
lookup_SK_HA <- function(the_town) if(the_town %in% SK_corr$town) SK_corr[town==the_town, HA] else ""
lookup_SK_HAs <- function(theList) unlist(lapply(theList, lookup_SK_HA))
# indices of all the Sask towns
Sask_indices <- which(All_CSDs$province=="Saskatchewan")
# run the lookup; a good few of them will be blank in this table due to the website not giving
# a comprehensive list of CSDs for each health region
All_CSDs[Sask_indices, HA:=lookup_SK_HAs(CSD)]

################################################### DATA ACQUISITION ################################################### 

UofT_api_case_data <- data.table(jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases)
fwrite(UofT_api_case_data, "CaseDataTables/UofT_cases.csv")
# 
# Case_Data <- data.table(province=character(), region=character(), date=numeric(), cases=numeric())
# 
# BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") %>%
#     select(-c(Province, HA, Cases_Reported_Smoothed)) %>%
#     filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) %>%
#     rename(date=Date, HA=HSDA, cases=Cases_Reported) %>%
#     mutate(date=as.Date(date), province="British Columbia")
# fwrite(BC_cases, "CaseDataTables/BC_cases.csv")
# 
# QC_cases <- fread("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv") %>%
#     select(Date, Nom, cas_quo_tot_n) %>%
#     filter(grepl("\\d", Date) & grepl("\\d - [a-zA-Z]", Nom)) %>%
#     mutate(Nom = Nom %>% replace_accents %>% trim_numbers) %>%
#     rename(date=Date, HA=Nom, cases=cas_quo_tot_n) %>%
#     data.table %>%
#     mutate(date=as.Date(date), province="Quebec")
# fwrite(QC_cases, "CaseDataTables/QC_cases.csv")
# 
# MB_cases <- UofT_api_case_data %>%
#     filter(province == "Manitoba") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date), province="Manitoba")
# fwrite(MB_cases, "CaseDataTables/MB_cases.csv")
# 
# SK_cases <- get_saskatchewan_case_data() %>%
#     select("Date", "Region", "New Cases") %>%
#     rename(date=Date, HA=Region, cases="New Cases") %>%
#     mutate(date=as.Date(date), province="Saskatchewan") %>%
#     data.table
# fwrite(SK_cases, "CaseDataTables/SK_cases.csv")
# 
# province_rename_helper <- function(x)
# {
#     if(x=="PEI") return("Prince Edward Island")
#     if(x=="NWT") return("Northwest Territories")
#     return(x)
# }
# rename_PEI_and_NWT <- function(theList) unlist(lapply(theList, province_rename_helper))
# Territories_cases <- UofT_api_case_data %>%
#     filter(province %in% c("PEI", "Yukon", "NWT", "Nunavut")) %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(province=rename_PEI_and_NWT(province), HA=rename_PEI_and_NWT(HA), date=as.Date(date))
# fwrite(Territories_cases, "CaseDataTables/Territories_cases.csv")
# 
# # Ontario data for confirmed cases - useful for delay distribution, etc
# # https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f
# ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") %>%
#     select(-Total) %>%
#     melt(., id.vars="Date") %>%
#     rename(date=Date, HA=variable, cases=value) %>%
#     mutate(province="Ontario") %>%
#     data.table
# fwrite(ON_cases, "CaseDataTables/ON_cases.csv")

# # Alberta data from https://www.alberta.ca/stats/covid-19-alberta-statistics.htm#data-export
# AB_cases <- fread("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") %>%
#     select(-V1) %>%
#     rename(date="Date reported", HA="Alberta Health Services Zone", type="Case type", age="Age group", status="Case status") %>%
#     mutate(date=as.Date(date)) %>%
#     data.table
# AB_cases <- AB_cases[HA!="Unknown", .N, by=c("date", "HA")][order(date)] %>%
#     rename(cases=N) %>% mutate(province="Alberta")
# fwrite(AB_cases, "CaseDataTables/AB_cases.csv")

# NB_cases <- UofT_api_case_data %>%
#     filter(province=="New Brunswick") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date))
# fwrite(NB_cases, "CaseDataTables/NB_cases.csv")
# 
# NS_cases <- UofT_api_case_data %>%
#     filter(province=="Nova Scotia") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date))
# fwrite(NS_cases, "CaseDataTables/NS_cases.csv")
# 
# NL_cases <- UofT_api_case_data %>%
#     filter(province=="NL") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date), province="Newfoundland and Labrador")
# fwrite(NL_cases, "CaseDataTables/NL_cases.csv")

Case_Data <- rbind(
    fread("CaseDataTables/BC_cases.csv"),
    fread("CaseDataTables/QC_cases.csv"),
    fread("CaseDataTables/MB_cases.csv"),
    fread("CaseDataTables/SK_cases.csv"),
    fread("CaseDataTables/Territories_cases.csv"),
    fread("CaseDataTables/ON_cases.csv"),
    fread("CaseDataTables/AB_cases.csv"),
    fread("CaseDataTables/NB_cases.csv"),
    fread("CaseDataTables/NS_cases.csv"),
    fread("CaseDataTables/NL_cases.csv")
)

######################################3 DATA PARSING ###################################

HA_crosswalk <- fread("Classifications/health_region_crosswalk.csv")

get_standard_name <- function(x) if(x %in% HA_crosswalk$case_data_name) HA_crosswalk[case_data_name==x, display_name] else x
get_standard_names <- function(theList) unlist(lapply(theList, get_standard_name))

Case_Data[, HA:=get_standard_names(HA)]
    
Scores <- data.table(province=character(), zone=character(), score=numeric(), total=numeric())

for(prov in intersect(unique(Case_Data$province), unique(All_CSDs$province)))
{
    Cases_Here <- Case_Data[province==prov]
    Classes_Here <- All_CSDs[province==prov]

    for(zone in unique(Classes_Here$HA))
    {
        cumul_sum <- Cases_Here[HA==zone, sum(cases)]

        temp_classes <- Classes_Here[HA==zone]

0        # written this way so that duplicate CMAs and CAs can be separated and single-counted, rather than double counted
        CMAs <- temp_classes[which(grepl("cma (*)", SAC)), SAC] # unique(temp_classes[which(grepl("cma (*)", temp))])
        CAs  <- temp_classes[which(grepl("ca (*)",  SAC)), SAC] # unique(temp[which(grepl("ca (*)", temp))])
        MIZs <- temp_classes[which(! SAC %in% c(CMAs,  CAs)), SAC]

        numerator <- length(CMAs)*zone_scores("cma") + length(CAs)*zone_scores("ca") + sum(zone_scores(MIZs))
        denominator <- length(CMAs) + length(CAs) + length(MIZs)

        final_score <- numerator # /denominator

        Scores <- rbind(Scores, list(prov, zone, final_score, cumul_sum))
    }
}

print(plot(Scores$score, Scores$total))   
    
    
    
    
    



