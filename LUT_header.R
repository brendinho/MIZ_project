library(dplyr)
library(data.table)
library(cancensus)
library(CanCovidData)
library(stringr)

setwd('/home/bren/Documents/GitHub/MIZ_project/')

# Saskatchewan gives COVID-19 data by geo-region rather than health authority, but the website
# gives a list, so we parse that and correct entries
SK_corr <- fread("grep -v '^#' Classifications/TRUE_SK_classification.csv", na.strings=c("", "NA")) |> na.omit()
# making the lookup table, searching by the name of the town
lookup_SK_HA <- \(the_town) if(the_town %in% SK_corr$town) SK_corr[town==the_town, HA] else ""
lookup_SK_HAs <- \(theList) unlist(lapply(theList, lookup_SK_HA))

HA_crosswalk <- fread("Classifications/health_region_crosswalk.csv") %>%
    mutate(
        case_data_name = trimws(case_data_name),
        display_name := trimws(display_name)
    )

get_standard_name <- \(x) if(trimws(x) %in% HA_crosswalk$case_data_name) HA_crosswalk[case_data_name==trimws(x), display_name] else trimws(x)
get_standard_names <- \(theList) unlist(lapply(theList, get_standard_name))


########################################## DATA ACQUISITION

UofT_api_case_data <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases |>
    mutate(date_report=as.Date(date_report, format="%d-%m-%Y")) |>
   data.table()
fwrite(UofT_api_case_data, "CaseDataTables/UofT_cases.csv")

Case_Data <- data.table(province=character(), region=character(), date=numeric(), cases=numeric())

BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") |>
    select(-c(Province, HA, Cases_Reported_Smoothed)) |>
    filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) |>
    rename(date=Date, HA=HSDA, cases=Cases_Reported) |>
    mutate(date=as.Date(date), province="British Columbia")
fwrite(BC_cases, "CaseDataTables/BC_cases.csv")

QC_cases <- fread("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv") |>
    select(Date, Nom, cas_quo_tot_n) |>
    filter(grepl("\\d", Date) & grepl("\\d - [a-zA-Z]", Nom)) |>
    mutate(Nom = Nom |> replace_accents() |> trim_numbers()) |>
    rename(date=Date, HA=Nom, cases=cas_quo_tot_n) |>
    data.table() |>
    mutate(date=as.Date(date), province="Quebec")
fwrite(QC_cases, "CaseDataTables/QC_cases.csv")

MB_cases <- UofT_api_case_data |>
    filter(province == "Manitoba") |>
    select(province, date_report, health_region, cases) |>
    rename(date=date_report, HA=health_region) |>
    mutate(date=as.Date(date), province="Manitoba")
fwrite(MB_cases, "CaseDataTables/MB_cases.csv")

SK_cases <- get_saskatchewan_case_data() |>
    select("Date", "Region", "New Cases") |>
    rename(date=Date, HA=Region, cases="New Cases") |>
    mutate(date=as.Date(date), province="Saskatchewan") |>
    data.table()
fwrite(SK_cases, "CaseDataTables/SK_cases.csv")

rename_PEI_and_NWT <- \(theList) unlist(lapply(theList, province_rename_helper))
Territories_cases <- UofT_api_case_data |>
    filter(province %in% c("PEI", "Yukon", "NWT", "Nunavut")) |>
    select(province, date_report, health_region, cases) |>
    rename(date=date_report, HA=health_region) |>
    mutate(province=rename_PEI_and_NWT(province), HA=rename_PEI_and_NWT(HA), date=as.Date(date))
fwrite(Territories_cases, "CaseDataTables/Territories_cases.csv")

# Ontario data for confirmed cases - useful for delay distribution, etc
# https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f
ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") |>
    select(-Total) |>
    melt(id.vars="Date") |>
    rename(date=Date, HA=variable, cases=value) |>
    mutate(province="Ontario") |>
    data.table()
fwrite(ON_cases, "CaseDataTables/ON_cases.csv")

# Alberta data from https://www.alberta.ca/stats/covid-19-alberta-statistics.htm#data-export
AB_cases <- fread("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") |>
    select(-V1) |>
    rename(date="Date reported", HA="Alberta Health Services Zone", type="Case type", age="Age group", status="Case status") |>
    mutate(date=as.Date(date)) |>
    data.table()
AB_cases <- AB_cases[HA!="Unknown", .N, by=c("date", "HA")][order(date)] |>
    rename(cases=N) |> mutate(province="Alberta")
fwrite(AB_cases, "CaseDataTables/AB_cases.csv")

NB_cases <- UofT_api_case_data |>
    filter(province=="New Brunswick") |>
    select(province, date_report, health_region, cases) |>
    rename(date=date_report, HA=health_region) |>
    mutate(date=as.Date(date))
fwrite(NB_cases, "CaseDataTables/NB_cases.csv")

NS_cases <- UofT_api_case_data |>
    filter(province=="Nova Scotia") |>
    select(province, date_report, health_region, cases) |>
    rename(date=date_report, HA=health_region) |>
    mutate(date=as.Date(date))
fwrite(NS_cases, "CaseDataTables/NS_cases.csv")

NL_cases <- UofT_api_case_data |>
    filter(province=="NL") |>
    select(province, date_report, health_region, cases) |>
    rename(date=date_report, HA=health_region) |>
    mutate(date=as.Date(date), province="Newfoundland and Labrador")
fwrite(NL_cases, "CaseDataTables/NL_cases.csv")

# comprehensive health region lookup file
# "https://www150.statcan.gc.ca/pub/82-402-x/2018001/corr/Comprehensive_HR2018_16.csv"

HR_info <- tibble(read.csv("Classifications/Comprehensive_HR2018_16.csv")) |> 
    select(-DBUID2016, -DBPOP2016, -FRENAME) |>
    mutate(ENGNAME = replace_accents(ENGNAME)) |>
    unique()

lookup_HR <- function(x)
{
    if(x %in% unique(HR_info$CSDUID2016)) 
    {
        temp <- HR_info |> filter(CSDUID2016 == x) |> pull(ENGNAME) |> as.character()
        if(length(temp) > 1) "duplicate" else temp
        
    } else { "missing" }
}





