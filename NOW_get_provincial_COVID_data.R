library(dplyr)
library(data.table)
library(cancensus)
library(CanCovidData)
library(stringr)
# library(readxl)
library(XLConnect)

setwd('/home/bren/Documents/GitHub/MIZ_project/')

source("NOW_function_header.R")

################################## ALL CANADA

UofT_api_case_data <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases |>
    dplyr::mutate(date_report=as.Date(date_report, format="%d-%m-%Y"))
    fwrite(UofT_api_case_data, "CaseDataTables/UofT_cases.csv")

jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=avaccine&loc=prov")$avaccine |>
    dplyr::mutate(date_vaccine_administered=as.Date(date_vaccine_administered, format="%d-%m-%Y")) |>
    fwrite("CaseDataTables/UofT_vaccines.csv")

# only give the weekend date for the administration of the vaccine, not the actual date
fread("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex.csv") |>
    fwrite("CaseDataTables/canada_wide_vacc_data_official.csv")

fread("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byVaccineType.csv") |>
    fwrite("CaseDataTables/canada_vacc_coverage_by_vaccine.csv")

################################## BRITISN COLUMBIA

BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") |>
    dplyr::select(-c(Province, HA, Cases_Reported_Smoothed)) |>
    dplyr::filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) |>
    dplyr::rename(date=Date, HR=HSDA, cases=Cases_Reported) |>
    dplyr::mutate(date=as.Date(date), province="British Columbia") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(BC_cases, "CaseDataTables/BC_cases.csv")

################################## QUEBEC

QC_cases <- fread("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv") |>
    dplyr::select(Date, Nom, cas_quo_tot_n) |>
    dplyr::filter(grepl("\\d", Date) & grepl("\\d - [a-zA-Z]", Nom)) |>
    dplyr::mutate(Nom = Nom |> replace_accents() |> trim_numbers()) |>
    dplyr::rename(date=Date, HR=Nom, cases=cas_quo_tot_n) |>
    dplyr::mutate(date=as.Date(date), province="Quebec") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(QC_cases, "CaseDataTables/QC_cases.csv")

##################################  MANITOBA

MB_cases <- UofT_api_case_data |>
    dplyr::filter(province == "Manitoba") |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(date=as.Date(date), province="Manitoba") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(MB_cases, "CaseDataTables/MB_cases.csv")

################################## SASKATCHEWAN

SK_cases <- get_saskatchewan_case_data() |>
    dplyr::select("Date", "Region", "New Cases") |>
    dplyr::rename(date=Date, HR=Region, cases="New Cases") |>
    dplyr::mutate(date=as.Date(date), province="Saskatchewan") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(SK_cases, "CaseDataTables/SK_cases.csv")

################################## PRINCE EDWARD ISLAND, NORTHWEST TERRITORIES, YUKON, NUNAVUT

rename_PEI_and_NWT <- \(theList) unlist(lapply(theList, province_rename_helper))
PEI_NWT_YU_NU_cases <- UofT_api_case_data |>
    dplyr::filter(province %in% c("PEI", "Yukon", "NWT", "Nunavut")) |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(province=rename_PEI_and_NWT(province), HR=rename_PEI_and_NWT(HR), date=as.Date(date)) |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(PEI_NWT_YU_NU_cases, "CaseDataTables/PEI_NWT_YU_NU_cases.csv")

################################## ONTARIO

# Ontario data for confirmed cases - useful for delay distribution, etc
# https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f
ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") |>
    dplyr::select(-Total) |>
    melt(id.vars="Date") |>
    dplyr::rename(date=Date, HR=variable, cases=value) |>
    dplyr::mutate(province = "Ontario", date=as.Date(date)) |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(ON_cases, "CaseDataTables/ON_cases.csv")

fread("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/8a89caa9-511c-4568-af89-7f2174b4378c/download/vaccine_doses.csv") |>
    fwrite("CaseDataTables/ON_vaxes.csv")

fread("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/775ca815-5028-4e9b-9dd4-6975ff1be021/download/vaccines_by_age.csv") |>
    dplyr::filter(! Agegroup %in% c("Adults_18plus", "Undisclosed_or_missing")) |> 
    dplyr::rename_with( \(x) gsub(" ", "_", x)) |> 
    dplyr::select(Date, Agegroup, Percent_at_least_one_dose) |>
    fwrite("CaseDataTables/ON_vaxes_by_age.csv")

################################## ALBERTA

# Alberta data from https://www.alberta.ca/stats/covid-19-alberta-statistics.htm#data-export
AB_cases <- fread("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") |>
    dplyr::select(-V1) |>
    dplyr::rename(date="Date reported", HR="Alberta Health Services Zone", type="Case type", age="Age group", status="Case status") |>
    dplyr::mutate(date=as.Date(date)) |>
    dplyr::filter(HR!="Unknown") |> 
    dplyr::group_by(date, HR) |> 
    dplyr::tally() |> 
    dplyr::rename(cases=n) |> 
    dplyr::mutate(province="Alberta") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(AB_cases, "CaseDataTables/AB_cases.csv")

################################## NEW BRUNSWICK

NB_cases <- UofT_api_case_data |>
    dplyr::filter(province=="New Brunswick") |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(date=as.Date(date)) |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(NB_cases, "CaseDataTables/NB_cases.csv")

################################## NOVA SCOTIA

NS_cases <- UofT_api_case_data |>
    dplyr::filter(province=="Nova Scotia") |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(date=as.Date(date)) |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(NS_cases, "CaseDataTables/NS_cases.csv")

################################## NEWFOUNDLAND LABRADOR

NL_cases <- UofT_api_case_data |>
    dplyr::filter(province=="NL") |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(date=as.Date(date), province="Newfoundland and Labrador") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(NL_cases, "CaseDataTables/NL_cases.csv")

################################## TOTAL DATA

Total_Case_Data <- rbind(BC_cases, QC_cases, MB_cases, SK_cases, PEI_NWT_YU_NU_cases, ON_cases, AB_cases, NB_cases, NS_cases, NL_cases) |> #
    mutate(HR = standard_HR_names(HR)) |>
    add_HRUIDs("HR", "province")
    fwrite(Total_Case_Data, "CaseDataTables/Total_Case_Data.csv")
