rm(list=ls())

library(dplyr)
library(data.table)
library(jsonlite)
library(cancensus)
library(CanCovidData)
library(stringr)
library(XLConnect)

setwd('/home/bren/Documents/GitHub/MIZ_project/')
dir.create(file.path(".", "CaseDataFiles"), showWarnings=FALSE)
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source("function_header.R")

############################################ RETRIEVE ALL SAC INFO - geography files may be cached

writeLines("\nCSD census information and geographies")
list_of_levels <- list_census_regions("CA16")
CSD_info <-  data.table()
for(csd_number in list_of_levels |> filter(level=="CSD") |> pull(region))
{
    CSD_info <- rbind(
        CSD_info,
        get_census(
            dataset = "CA16",
            regions = list(CMA = csd_number),
            vectors = c(
                sprintf("v_CA16_%i", 478:482), # crude family size
                sprintf("v_CA16_%i", 492:501), # household structure information
                sprintf("v_CA16_57%i", c(80, 83, 86, 89)), # commuting information
                "v_CA16_406"
            ),
            level = "CSD",
            geo_format = 'sf',
            labels = 'short',
            # use_cache = FALSE
        ),
        fill = TRUE
    )
}
saveRDS(CSD_info, "Classifications/raw_csd_information.rda")

writeLines("\nassembling and renaming CSD table")
CSD_info <- readRDS("Classifications/raw_csd_information.rda") |>
    dplyr::mutate(province = unlist(lapply( GeoUID, lookup_province )) ) |>
    dplyr::rename(
        area_sq_km = "Area (sq km)",
        number_of_families = "v_CA16_478",
        number_of_families_of_size_2 = "v_CA16_479",
        number_of_families_of_size_3 = "v_CA16_480",
        number_of_families_of_size_4 = "v_CA16_481",
        number_of_families_of_size_5_or_larger = "v_CA16_482",
        population_density = "v_CA16_406", # :  Population density per square kilometre
        couples_with_children = "v_CA16_493",
        couples_with_0_children = "v_CA16_492",
        couples_with_1_child = "v_CA16_494",
        couples_with_2_children =  "v_CA16_495",
        couples_with_3_or_more_children = "v_CA16_496",
        singles_with_children = "v_CA16_497",
        singles_with_1_child = "v_CA16_498",
        singles_with_2_children = "v_CA16_499",
        singles_with_3_or_more_children = "v_CA16_500",
        people_not_in_census_families = "v_CA16_501",
        people_commuting_within_CSD = "v_CA16_5780",
        people_commuting_within_CD_but_not_CSD = "v_CA16_5783",
        people_commuting_within_province_but_not_CD = "v_CA16_5786",
        people_commuting_outside_province = "v_CA16_5789",
    ) |>
    dplyr::mutate(
        region = strip_region_types(`Region Name`), 
        csd_type = get_region_types(`Region Name`),
        GeoUID = as.numeric(GeoUID)
    ) |>
    dplyr::select(-`Region Name`) |>
    dplyr::relocate(GeoUID, region, province, csd_type)

writeLines("\nreading and parsing local SAC info")

# https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314313&CPV=A&CST=01012016&CLV=1&MLV=3
raw_CMAs <- data.table(names=readLines("Classifications/raw_CMAs.txt"))
CMAs <- data.table(GeoUID=numeric(), region=numeric(), title=character(), class=character()) # , sac_code=numeric(), cma_type=character()
for(index in 1:nrow(raw_CMAs))
{
    the_number <- str_extract(raw_CMAs$names[index], "[0-9]+")
    the_name <- gsub(paste0(the_number, " - "), '', raw_CMAs$names[index])
    
    if(nchar(the_number) == 3)
    {
        cma_name <- gsub("--", "-", the_name)
        cma_number <-  the_number
    } else {
        CMAs <- rbind(CMAs, list(GeoUID = as.integer(the_number), region = the_name, class = "CMA", title =  cma_name))
    }
}

# https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314313&CPV=B&CST=01012016&CLV=1&MLV=3
raw_CAs <- data.table(names=readLines("Classifications/raw_CAs.txt"))
CAs <- data.table(GeoUID=numeric(), region=numeric(), title=character(), class=character()) # , sac_code=numeric(), cma_type=character(), sac_type=character()
for(index in 1:nrow(raw_CAs))
{
    the_number <- str_extract(raw_CAs$names[index], "[0-9]+")
    the_name <- gsub(paste0(the_number, " - "), '', raw_CAs$names[index])
    
    if(nchar(the_number) == 3)
    {
        ca_name <- gsub("--", "-", the_name)
        ca_number <- the_number
    } else {
        CAs <- rbind(CAs, list(GeoUID = as.integer(the_number),region = the_name,class = "CA",title = ca_name))
    }
}

# https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314314&CPV=996&CST=01012016&CLV=2&MLV=3
MIZ_strong <- {fread("Classifications/raw_MIZ_strong.csv") |>
        dplyr::mutate(
            title = "Strong MIZ",
            GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
            region = unlist(lapply(Code, str_sub, start=8)),
            class = "Strong"
        ) |>
        dplyr::select(-Code, -`Census subdivision`)}

# https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314314&CPV=997&CST=01012016&CLV=2&MLV=3
MIZ_moderate <- {fread("Classifications/raw_MIZ_moderate.csv") |> 
        dplyr::mutate(
            title = "Moderate MIZ",
            GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
            region = unlist(lapply(Code, str_sub, start=8)),
            class = "Moderate"
        ) |>
        dplyr::select(-Code, -`Census subdivision`)}

# http://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314314&CPV=998&CST=01012016&CLV=2&MLV=3
MIZ_weak <- {fread("Classifications/raw_MIZ_weak.csv") |>
        dplyr::mutate(
            title = "Weak MIZ",
            GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
            region = unlist(lapply(Code, str_sub, start=8)),
            class = "Weak"
        ) |>
        dplyr::select(-Code, -`Census subdivision`)}

# http://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314314&CPV=999&CST=01012016&CLV=2&MLV=3
MIZ_none <- {fread("Classifications/raw_MIZ_none.csv") |>
        dplyr::mutate(
            title = "No MIZ",
            GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
            region = unlist(lapply(Code, str_sub, start=8)),
            class = "None"
        ) |>
        dplyr::select(-Code, -`Census subdivision`)}

Influence_info <- rbind(CMAs, CAs, MIZ_strong, MIZ_moderate, MIZ_weak, MIZ_none, fill=T) |>
    dplyr::mutate(GeoUID = as.integer(GeoUID))

writeLines("\nStatCan Index of Remoteness scores")
Index_of_Remoteness <- fread("Classifications/Index_of_remoteness.csv") |>
    dplyr::select(CSDuid, Index_of_remoteness) |>
    dplyr::rename(GeoUID = CSDuid) |>
    dplyr::mutate(Index_of_remoteness = as.numeric(Index_of_remoteness)) |>
    suppressWarnings()

# assemble the complete table we need for the correlation before

writeLines("\nassembling Total Geography Table")
Total_Data_Geo <- data.table(Reduce(
    function(x, y, ...) merge(x, y, by=c("GeoUID"), all = TRUE, ...),
    list(CSD_info, Influence_info, Remoteness)
)) |>
    dplyr::rename(CSDUID2016 = GeoUID) |>
    dplyr::mutate(
        region = coalesce(region.y, region.x),
        class  = ifelse(is.na(class), "NA", class)
    ) |>
    dplyr::rename_with(tolower) |>
    dplyr::select(-region.x, -region.y, -title, -type.y, -type.x, -cma_uid, -name) |>
    add_HRs("csduid2016", "province") |>
    dplyr::relocate("csduid2016", "region", "province", "HR", "class", "index_of_remoteness", "population_density")
    saveRDS(Total_Data_Geo, "Classifications/Total_CSD_Info.rda")

################################## COVID DATA

writeLines("\nAPI data - cases")
UofT_api_case_data <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases |>
    dplyr::mutate(date_report=as.Date(date_report, format="%d-%m-%Y"))
    fwrite(UofT_api_case_data, "CaseDataTables/UofT_cases.csv")

writeLines("\nAPI data - vaccines")
jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=avaccine&loc=prov")$avaccine |>
    dplyr::mutate(date_vaccine_administered=as.Date(date_vaccine_administered, format="%d-%m-%Y")) |>
    fwrite("CaseDataTables/UofT_vaccines.csv")

writeLines("\nCanada-wide vaccine coverage - stratified0")
# only give the weekend date for the administration of the vaccine, not the actual date
fread("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex.csv") |>
    fwrite("CaseDataTables/canada_wide_vacc_data_official.csv")
fread("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byVaccineType.csv") |>
    fwrite("CaseDataTables/canada_vacc_coverage_by_vaccine.csv")

################################## BRITISH COLUMBIA

writeLines("\nBritish Columbia")
BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") |>
    dplyr::select(-c(Province, HA, Cases_Reported_Smoothed)) |>
    dplyr::filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) |>
    dplyr::rename(date=Date, HR=HSDA, cases=Cases_Reported) |>
    dplyr::mutate(date=as.Date(date), province="British Columbia") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(BC_cases, "CaseDataTables/BC_cases.csv")

################################## QUEBEC

writeLines("\nQuebec")
QC_cases <- fread("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv") |>
    dplyr::select(Date, Nom, cas_quo_tot_n) |>
    dplyr::filter(grepl("\\d", Date) & grepl("\\d - [a-zA-Z]", Nom)) |>
    dplyr::mutate(Nom = Nom |> replace_accents() |> trim_numbers()) |>
    dplyr::rename(date=Date, HR=Nom, cases=cas_quo_tot_n) |>
    dplyr::mutate(date=as.Date(date), province="Quebec") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(QC_cases, "CaseDataTables/QC_cases.csv")

##################################  MANITOBA

writeLines("\nManitoba")
MB_cases <- UofT_api_case_data |>
    dplyr::filter(province == "Manitoba") |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(date=as.Date(date), province="Manitoba") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(MB_cases, "CaseDataTables/MB_cases.csv")

################################## SASKATCHEWAN

writeLines("\nSaskatchewan")
SK_cases <- get_saskatchewan_case_data() |>
    dplyr::select("Date", "Region", "New Cases") |>
    dplyr::rename(date=Date, HR=Region, cases="New Cases") |>
    dplyr::mutate(date=as.Date(date), province="Saskatchewan") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(SK_cases, "CaseDataTables/SK_cases.csv")

################################## PRINCE EDWARD ISLAND, NORTHWEST TERRITORIES, YUKON, NUNAVUT

writeLines("\nPEI, NWT, Yukon, Nunavut")
rename_PEI_and_NWT <- \(theList) unlist(lapply(theList, province_rename_helper))
PEI_NWT_YU_NU_cases <- UofT_api_case_data |>
    dplyr::filter(province %in% c("PEI", "Yukon", "NWT", "Nunavut")) |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(province=rename_PEI_and_NWT(province), HR=rename_PEI_and_NWT(HR), date=as.Date(date)) |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(PEI_NWT_YU_NU_cases, "CaseDataTables/PEI_NWT_YU_NU_cases.csv")

################################## ONTARIO

writeLines("\nOntario - cases")
# Ontario data for confirmed cases - useful for delay distribution, etc
# https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f
ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") |>
    dplyr::select(-Total) |>
    melt(id.vars="Date") |>
    dplyr::rename(date=Date, HR=variable, cases=value) |>
# dplyr::mutate(HR = standard_HR_names(HR)) |>
    dplyr::mutate(province = "Ontario", date=as.Date(date)) |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(ON_cases, "CaseDataTables/ON_cases.csv")
    
writeLines("\nOntario - vaccination")
fread("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/8a89caa9-511c-4568-af89-7f2174b4378c/download/vaccine_doses.csv") |>
    fwrite("CaseDataTables/ON_vaxes.csv")

writeLines("\nOntario - vaccination by age")
fread("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/775ca815-5028-4e9b-9dd4-6975ff1be021/download/vaccines_by_age.csv") |>
    dplyr::filter(! Agegroup %in% c("Adults_18plus", "Undisclosed_or_missing")) |> 
    dplyr::rename_with( \(x) gsub(" ", "_", x)) |> 
    dplyr::select(Date, Agegroup, Percent_at_least_one_dose) |>
    fwrite("CaseDataTables/ON_vaxes_by_age.csv")

################################## ALBERTA

writeLines("\nAlberta")
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

writeLines("\nNew Brunswick")
NB_cases <- UofT_api_case_data |>
    dplyr::filter(province=="New Brunswick") |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(date=as.Date(date)) |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(NB_cases, "CaseDataTables/NB_cases.csv")

################################## NOVA SCOTIA

writeLines("\nNova Scotia")
NS_cases <- UofT_api_case_data |>
    dplyr::filter(province=="Nova Scotia") |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(date=as.Date(date)) |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(NS_cases, "CaseDataTables/NS_cases.csv")

################################## NEWFOUNDLAND LABRADOR

writeLines("\nNewfoundland and Labrador")
NL_cases <- UofT_api_case_data |>
    dplyr::filter(province=="NL") |>
    dplyr::select(province, date_report, health_region, cases) |>
    dplyr::rename(date=date_report, HR=health_region) |>
    dplyr::mutate(date=as.Date(date), province="Newfoundland and Labrador") |>
    add_wave_numbers() %>% .[['cases']]
    fwrite(NL_cases, "CaseDataTables/NL_cases.csv")

################################## TOTAL DATA

writeLines("\nTotal Case Data")
Total_Case_Data <- rbind(BC_cases, QC_cases, MB_cases, SK_cases, PEI_NWT_YU_NU_cases, ON_cases, AB_cases, NB_cases, NS_cases, NL_cases) |>
    dplyr::mutate(HR = standard_HR_names(HR)) |>
    add_HRUIDs("HR", "province")
    fwrite(Total_Case_Data, "CaseDataTables/Total_Case_Data.csv")
    
writeLines("\nMeasures of Remoteness")
# read the MIZ score calculation, here using the classification and aggregation given in the MIZ paper from Seyed
Regions <- readRDS("Classifications/Total_CSD_Info.rda") |>
    dplyr::filter(population != 0) |>
    dplyr::select(-csd_type, -cd_uid) |>
    dplyr::mutate(
        index_of_remoteness = unlist(lapply(index_of_remoteness, \(x) if(x==".") NA else as.numeric(x) )),
        num_csds = 1,
        province = factor(province), 
        region = factor(region),
        class = fct_relevel(class, "CMA", "CA", "Strong", "Moderate", "Weak", "None", "NA"),
        csduid2016 = factor(csduid2016),
        aR_score = as.integer(unlist(lapply(class, CSD_score_class))),
    )
    saveRDS(Regions, "CaseDataTables/Regions.rda")

writeLines("\nCumulative cases")
Cumul_Cases <- Total_Case_Data[
        HR!="Not Reported",
        .(
            first_wave_cases=sum(.SD[wave==1]$cases, na.rm=T),
            second_wave_cases=sum(.SD[wave==2]$cases, na.rm=T),
            total_cases=sum(cases, na.rm=T)
        ),
        by=.(HR, province, HRUID2018)
    ]

writeLines("\nTimes to peak")
Time_to_Peak <- merge(
        Total_Case_Data[HR!="Not Reported", .SD[wave==1][which.max(cases)], by=.(HRUID2018)] |>
            dplyr::mutate(first_wave_days_since=date-as.Date('2020-01-23')) |>
            dplyr::rename(first_wave_peak_date=date, first_wave_peak_cases=cases) |>
            dplyr::select(-wave),
        Total_Case_Data[HR!="Not Reported", .SD[wave==2][which.max(cases)], by=.(HRUID2018)] |>
            dplyr::mutate(second_wave_days_since=date-as.Date('2020-01-23'))|>
            dplyr::rename(second_wave_peak_date=date, second_wave_peak_cases=cases) |>
            dplyr::select(-wave),
        by=c("HRUID2018", "HR", "province")
    ) |>
    dplyr::mutate(interpeak_distance=as.numeric(second_wave_days_since-first_wave_days_since))

writeLines("\nRemoteness")
Remoteness <- Regions[,
        lapply(.SD, sum, na.rm=TRUE),
        .SDcols = setdiff(
            names(Regions),
            c(
                "csduid2016", "region", "province", "HR", "class", "population_density", 
                "HRUID2018", "csd_type", "pr_uid", "cd_uid", "geometry"
            )
        ),
        by=.(HR, HRUID2018, province, pr_uid)
    ]

writeLines("\nTotal Data - adding HRs and saving")
Total_Data <- Reduce(
        function(...) merge(..., all = TRUE, by = c("HR", "HRUID2018", "province")),
        list(Remoteness, Time_to_Peak, Cumul_Cases)
    ) |>
    dplyr::mutate(
        first_wave_proportion = first_wave_cases/population,
        second_wave_proportion = second_wave_cases/population,
        total_proportion = first_wave_proportion + second_wave_proportion,
        total_commuters = people_commuting_within_csd + people_commuting_within_cd_but_not_csd + people_commuting_within_province_but_not_cd +
            people_commuting_outside_province,
        people_commuting_outside_their_csd = people_commuting_within_cd_but_not_csd + people_commuting_within_province_but_not_cd + 
            people_commuting_outside_province,
        people_commuting_within_province = people_commuting_within_csd + people_commuting_within_cd_but_not_csd + 
            people_commuting_within_province_but_not_cd,
        HR = factor(HR),
        HRUID2018 = factor(HRUID2018),
    )
    fwrite(Total_Data, "CaseDataTables/Total_Data.csv")

    
    
