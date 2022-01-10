rm(list=ls())

# for some reason, running the complete script in RStudio results in the interventions
# data table being read and parsed incorrectly. I'm not sure why, gut I'll find a way
# around that for the purpose for the GitHub

library(dplyr)
library(data.table)
library(jsonlite)
library(cancensus)
library(CanCovidData)
library(stringr)
library(sf)
library(ggplot2)
library(vistime)
library(sf)
library(plotly)
# library(doParallel)

if(getElement(Sys.info(), "sysname") == "Windows"){
    PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
    PROJECT_FOLDER <- "/home/bren/Documents/GitHub/MIZ_project"
}

library(openxlsx)

setwd(PROJECT_FOLDER)

dir.create(file.path(".", "CaseDataFiles"), showWarnings=FALSE)
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source(sprintf("%s/function_header.R", PROJECT_FOLDER))

start_time <- Sys.time()

#####################################
############## TIME-INVARIANT METRICS
#####################################

if(! file.exists(file.path(PROJECT_FOLDER, "Classifications/CSD_age_cohorts.csv")))
{
    province_folder_map <- data.table(
        number = 61:73, 
        alpha = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", 
                  "AB", "BC", "YT", "NT", "NU"), 
        province = c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", 
             "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", 
             "British Columbia", "Yukon", "Northwest Territories", "Nunavut"
        )
    )
    
    the_profiles <- list()
    
    for(province_folder in Sys.glob(file.path(PROJECT_FOLDER, "98-401*")) %>% .[!grepl(".zip", .)])
    {
        for(file in Sys.glob(file.path(province_folder, "*_CSV_*")))
        {
            folder_number <- file %>% 
                basename %>% 
                gsub("98-401-X20160|_English_CSV_data.csv", "", .) %>% 
                as.numeric
            
            the_profiles[[file]] <- cbind(
                fread(file), 
                file_name = file, 
                alpha = province_folder_map[number == folder_number, alpha],
                province = province_folder_map[number == folder_number, province]
            )
        }
    }
    
    prepare_CSD_table <- function(theTable, geoCodes)
    {
        profiles <- list()
        
        # num_cores <- detectCores(logical = TRUE)
        # cl <- makeCluster(num_cores-2)  
        # registerDoParallel(cl) 
    
        # foreach(code=geoCodes, .packages='dplyr') %dopar%
        for(code in geoCodes)
        {
            print(code)
            
            if(! code %in% unique(theTable$geo_code)) next
    
            temp <- theTable %>% dplyr::filter(geo_code == code)
    
            subset_table <- temp %>%
                dplyr::pull(attribute) %>%
                grepl("Total_-_Age_|Total_-_Distribution", .) %>%
                which() %>%
                .[1:2] %>%
                {.[1] : .[2]} %>%
                temp[.,] %>%
                dplyr::mutate(value = value_if_number_else_NA(value))
    
            sum_cohorts <- \(...) subset_table %>%
                dplyr::filter(attribute %in% list(...)) %>%
                dplyr::tally(value) %>%
                dplyr::pull(n)
    
            profiles[[code]] <- data.table(
                    alpha = unique(subset_table$alpha),
                    province = unique(subset_table$province),
                    geo_code = unique(subset_table$geo_code),
                    geo_name = unique(subset_table$geo_name),
                    cohort_0_to_4 = sum_cohorts("0_to_4_years"),
                    cohort_5_to_9 = sum_cohorts("5_to_9_years"),
                    cohort_10_to_14 = sum_cohorts("10_to_14_years"),
                    cohort_15_to_19 = sum_cohorts("15_to_19_years"),
                    cohort_20_to_24 = sum_cohorts("20_to_24_years"),
                    cohort_25_to_29 = sum_cohorts("25_to_29_years"),
                    cohort_30_to_34 = sum_cohorts("30_to_34_years"),
                    cohort_35_to_39 = sum_cohorts("35_to_39_years"),
                    cohort_40_to_44 = sum_cohorts("40_to_44_years"),
                    cohort_45_to_49 = sum_cohorts("45_to_49_years"),
                    cohort_50_to_54 = sum_cohorts("50_to_54_years"),
                    cohort_55_to_59 = sum_cohorts("55_to_59_years"),
                    cohort_60_to_64 = sum_cohorts("60_to_64_years"),
                    cohort_65_to_69 = sum_cohorts("65_to_69_years"),
                    cohort_70_to_74 = sum_cohorts("70_to_74_years"),
                    cohort_75_to_79 = sum_cohorts("75_to_79_years"),
                    cohort_80_to_84 = sum_cohorts("80_to_84_years"),
                    cohort_85_to_89 = sum_cohorts("85_to_89_years"),
                    cohort_90_to_94 = sum_cohorts("90_to_94_years"),
                    cohort_95_to_99 = sum_cohorts("95_to_99_years"),
                    cohort_100_plus = sum_cohorts("100_years_and_over")
                ) %>%
                dplyr::mutate(
                    CSD_population = cohort_0_to_4 + cohort_5_to_9 + cohort_10_to_14 + 
                        cohort_15_to_19 + cohort_20_to_24 + cohort_25_to_29 + cohort_30_to_34 + 
                        cohort_35_to_39 + cohort_40_to_44 + cohort_45_to_49 + cohort_50_to_54 + 
                        cohort_55_to_59 + cohort_60_to_64 + cohort_65_to_69 + cohort_70_to_74 + 
                        cohort_75_to_79 + cohort_80_to_84 + cohort_85_to_89 + cohort_90_to_94 + 
                        cohort_95_to_99 + cohort_100_plus,
                    CSD_dwellings = temp %>%
                        dplyr::filter(grepl("total_private_dwellings", tolower(attribute))) %>%
                        dplyr::pull(value) %>%
                        as.numeric %>%
                        unique
                )
        }
        # stopCluster(cl)
    
        return(rbindlist(profiles))
    }

    # parallelise this loop - not important, since it's only run once
    start_time <- Sys.time()
    CSD_data <- rbindlist(the_profiles) %>%
        data.table %>%
        dplyr::rename_with(\(x) tolower(x) %>% gsub(' ', '_', .)) %>%
        dplyr::rename(
            geo_code=`geo_code_(por)`,
            attribute=`dim:_profile_of_census_subdivisions_(2247)`,
            value=`dim:_sex_(3):_member_id:_[1]:_total_-_sex`
        ) %>%
        dplyr::mutate(attribute = attribute %>% gsub(',', '', .) %>% gsub(' ', '_', .)) %>%
        prepare_CSD_table(., unique(.$geo_code)) %>%
        add_HRs(uid_column = "geo_code", province_column = "province")
        
    print(Sys.time() - start_time)
    fwrite(CSD_data, file.path(PROJECT_FOLDER, "Classifications/CSD_age_cohorts.csv"))
}
    
CSD_data <- fread(file.path(PROJECT_FOLDER, "Classifications/CSD_age_cohorts.csv"))

############## CATEGORICAL AND CONTINUOUS REMOTENESS

writeLines("\nreading and parsing local SAC info")
{
    # https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314313&CPV=A&CST=01012016&CLV=1&MLV=3
    raw_CMAs <- data.table(names=readLines("Classifications/raw_CMAs.txt"))
    CMAs <- data.table(GeoUID=numeric(), region=numeric(), title=character(), class=character())
    for(index in 1:nrow(raw_CMAs))
    {
        the_number <- str_extract(raw_CMAs$names[index], "[0-9]+")
        the_name <- gsub(paste0(the_number, " - "), '', raw_CMAs$names[index])
    
        if(nchar(the_number) == 3)
        {
            cma_name <- gsub("--", "-", the_name)
            cma_number <-  the_number
        } else {
            CMAs <- rbind(CMAs, list(
                GeoUID = as.integer(the_number), 
                region = the_name, 
                class = "CMA", 
                title =  cma_name
            ))
        }
    }
    
    # https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314313&CPV=B&CST=01012016&CLV=1&MLV=3
    raw_CAs <- data.table(names=readLines("Classifications/raw_CAs.txt"))
    CAs <- data.table(GeoUID=numeric(), region=numeric(), title=character(), class=character())
    for(index in 1:nrow(raw_CAs))
    {
        the_number <- str_extract(raw_CAs$names[index], "[0-9]+")
        the_name <- gsub(paste0(the_number, " - "), '', raw_CAs$names[index])
    
        if(nchar(the_number) == 3)
        {
            ca_name <- gsub("--", "-", the_name)
            ca_number <- the_number
        } else {
            CAs <- rbind(CAs, list(
                GeoUID = as.integer(the_number),
                region = the_name,
                class = "CA",
                title = ca_name
            ))
        }
    }
    
    # https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314314&CPV=996&CST=01012016&CLV=2&MLV=3
    MIZ_strong <- {fread("Classifications/raw_MIZ_strong.csv") %>%
            dplyr::mutate(
                title = "Strong MIZ",
                GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
                region = unlist(lapply(Code, str_sub, start=8)),
                class = "Strong"
            ) %>%
            dplyr::select(-Code, -`Census subdivision`)}
    
    # https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314314&CPV=997&CST=01012016&CLV=2&MLV=3
    MIZ_moderate <- {fread("Classifications/raw_MIZ_moderate.csv") %>%
            dplyr::mutate(
                title = "Moderate MIZ",
                GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
                region = unlist(lapply(Code, str_sub, start=8)),
                class = "Moderate"
            ) %>%
            dplyr::select(-Code, -`Census subdivision`)}
    
    # http://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314314&CPV=998&CST=01012016&CLV=2&MLV=3
    MIZ_weak <- {fread("Classifications/raw_MIZ_weak.csv") %>%
            dplyr::mutate(
                title = "Weak MIZ",
                GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
                region = unlist(lapply(Code, str_sub, start=8)),
                class = "Weak"
            ) %>%
            dplyr::select(-Code, -`Census subdivision`)}
    
    # http://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=314312&CVD=314314&CPV=999&CST=01012016&CLV=2&MLV=3
    MIZ_none <- {fread("Classifications/raw_MIZ_none.csv") %>%
            dplyr::mutate(
                title = "No MIZ",
                GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
                region = unlist(lapply(Code, str_sub, start=8)),
                class = "None"
            ) %>%
            dplyr::select(-Code, -`Census subdivision`)}
    
    Influence_Info <- rbind(CMAs, CAs, MIZ_strong, MIZ_moderate, MIZ_weak, MIZ_none, fill=T) %>%
        dplyr::mutate(
            title = paste0(title, " (", class, ")"),
            geo_code = as.integer(GeoUID)
        ) %>%
        dplyr::mutate(
            is_CMA = (class == "CMA"), 
            is_CA = (class == "CA"),
            is_MIZ_strong = (class == "Strong"),
            is_MIZ_moderate = (class == "Moderate"),
            is_MIZ_weak = (class == "Weak"),
            is_MIZ_none= (class == "None")
        ) %>%
        dplyr::rename_with(tolower) %>%
        dplyr::select(-geouid)
    
    # writeLines("\nStatCan Index of Remoteness scores")
    # Index_of_Remoteness <- fread("Classifications/Index_of_remoteness.csv") %>%
    #     dplyr::select(CSDuid, Index_of_remoteness) %>%
    #     dplyr::rename(geo_code = CSDuid) %>%
    #     dplyr::mutate(Index_of_remoteness = as.numeric(Index_of_remoteness)) %>%
    #     suppressWarnings() %>%
    #     dplyr::rename_with(tolower)
}

# assemble the complete table we need for the correlation before
if(!file.exists( sprintf("%s/Classifications/Total_CSD_Info.rda", PROJECT_FOLDER) ))
{
    Total_Data_Geo <- data.table(Reduce(
            function(x, y, ...) merge(x, y, by=c("geo_code"), all = TRUE, ...),
            list(CSD_data, Influence_Info) # , Index_of_Remoteness)
        )) %>%
        dplyr::mutate(
            province = lookup_provinces(geo_code),
            geo_name = coalesce(geo_name, region),
            class  = ifelse(is.na(class), "", class),
            is_cma = ifelse(is.na(is_cma), FALSE, is_cma),
            is_ca = ifelse(is.na(is_ca), FALSE, is_ca),
            is_miz_strong = ifelse(is.na(is_miz_strong), FALSE, is_miz_strong),
            is_miz_moderate = ifelse(is.na(is_miz_moderate), FALSE, is_miz_moderate),
            is_miz_weak = ifelse(is.na(is_miz_weak), FALSE, is_miz_weak),
            is_miz_none = ifelse(is.na(is_miz_none), FALSE, is_miz_none)
        ) %>%
        dplyr::select(-region, -type, -title) %>%
        dplyr::filter(!is.na(HRUID2018))
    saveRDS(Total_Data_Geo, sprintf("%s/Classifications/Total_CSD_Info.rda", PROJECT_FOLDER))
}
    
Total_Data_Geo <- readRDS(sprintf("%s/Classifications/Total_CSD_Info.rda", PROJECT_FOLDER))

# Total_Data_Geo  %>% dplyr::select(geo_code, HRUID2018) %>% unique() %>% dplyr::pull(geo_code) %>% table() %>% data.table %>% .[N>1] -> fall_into_multiple_PHUS
# 
# fall_into_multiple_PHUs <- Total_Data_Geo[, .(count=.N), by=geo_code][count>1]
    
# if(!file.exists(file.path(PROJECT_FOLDER, "Classifications/PHUs_hosting_airports.csv")))
# {
#     PHU_shapes <- readRDS(file.path(PROJECT_FOLDER, "Classifications/HR_shapes.rda"))
#     
#     Airport_Locations <- st_read(file.path(
#         PROJECT_FOLDER, 
#         "Airports_Aeroports_en_shape/Airports_Aeroports_en_shape.shp"
#     ))
#     
#     which_PHUs <- function(icaos)
#     {
#         return(unlist(lapply(
#             icaos,
#             \(xx) st_intersects(
#                 Airport_Locations %>% dplyr::filter(ICAO == xx) %>% dplyr::pull(geometry) %>% 
#                     st_sf() %>% st_transform(3857), 
#                 PHU_shapes$geometry %>% st_sf() %>% st_transform(3857)
#             ) %>% .[1] %>% .[[1]] %>% PHU_shapes[., HRUID2018]
#         )))
#     }
#     
#     start_time <- Sys.time()
#     PHUs_with_airports <- which_PHUs(unique(Airport_Locations$ICAO)) %>%
#         table %>% 
#         data.table %>% 
#         setNames(c("HRUID2018", "airports")) %>%
#         dplyr::mutate(HRUID2018 = as.character(HRUID2018)) %>%
#         dplyr::rename()
#     fwrite(PHUs_with_airports, file.path(
#         PROJECT_FOLDER, "Classifications/PHUs_hosting_airports.csv"
#     ))
#     print(Sys.time() - start_time)
# }

if(!file.exists(file.path(PROJECT_FOLDER, "/CaseDataTables/canada_wide_vacc_data_official.csv")))
{
    fread(
        "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex.csv"
    ) %>%
    fwrite(file.path(PROJECT_FOLDER, "/CaseDataTables/canada_wide_vacc_data_official.csv"))
}

#### LONG TERM CARE HOES

if(! file.exists(file.path(PROJECT_FOLDER, "Classifications/LTCH_locations.csv")))
{
    fread(file.path(PROJECT_FOLDER, "ODHF_v1.1/odhf_v1.1.csv")) %>%
        dplyr::rename_with(tolower) %>%
        dplyr::filter(!is.na(csduid)) %>%
        dplyr::filter(grepl("residen", tolower(odhf_facility_type))) %>%
        dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
        st_as_sf(coords=c("longitude", "latitude")) %>%
        dplyr::mutate(province = lookup_provinces(csduid)) %>%
        fwrite(file.path(PROJECT_FOLDER, "Classifications/LTCH_locations.csv"))
}

LTCHs <- fread(file.path(PROJECT_FOLDER, "Classifications/LTCH_locations.csv"))

LTCHs_per_PHU <- LTCHs %>%
    add_HRs(uid_column = "csduid", province_column = "province", raw_numbers=TRUE) %>%
    table %>% data.table %>%
    setNames(c("HRUID2018", "LTCHs")) %>%
    dplyr::mutate(HRUID2018 = as.character(HRUID2018))

PHU_information <- Total_Data_Geo[,
    lapply(.SD, sum, na=TRUE),
        .SDcols = setdiff(
            names(Total_Data_Geo),
            c("geo_code", "geo_name", "class", "province", "HR", "HRUID2018", "province", "alpha")
        ),
        by=.(HRUID2018, province, HR)
    ] %>%
    dplyr::rename(PHU_population = CSD_population, PHU_dwellings = CSD_dwellings) %>%
    merge(
        readRDS(file.path(PROJECT_FOLDER, "Classifications/HR_shapes.rda")), 
        by = c("HRUID2018")
    ) %>%
    merge(
        fread(file.path(PROJECT_FOLDER, "Classifications/PHUs_hosting_airports.csv")), 
        by = c("HRUID2018"), 
        all=TRUE
    ) %>%
    dplyr::mutate(airports = ifelse(is.na(airports), 0, airports)) %>%
    merge(
        LTCHs_per_PHU %>% 
            dplyr::mutate(HRUID2018 = as.numeric(HRUID2018)), 
        by=c("HRUID2018"), 
        all=TRUE
    ) %>%
    dplyr::mutate(LTCHs = ifelse(is.na(LTCHs), 0, LTCHs))

province_populations <- PHU_information %>% 
    dplyr::select(province, PHU_population) %>% 
    unique() %>% 
    dplyr::group_by(province) %>% 
    dplyr::summarise(PROV_population = sum(PHU_population, na.rm=TRUE))
    
#####################################
############## TIME-DEPENDENT METRICS
#####################################

# WAVE DATES

if(!file.exists(file.path(PROJECT_FOLDER, "CaseDataTables/all_canada.csv")))
{
    jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=canada")$cases %>%
        fwrite(file.path(PROJECT_FOLDER, "CaseDataTables/all_canada.csv"))
}

# retrieved 27 Nov 2021
if(! file.exists(file.path(PROJECT_FOLDER, "covid-19-intervention-scan-data-tables-en.xlsx")) )
{
    download.file(
        "https://www.cihi.ca/sites/default/files/document/covid-19-intervention-scan-data-tables-en.xlsx",
        file.path(PROJECT_FOLDER, "covid-19-intervention-scan-data-tables-en.xlsx")
    )
}

LUT <- rbind(
        province_LUT, 
        list("Canada", "Can.", "CN", -1, "Canada")
    ) %>% dplyr::select(province, abbreviations)

if(! file.exists(file.path(PROJECT_FOLDER, "Classifications/interventions.csv")))
{
    interventions <- read.xlsx(
            sprintf("%s/%s", PROJECT_FOLDER, "covid-19-intervention-scan-data-tables-en.xlsx"),
            sheet = "Intervention scan",
            startRow = 3
        ) %>%
        rename_with(\(x) x %>% tolower %>% gsub("intervention.", "", .))  %>%
        filter(!grepl("end", tolower(entry.id))) %>%
        mutate(
            date.announced = convertToDate(date.announced) %>% as.integer,
            date.implemented = convertToDate(date.implemented) %>% as.integer,
            jurisdiction = dplyr::recode(jurisdiction, "Nun." = "Nvt.")
        ) %>%
        dplyr::mutate(
            jurisdiction = left_join(., LUT, by=c("jurisdiction" = "abbreviations"))$province,
            # # be careful - the dash ion the code below is longer than the standard one
            # — 
            type = unlist(lapply(type, \(xx){ str_split(xx, '—')[[1]][2] %>% trimws })),
            indigenous.population.group = dplyr::recode(
                indigenous.population.group,
                "No"=FALSE, "Yes"=TRUE, .default=NA
            ),
            summary = tolower(summary),
            who = unlist(lapply(
                summary,
                \(x) gsub('^.*who:\\s*|\\s*what.*$', '', x)
            )),
            what = unlist(lapply(
                summary,
                \(x)  gsub('^.*what:\\s*|\\s*effective until.*$', '', x)
            )),
            effective.until = unlist(lapply(
                summary,
                \(x){
                    if(grepl("effective until", x)) gsub('^.*effective until\\s*|\\.', '', x) else ""
                }
            )) %>%
                gsub("\t|\n|:", "", .) %>%
                trimws,
            effective.until = unlist(lapply(
                effective.until,
                \(x) unlist(lapply(x, date_corrector))
            )),
            category = category %>% tolower %>% trimws,
            jurisdiction = ifelse(is.na(jurisdiction), "all Canada", jurisdiction)
        ) %>%
        dplyr::select(-summary) %>%
        dplyr::filter(!is.na(date.implemented) & (effective.until != "-1")) %>%
        dplyr::mutate(
            effective.until = ifelse(effective.until == "", as.numeric(Sys.Date()), effective.until)
        ) %>%
        dplyr::mutate(effective.until = effective.until %>% as.numeric) %>%
        dplyr::filter(abs(effective.until-date.implemented)>14) %>% 
        # we'll assume that the effective interventions must last at least two weeks
        data.table()
        fwrite(interventions, file.path(PROJECT_FOLDER, "Classifications/interventions.csv"))
}

###### EDUCATION INTERVENTIONS

if(!file.exists( file.path(PROJECT_FOLDER, "Classifications/tightened_educational_restrictions.csv") ))
{
    interventions <- fread(file.path(PROJECT_FOLDER, "Classifications/interventions.csv"))
    
    # tightened education interventions
    TEI <- interventions %>% 
        dplyr::filter(grepl("education", tolower(type))) %>%
        dplyr::filter(grepl("provincial", tolower(level))) %>%
        dplyr::filter(grepl("clos|open", tolower(category))) %>% 
        dplyr::filter(grepl("tigh", tolower(action)) & !grepl("eas", tolower(action))) %>% 
        # some of the restrictions are marked tightening/easing - those are easing
        dplyr::filter(!grepl("guidance|strengthened|distributed|amended", tolower(what))) %>%
        dplyr::filter(!grepl("working with|extended online teacher-led", tolower(what))) %>%
        dplyr::filter(!grepl("implemented new measures|mask|release|update", tolower(what))) %>%
        dplyr::mutate(
            what = substr(what, start=1, stop=20),
            effective.until = ifelse(is.na(effective.until), Sys.Date(), effective.until),
            jurisdiction = ifelse(is.na(jurisdiction), "Canada", jurisdiction),
            colour = "red",
            alpha = lookup_alphas(jurisdiction)
        )
        fwrite(TEI, file.path(PROJECT_FOLDER, "Classifications/tightened_educational_restrictions.csv"))
}

TEI <- fread(file.path(PROJECT_FOLDER, "Classifications/tightened_educational_restrictions.csv"))

educational_interventions_in_waves <- TEI %>%
    dplyr::select(jurisdiction, date.implemented, effective.until) %>% 
    dplyr::mutate(effective.until = pmin(effective.until, Sys.time(), na.rm=T)) %>% 
    split(by="jurisdiction") %>% 
    lapply(. %>%  dplyr::select(-jurisdiction)) %>%
    lapply( \(tab) mapply(`:`, tab$date.implemented, tab$effective.until) %>% 
        unlist %>% unique %>% sort 
    ) %>%
    list( # turn this into an lapply
        data.table(
            name = names(.), wave = 1, 
            EIs = unlist(lapply(., \(x) x %>% how_much_of_the_wave(1)))
        ),
        data.table(
            name = names(.), wave = 2, 
            EIs = unlist(lapply(., \(x) x %>% how_much_of_the_wave(2)))
        ),
        data.table(
            name = names(.), wave = 3, 
            EIs = unlist(lapply(., \(x) x %>% how_much_of_the_wave(3)))
        ),
        data.table(
            name = names(.), wave = 4, 
            EIs = unlist(lapply(., \(x) x %>% how_much_of_the_wave(4)))
        )
    ) %>%
    tail(-1) %>%
    rbindlist() %>%
    dplyr::rename(province = name)

###### LOCKDOWNS
 
# lockdown measures
LDM <- fread(file.path(PROJECT_FOLDER, "Classifications/interventions.csv")) %>% 
    dplyr::filter(
        grepl("clos", tolower(category)) & 
        grepl("essential|recrea", tolower(type)) & 
        grepl("provincial", tolower(level))
    ) %>%
    dplyr::filter(
        grepl("tigh", tolower(action)) & 
            !grepl("eas", tolower(action))
    ) %>%
    dplyr::filter(grepl("clos", tolower(what)))
    fwrite(LDM, file.path(PROJECT_FOLDER, "Classifications/lockdown_measures.csv"))


lockdown_interventions_in_waves <- LDM %>%
    dplyr::select(jurisdiction, date.implemented, effective.until) %>% 
    dplyr::mutate(effective.until = pmin(effective.until, Sys.time(), na.rm=T)) %>% 
    split(by="jurisdiction") %>% 
    lapply(. %>%  dplyr::select(-jurisdiction)) %>%
    lapply( \(tab) mapply(`:`, tab$date.implemented, tab$effective.until) %>% 
                unlist %>% unique %>% sort
    ) %>%
    list( # turn this into an lapply
        data.table(
            name = names(.),
            wave = 1, 
            LIs=unlist(lapply(., \(x) x %>% how_much_of_the_wave(1)))
        ),
        data.table(
            name = names(.), wave = 2, 
            LIs=unlist(lapply(., \(x) x %>% how_much_of_the_wave(2)))
        ),
        data.table(
            name = names(.), wave = 3, 
            LIs = unlist(lapply(., \(x) x %>% how_much_of_the_wave(3)))
        ),
        data.table(
            name = names(.), wave = 4, 
            LIs=unlist(lapply(., \(x) x %>% how_much_of_the_wave(4)))
        )
    ) %>%
    tail(-1) %>%
    rbindlist() %>%
    dplyr::rename(province = name)

################# VACCINATION

if(!file.exists( sprintf("%s/CaseDataTables/canada_wide_vacc_data_official.csv", PROJECT_FOLDER) ))
{
    fread(
        "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex.csv"
    ) %>%
    fwrite(sprintf("%s/CaseDataTables/canada_wide_vacc_data_official.csv", PROJECT_FOLDER))
}

vaxx_info <- fread(file.path(
        PROJECT_FOLDER, 
        "/CaseDataTables/canada_wide_vacc_data_official.csv"
    )) %>%
    dplyr::filter(prfname != "Canada") %>%
    group_by(pruid, prfname, week_end) %>%
    dplyr::summarise(
        PROV_vaxx_AL1D=sum(as.numeric(numtotal_atleast1dose), na.rm=TRUE),
        PROV_vaxx_FULL=sum(as.numeric(numtotal_fully), na.rm=TRUE)
    ) %>%
    data.table %>%
    dplyr::mutate(
        week_end = as.Date(week_end),
        province = lookup_provinces(pruid)
    ) %>%
    dplyr::select(-prfname)
    fwrite(vaxx_info, file.path(PROJECT_FOLDER, "Classifications/vaxx_info_dates.csv"))

vaxx_implementation <- function(the_vaxx_table, prov)
{
    proportion_vaccination <- \(x, wave_number=1)
    {
        if(nrow(x) ==  0) return("None")
        the_table <- x %>% 
            dplyr::filter(wave == wave_number)
        if(nrow(the_table) == 0) return("None")
        if( 
            (abs(min(the_table$week_end) - Canada_Wave_Dates[wave_number])<7) & 
            (abs(max(the_table$week_end) - Canada_Wave_Dates[wave_number+1])<7) 
        ){ return("Entire") }
        return("Partial")
    }
    
    temp_table <- the_vaxx_table %>% 
        dplyr::filter(province == prov) %>% 
        ### ask Seyed whether we're looking at 'at least one dose' or 'fully vaccinated'
        dplyr::filter(PROV_vaxx_AL1D != 0)
    
    return( rbindlist( lapply( 1:4, \(xx)
    {
        temp_table %>%
            dplyr::filter(
                (week_end>=Canada_Wave_Dates[xx]) & 
                    (week_end<=Canada_Wave_Dates[xx+1])
            ) %>%
            dplyr::mutate(wave = xx) %>%
            proportion_vaccination(wave_number = xx) %>%
            data.table %>%
            setNames(c("VIs")) %>%
            dplyr::mutate(wave = xx, province = lookup_province(unique(temp_table$pruid)))
    })))
}

categorical_vaccination <- lapply(
        unique(vaxx_info$province), 
        \(x) vaxx_implementation(vaxx_info, x)
    ) %>%
    rbindlist()
    
vaccination_in_waves <- vaxx_info %>%
    dplyr::pull(pruid) %>%
    .[.!=1] %>%
    unique %>%
    list(
        lapply(., \(x){ 
            vaxx_info %>% dplyr::filter(pruid == x) %>% 
                dplyr::filter(week_end <= Canada_Wave_Dates[2]) %>% 
                dplyr::arrange(week_end) %>% tail(1) %>% 
                dplyr::mutate(wave=1) }
            ) %>% rbindlist,
        lapply(., \(x){ 
            vaxx_info %>% dplyr::filter(pruid == x) %>% 
                dplyr::filter(week_end <= Canada_Wave_Dates[3]) %>% 
                dplyr::arrange(week_end) %>% tail(1) %>% 
                dplyr::mutate(wave=2) }
            ) %>% rbindlist,
        lapply(., \(x){ 
            vaxx_info %>% dplyr::filter(pruid == x) %>% 
                dplyr::filter(week_end <= Canada_Wave_Dates[4]) %>% 
                dplyr::arrange(week_end) %>% tail(1) %>% 
                dplyr::mutate(wave=3) }
            ) %>% rbindlist,
        lapply(., \(x){ 
            vaxx_info %>% dplyr::filter(pruid == x) %>% 
                dplyr::filter(week_end <= Canada_Wave_Dates[5]) %>% 
                dplyr::arrange(week_end) %>% tail(1) %>% 
                dplyr::mutate(wave=4) }
            ) %>% rbindlist
        ) %>%
        tail(-1) %>%
        rbindlist

full_vaccination_info <- merge(
        categorical_vaccination, 
        vaccination_in_waves, 
        by=c("province", "wave"), 
        all=TRUE
    ) %>%
    dplyr::select(-week_end) %>%
    dplyr::mutate(
        PROV_vaxx_AL1D = ifelse(is.na(PROV_vaxx_AL1D), 0, PROV_vaxx_AL1D),
        PROV_vaxx_FULL = ifelse(is.na(PROV_vaxx_FULL), 0, PROV_vaxx_FULL),
        pruid = lookup_province_UIDs(province)
    )
   
interventions_by_wave_province <- full_vaccination_info %>%
    merge(educational_interventions_in_waves, by=c("province", "wave"), all=TRUE) %>%
    merge(lockdown_interventions_in_waves, by=c("province", "wave"), all=TRUE) %>%
    # merge(province_UIDs, by=c("province")) %>%
    dplyr::mutate(
        PROV_vaxx_AL1D = ifelse(is.na(PROV_vaxx_AL1D), 0, PROV_vaxx_AL1D),
        PROV_vaxx_FULL = ifelse(is.na(PROV_vaxx_FULL), 0, PROV_vaxx_FULL),
        # pruid = dplyr::coalesce(pruid.x, pruid.y)
    ) %>%
    dplyr::filter(!grepl("all canada", tolower(province))) %>% 
    dplyr::filter(!is.na(pruid))
    
###################################################################
############################################ COVID INCIDENCE DATA
###################################################################

if(FALSE)
{
    writeLines("\nAPI data - cases")
    UofT_api_case_data <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases %>%
        dplyr::mutate(date_report=as.Date(date_report, format="%d-%m-%Y"))
    fwrite(UofT_api_case_data, file=sprintf("%s/CaseDataTables/UofT_cases.csv", PROJECT_FOLDER))
    
    writeLines("\nAPI data - vaccines")
    UofT_api_vaccine_data <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=avaccine&loc=hr")$avaccine %>%
        dplyr::mutate(date_vaccine_administered=as.Date(date_vaccine_administered, format="%d-%m-%Y"))
    fwrite(UofT_api_vaccine_data, file=sprintf("%s/CaseDataTables/UofT_vaccines.csv", PROJECT_FOLDER))
    
    # writeLines("\nCanada-wide vaccine coverage - stratified")
    # # only give the weekend date for the administration of the vaccine, not the actual date
    # fread("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex.csv") %>%
    #     fwrite(sprintf("%s/CaseDataTables/canada_wide_vacc_data_official.csv", PROJECT_FOLDER))
    # fread("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byVaccineType.csv") %>%
    #     fwrite(sprintf("%s/CaseDataTables/canada_vacc_coverage_by_vaccine.csv", PROJECT_FOLDER))
    
    ################################## BRITISH COLUMBIA
    
    writeLines("\nBritish Columbia")
    BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") %>%
        dplyr::select(-c(Province, HA, Cases_Reported_Smoothed)) %>%
        dplyr::filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) %>%
        dplyr::rename(date=Date, HR=HSDA, cases=Cases_Reported) %>%
        dplyr::mutate(date=as.Date(date), province="British Columbia") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(BC_cases, sprintf("%s/CaseDataTables/BC_cases.csv", PROJECT_FOLDER))
    
    ################################## QUEBEC
    
    writeLines("\nQuebec")
    QC_cases <- fread("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv") %>%
        dplyr::select(Date, Nom, cas_quo_tot_n) %>%
        dplyr::filter(grepl("\\d", Date) & grepl("\\d - [a-zA-Z]", Nom)) %>%
        dplyr::mutate(Nom = Nom %>% replace_accents() %>% trim_numbers()) %>%
        dplyr::rename(date=Date, HR=Nom, cases=cas_quo_tot_n) %>%
        dplyr::mutate(date=as.Date(date), province="Quebec", cases = as.numeric(cases)) %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(QC_cases, sprintf("%s/CaseDataTables/QC_cases.csv", PROJECT_FOLDER))
    
    ##################################  MANITOBA
    
    writeLines("\nManitoba")
    MB_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "Manitoba") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Manitoba") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(MB_cases, sprintf("%s/CaseDataTables/MB_cases.csv", PROJECT_FOLDER))
    
    ################################## SASKATCHEWAN
    
    writeLines("\nSaskatchewan")
    SK_cases <- get_saskatchewan_case_data() %>%
        dplyr::filter(!is.na(`Total Cases`) & !grepl("total", tolower(Region))) %>%
        dplyr::select("Date", "Region", "New Cases") %>%
        dplyr::rename(date=Date, HR=Region, cases="New Cases") %>%
        dplyr::mutate(date=as.Date(date), province="Saskatchewan") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(SK_cases, sprintf("%s/CaseDataTables/SK_cases.csv", PROJECT_FOLDER))
    
    SK_vacc <- fread("https://dashboard.saskatchewan.ca/export/vaccines/4159.csv") %>%
        dplyr::mutate(Date = as.Date(Date, format="%Y/%m/%d"))
    fwrite(SK_vacc, sprintf("%s/CaseDataTables/SK_vacc.csv", PROJECT_FOLDER))
    
    ################################## PRINCE EDWARD ISLAND, NORTHWEST TERRITORIES, YUKON, NUNAVUT
    
    writeLines("\nPEI, NWT, Yukon, Nunavut")
    
    PE_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "PEI") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Prince Edward Island") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(PE_cases, sprintf("%s/CaseDataTables/PE_cases.csv", PROJECT_FOLDER))
    
    PE_vacc <- data.table(UofT_api_vaccine_data)[grepl("pei", tolower(province))]
    fwrite(PE_vacc, sprintf("%s/CaseDataTables/PE_vacc.csv", PROJECT_FOLDER))
    
    NT_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "NWT") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Northwest Territories", HR="Northwest Territories") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(NT_cases, sprintf("%s/CaseDataTables/NT_cases.csv", PROJECT_FOLDER))
    
    NT_vacc <- data.table(UofT_api_vaccine_data)[grepl("northwest", tolower(province))]
    fwrite(NT_vacc, sprintf("%s/CaseDataTables/NT_vacc.csv", PROJECT_FOLDER))
    
    YT_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "Yukon") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Yukon") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(YT_cases, sprintf("%s/CaseDataTables/YT_cases.csv", PROJECT_FOLDER))
    
    YT_vacc <- data.table(UofT_api_vaccine_data)[grepl("yukon", tolower(province))]
    fwrite(YT_vacc, sprintf("%s/CaseDataTables/YT_vacc.csv", PROJECT_FOLDER))
    
    NU_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "Nunavut") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Nunavut") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(NU_cases, sprintf("%s/CaseDataTables/NU_cases.csv", PROJECT_FOLDER))
    
    NU_vacc <- data.table(UofT_api_vaccine_data)[grepl("nuna", tolower(province))]
    fwrite(NU_vacc, sprintf("%s/CaseDataTables/NU_vacc.csv", PROJECT_FOLDER))
    
    ################################## ONTARIO
    
    writeLines("\nOntario - cases")
    # Ontario data for confirmed cases - useful for delay distribution, etc
    # https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f
    ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") %>%
        dplyr::select(-Total) %>%
        melt(id.vars="Date") %>%
        dplyr::rename(date=Date, HR=variable, cases=value) %>%
        # dplyr::mutate(HR = standard_HR_names(HR)) %>%
        dplyr::mutate(province = "Ontario", date=as.Date(date)) %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(ON_cases, sprintf("%s/CaseDataTables/ON_cases.csv", PROJECT_FOLDER))
    
    ON_vacc <- fread("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/2a362139-b782-43b1-b3cb-078a2ef19524/download/vaccines_by_age_phu.csv")
    fwrite(ON_vacc, sprintf("%s/CaseDataTables/ON_vacc.csv", PROJECT_FOLDER))
    
    ################################## ALBERTA
    
    writeLines("\nAlberta")
    # Alberta data from https://www.alberta.ca/stats/covid-19-alberta-statistics.htm#data-export
    AB_cases <- fread("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") %>%
        dplyr::select(-V1) %>%
        dplyr::rename(date="Date reported", HR="Alberta Health Services Zone", type="Case type", age="Age group", status="Case status") %>%
        dplyr::mutate(date=as.Date(date)) %>%
        dplyr::filter(HR!="Unknown") %>%
        dplyr::group_by(date, HR) %>%
        dplyr::tally() %>%
        dplyr::rename(cases=n) %>%
        dplyr::mutate(province="Alberta") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(AB_cases, sprintf("%s/CaseDataTables/AB_cases.csv", PROJECT_FOLDER))
    
    ################################## NEW BRUNSWICK
    
    writeLines("\nNew Brunswick")
    NB_cases <- UofT_api_case_data %>%
        dplyr::filter(province=="New Brunswick") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date)) %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(NB_cases, sprintf("%s/CaseDataTables/NB_cases.csv", PROJECT_FOLDER))
    
    ################################## NOVA SCOTIA
    
    writeLines("\nNova Scotia")
    NS_cases <- UofT_api_case_data %>%
        dplyr::filter(province=="Nova Scotia") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date)) %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(NS_cases, sprintf("%s/CaseDataTables/NS_cases.csv", PROJECT_FOLDER))
    
    ################################## NEWFOUNDLAND LABRADOR
    
    writeLines("\nNewfoundland and Labrador")
    NL_cases <- UofT_api_case_data %>%
        dplyr::filter(province=="NL") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Newfoundland and Labrador") %>%
        # add_wave_numbers() %>% .[['cases']] %>%
        data.table()
    fwrite(NL_cases, sprintf("%s/CaseDataTables/NL_cases.csv", PROJECT_FOLDER))
    
    ################################## TOTAL DATA
    
    writeLines("\nTotal Case Data")
    Total_Case_Data <- rbind(
            fread(sprintf("%s/CaseDataTables/BC_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/QC_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/MB_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/SK_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/ON_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/AB_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/NB_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/NS_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/NL_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/PE_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/NT_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/YT_cases.csv", PROJECT_FOLDER)),
            fread(sprintf("%s/CaseDataTables/NU_cases.csv", PROJECT_FOLDER))
        ) %>%
        dplyr::mutate(HR = standard_HR_names(HR)) %>%
        add_HRUIDs("HR", "province")
    fwrite(Total_Case_Data, sprintf("%s/CaseDataTables/Total_Case_Data.csv", PROJECT_FOLDER))
}

Total_Case_Data <- fread(file.path(PROJECT_FOLDER, "CaseDataTables/Total_Case_Data.csv")) %>%
    dplyr::filter(!is.na(HRUID2018)) %>%
    merge(province_LUT %>% dplyr::select(province, SGC), by=c("province")) %>%    
    dplyr::rename(pruid = SGC) %>%
    dplyr::mutate(wave = 4, pruid = as.integer(pruid)) %>%
    dplyr::mutate(wave = ifelse(date < Canada_Wave_Dates[4], 3, wave)) %>%
    dplyr::mutate(wave = ifelse(date < Canada_Wave_Dates[3], 2, wave)) %>%
    dplyr::mutate(wave = ifelse(date < Canada_Wave_Dates[2], 1, wave)) %>%
    dplyr::group_by(HRUID2018,HR, province, pruid, wave) %>%
    dplyr::summarise(incidence = sum(cases, na.rm=TRUE)) %>%
    data.table

# there's no Saskatchewan data for the first wave (not until October), and I can't match with the Toronto
# API data since they use different regions to group the incidences
Regression_Data <- Total_Case_Data %>%
    merge(interventions_by_wave_province, by=c("province", "pruid", "wave"), all=TRUE) %>%
    merge(PHU_information, by=c("province", "HRUID2018", "HR"), all=TRUE) %>%
    merge(province_populations, by=c("province"), all=TRUE) %>%
    dplyr::filter(!is.na(HRUID2018)) %>%
    dplyr::mutate(
        LIs = factor(ifelse(is.na(LIs), "None", LIs)),
        EIs = factor(ifelse(is.na(EIs), "None", EIs)),
        VIs = factor(ifelse(is.na(VIs), "None", VIs)),
        PHU_area_km2 =  as.numeric(st_area(geometry))/1000**2
    ) %>%
    dplyr::mutate(PHU_pop_density = as.numeric(PHU_population/PHU_area_km2)) %>%
    st_sf
    saveRDS(Regression_Data, file=file.path(PROJECT_FOLDER, "CaseDataFiles/regression_data.rda"))
