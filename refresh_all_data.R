rm(list=ls())

library(dplyr)
library(data.table)
library(jsonlite)
library(stringr)
library(sf)
library(openxlsx)
library(CanCovidData)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(PROJECT_FOLDER)

dir.create(file.path(".", "Classifications"), showWarnings=FALSE)
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source(sprintf("%s/function_header.R", PROJECT_FOLDER))

start_time <- Sys.time()

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
        return(rbindlist(profiles))
    }

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
}

#### LONG TERM CARE HOMES
{
    LTCHs_per_PHU <- fread(file.path(PROJECT_FOLDER, "ODHF_v1.1/odhf_v1.1.csv")) %>%
        dplyr::rename_with(tolower) %>%
        dplyr::filter(!is.na(csduid)) %>%
        dplyr::filter(grepl("residen", tolower(odhf_facility_type))) %>%
        dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
        st_as_sf(coords=c("longitude", "latitude")) %>%
        add_HRs(uid_column = "csduid", province_column = "province", raw_numbers=TRUE) %>%
        table %>% data.table %>%
        setNames(c("HRUID2018", "LTCHs")) %>%
        dplyr::mutate(HRUID2018 = as.numeric(HRUID2018))
    
    # assemble the complete table we need for the correlation before
    PHU_information <- data.table(Reduce(
            function(x, y, ...) merge(x, y, by=c("geo_code"), all = TRUE, ...),
            list(CSD_data, Influence_Info)
        )) %>%
        dplyr::mutate(
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
        dplyr::filter(!is.na(HRUID2018)) %>%
        dplyr::group_by(HRUID2018, province, HR) %>%
        dplyr::summarise(across(
            names(.) %>% .[!grepl("geo|class|HR|alpha|province", .)],
            sum
        )) %>%
        dplyr::arrange(province, HR) %>%
        dplyr::rename(PHU_population = CSD_population, PHU_dwellings = CSD_dwellings) %>%
        merge(
            readRDS(file.path(PROJECT_FOLDER, "Spatial_Features/All_HRs.rds")),
            by = c("HRUID2018")
        ) %>%
        merge(LTCHs_per_PHU, by=c("HRUID2018"), all=TRUE) %>%
        dplyr::mutate(LTCHs = replace(LTCHs, is.na(LTCHs), 0)) %>%
        dplyr::group_by(province) %>%
        dplyr::mutate(PROV_population = sum(PHU_population))
}

if(!file.exists(file.path(PROJECT_FOLDER, "/CaseDataTables/canada_wide_vacc_data_official.csv")))
{
    fread(
        "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex.csv"
    ) %>%
    fwrite(file.path(PROJECT_FOLDER, "/CaseDataTables/canada_wide_vacc_data_official.csv"))
}

province_populations <- PHU_information %>%
    dplyr::select(province, PHU_population) %>%
    unique() %>%
    dplyr::group_by(province) %>%
    dplyr::summarise(PROV_population = sum(PHU_population, na.rm=TRUE))

# # retrieved 27 Nov 2021
# if(! file.exists(file.path(PROJECT_FOLDER, "Interventions/covid-19-intervention-scan-data-tables-en.xlsx")) )
# {
#     download.file(
#         "https://www.cihi.ca/sites/default/files/document/covid-19-intervention-scan-data-tables-en.xlsx",
#         file.path(PROJECT_FOLDER, "Interventions/covid-19-intervention-scan-data-tables-en.xlsx")
#     )
# }

############ INTERVENTIONS
{
    LUT <- rbind(
            province_LUT,
            list("Canada", "Can.", "CN", -1, "Canada")
        ) %>% dplyr::select(province, abbreviations)
    
    if(!file.exists( sprintf("%s/CaseDataTables/canada_wide_vacc_data_official.csv", PROJECT_FOLDER) ))
    {
        UofT_api_vaccine_data <- 
            jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=avaccine&loc=prov")$avaccine %>%
            dplyr::mutate(date_vaccine_administered=as.Date(date_vaccine_administered, format="%d-%m-%Y"))
        fwrite(UofT_api_vaccine_data, file=sprintf("%s/CaseDataTables/UofT_vaccines.csv", PROJECT_FOLDER))
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
    
    
    interventions <- read.xlsx(
            sprintf("%s/%s", PROJECT_FOLDER, "Interventions/scan-data-tables-covid-19-intervention-update13-en.xlsx"),
            sheet = "Intervention scan",
            startRow = 3
        ) %>%
        # filtering lockdown as work-from-home, daycare affecting the 0-4 and education affecting the 5-19
        dplyr::filter(Entry.ID %in% c(
            sprintf("AB%03i", c(236, 368, 410, 034, 120, 033, 211, 216, 271, 333, 380)), # 11
            sprintf("BC%03i", c(087, 018, 203)), # 3
            sprintf("MB%03i", c(008, 021, 133, 025, 166)), # 5
            sprintf("NB%03i", c(011, 010, 077, 017, 162, 268, 288, 382, 363)),
            sprintf("NL%03i", c(007, 123, 010, 136, 180, 235)), # 6
            sprintf("NT%03i", c(008, 016, 116, 170, 175)), # 4
            sprintf("NS%03i", c(022, 009, 082, 132, 257, 303)), # 6
            sprintf("NU%03i", c(012, 064, 114, 168, 249, 004, 050, 111, 167, 189, 196, 211, 223, 262, 347)), # 15
            sprintf("ON%03i", c(039, 009, 249, 434, 474, 660, 756)), # 7
            sprintf("PE%03i", c(020, 012, 119, 022, 146, 204, 213)), # 7
            sprintf("QC%03i", c(046, 043, 138, 033, 336, 579, 641, 735, 775)), # 9
            sprintf("SK%03i", c(026, 024, 096, 021, 157)), # 5
            sprintf("YT%03i", c(004, 051, 081, 010, 120))  # 5
        )) %>%
        # action is ambiguous here, so we'll make it clear that this is an easing scenario
        dplyr::mutate(Action = replace(Action, Entry.ID == "QC641", "Easing")) %>%
        rename_with(\(x) x %>% tolower %>% gsub("intervention.", "", .)) %>%
        mutate(
            date.announced = convertToDate(date.announced), # %>% as.integer,
            date.implemented = convertToDate(date.implemented), # %>% as.integer,
            jurisdiction = dplyr::recode(jurisdiction, "Nun." = "Nvt.")
        ) %>%
        dplyr::mutate(
            jurisdiction = left_join(., LUT, by=c("jurisdiction" = "abbreviations"))$province,
            # # be careful - the dash in the code below is longer than the standard one
            # '-' 
            type = unlist(lapply(type, \(xx){ str_split(xx, '-')[[1]][2] %>% trimws })),
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
        # dplyr::filter(!is.na(date.implemented) & (effective.until != "-1")) %>%
        dplyr::mutate(
            effective.until = ifelse(effective.until == "", Sys.Date(), as.Date(effective.until, origin="1970-01-01")),
            effective.until = as.Date(effective.until, origin="1970-01-01")
        ) %>%
        data.table() %>%
        dplyr::select(entry.id, jurisdiction, type, action, date.implemented) %>%
        dplyr::arrange(jurisdiction, type, date.implemented)
    
    tuple_table <- interventions %>% 
        # filter(jurisdiction == "Quebec") %>%
        dplyr::select(jurisdiction, type) %>% 
        unique
    
    interventions_display_table <- rbindlist(
        lapply(
            1:nrow(tuple_table),
            \(xx){
                haha <- interventions %>%
                    dplyr::filter(jurisdiction == tuple_table[xx]$jurisdiction) %>%
                    dplyr::filter(type == tuple_table[xx]$type)
                
                haha1 <- haha[grepl("tight", tolower(action))] %>% 
                    dplyr::select(-action) %>%
                    dplyr::rename(start = date.implemented)
                haha2 <- haha[grepl("eas", tolower(action))] %>%
                    dplyr::select(-action) %>%
                    dplyr::rename(end = date.implemented)
                
                cbind.fillNA(haha1, haha2) %>%
                    dplyr::select(jurisdiction, type, start, end)
            }
        )) %>%
        # because of an anomaly in the Intervention Scan dataset, we have to add this BC row manually
        rbind(list("British Columbia", "work from home", "2020-03-19", as.character(Sys.Date()))) %>%
        dplyr::mutate(
            end = ifelse(is.na(end), as.character(Sys.Date()), end),
            start = as.Date(start), end = as.Date(end),
        ) %>%
        rbind(
            vaxx_info[PROV_vaxx_AL1D>0, .SD[1], by=province] %>% 
                dplyr::select(province, week_end) %>% 
                dplyr::rename(jurisdiction = province, start = week_end) %>% 
                dplyr::mutate(end = Sys.Date(), type="vaccination")
        ) %>%
        dplyr::mutate(
            colour = unlist(lapply(type, \(xx){
                if(grepl("work", xx)) return("red") 
                if(grepl("day", xx)) return("green")
                if(grepl("educ", xx)) return("blue")
                return("black")
            }))
        ) %>%
        dplyr::filter(!is.na(start))
    
    tuple_table <- interventions_display_table %>% 
        dplyr::select(jurisdiction, type) %>% 
        unique %>% .[1]
    
    categorical_interventions_table <- lapply(
            unique(interventions_display_table$jurisdiction),
            \(xx){ 
                zusammen <- data.table()
                bestimmt <- interventions_display_table[jurisdiction == xx]
                
                for(wave_number in 1:4)
                {
                    wave_specific <- data.table()
                    
                    for(typ in unique(bestimmt$type))
                    {
                        konkret <-  bestimmt[type == typ] %>%
                            how_much_of_the_wave(wave_number) %>%
                            data.table() %>%
                            dplyr::select(!!typ := ".")
                            
                        wave_specific <- cbind.fill(wave_specific, konkret)
                    }
                    
                    zusammen <- rbind(
                        zusammen,
                        wave_specific %>%
                            data.table() %>%
                            dplyr::mutate(province = xx, wave = wave_number)
                    ) 
                    
                }
                return(zusammen)
            }
        ) %>%
        rbindlist(fill=TRUE) %>%
        replace(is.na(.), "None")
        

    # interventions_timelimes_plot <- interventions_display_table %>%
    #     gg_vistime(
    #         col.start = "start", 
    #         col.end = "end", 
    #         col.event = "type", #"type", 
    #         col.group = "jurisdiction", 
    #         col.color = "colour", 
    #         show_labels = FALSE
    #     ) +
    #     geom_vline(xintercept = as.POSIXct(Canada_Wave_Dates)) + # wave dates are defined in the function header
    #     theme(
    #         axis.text.x = element_text(angle=45, hjust=1),
    #         axis.text = element_text(size = 13),
    #         axis.title = element_text(size = 15)
    #     ) +
    #     scale_x_datetime(
    #         breaks = seq(
    #             min(as.POSIXct(display_table$start)),
    #             as.POSIXct(Sys.Date()),
    #             "months"
    #         ),
    #         date_labels = "%b %Y",
    #         expand = c(0,0)
    #     ) +
    #     labs(x="Month", y="Province")
    # ggsave(
    #     interventions_timelimes_plot, 
    #     file = file.path(PROJECT_FOLDER, "Graphs/interventionsplot.png"), 
    #     width=10, height=12
    # )
    
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
        rbindlist %>% 
        dplyr::select(-pruid, -week_end)
    
    interventions_by_wave_province <- merge(
        vaccination_in_waves, 
        categorical_interventions_table, all=TRUE) %>% 
            dplyr::mutate(
                PROV_vaxx_AL1D = replace(PROV_vaxx_AL1D, is.na(PROV_vaxx_AL1D), 0),
                PROV_vaxx_FULL = replace(PROV_vaxx_FULL, is.na(PROV_vaxx_FULL), 0)
        ) %>%
        dplyr::select(-vaccination)
}
    
############## COVID_19 DATA
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

    ################################## BRITISH COLUMBIA

    writeLines("\nBritish Columbia")
    BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") %>%
        dplyr::select(-c(Province, HA, Cases_Reported_Smoothed)) %>%
        dplyr::filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) %>%
        dplyr::rename(date=Date, HR=HSDA, cases=Cases_Reported) %>%
        dplyr::mutate(date=as.Date(date), province="British Columbia") %>%
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
        data.table()
    fwrite(QC_cases, sprintf("%s/CaseDataTables/QC_cases.csv", PROJECT_FOLDER))

    ##################################  MANITOBA

    writeLines("\nManitoba")
    MB_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "Manitoba") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Manitoba") %>%
        data.table()
    fwrite(MB_cases, sprintf("%s/CaseDataTables/MB_cases.csv", PROJECT_FOLDER))

    ################################## SASKATCHEWAN

    writeLines("\nSaskatchewan")
    SK_cases <- get_saskatchewan_case_data() %>%
        dplyr::filter(!is.na(`Total Cases`) & !grepl("total", tolower(Region))) %>%
        dplyr::select("Date", "Region", "New Cases") %>%
        dplyr::rename(date=Date, HR=Region, cases="New Cases") %>%
        dplyr::mutate(date=as.Date(date), province="Saskatchewan") %>%
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
        data.table()
    fwrite(PE_cases, sprintf("%s/CaseDataTables/PE_cases.csv", PROJECT_FOLDER))

    PE_vacc <- data.table(UofT_api_vaccine_data)[grepl("pei", tolower(province))]
    fwrite(PE_vacc, sprintf("%s/CaseDataTables/PE_vacc.csv", PROJECT_FOLDER))

    NT_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "NWT") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Northwest Territories", HR="Northwest Territories") %>%
        data.table()
    fwrite(NT_cases, sprintf("%s/CaseDataTables/NT_cases.csv", PROJECT_FOLDER))

    NT_vacc <- data.table(UofT_api_vaccine_data)[grepl("northwest", tolower(province))]
    fwrite(NT_vacc, sprintf("%s/CaseDataTables/NT_vacc.csv", PROJECT_FOLDER))

    YT_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "Yukon") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Yukon") %>%
        data.table()
    fwrite(YT_cases, sprintf("%s/CaseDataTables/YT_cases.csv", PROJECT_FOLDER))

    YT_vacc <- data.table(UofT_api_vaccine_data)[grepl("yukon", tolower(province))]
    fwrite(YT_vacc, sprintf("%s/CaseDataTables/YT_vacc.csv", PROJECT_FOLDER))

    NU_cases <- UofT_api_case_data %>%
        dplyr::filter(province == "Nunavut") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Nunavut") %>%
        data.table()
    fwrite(NU_cases, sprintf("%s/CaseDataTables/NU_cases.csv", PROJECT_FOLDER))

    NU_vacc <- data.table(UofT_api_vaccine_data)[grepl("nuna", tolower(province))]
    fwrite(NU_vacc, sprintf("%s/CaseDataTables/NU_vacc.csv", PROJECT_FOLDER))

    ################################## ONTARIO

    writeLines("\nOntario - cases")
    ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") %>%
        dplyr::select(-Total) %>%
        melt(id.vars="Date") %>%
        dplyr::rename(date=Date, HR=variable, cases=value) %>%
        dplyr::mutate(province = "Ontario", date=as.Date(date)) %>%
        data.table()
    fwrite(ON_cases, sprintf("%s/CaseDataTables/ON_cases.csv", PROJECT_FOLDER))

    ON_vacc <- fread("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/2a362139-b782-43b1-b3cb-078a2ef19524/download/vaccines_by_age_phu.csv")
    fwrite(ON_vacc, sprintf("%s/CaseDataTables/ON_vacc.csv", PROJECT_FOLDER))

    ################################## ALBERTA

    writeLines("\nAlberta")
    AB_cases <- fread("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") %>%
        dplyr::select(-V1) %>%
        dplyr::rename(date="Date reported", HR="Alberta Health Services Zone", type="Case type", age="Age group", status="Case status") %>%
        dplyr::mutate(date=as.Date(date)) %>%
        dplyr::filter(HR!="Unknown") %>%
        dplyr::group_by(date, HR) %>%
        dplyr::tally() %>%
        dplyr::rename(cases=n) %>%
        dplyr::mutate(province="Alberta") %>%
        data.table()
    fwrite(AB_cases, sprintf("%s/CaseDataTables/AB_cases.csv", PROJECT_FOLDER))

    ################################## NEW BRUNSWICK

    writeLines("\nNew Brunswick")
    NB_cases <- UofT_api_case_data %>%
        dplyr::filter(province=="New Brunswick") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date)) %>%
        data.table()
    fwrite(NB_cases, sprintf("%s/CaseDataTables/NB_cases.csv", PROJECT_FOLDER))

    ################################## NOVA SCOTIA

    writeLines("\nNova Scotia")
    NS_cases <- UofT_api_case_data %>%
        dplyr::filter(province=="Nova Scotia") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date)) %>%
        data.table()
    fwrite(NS_cases, sprintf("%s/CaseDataTables/NS_cases.csv", PROJECT_FOLDER))

    ################################## NEWFOUNDLAND LABRADOR

    writeLines("\nNewfoundland and Labrador")
    NL_cases <- UofT_api_case_data %>%
        dplyr::filter(province=="NL") %>%
        dplyr::select(province, date_report, health_region, cases) %>%
        dplyr::rename(date=date_report, HR=health_region) %>%
        dplyr::mutate(date=as.Date(date), province="Newfoundland and Labrador") %>%
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
    merge(interventions_by_wave_province, by=c("province", "wave"), all=TRUE) %>%
    merge(PHU_information, by=c("province", "HRUID2018", "HR"), all=TRUE) %>%
    dplyr::filter(!is.na(HRUID2018)) %>%
    dplyr::mutate( PHU_area_km2 =  as.numeric(st_area(geometry))/1000**2 ) %>%
    dplyr::mutate(PHU_pop_density = as.numeric(PHU_population/PHU_area_km2)) %>%
    setNames(gsub(" ", "_", names(.))) %>%
    st_sf
    saveRDS(Regression_Data, file=file.path(PROJECT_FOLDER, "Classifications/regression_data.rda"))
    
