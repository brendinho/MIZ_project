library(dplyr)
library(data.table)
# library(cancensus)
# library(CanCovidData)
# library(stringr)
library(XLConnect)

setwd('/home/bren/Documents/GitHub/MIZ_project/')
source("NOW_function_header.R")

# comprehensive health region lookup file
# "https://www150.statcan.gc.ca/pub/82-402-x/2018001/corr/Comprehensive_HR2018_16.csv"

# we need this one for the BC HRs
HR_info_mine <- read.csv("https://www150.statcan.gc.ca/pub/82-402-x/2018001/corr/Comprehensive_HR2018_16.csv") |>
    dplyr::select(-DBUID2016, -DBPOP2016, -FRENAME) |>
    unique() |>
    dplyr::rename(HR = ENGNAME) |>
    dplyr::mutate(HR = unlist(lapply(HR, replaceAccents))) |>
    dplyr::mutate(Province = unlist(lapply(CSDUID2016, lookup_province))) |>
    tibble()

lookup_HR_mine <- function(x)
{
    if(x %in% unique(HR_info_mine$CSDUID2016))
    {
        temp <- HR_info_mine |> filter(CSDUID2016 == x) |> pull(HR) |> as.character()
        if(length(temp) > 1) "duplicate" else temp
    } else { "missing" }
}

# we need to use this one for Saskatchewan and Ontario
# I'll use this one for all the other provinces, unless the city name isn't mapped, then I'll default to the first one
HR_info_StatCan <- readWorksheetFromFile(
        "StatCan_CSD-HA.XLSX",
        sheet="CSD_HA",
        startRow = 0,
        endCol = 6
    ) |>
    dplyr::select(-HealthAuthNameFre) |>
    dplyr::rename(HR = HealthAuthNameEng) |>
    dplyr::mutate(HR = unlist(lapply(HR, replaceAccents))) |>
    dplyr::mutate(Province = unlist(lapply(CSDUID2016, lookup_province))) |>
    tibble()

lookup_HR_StatCan <- function(x)
{
    if(x %in% unique(HR_info_StatCan$CSDUID2016)) {
        temp <- HR_info_StatCan |> filter(CSDUID2016 == x) |> pull(HR) |> as.character()
        if(length(temp) > 1) "duplicate" else temp
    } else { "missing" }
}

HA_crosswalk <- fread("Classifications/health_region_crosswalk.csv")
standard_HR_name <- \(x) if(x %in% HA_crosswalk$case_data_name) HA_crosswalk[case_data_name == x, display_name] else x

CSD_info <- read.csv("Classifications/fixed_csd_information.csv") |> tibble()
add_HRs <-  function(table)
{
    here <- data.table(table)
    here[, HR:=""]

    for(row_index in 1:nrow(here))
    {
        if(here[row_index, province] == "British Columbia")
        {
            prospectives <- HR_info_mine |> filter(CSDUID2016==here[row_index, GeoUID]) |> pull(HR)
        } else {
            prospectives <- HR_info_StatCan |> filter(CSDUID2016==here[row_index, GeoUID]) |> pull(HR)
        }

        here[row_index]$HR <- standard_HR_name(prospectives[1])
        for(shared_HR in prospectives[-1])
        {
            here <- rbind(
                here,
                cbind(here[row_index] |> mutate(HR = standard_HR_name(shared_HR)))

            )
        }
    }

    return(here)
}
haha <- add_HRs(CSD_info)


