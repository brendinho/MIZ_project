library(dplyr)
library(data.table)
library(cancensus)
library(CanCovidData)
library(stringr)

setwd('/home/bren/Documents/GitHub/MIZ_project/')

source("function_header.R")

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

Remoteness <- fread("Classifications/Index_of_remoteness.csv") |>
    dplyr::select(CSDuid, Index_of_remoteness) |>
    dplyr::rename(GeoUID = CSDuid) |>
    dplyr::mutate(Index_of_remoteness = as.numeric(Index_of_remoteness)) |>
    suppressWarnings()

# test <- data.table(date=as.Date("2020-03-24"), HR="Huron-Perth", province="Ontario")

# assemble the complete table we need for the correlation before
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
