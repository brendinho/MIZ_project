library(dplyr)
library(data.table)
library(cancensus)
library(CanCovidData)
library(stringr)

setwd('/home/bren/Documents/GitHub/MIZ_project/')

source("NOW_function_header.R")

# list_of_levels <- list_census_regions("CA16")
# CSD_info <-  data.table()
# for(csd_number in list_of_levels |> filter(level=="CSD") |> pull(region))
# {
#     CSD_info <- rbind(
#         CSD_info,
#         get_census(
#             dataset="CA16",
#             regions=list(CMA=csd_number),
#             vectors=c(
#                 sprintf("v_CA16_%i", 492:501), # household structure information
#                 sprintf("v_CA16_57%i", c(80, 83, 86, 89)), # commuting information
#                 "v_CA16_406"
#             ),
#             level="CSD"
#         )
#     )
# }
# write.csv(CSD_info, "Classifications/raw_csd_information.csv")

CSD_info <- fread("Classifications/raw_csd_information.csv") |>
    dplyr::select(-V1, -Type) |>
    dplyr::mutate(province = unlist(lapply( GeoUID, lookup_province )) ) |>
    dplyr::rename(
        area_sq_km = "Area (sq km)",
        population_density = "v_CA16_406: Population density per square kilometre",
        cpls_w_children = "v_CA16_493: Couples with children",
        cpls_w_0_ch = "v_CA16_492: Couples without children",
        cpls_w_1_ch = "v_CA16_494: 1 child",
        cpls_w_2_ch =  "v_CA16_495: 2 children",
        cpls_w_3_or_more_ch = "v_CA16_496: 3 or more children",
        sgls_w_ch = "v_CA16_497: Total - Lone-parent census families in private households - 100% data",
        sgls_w_1_ch = "v_CA16_498: 1 child",
        sgls_w_2_ch = "v_CA16_499: 2 children",
        sgls_w_3_or_more_ch = "v_CA16_500: 3 or more children",
        not_in_families = "v_CA16_501: Total - Persons not in census families in private households - 100% data",
        commute_wi_CSD = "v_CA16_5780: Commute within census subdivision (CSD) of residence",
        commute_wi_CD_not_CSD = "v_CA16_5783: Commute to a different census subdivision (CSD) within census division (CD) of residence",
        commute_wi_prov_not_CD = "v_CA16_5786: Commute to a different census subdivision (CSD) and census division (CD) within province or territory of residence",
        commute_extra_prov = "v_CA16_5789: Commute to a different province or territory",
    ) |>
    dplyr::mutate(region = strip_region_types(`Region Name`), csd_type = get_region_types(`Region Name`)) |>
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
    dplyr::rename(GeoUID = CSDuid)

# test <- data.table(date=as.Date("2020-03-24"), HR="Huron-Perth", province="Ontario")

# assemble the complete table we need for the correlation before
data.table(Reduce(
    function(x, y, ...) merge(x, y, by=c("GeoUID"), all = TRUE, ...),
        list(CSD_info, Influence_info, Remoteness)
    )) |>
    dplyr::rename(CSDUID2016 = GeoUID) |>
    dplyr::mutate(
        region = coalesce(region.y, region.x),
        class  = ifelse(is.na(class), "NA", class),
        # class  = ifelse(is.na(class), "None", class),
        # miz_score = zone_scores(class)
    ) |>
    dplyr::select(-region.x, -region.y, -Type, -title, -csd_type, -CD_UID, -PR_UID, -CMA_UID, -rguid) |> #
    dplyr::rename_with(tolower) |>
    add_HRs("csduid2016", "province") |>
    dplyr::relocate("csduid2016", "region", "province", "HR", "class", "index_of_remoteness", "population_density") |> # "miz_score"
    write.csv("Classifications/Total_CSD_Info.csv", row.names=F)
