library(dplyr)
library(data.table)
library(cancensus)
library(CanCovidData)
library(stringr)

source("LUT_header.R")

# setwd('/home/bren/Documents/GitHub/MIZ_project/')

list_of_levels <- list_census_regions("CA16")

# CSD_info <-  data.table()
# for(csd_number in list_of_levels |> filter(level=="CSD") |> pull(region))
# {
#     CSD_info <- rbind(
#         CSD_info,
#         get_census(
#             dataset="CA20",
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
# write.csv(CSD_info, "Classifications/csd_information.csv")

CSD_info <- tibble(read.csv("Classifications/csd_information.csv")) 

CSD_info <- CSD_info |>
    select(names(which(colSums(CSD_info != "", na.rm=T) != 0)), -X) |>
    mutate(HR = unlist(lapply(GeoUID, lookup_HR))) |>
    mutate(Province = unlist(lapply(GeoUID, lookup_province))) |>
    rename(
        Region = Region.Name, 
        Area_sq_km = "Area..sq.km.", 
        Population_Density = "v_CA16_406..Population.density.per.square.kilometre",
        Cpls_w_children = "v_CA16_493..Couples.with.children",
        Cpls_w_0_ch = "v_CA16_492..Couples.without.children",
        Cpls_w_1_ch = "v_CA16_494..1.child",
        Cpls_w_2_ch =  "v_CA16_495..2.children",
        Cpls_w_3_or_more_ch = "v_CA16_496..3.or.more.children",
        Sgls_w_ch = "v_CA16_497..Total...Lone.parent.census.families.in.private.households...100..data",
        Sgls_w_1_ch = "v_CA16_498..1.child",
        Sgls_w_2_ch = "v_CA16_499..2.children",
        Sgls_w_3_or_more_ch = "v_CA16_500..3.or.more.children",
        Not_in_families = "v_CA16_501..Total...Persons.not.in.census.families.in.private.households...100..data",
        Commute_wi_CSD = "v_CA16_5780..Commute.within.census.subdivision..CSD..of.residence",
        Commute_wi_CD_not_CSD = "v_CA16_5783..Commute.to.a.different.census.subdivision..CSD..within.census.division..CD..of.residence",
        Commute_wi_prov_not_CD = "v_CA16_5786..Commute.to.a.different.census.subdivision..CSD..and.census.division..CD..within.province.or.territory.of.residence",
        Commute_extra_prov = "v_CA16_5789..Commute.to.a.different.province.or.territory",
    ) |>
    mutate(CSDtype = get_region_types(Region), Region = strip_region_types(Region))

write.csv(
    CSD_info |> filter(Province=="Saskatchewan") |>select("GeoUID", "Type", "Region", "Province", "HR"),
    "~/Saskatchewan_MWE.csv",
    row.names=FALSE
)

raw_CMAs <- data.table(names=readLines("Classifications/raw_CMAs.txt"))
CMAs <- data.table(GeoUID=numeric(), Region=numeric(), Title=character(), SACCODE=numeric(), CMAtype=character(), Class=character())
for(index in 1:nrow(raw_CMAs))
{
    the_number <- str_extract(raw_CMAs$names[index], "[0-9]+")
    the_name <- gsub(paste0(the_number, " - "), '', raw_CMAs$names[index])
    
    if(nchar(the_number) == 3)
    {
        cma_name <- gsub("--", "-", the_name)
        cma_number <-  the_number
    } else {
        CMAs <- rbind(
            CMAs,
            list(
                GeoUID = as.integer(the_number),
                Region = the_name,
                Class = "CMA",
                Title =  cma_name,
                CMAtype = "B",
                SACCODE = cma_number
            )
        )
    }
}

raw_CAs <- data.table(names=readLines("Classifications/raw_CAs.txt"))
CAs <- data.table(GeoUID=numeric(), Region=numeric(), Title=character(), SACCODE=numeric(), CMAtype=character(), Class=character(), SACtype=character())
for(index in 1:nrow(raw_CAs))
{
    the_number <- str_extract(raw_CAs$names[index], "[0-9]+")
    the_name <- gsub(paste0(the_number, " - "), '', raw_CAs$names[index])
    
    if(nchar(the_number) == 3)
    {
        ca_name <- gsub("--", "-", the_name)
        ca_number <- the_number
    } else {
        CAs <- rbind(
            CAs,
            list(
                GeoUID = as.integer(the_number),
                Region = the_name,
                Class = "CA",
                Title = ca_name,
                CMAtype = "D/K",
                SACCODE = ca_number,
                SACtype = "2/3"
            )
        )
    }
}

MIZ_strong <- {fread("Classifications/raw_MIZ_strong.csv") |>
        mutate(
            Title = "Strong MIZ", 
            SACCODE = 996, 
            SACtype = "4", 
            CMAtype = "G",
            GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
            Region = unlist(lapply(Code, str_sub, start=8)),
            Class = "Strong"
        ) |>
        select(-Code, -`Census subdivision`)}

MIZ_moderate <- {fread("Classifications/raw_MIZ_moderate.csv") |> 
        mutate(
            Title = "Moderate MIZ", 
            SACCODE = 997, 
            SACtype = "5", 
            CMAtype = "H",
            GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
            Region = unlist(lapply(Code, str_sub, start=8)),
            Class = "Moderate"
        ) |>
        select(-Code, -`Census subdivision`)}

MIZ_weak <- {fread("Classifications/raw_MIZ_weak.csv") |>
        mutate(
            Title = "Weak MIZ", 
            SACCODE = 998, 
            SACtype = "6", 
            CMAtype = "I",
            GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
            Region = unlist(lapply(Code, str_sub, start=8)),
            Class = "Weak"
        ) |>
        select(-Code, -`Census subdivision`)}

MIZ_none <- {fread("Classifications/raw_MIZ_none.csv") |>
        mutate(
            Title = "No MIZ", 
            SACCODE = 999, 
            SACtype = "7/8", 
            CMAtype = "J/L",
            GeoUID = unlist(lapply(Code, str_extract, "[0-9]+")),
            Region = unlist(lapply(Code, str_sub, start=8)),
            Class = "None"
        ) |>
        select(-Code, -`Census subdivision`)}

Influence_info <- rbind(CMAs, CAs, MIZ_strong, MIZ_moderate, MIZ_weak, MIZ_none, fill=T)

Remoteness <- fread("Classifications/Index_of_remoteness.csv") |>
    select(CSDuid, Index_of_remoteness) |>
    rename(GeoUID = CSDuid)

Total_Info <- data.table(Reduce(
    function(x, y, ...) merge(x, y, by=c("GeoUID"), all = TRUE, ...),
        list(CSD_info, Influence_info, Remoteness)
    )) |>
    rename(
        Type = Type.y,
        SGC2016 = Type.x,
        CSDUID2016 = GeoUID
    ) |>
    mutate(Region = coalesce(Region.y, Region.x)) |>
    select(-Region.x, -Region.y) |>
    relocate(
        "CSDUID2016", "Region", "SGC2016", "CSDtype", "SACtype", "Type", 
        "CMAtype", "SACCODE", "Class", "Title", "Province", "HR", "Index_of_remoteness"
    ) |>
    arrange(is.na(SACtype))

duped <- Total_Info |> filter(HR == "duplicate")
Total_Info <- Total_Info |> filter(! HR %in% c("missing", "duplicate"))

for(index in 1:nrow(duped))
{
    the_row <- duped |> slice(index) |> select(-HR)
    geouid <- the_row |> pull(CSDUID2016)
    
    HRs <- HR_info |> filter(CSDUID2016 == geouid) |> pull(ENGNAME) |> as.character()
    
    for(region in HRs) Total_Info <- rbind(Total_Info, the_row |> mutate(HR=region))
}

# I'm assuming that all the classes that aren't given are none MIZ
Total_Info <- Total_Info |>
    select(-SGC2016, -CSDtype, -Type, -CMAtype, -Title, -SACtype, -SACCODE, -CD_UID, -PR_UID, -CMA_UID) |>
    mutate(Class  = ifelse(is.na(Class), "None", Class)) |>
    mutate(MIZ_score = zone_scores(Class))

write.csv(Total_Info, "Classifications/Classification_HR_Households_Commute.csv", row.names=F)

write.csv(
    Total_Info[Province=="Saskatchewan"] |> select("CSDUID2016", "Province", "Region", "HR", "Class", "Index_of_remoteness"),
    "~/Saskatchewan_MWE.csv",
    row.names=FALSE
)

print("DONE WITH HEADER FUNCTIONS")
