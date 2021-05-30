rm(list=ls())

library(cancensus)
library(data.table)
library(dplyr)
library(readxl)
library(XLConnect)
library(stringr)

source("LUT_header.R")

setwd('/home/bren/Documents/GitHub/MIZ_project/')

census_info <- tibble()
list_of_levels <- list_census_regions("CA16")

CSD_info <- read.csv("Classifications/csd_information.csv") %>%
    rename(
        Region = Region.Name, 
        Area = "Area..sq.km.", 
        Cpls_w_children = "v_CA16_493..Couples.with.children",
        Cpls_w_0_ch = "v_CA16_492..Couples.without.children",
        Cpls_w_1_ch = "v_CA16_494..1.child",
        Cpls_w_2_ch =  "v_CA16_495..2.children",
        Cpls_w_3_or_more_ch = "v_CA16_496..3.or.more.children",
        Sgls_w_ch = "v_CA16_497..Total...Lone.parent.census.families.in.private.households...100..data",
        Sgls_w_1_ch = "v_CA16_498..1.child",
        Sgls_w_2_ch = "v_CA16_499..2.children",
        Sgls_w_3_or_more_ch = "v_CA16_500..3.or.more.children",
        Not_in_families = "v_CA16_501..Total...Persons.not.in.census.families.in.private.households...100..data"
    ) %>%
    mutate(Region = strip_region_types(Region)) %>%
    select(-X, -Type)
    
xl_data <- "Classifications/CONNECT1_Mixing matrices_20210512.xlsx"

# problem - StatCan definition: "Furthermore, it defines child, for census purposes, as "any unmarried individual, regardless of age, who lives with his or her parent(s) and has no children in the same household." (The subsequent discussion clarifies that unmarried includes never married and divorced.)"

# executive decision - child means 0-14, adult means >=80, in line with the matrices we have

child_indices <- c("X0.4","X5.9", "X10.14")
adult_indices <- c("X15.19", "X20.24", "X25.29", "X30.34", "X35.39", "X40.44", "X45.49", 
                   "X50.54", "X55.59", "X60.64", "X65.69", "X70.74", "X75.79", "X.80")

contacts_home_w_ch <- as.matrix(readWorksheetFromFile(
    xl_data, 
    sheet="Home with children", 
    startRow = 2, 
    endRow=19, 
    endCol = 18
))
rownames(contacts_home_w_ch) <- sprintf("X%s", gsub('-|≥', '.', contacts_home_w_ch[, "Age.contacts"]))
class(contacts_home_w_ch) <- "numeric"
contacts_home_w_ch <- contacts_home_w_ch[,-1:-1]

Homes_w_Ch <- matrix(nrow=2,ncol=2); rownames(Homes_w_Ch) <- c("C", "A"); colnames(Homes_w_Ch) <- c("C", "A")
Homes_w_Ch["C", "C"] <- mean(contacts_home_w_ch[child_indices, child_indices])
Homes_w_Ch["C", "A"] <- mean(contacts_home_w_ch[child_indices, adult_indices])
Homes_w_Ch["A", "C"] <- mean(contacts_home_w_ch[adult_indices, child_indices])
Homes_w_Ch["A", "A"] <- mean(contacts_home_w_ch[adult_indices, adult_indices])

contacts_home_wo_ch <- as.matrix(readWorksheetFromFile(
    xl_data,
    sheet="Home without children",
    startRow = 2,
    endRow=19,
    endCol = 18
))
rownames(contacts_home_wo_ch) <- sprintf("X%s", gsub('-|≥', '.', contacts_home_wo_ch[, "Age.contacts"]))
class(contacts_home_wo_ch) <- "numeric"
contacts_home_wo_ch <- contacts_home_wo_ch[,-1:-1]

Homes_wo_Ch <- matrix(nrow=2,ncol=2); rownames(Homes_wo_Ch) <- c("C", "A"); colnames(Homes_wo_Ch) <- c("C", "A")
Homes_wo_Ch["C", "C"] <- 0 # mean(contacts_home_wo_children[child_indices, child_indices])
Homes_wo_Ch["C", "A"] <- 0 # mean(contacts_home_wo_children[child_indices, adult_indices])
Homes_wo_Ch["A", "C"] <- 0 # mean(contacts_home_wo_children[adult_indices, child_indices])
Homes_wo_Ch["A", "A"] <- mean(contacts_home_wo_ch[adult_indices, adult_indices])

CSD_info$Cpls_w_0_ch_interac <- CSD_info$Cpls_w_0_ch * (2*Homes_wo_Ch['A','A'])

CSD_info$Cpls_w_1_ch_interac <- CSD_info$Cpls_w_1_ch * (2*Homes_w_Ch['A','A'] + 2*Homes_w_Ch['A','C'] + 2*Homes_w_Ch['C','A'])
CSD_info$Cpls_w_2_ch_interac <- CSD_info$Cpls_w_2_ch * (2*Homes_w_Ch['A','A'] + 4*Homes_w_Ch['A','C'] + 4*Homes_w_Ch['C','A'] + 2*Homes_w_Ch['C','C'])
CSD_info$Cpls_w_3_or_more_ch_interac <- CSD_info$Cpls_w_3_or_more_ch * (2*Homes_w_Ch['A','A'] + 6*Homes_w_Ch['A','C'] + 6*Homes_w_Ch['C','A'] + 2*Homes_w_Ch['C','C'])

CSD_info$Sgls_w_1_ch_interac <- CSD_info$Sgls_w_1_ch * (2*Homes_w_Ch['A','A'] + 2*Homes_w_Ch['A','C'] + 2*Homes_w_Ch['C','A'])
CSD_info$Sgls_w_2_ch_interac <- CSD_info$Sgls_w_2_ch * (2*Homes_w_Ch['A','A'] + 4*Homes_w_Ch['A','C'] + 4*Homes_w_Ch['C','A'] + 2*Homes_w_Ch['C','C'])
CSD_info$Sgls_w_3_or_more_ch_interac <- CSD_info$Sgls_w_3_or_more_ch * (2*Homes_w_Ch['A','A'] + 6*Homes_w_Ch['A','C'] + 6*Homes_w_Ch['C','A'] + 2*Homes_w_Ch['C','C'])

CSD_info <- CSD_info %>%
    mutate(HA=lookup_HAs(CD_UID)) %>%
    mutate(Province=lookup_provinces(PR_UID)) %>%
    as.data.table

Sask_indices <- which(CSD_info$Province=="Saskatchewan")
CSD_info[Sask_indices, HA:=lookup_SK_HAs(Region)]

summing_columns <- setdiff(names(CSD_info), c("GeoUID", "Region", "CD_UID", "PR_UID", "CMA_UID", "rguid", "HA", "Province"))
CSD_info <- CSD_info[, lapply(.SD, sum, na.rm=T), .SDcols=summing_columns, by=c("Province", "HA")]

####################################################### STATCAN REMOTENESS SCORE

Remoteness <-  fread("Classifications/Index_of_remoteness.csv") %>% 
    mutate(HA_code=extract_codes(CSDuid)) %>%
    select(-Pruid, -CSDtype, -CSDpop2016, -DGUID) %>%
    mutate(HA=lookup_HAs(HA_code)) %>%
    mutate(Province=lookup_provinces(HA_code)) %>%
    rename(Index=Index_of_remoteness, CSD=CSDname) %>%
    mutate(Index=as.numeric(Index)) 

Remoteness[Province=="Saskatchewan", HA:=lookup_SK_HAs(CSD)]
Remoteness <- Remoteness[, .(Cumul_Index=sum(Index, na.rm=T)), by=.(Province, HA)]

####################################################### CMA/CA/MIZ SCORES

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

MIZ_strong <- fread("Classifications/raw_MIZ_strong.csv") %>% mutate(SAC="strong")
MIZ_moderate <- fread("Classifications/raw_MIZ_moderate.csv") %>% mutate(SAC="moderate")
MIZ_weak <- fread("Classifications/raw_MIZ_weak.csv") %>% mutate(SAC="weak")
MIZ_none <- fread("Classifications/raw_MIZ_none.csv") %>% mutate(SAC="none")

MIZs <-  rbind(MIZ_strong, MIZ_moderate, MIZ_weak, MIZ_none)
setnames(MIZs, "Code", "SGC2016")
setnames(MIZs, "Census subdivision", "CSD")
MIZs[, SGC2016:=lapply(SGC2016, str_extract, "[0-9]+")]
MIZs[, HA_code:=extract_codes(SGC2016)]

CSDs <- rbind(CMAs, CAs, MIZs %>% select(-Type)) %>%
    mutate(HA=lookup_HAs(HA_code)) %>%
    mutate(province=lookup_provinces(HA_code)) %>%
    arrange(HA_code)
CSDs[province=="Saskatchewan", HA:=lookup_SK_HAs(CSD)]
CSDs <- CSDs[HA != ""]

Scores <- data.table(Province=character(), HA=character(), MIZ_score=numeric())

for(prov in unique(CSDs$province))
{
    Classes_Here <- CSDs[province==prov]
    for(zone in unique(Classes_Here$HA))
    {
        temp_classes <- Classes_Here[HA==zone]
        CMAs <- temp_classes[which(grepl("cma (*)", SAC)), SAC]
        CAs  <- temp_classes[which(grepl("ca (*)",  SAC)), SAC]
        MIZs <- temp_classes[which(! SAC %in% c(CMAs,  CAs)), SAC]

        numerator <- length(CMAs)*zone_scores("cma") + length(CAs)*zone_scores("ca") + sum(zone_scores(MIZs))
        denominator <- length(CMAs) + length(CAs) + length(MIZs)

        final_score <- numerator/denominator

        Scores <- rbind(Scores, list(prov, zone, final_score))
    }
}

######################################################################

Total_Info <- Reduce(
        function(...) merge(..., all=TRUE, by=c("Province", "HA")),
        list(Scores, CSD_info, Remoteness)
    )

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

Case_Data[, HA:=get_standard_names(HA)]

