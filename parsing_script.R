rm(list=ls())

library(data.table)
library(dplyr)
library(RcppAlgos)
library(CanCovidData)
library(ggplot2)
library(janitor)
library(stringr)
library(viridis)
library(cancensus)

setwd('/home/bren/Documents/GitHub/MIZ_project/')


############################### DATA CLEANING AND FINDING HEALTH AUTHORRITY LOOKUPS ############################### 

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

#-----------------------------------------------------------------------------------------------------------------

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

#-----------------------------------------------------------------------------------------------------------------

MIZ_strong <- fread("Classifications/raw_MIZ_strong.csv") %>% mutate(SAC="strong")
MIZ_moderate <- fread("Classifications/raw_MIZ_moderate.csv") %>% mutate(SAC="moderate")
MIZ_weak <- fread("Classifications/raw_MIZ_weak.csv") %>% mutate(SAC="weak")
MIZ_none <- fread("Classifications/raw_MIZ_none.csv") %>% mutate(SAC="none")

Remoteness <- fread("Classifications/Index_of_remoteness.csv") %>% select(-Pruid, -CSDtype, -CSDpop2016, -DGUID)

Remoteness <-  fread("Classifications/Index_of_remoteness.csv") %>% 
    mutate(HA_code=extract_codes(CSDuid)) %>%
    select(-Pruid, -CSDtype, -CSDpop2016, -DGUID) %>%
    mutate(HA=lookup_HAs(HA_code)) %>%
    mutate(province=lookup_provinces(HA_code)) %>%
    rename(Index=Index_of_remoteness, CSD=CSDname) %>%
    mutate(Index=as.numeric(Index)) %>%
    arrange(HA_code)

MIZs <-  rbind(MIZ_strong, MIZ_moderate, MIZ_weak, MIZ_none)
setnames(MIZs, "Code", "SGC2016")
setnames(MIZs, "Census subdivision", "CSD")
MIZs[, SGC2016:=lapply(SGC2016, str_extract, "[0-9]+")]
MIZs[, HA_code:=extract_codes(SGC2016)]

CSDs <- rbind(CMAs, CAs, MIZs %>% select(-Type)) %>%
    mutate(HA=lookup_HAs(HA_code)) %>%
    mutate(province=lookup_provinces(HA_code)) %>%
    arrange(HA_code)

# indices of all the Sask towns
Sask_indices <- which(CSDs$province=="Saskatchewan")
# run the lookup; a good few of them will be blank in this table due to the website not giving
# a comprehensive list of CSDs for each health region
CSDs[Sask_indices, HA:=lookup_SK_HAs(CSD)]
Remoteness[Sask_indices, HA:=lookup_SK_HAs(CSD)]

################################################### DATA ACQUISITION ###################################################

# UofT_api_case_data <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr")$cases %>%
#     mutate(date_report=as.Date(date_report, format="%d-%m-%Y")) %>%
#    data.table()
# fwrite(UofT_api_case_data, "CaseDataTables/UofT_cases.csv")
# 
# Case_Data <- data.table(province=character(), region=character(), date=numeric(), cases=numeric())
# 
# BC_cases <- fread("http://www.bccdc.ca/Health-Info-Site/Documents/BCCDC_COVID19_Regional_Summary_Data.csv") %>%
#     select(-c(Province, HA, Cases_Reported_Smoothed)) %>%
#     filter(! HSDA %in% c("All", "Unknown", "Out of Canada")) %>%
#     rename(date=Date, HA=HSDA, cases=Cases_Reported) %>%
#     mutate(date=as.Date(date), province="British Columbia")
# fwrite(BC_cases, "CaseDataTables/BC_cases.csv")
# 
# QC_cases <- fread("https://www.inspq.qc.ca/sites/default/files/covid/donnees/covid19-hist.csv") %>%
#     select(Date, Nom, cas_quo_tot_n) %>%
#     filter(grepl("\\d", Date) & grepl("\\d - [a-zA-Z]", Nom)) %>%
#     mutate(Nom = Nom %>% replace_accents %>% trim_numbers) %>%
#     rename(date=Date, HA=Nom, cases=cas_quo_tot_n) %>%
#     data.table %>%
#     mutate(date=as.Date(date), province="Quebec")
# fwrite(QC_cases, "CaseDataTables/QC_cases.csv")
# 
# MB_cases <- UofT_api_case_data %>%
#     filter(province == "Manitoba") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date), province="Manitoba")
# fwrite(MB_cases, "CaseDataTables/MB_cases.csv")
# 
# SK_cases <- get_saskatchewan_case_data() %>%
#     select("Date", "Region", "New Cases") %>%
#     rename(date=Date, HA=Region, cases="New Cases") %>%
#     mutate(date=as.Date(date), province="Saskatchewan") %>%
#     data.table
# fwrite(SK_cases, "CaseDataTables/SK_cases.csv")
# 
# province_rename_helper <- function(x)
# {
#     if(x=="PEI") return("Prince Edward Island")
#     if(x=="NWT") return("Northwest Territories")
#     return(x)
# }
# rename_PEI_and_NWT <- function(theList) unlist(lapply(theList, province_rename_helper))
# Territories_cases <- UofT_api_case_data %>%
#     filter(province %in% c("PEI", "Yukon", "NWT", "Nunavut")) %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(province=rename_PEI_and_NWT(province), HA=rename_PEI_and_NWT(HA), date=as.Date(date))
# fwrite(Territories_cases, "CaseDataTables/Territories_cases.csv")
# 
# # Ontario data for confirmed cases - useful for delay distribution, etc
# # https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f
# ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") %>%
#     select(-Total) %>%
#     melt(., id.vars="Date") %>%
#     rename(date=Date, HA=variable, cases=value) %>%
#     mutate(province="Ontario") %>%
#     data.table
# fwrite(ON_cases, "CaseDataTables/ON_cases.csv")
# 
# # Alberta data from https://www.alberta.ca/stats/covid-19-alberta-statistics.htm#data-export
# AB_cases <- fread("https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv") %>%
#     select(-V1) %>%
#     rename(date="Date reported", HA="Alberta Health Services Zone", type="Case type", age="Age group", status="Case status") %>%
#     mutate(date=as.Date(date)) %>%
#     data.table
# AB_cases <- AB_cases[HA!="Unknown", .N, by=c("date", "HA")][order(date)] %>%
#     rename(cases=N) %>% mutate(province="Alberta")
# fwrite(AB_cases, "CaseDataTables/AB_cases.csv")
# 
# NB_cases <- UofT_api_case_data %>%
#     filter(province=="New Brunswick") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date))
# fwrite(NB_cases, "CaseDataTables/NB_cases.csv")
# 
# NS_cases <- UofT_api_case_data %>%
#     filter(province=="Nova Scotia") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date))
# fwrite(NS_cases, "CaseDataTables/NS_cases.csv")
# 
# NL_cases <- UofT_api_case_data %>%
#     filter(province=="NL") %>%
#     select(province, date_report, health_region, cases) %>%
#     rename(date=date_report, HA=health_region) %>%
#     mutate(date=as.Date(date), province="Newfoundland and Labrador")
# fwrite(NL_cases, "CaseDataTables/NL_cases.csv")

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

Scores <- data.table(province=character(), zone=character(), score=numeric(), total=numeric())

for(prov in intersect(unique(Case_Data$province), unique(CSDs$province)))
{
    Cases_Here <- Case_Data[province==prov]
    Classes_Here <- CSDs[province==prov]
    
    for(zone in unique(Classes_Here$HA))
    {
        cumul_sum <- Cases_Here[HA==zone, sum(cases, na.rm=T)]
        
        temp_classes <- Classes_Here[HA==zone]
        
        # written this way so that duplicate CMAs and CAs can be separated and single-counted, rather than double counted
        CMAs <- temp_classes[which(grepl("cma (*)", SAC)), SAC] # unique(temp_classes[which(grepl("cma (*)", temp))])
        CAs  <- temp_classes[which(grepl("ca (*)",  SAC)), SAC] # unique(temp[which(grepl("ca (*)", temp))])
        MIZs <- temp_classes[which(! SAC %in% c(CMAs,  CAs)), SAC]
        
        numerator <- length(CMAs)*zone_scores("cma") + length(CAs)*zone_scores("ca") + sum(zone_scores(MIZs))
        denominator <- length(CMAs) + length(CAs) + length(MIZs)
        
        final_score <- numerator # /denominator
        
        Scores <- rbind(Scores, list(prov, zone, final_score, cumul_sum))
    }
}

Cumul_Cases <- Case_Data[HA!="Not Reported", .(cumul_cases=sum(cases, na.rm=T)), by=c("province", "HA")]
Time_to_Peak <- Case_Data[Case_Data[HA!="Not Reported", .I[which.max(cases)], by=.(HA, province)]$V1][order(HA)] %>% mutate(time_since=date-as.IDate('2020-01-23')) %>% rename(peak_cases=cases, peak_date=date)
Cumul_Remoteness <- Remoteness[, .(cumul_remoteness=sum(Index, na.rm=T)), by=c("province", "HA")]

Scores <- merge(Time_to_Peak, Cumul_Remoteness, Cumul_Cases, by=c("province", "HA"))

######################################3 DATA PARSING ###################################

# CSDs[, SAC:=remove_CA_names(SAC)]
# 
# SAC_distribution <- ggplot(CSDs %>% group_by(province, SAC) %>% tally()) +
#     geom_bar(aes(fill=SAC, y=n, x=str_wrap(province, width=15)), position="dodge", stat="identity") +
#     labs(x="Province", y="CSD count") +
#     scale_fill_viridis(discrete = T) +
#     theme_bw() + 
#     theme(
#         axis.text.x = element_text(angle = 45, vjust = 0.5)
#     )
# ggsave(SAC_distribution, file="Graphs/SAC_dist.jpg", width=10, height=4)
# 
# total_SAC_distribution <- ggplot(CSDs %>% group_by(SAC) %>% tally()) +
#     geom_bar(aes(y=n, x=str_wrap(SAC, width=15)), fill="blue", position="dodge", stat="identity") +
#     labs(x="Province", y="SAC count") +
#     theme_bw() + 
#     theme(
#     )
# ggsave(total_SAC_distribution, file="Graphs/total_SAC_dist.jpg", width=10, height=4)
# 
# 
# ontario_score_distribution <- ggplot(CSDs %>% filter(province=="Ontario") %>% group_by(HA) %>% tally() %>% arrange(n)) +
#     geom_bar(aes(y=n, x=str_wrap(HA, width=20)), fill="blue", position="dodge", stat="identity") +
#     labs(x="Health Authority", y="SAC Score") +
#     theme_bw() +
#     theme(
#         axis.text.x = element_text(angle = 45, vjust = 0.5)
#     )
# ggsave(ontario_score_distribution, file="Graphs/ontario_score_dist.jpg", width=10, height=4)

# 
# all_province_cases <- ggplot(Case_Data[cases>=0, sum(cases), by=c("date", "province")]) +
#     geom_line(aes(x=date, y=V1, colour=province)) +
#     labs(x="Date", y="Cases Reported", legend="Province") +
#     scale_x_date(expand = c(0, 0)) + 
#     scale_y_continuous(expand = c(0, 0)) +
#     theme_bw() +
#     guides(
#         colour = guide_legend(
#             override.aes = list(shape = 1),
#             title = guide_legend(title = "Province")
#         )
#     )
# ggsave(all_province_cases, file="Graphs/all_province_cases.jpg", width=10, height=4)
# 
# 
# all_province_cumulative <- ggplot(Case_Data[, sum(cases, na.rm=T), by=c("date", "province")] %>% group_by(province) %>% mutate(V2=cumsum(V1))) +
#     geom_line(aes(x=date, y=V2, colour=province)) +
#     labs(x="Date", y="Cumulative Cases", legend="Province") +
#     scale_x_date(expand = c(0, 0)) +
#     scale_y_continuous(expand = c(0, 0)) +
#     theme_bw() +
#     guides(
#         colour = guide_legend(
#             override.aes = list(shape = 1),
#             title = guide_legend(title = "Province")
#         )
#     )
# 
# ontario_cases <- ggplot(Case_Data[cases>=0 & province=="Ontario", sum(cases), by=c("date", "HA")]) +
#     geom_line(aes(x=date, y=V1, colour=str_wrap(HA, 20))) +
#     labs(x="Date", y="Cases Reported", legend="Health Region") +
#     scale_x_date(expand = c(0, 0)) + 
#     scale_y_continuous(expand = c(0, 0)) +
#     theme_bw() +
#     theme(
#     ) +
#     guides(
#         colour = guide_legend(
#             override.aes = list(shape = 1),
#             title = guide_legend(title = "Health Authority")
#         )
#     )
# ggsave(ontario_cases, file="Graphs/ontario_cases.jpg", width=10, height=4)
# 
# 
# ggplot(Scores, aes(x=zone, y=score)) + geom_bar(stat="identity")

all_provinces_points <- ggplot(Scores, aes(x=score, y=total, colour=province)) +
    geom_point(size=3) +
    theme_bw() +
    labs(x="Score", y="cumulative case count") +
    theme(
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    guides(
        colour = guide_legend(
            # override.aes = list(shape = 2),
            title = guide_legend(title = "Province")
        )
    )
ggsave(all_provinces_points, file="Graphs/all_provinces_points.jpg", width=10, height=4)

# sprintf(CMA="%s", list_of_levels[level %in% c("CMA"), region])

# for(prov in unique(Scores$province))
# {
#     this_provinces_points <- ggplot(Scores[province==prov], aes(x=score, y=total, colour=province)) +
#         geom_point(size=3) +
#         theme_bw() +
#         labs(x="Score", y="cumulative case count") +
#         theme(
#         ) +
#         scale_x_discrete(expand = c(0, 0)) + 
#         scale_y_continuous(expand = c(0, 0)) +
#         guides(
#             colour = guide_legend(
#                 # override.aes = list(shape = 2),
#                 title = guide_legend(title = "Province")
#             )
#         )
#     ggsave(
#         this_provinces_points, 
#         file=paste0("Graphs/", gsub(' ', '_', prov), "_points.jpg"), 
#         width=10, height=4
#     )
#     
# }


# quebec
print(plot(Scores$score, Scores$total))

print(haha)
    
    
    
    
    



