rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)

setwd('/home/bren/Documents/GitHub/MIZ_project/') 

source("NOW_function_header.R")

CSD_score_class <- function(x)
{
    if(grepl("cma", tolower(x))) return(1)
    if(grepl("ca", tolower(x))) return(1)
    if(grepl("strong", tolower(x))) return(2)
    if(grepl("moderate", tolower(x))) return(2)
    if(grepl("weak", tolower(x))) return(3)
    if(grepl("none", tolower(x))) return(3)
    if(grepl("na", tolower(x))) return(3)
    return(NA)
}

CSD_score_normal <- function(x)
{
    if(grepl("cma", tolower(x))) return(1)
    if(grepl("ca", tolower(x))) return(2)
    if(grepl("strong", tolower(x))) return(3)
    if(grepl("moderate", tolower(x))) return(4)
    if(grepl("weak", tolower(x))) return(5)
    if(grepl("none", tolower(x))) return(6)
    if(grepl("na", tolower(x))) return(7)
    return(NA)
}

# read the MIZ score calculation, here using the classification and aggregation given in the MIZ paper from Seyed
Regions <- fread("Classifications/Total_CSD_Info.csv") |>
    dplyr::mutate(
        index_of_remoteness = unlist(lapply(index_of_remoteness, \(x) if(x==".") NA else as.numeric(x) )),
        num_csds = 1,
        miz_score = unlist(lapply(class, CSD_score_class)),
        miz_score_raw = unlist(lapply(class, CSD_score_normal)),
    )

Case_Data <- fread("CaseDataTables/Total_Case_Data.csv") |> dplyr::mutate(date = as.Date(date))

Cumul_Cases <- Case_Data[
        HR!="Not Reported",
        .(
            first_wave_cases=sum(.SD[wave==1]$cases, na.rm=T),
            second_wave_cases=sum(.SD[wave==2]$cases, na.rm=T),
            cases=sum(cases, na.rm=T)
        ),
        by=.(HR, province, HRUID2018)
    ]

Time_to_Peak <- merge(
        Case_Data[HR!="Not Reported", .SD[wave==1][which.max(cases)], by=.(HRUID2018)] |>
            dplyr::mutate(first_wave_days_since=date-as.Date('2020-01-23')) |>
            dplyr::rename(first_wave_peak_date=date, first_wave_peak_cases=cases) |>
            dplyr::select(-wave),
        Case_Data[HR!="Not Reported", .SD[wave==2][which.max(cases)], by=.(HRUID2018)] |>
            dplyr::mutate(second_wave_days_since=date-as.Date('2020-01-23'))|>
            dplyr::rename(second_wave_peak_date=date, second_wave_peak_cases=cases) |>
            dplyr::select(-wave),
        by=c("HRUID2018", "HR", "province")
    ) |>
    dplyr::mutate(interpeak_distance=as.numeric(second_wave_days_since-first_wave_days_since))
    
Remoteness <- Regions[,
        lapply(.SD, sum, na.rm=TRUE),
        .SDcols = setdiff(names(Regions), c("csduid2016", "region", "province", "HR", "class", "population_density", "HRUID2018")),
        by=.(HR, HRUID2018, province)
    ]

Total_Data <- Reduce(
    function(...) merge(..., all = TRUE, by = c("HR", "HRUID2018", "province")),
    list(Remoteness, Time_to_Peak, Cumul_Cases)
)

###################################### DATA PARSING######################################

# SAC_distribution <- ggplot(Regions %>% group_by(province, miz_score) %>% tally()) +
#     geom_bar(aes(fill=miz_score, y=n, x=str_wrap(province, width=15)), position="dodge", stat="identity") +
#     labs(x="Province", y="CSD count") +
#     # scale_fill_viridis(discrete = T) +
#     theme_bw() +
#     theme(
#         axis.text.x = element_text(angle = 45, vjust = 0.5)
#     )
# ggsave(SAC_distribution, file="Graphs/SAC_dist.jpg", width=10, height=4)

# total_SAC_distribution <- ggplot(Regions %>% group_by(miz_score) %>% tally()) +
#     geom_bar(aes(y=n, x=str_wrap(miz_score, width=15)), fill="blue", position="dodge", stat="identity") +
#     labs(x="MIZ scoree", y="SAC count") +
#     theme_bw() +
#     theme(
#     )
# ggsave(total_SAC_distribution, file="Graphs/total_SAC_dist.jpg", width=10, height=4)

# ON_score_distribution <- ggplot(Regions %>% filter(province=="Ontario") %>% group_by(HR) %>% tally() %>% arrange(n)) +
#     geom_bar(aes(y=n, x=str_wrap(HR, width=20)), fill="blue", position="dodge", stat="identity") +
#     labs(x="Health Region", y="CSD count") +
#     theme_bw() +
#     theme(
#         axis.text.x = element_text(angle = 45, vjust = 0.5)
#     )
# print(ON_score_distribution)
# ggsave(ON_score_distribution, file="Graphs/ontario_score_dist.jpg", width=10, height=4)
# 
# MB_score_distribution <- ggplot(Regions %>% filter(province=="Manitoba") %>% group_by(HR) %>% tally() %>% arrange(n)) +
#     geom_bar(aes(y=n, x=str_wrap(HR, width=20)), fill="blue", position="dodge", stat="identity") +
#     labs(x="Health Region", y="CSD count") +
#     theme_bw() +
#     theme(
#         axis.text.x = element_text(angle = 45, vjust = 0.5)
#     )
# print(MB_score_distribution)
# ggsave(MB_score_distribution, file="Graphs/manitoba_score_dist.jpg", width=10, height=4)

# all_province_cases <- ggplot(Case_Data[cases>=0, sum(cases, na.rm=T), by=c("date", "province")]) +
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
# ontario_cases <- ggplot(Case_Data[cases>=0 & province=="Ontario", sum(cases), by=c("date", "HR", "province")]) +
#     geom_line(aes(x=date, y=V1, colour=str_wrap(HR, 20))) +
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

# print(ggplot(
#         Total_Data |> dplyr::mutate(x=miz_score_raw, y=cumul_cases), 
#         aes(x=x, y=y, colour=province)) +
#     geom_point(size=3) +
#     theme_bw() +
#     labs(x="Score", y="cumulative case count") +
#     theme(
#     ) +
#     scale_x_discrete(expand = c(0, 0)) +
#     scale_y_continuous(expand = c(0, 0)) +
#     guides(
#         colour = guide_legend(
#             # override.aes = list(shape = 2),
#             title = guide_legend(title = "Province")
#         )
#     ))
# ggsave(all_provinces_points, file="Graphs/all_provinces_points.jpg", width=10, height=4)
# 
# for(prov in Total_Data |> filter(! province %in% c("Northwest Territories", "Yukon", "Prince Edward Island", "Nunatut")) |> pull(province) |> unique())
# {
#     ggplot(Total_Data[province==prov], aes(x=index_of_remoteness, y=cumul_cases, colour=province)) +
#         geom_point(size=3) +
#         theme_bw() +
#         labs(x="Remoteness", y="cumulative case count") +
#         theme(
#         ) +
#         scale_x_discrete(expand = c(0, 0)) +
#         scale_y_continuous(expand = c(0, 0)) +
#         guides(
#             colour = guide_legend(
#                 # override.aes = list(shape = 2),
#                 title = guide_legend(title = "Province")
#             )
#         ) +
#         ggsave(
#             file=paste0("Graphs/", gsub(' ', '_', prov), "_remoteness_points.jpg"),
#             width=10, height=4
#         )
#     
#     ggplot(Total_Data[province==prov], aes(x=miz_score, y=cumul_cases, colour=province)) +
#         geom_point(size=3) +
#         theme_bw() +
#         labs(x="MIZ score", y="cumulative case count") +
#         theme(
#         ) +
#         scale_x_discrete(expand = c(0, 0)) +
#         scale_y_continuous(expand = c(0, 0)) +
#         guides(
#             colour = guide_legend(
#                 # override.aes = list(shape = 2),
#                 title = guide_legend(title = "Province")
#             )
#         ) +
#         ggsave(
#             file=paste0("Graphs/", gsub(' ', '_', prov), "_miz_score_points.jpg"),
#             width=10, height=4
#         )
#     
#     ggplot(Total_Data[province==prov], aes(x=households, y=cumul_cases, colour=province)) +
#         geom_point(size=3) +
#         theme_bw() +
#         labs(x="MIZ score", y="cumulative case count") +
#         theme(
#         ) +
#         scale_x_discrete(expand = c(0, 0)) +
#         scale_y_continuous(expand = c(0, 0)) +
#         guides(
#             colour = guide_legend(
#                 # override.aes = list(shape = 2),
#                 title = guide_legend(title = "Province")
#             )
#         ) +
#         ggsave(
#             file=paste0("Graphs/", gsub(' ', '_', prov), "_households_points.jpg"),
#             width=10, height=4
#         )
# }

    
    
    
    
    



