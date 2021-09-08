rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(stringr)
library(sf)
library(forcats)
library(ggpubr)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(PROJECT_FOLDER)

source("function_header.R")

dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

Regions <- data.table(readRDS("CaseDataTables/Regions.rda"))
Total_Toronto_Data <- readRDS("CaseDataFiles/Total_Toronto_Data_with_odds_ratios.rda")

densities <- data.table(
    province = c(
        "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec", "Ontario",
        "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", "Northwest Territories", "Nunavut"
    ),
    population_density = c(1.4, 24.7, 17.4, 10.5, 5.8, 14.1, 2.2, 1.8, 5.7, 4.8, 0.1, 0.032, 0.017)
)

# saveRDS(st_union(Regions$geometry), sprintf("%s/All_Canada.rds", PROJECT_FOLDER))
# saveRDS(Regions[, .(geometry=st_union(geometry)%>% st_cast("MULTIPOLYGON")), by=.(province)], sprintf("%s/All_Provinces.rds", PROJECT_FOLDER))

All_Canada <- st_sf(readRDS(sprintf("%s/All_Canada.rds", PROJECT_FOLDER)))
All_Provinces <- st_sf(readRDS(sprintf("%s/All_Provinces.rds", PROJECT_FOLDER))) %>%
    merge(., province_LUT %>% dplyr::select(province, abbreviations, alpha), all=TRUE, by="province")

province_metrics <- Regions[, .(
        num_HR = length(unique(HR)),
        num_csd = sum(num_csds, na.rm=TRUE),
        population = sum(population, na.rm=TRUE),
        area_sq_km = as.numeric(sum(st_area(geometry), na.rm=TRUE)/1000**2)
    ), by=.(province)] %>% # [order(num_HR)] %>%
    dplyr::mutate(
        province = factor(province, levels=as.character(.[order(num_csd)]$province)),
        # num_HR = factor(num_HR, levels=sort(unique(.$num_HR)))
    ) %>%
    merge(., densities, by="province") %>%
    merge(., province_LUT, by="province", all=TRUE) %>%
    dplyr::select(-SGC, -region) %>%
    dplyr::arrange(num_HR) %>%
    dplyr::mutate(num_HR = factor(num_HR, levels=sort(unique(.$num_HR))))

SAC_distribution <- ggplot(province_metrics, aes(x=alpha, group=province)) +
    geom_bar(aes(y=num_csd, fill=num_HR), position="dodge", stat="identity") +
    labs(y="Census Subdivisions", x="") + # x="Province",
    theme_bw() +
    theme(
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=14),
        axis.text.x = element_text( size=14), # angle=45, vjust=1, hjust=1
        legend.position = "top",
        legend.direction="horizontal",
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
    ) +
    scale_fill_brewer(type="div", palette="BrBG") +
    guides(fill=guide_legend(title="No. of reporting regions", nrow=1))
    # ggsave(SAC_distribution, file=sprintf("%s/Graphs/SAC_distribution.png", PROJECT_FOLDER), width=15, height=6)

SAC_map_with_census_info <- ggplot(st_sf(
        Regions %>% dplyr::mutate(class = fct_relevel(class, "CMA", "CA", "Strong", "Moderate", "Weak", "None", "Not given"))
    )) +
    geom_sf(aes(fill = class), lwd = 0) +
    geom_sf(data=All_Provinces, fill=NA, colour="black", inherit.aes=FALSE, size=0.5) +
    geom_sf_label(data=All_Provinces, aes(label=alpha)) +
    scale_fill_brewer(type="div", palette="RdYlBu", name="Statistical\nArea\nClassification") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=13),
        legend.title = element_text(size=13),
        legend.key.height = unit(1, 'cm'),
        plot.background = element_rect(fill="white", colour="white")
    ) +
    coord_sf(datum=NA) +
    labs(x="", y="") +
    guides(colour = guide_legend(legend.title.align=0.5))
    # ggsave(SAC_map_with_census_info, file=sprintf("%s/Graphs/SAC_choropleth.png", PROJECT_FOLDER))
    # system(sprintf("convert %s/Graphs/SAC_choropleth.png -trim %s/Graphs/SAC_choropleth.png", PROJECT_FOLDER, PROJECT_FOLDER))

# Figure_1 <- ggarrange(SAC_distribution, SAC_map_with_census_info, labels=c("(A)","(B)"), ncol=1, nrow=2, heights=c(0.9,1.5))
ggsave(
        ggarrange(
            SAC_distribution, SAC_map_with_census_info,
            labels=c("(A)","(B)"), heights=c(0.9,1.5),
            ncol=1, nrow=2
        ),
        file = sprintf("%s/Graphs/SAC_distribution_plots.jpg", PROJECT_FOLDER),
        width=10, height=11.92
    )
system(sprintf("convert %s/Graphs/SAC_distribution_plots.jpg -trim %s/Graphs/SAC_distribution_plots.jpg", PROJECT_FOLDER, PROJECT_FOLDER))

##############################################

weekly_moving_average <- function(x) stats::filter(x, rep(1,7), sides = 1)/7

canada_temp <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=canada") %>%
    .$cases %>%
    dplyr::mutate(
        date = as.Date(date_report, format="%d-%m-%Y"),
        avg = weekly_moving_average(cases)
    ) %>%
    data.table()

break_points <- sort(as.Date(c("2021-07-15", "2021-02-15", "2020-07-15")))
break_point_matrix <- data.table(t(matrix(c(
        as.character(mean(c(min(canada_temp$date), break_points[1]))), max(canada_temp$cases)*0.9, "Wave 1",
        as.character(mean(c(break_points[1],       break_points[2]))), max(canada_temp$cases)*0.9, "Wave 2",
        as.character(mean(c(break_points[2],       break_points[3]))), max(canada_temp$cases)*0.9, "Wave 3"
    ), ncol=3))
    ) %>%
    dplyr::rename(date=V1, cases=V2, wave=V3) %>%
    dplyr::mutate(date=as.Date(date), cases=as.numeric(cases))


Canada_waves <- ggplot(
        canada_temp, #  %>% dplyr::filter(!is.na(avg))
        aes(date, cases)
    ) +
    geom_bar(stat="identity", fill="grey30") +
    geom_line(aes(y=avg), colour="blue", size=1) +
    theme_bw() +
    theme(
        axis.text = element_text(size=13),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1),
        axis.title = element_text(size=13)
    ) +
    labs(y="Daily Incidence", x="") +
    scale_x_date(date_breaks = "1 month" , date_labels = "%d-%b-%Y", expand=c(0,0)) +
    annotate(geom = "rect", xmin=min(canada_temp$date), xmax=break_points[1], ymin=-Inf, ymax=Inf, fill="red", alpha=0.04) +
    geom_label(data = break_point_matrix, aes(x=date, y=cases, label=wave), size=7, fill="grey90") +
    annotate(geom = "rect", xmin=break_points[1], xmax=break_points[2], ymin=-Inf, ymax=Inf, fill="green", alpha=0.04) +
    annotate(geom = "rect", xmin=break_points[2], xmax=break_points[3], ymin=-Inf, ymax=Inf, fill="blue", alpha=0.04) +
    geom_vline(xintercept=break_points[1], linetype="dashed", color = "red", size=0.5) +
    geom_vline(xintercept=break_points[2], linetype="dashed", color = "red", size=0.5) +
    geom_vline(xintercept=break_points[3], linetype="dashed", color = "red", size=0.5)
    ggsave(Canada_waves, file=sprintf("%s/Graphs/canada_waves.png", PROJECT_FOLDER), height=5, width=10)
    system(sprintf("convert %s/Graphs/canada_waves.png -trim %s/Graphs/canada_waves.png", PROJECT_FOLDER, PROJECT_FOLDER))
    
##############################################
    

# # a map of the health regions of each province colour coded and aR score given
# province_map <- function(prov_names, legend_position="none")
# {
#     if(! legend_position %in% c("top", "bottom", "left", "right"))
#     {
#         legend_position = "none"
#     }
# 
#     prov_HR_map <- ggplot()
# 
#     for(prov in prov_names)
#     {
#         if(! prov %in% unique(Regions$province))
#         {
#             print(sprintf("the province %s was not found in the Regions table", prov))
#         }
# 
#         for(hr_code in Regions[province==prov, unique(HRUID2018)])
#         {
#             prov_HR_map <- prov_HR_map +
#                 geom_sf(
#                     data = st_union(Regions[HRUID2018 == hr_code, geometry]),
#                     mapping = aes_(fill = Total_Data[
#                         HRUID==hr_code,
#                         sprintf("%s - %s - %s", pr_uid, HR, aR_score)
#                     ])
#                 )
#         }
#     }
# 
#     prov_HR_map <- prov_HR_map +
#         theme_minimal() +
#         labs(fill=paste(prov_names, collapse=", ")) +
#         theme(
#             panel.grid = element_blank(),
#             axis.text = element_blank(),
#             axis.ticks = element_blank(),
#             legend.position = legend_position
#         ) +
#         coord_sf(datum=NA)
# 
#     return(prov_HR_map)
# }