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
Total_Data <- fread("CaseDataTables/Total_Data.csv")
Total_Toronto_Data <- readRDS("CaseDataFiles/Total_Toronto_Data_with_odds_ratios.rda")

# saveRDS(st_union(Regions$geometry), sprintf("%s/All_Canada.rds", PROJECT_FOLDER))

All_Canada <- st_sf(readRDS(sprintf("%s/All_Canada.rds", PROJECT_FOLDER)))

province_metrics <- Regions[, .(
        num_HR = factor(length(unique(HR))), 
        num_csd = sum(num_csds, na.rm=TRUE), 
        population = sum(population, na.rm=TRUE),
        area_sq_km = as.numeric(sum(st_area(geometry), na.rm=TRUE)/1000**2)
    ), by=.(province)] %>% 
    dplyr::mutate(
        population_density = population/area_sq_km,
        province = factor(province, levels=as.character(.[order(num_csd)]$province))
    )

scaling_factor <- province_metrics[, max(num_csd)/max(population_density)]
SAC_distribution <- ggplot(province_metrics, aes(x=reorder(str_wrap(province, width=15), num_csd), group=province)) +
    geom_bar(aes(y=num_csd, fill=num_HR), position="dodge", stat="identity") +
    geom_line(aes(y=population_density*scaling_factor), size=2, group=1, colour="blue") + 
    geom_hline(aes(yintercept = 1.1*max(province_metrics$num_csd))) +
    labs(x="Province", y="Census Subdivisions (CSDs)") +
    scale_y_continuous(sec.axis = sec_axis(~./scaling_factor, name="Population Density [km^2]"), expand=c(0,0)) +
    theme_bw() +
    theme(
        axis.text.y = element_text(size=16),
        axis.title = element_text(size=16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16),
        axis.text.x = element_text(angle=45, vjust=0.5, size=16),
        legend.position = "top",
        legend.direction="horizontal",
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
    ) +
    scale_fill_brewer(type="div", palette="BrBG") +
    guides(fill=guide_legend(title="Reporting Region count:", nrow=1))
    ggsave(SAC_distribution, file=sprintf("%s/Graphs/SAC_distribution.png", PROJECT_FOLDER), width=15, height=6)
    system(sprintf("convert %s/Graphs/SAC_distribution.png -trim %s/Graphs/SAC_distribution.png", PROJECT_FOLDER, PROJECT_FOLDER))

SAC_map_with_census_info <- ggplot(st_sf(Regions) %>% dplyr::mutate(class = fct_relevel(class, "CMA", "CA", "Strong", "Moderate", "Weak", "None", "Not given"))) +
    geom_sf(data=All_Canada, fill="white") +
    geom_sf(aes(fill = class), lwd = 0) +
    scale_fill_brewer(type="div", palette="RdYlBu", name="Statistical\nArea\nClassification") +
    # scale_fill_brewer(type="div", name="BrBG") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=13),
        legend.title = element_text(size=13),
        legend.key.height = unit(1.5, 'cm'),
        plot.background = element_rect(fill="white")
    ) +
    coord_sf(datum=NA) +
    guides(colour = guide_legend(legend.title.align=0.5))
    ggsave(SAC_map_with_census_info, file=sprintf("%s/Graphs/SAC_choropleth.png", PROJECT_FOLDER))
    system(sprintf("convert %s/Graphs/SAC_choropleth.png -trim %s/Graphs/SAC_choropleth.png", PROJECT_FOLDER, PROJECT_FOLDER))

Figure_1 <- ggarrange(SAC_distribution, SAC_map_with_census_info, labels=c("A","B"), ncol=1, nrow=2, heights=c(4,7), widths=c(1,1.5))
ggsave(Figure_1, file=sprintf("%s/Graphs/SAC_distribution_plots.jpg", PROJECT_FOLDER))
# system(sprintf("convert %s/Graphs/SAC_distribution_plots.jpg -trim %s/Graphs/SAC_distribution_plots.jpg", PROJECT_FOLDER, PROJECT_FOLDER))
# ggsave(SAC_map_with_census_info, file=sprintf("%s/Graphs/SAC_distribution_plots.jpg", PROJECT_FOLDER))

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

Canada_waves <- ggplot(
        canada_temp, #  %>% dplyr::filter(!is.na(avg))
        aes(date, cases)
    ) +
    geom_bar(stat="identity", fill="grey30") +
    geom_line(aes(y=avg), colour="blue", size=1) +
    theme_bw() +
    theme(
        axis.text = element_text(size=13),
        axis.text.x = element_text(angle=45, vjust=0.5)
    ) +
    labs(x="Date", y="Cases") +
    scale_x_date(date_breaks = "1 month" , date_labels = "%b-%Y", expand=c(0,0)) + # limits=c(min(canada_temp$date), max(canada_temp$date))
    annotate(geom = "rect", xmin=min(canada_temp$date), xmax=break_points[1], ymin=-Inf, ymax=Inf, fill="red", alpha=0.04) +
    annotate(geom = "rect", xmin=break_points[1], xmax=break_points[2], ymin=-Inf, ymax=Inf, fill="green", alpha=0.04) +
    annotate(geom = "rect", xmin=break_points[2], xmax=break_points[3], ymin=-Inf, ymax=Inf, fill="blue", alpha=0.04) +
    geom_vline(xintercept=break_points[1], linetype="dashed", color = "red", size=0.5) +
    geom_vline(xintercept=break_points[2], linetype="dashed", color = "red", size=0.5) +
    geom_vline(xintercept=break_points[3], linetype="dashed", color = "red", size=0.5)
    ggsave(Canada_waves, file=sprintf("%s/Graphs/canada_waves.png", PROJECT_FOLDER), height=4, width=10)
    


# full_csds <- merge(
#         st_read("Canada_CSD_shapefiles/lcsd000b16a_e.shp") %>% dplyr::select(CSDUID, geometry, CSDNAME, PRUID, PRNAME),
#         Regions %>% dplyr::select(-geometry),
#         by.x="CSDUID", by.y="csduid2016",
#         all=TRUE
#     ) %>% 
#     dplyr::mutate( class = unlist(lapply(class, \(x) if(is.na(x) || (x=="NA")) "Not given" else x )) ) %>%
#     dplyr::mutate(
#         class = factor(class),
#         province = unlist(lapply( PRNAME, function(x) trimws(str_split(x, "/")[[1]][1]) ))
#     ) %>%
#     dplyr::select(-PRNAME) 
# 
# full_csds <- rbind(
#         full_csds %>% dplyr::filter(is.na(HR)) %>% add_HRs("CSDUID", "province"),
#         full_csds %>% dplyr::filter(!is.na(HR))
#     )

# SAC_map <- ggplot(st_sf(full_csds)) +
#     geom_sf(aes(fill = class), lwd = 0) +
#     scale_fill_viridis_d("Statistical\nArea\nClassification") +
#     theme_minimal() +
#     theme(
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.text = element_text(size=13),
#         legend.title = element_text(size=13),
#         legend.key.height= unit(1.5, 'cm')
#     ) +
#     coord_sf(datum=NA)
#     # ggsave(SAC_map, file="Graphs/SAC_map.png", height=11, width=10)
#     # system(sprintf("convert %s -trim %s", "Graphs/SAC_map.png", "Graphs/SAC_map.png"))
# 
# 
# # plots of the distribution of remoteness and MIZ scores
# Remoteness_map <- ggplot(st_sf(Regions)) +
#     geom_sf(aes(fill = index_of_remoteness), lwd = 0) +
#     scale_fill_viridis("Remoteness", na.value = "black") +
#     theme_minimal() +
#     theme(
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank()
#     ) +
#     coord_sf(datum=NA)
#     # ggsave(Remoteness_map, file="Graphs/Remoteness_map.png", height=10, width=10)
# 
# aR_map <- ggplot(st_sf(Regions %>% dplyr::mutate(aR_score = factor(aR_score)))) +
#     geom_sf(aes(fill = aR_score), lwd = 0) +
#     scale_fill_viridis_d("aR score",  na.value = "black") +
#     theme_minimal() +
#     theme(
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         plot.margin=grid::unit(c(0,0,0,0), "mm")
#     ) +
#     coord_sf(datum=NA)
#     # ggsave(aR_map, file="Graphs/aR_map.png", height=10, width=10)

# # get the geo boundaries of each health region for plotting the map
# class_map <- ggplot()
# # for(hr_code in Regions[, unique(HRUID2018)])
# # {
# #     class_map <- class_map +
# #         geom_sf(
# #             data = st_union(Regions[HRUID2018 == hr_code, geometry]),
# #             mapping = aes_(fill = Total_Data[HRUID2018==hr_code, aR_score])
# #         )
# # }
# for(hr_code in unique(full_csds$HRUID2018))
# {
#     class_map <- class_map +
#         geom_sf(
#             data = st_union(full_csds[HRUID2018 == hr_code, geometry]),
#             mapping = aes_(fill = Total_Data[HRUID2018==hr_code, aR_score]),
#             lwd = 0
#         )
# }
# 
# class_map <- class_map +
#     theme_minimal() +
#     theme(
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank()
#     ) +
#     scale_fill_viridis("aMIZ", na.value = "black") +
#     # scale_colour_viridis("aMIZ") +
#     coord_sf(datum=NA) #+
#     # ggsave(class_map, "Graphs/class_map_c2.png", device='png', dpi=600)

# a map of the health regions of each province colour coded and aR score given
province_map <- function(prov_names, legend_position="none")
{
    if(! legend_position %in% c("top", "bottom", "left", "right"))
    {
        legend_position = "none"
    }

    prov_HR_map <- ggplot()

    for(prov in prov_names)
    {
        if(! prov %in% unique(Regions$province))
        {
            print(sprintf("the province %s was not found in the Regions table", prov))
        }

        for(hr_code in Regions[province==prov, unique(HRUID2018)])
        {
            prov_HR_map <- prov_HR_map +
                geom_sf(
                    data = st_union(Regions[HRUID2018 == hr_code, geometry]),
                    mapping = aes_(fill = Total_Data[
                        HRUID==hr_code,
                        sprintf("%s - %s - %s", pr_uid, HR, aR_score)
                    ])
                )
        }
    }

    prov_HR_map <- prov_HR_map +
        theme_minimal() +
        labs(fill=paste(prov_names, collapse=", ")) +
        theme(
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = legend_position
        ) +
        coord_sf(datum=NA)

    return(prov_HR_map)
}

# ################ toronto chloropleths
# 
# toronto_nbds_cases <- ggplot(st_sf(Total_Toronto_Data)) +
#     geom_sf(aes(fill = num_reports)) +
#     scale_fill_viridis("Case Reports") +
#     theme_minimal() +
#     theme(
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.key.height= unit(2.5, 'cm')
#     ) +
#     coord_sf(datum=NA)
#     # ggsave(toronto_nbds_cases, file="Graphs/toronto_case_reports_map.png", height=10, width=10)
#     # system(sprintf("convert %s -trim %s", "Graphs/toronto_case_reports_map.png", "Graphs/toronto_case_reports_map.png"))
# 
# toronto_nbds_odds_ratios <- ggplot(st_sf(Total_Toronto_Data) %>% dplyr::filter(odds_ratio != 1.0)) +
#     geom_sf(aes(fill = odds_ratio)) +
#     scale_fill_viridis("Odds Ratios") +
#     theme_minimal() +
#     theme(
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.key.height= unit(2.5, 'cm'),
#         legend.text = element_text(size = 10),
#         legend.title = element_text(size = 10)
#     ) +
#     coord_sf(datum=NA)
#     # ggsave(toronto_nbds_odds_ratios, file="Graphs/toronto_odds_ratios_map.png", height=10, width=10)
#     # system(sprintf("convert %s -trim %s", "Graphs/toronto_odds_ratios_map.png", "Graphs/toronto_odds_ratios_map.png"))