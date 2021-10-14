 rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(stringr)
library(sf)
library(forcats)
library(ggpubr)
library(gganimate)
library(tidyr)
library(future)
library(gapminder)
library(av)
library(transformr)
library(foreach)
library(doParallel)

# PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
PROJECT_FOLDER <- '/home/bren/Documents/GitHub/MIZ_project'

source(file.path(PROJECT_FOLDER, "function_header.R"))
setwd(PROJECT_FOLDER)

dir.create(file.path(PROJECT_FOLDER, "Graphs"), showWarnings=FALSE)

Regions <- data.table(readRDS("CaseDataTables/Regions.rda"))

# densities <- data.table(
#     province = c(
#         "Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec", "Ontario",
#         "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", "Northwest Territories", "Nunavut"
#     ),
#     population_density = c(1.4, 24.7, 17.4, 10.5, 5.8, 14.1, 2.2, 1.8, 5.7, 4.8, 0.1, 0.032, 0.017)
# )
#
# # saveRDS(st_union(Regions$geometry), sprintf("%s/All_Canada.rds", PROJECT_FOLDER))
# # saveRDS(Regions[, .(geometry=st_union(geometry)%>% st_cast("MULTIPOLYGON")), by=.(province)], sprintf("%s/All_Provinces.rds", PROJECT_FOLDER))
#
# All_Canada <- st_sf(readRDS(file.path(PROJECT_FOLDER, "All_Canada.rds")))
# All_Provinces <- st_sf(readRDS(file.path(PROJECT_FOLDER, "All_Provinces.rds"))) %>%
#     merge(., province_LUT %>% dplyr::select(province, abbreviations, alpha), all=TRUE, by="province")
#
# province_metrics <- Regions[, .(
#         num_HR = length(unique(HR)),
#         num_csd = sum(num_csds, na.rm=TRUE),
#         population = sum(population, na.rm=TRUE),
#         area_sq_km = as.numeric(sum(st_area(geometry), na.rm=TRUE)/1000**2)
#     ), by=.(province)] %>% # [order(num_HR)] %>%
#     dplyr::mutate(
#         province = factor(province, levels=as.character(.[order(num_csd)]$province)),
#         # num_HR = factor(num_HR, levels=sort(unique(.$num_HR)))
#     ) %>%
#     merge(., densities, by="province") %>%
#     merge(., province_LUT, by="province", all=TRUE) %>%
#     dplyr::select(-SGC, -region) %>%
#     dplyr::arrange(num_HR) %>%
#     dplyr::mutate(num_HR = factor(num_HR, levels=sort(unique(.$num_HR))))
#
# SAC_distribution <- ggplot(province_metrics, aes(x=alpha, group=province)) +
#     geom_bar(aes(y=num_csd, fill=num_HR), position="dodge", stat="identity") +
#     labs(y="Census Subdivisions", x="") + # x="Province",
#     theme_bw() +
#     theme(
#         axis.text.y = element_text(size=13),
#         axis.title = element_text(size=15),
#         legend.title = element_text(size=15),
#         legend.text = element_text(size=14),
#         axis.text.x = element_text( size=14), # angle=45, vjust=1, hjust=1
#         legend.position = "top",
#         legend.direction="horizontal",
#         axis.line.y.right = element_line(color = "blue"),
#         axis.ticks.y.right = element_line(color = "blue"),
#         axis.title.y.right = element_text(color = "blue"),
#         axis.text.y.right = element_text(color = "blue"),
#     ) +
#     scale_fill_brewer(type="div", palette="BrBG") +
#     guides(fill=guide_legend(title="Number of health regions", nrow=1))
#     # ggsave(SAC_distribution, file=sprintf("%s/Graphs/SAC_distribution.png", PROJECT_FOLDER), width=15, height=6)
#
# SAC_map_with_census_info_SAC <- ggplot(st_sf(
#         Regions %>%
#             dplyr::mutate(
#                 class = fct_relevel(class, "CMA", "CA", "Strong", "Moderate", "Weak", "None", "Not given"),
#                 class = dplyr::recode(class, "Strong"="strong MIZ", "Moderate"="moderate MIZ", "Weak"="weak MIZ", "None"="none MIZ", "None"="no MIZ")
#             )
#     )) +
#     geom_sf(aes(fill = class), lwd = 0) +
#     geom_sf(data=All_Provinces, fill=NA, colour="black", inherit.aes=FALSE, size=0.5) +
#     geom_sf_label(data=All_Provinces, aes(label=alpha)) +
#     scale_fill_brewer(type="div", palette="RdYlBu", name="Statistical\nArea\nClassification") +
#     theme_minimal() +
#     theme(
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.text = element_text(size=13),
#         legend.title = element_text(size=13),
#         legend.key.height = unit(1, 'cm'),
#         plot.background = element_rect(fill="white", colour="white")
#     ) +
#     coord_sf(datum=NA) +
#     labs(x="", y="") +
#     guides(colour = guide_legend(legend.title.align=0.5))
#     ggsave(SAC_map_with_census_info_SAC, file=sprintf("%s/Graphs/SAC_choropleth.png", PROJECT_FOLDER))
#     system(sprintf("convert %s/Graphs/SAC_choropleth.png -trim %s/Graphs/SAC_choropleth.png", PROJECT_FOLDER, PROJECT_FOLDER))
#
# SAC_map_with_census_info_Index <- ggplot(st_sf(
#             Regions %>% dplyr::mutate(binned = cut_number(index_of_remoteness, n = 7))
#         )) +
#         geom_sf(aes(fill = binned), lwd = 0) +
#         geom_sf(data=All_Provinces, fill=NA, colour="black", inherit.aes=FALSE, size=0.5) +
#         geom_sf_label(data=All_Provinces, aes(label=alpha)) +
#         # scale_fill_brewer(type="div", palette="RdYlBu", name="Statistical\nArea\nClassification") +
#         theme_minimal() +
#         theme(
#             panel.grid = element_blank(),
#             axis.text = element_blank(),
#             axis.ticks = element_blank(),
#             legend.text = element_text(size=13),
#             legend.title = element_text(size=13),
#             legend.key.height = unit(1, 'cm'),
#             plot.background = element_rect(fill="white", colour="white")
#         ) +
#         coord_sf(datum=NA) +
#         labs(x="", y="") +
#         scale_fill_discrete(name = "Index of\nRemoteness")
#         guides(colour = guide_legend(legend.title.align=0.5, title="Index of\nRemoteness"))
#     ggsave(SAC_map_with_census_info_Index, file=sprintf("%s/Graphs/SAC_choropleth_index.png", PROJECT_FOLDER))
#     system(sprintf("convert %s/Graphs/SAC_choropleth_index.png -trim %s/Graphs/SAC_choropleth_index.png", PROJECT_FOLDER, PROJECT_FOLDER))
# 
# ggsave(
#         ggarrange(
#             SAC_distribution, SAC_map_with_census_info_SAC,
#             labels=c("(A)","(B)"), heights=c(0.9,1.5),
#             ncol=1, nrow=2
#         ),
#         file = sprintf("%s/Graphs/SAC_distribution_plots.jpg", PROJECT_FOLDER),
#         width=10, height=11.92
#     )
# system(sprintf("convert %s/Graphs/SAC_distribution_plots.jpg -trim %s/Graphs/SAC_distribution_plots.jpg", PROJECT_FOLDER, PROJECT_FOLDER))

HRs_remoteness_metrics <- Regions[, .(
        mean_index = meann(index_of_remoteness),
        weighted_remoteness = summ(mR_score*population)/summ(population),
        geometry = st_union(geometry)
    ), by=.(HRUID2018)]
#     saveRDS(HRs_remoteness_metrics, file=sprintf("%s/Classifications/HRs_remoteness_metrics.rda", PROJECT_FOLDER))

HRs_remoteness_metrics <- readRDS(sprintf("%s/Classifications/HRs_remoteness_metrics.rda", PROJECT_FOLDER)) %>%
    melt(measure.vars=c("mean_index", "weighted_remoteness"), id.vars=c("HRUID2018", "geometry")) %>%
    st_sf()

st_crs(HRs_remoteness_metrics) = 4326

st_sf(HRs_remoteness_metrics %>% filter(variable == "mean_index")) -> haha

ggplot(HRs_remoteness_metrics) +
    geom_sf(aes(fill = value), lwd=0) #+
    # facet_grid(variable~.)
    

#### Health Region Cumulative Case animation

# HR_shapes <- Regions %>%
#     data.table %>%
#     .[, .(geometry=st_union(geometry) %>% st_cast("MULTIPOLYGON")), by=.(HRUID2018)]
#     saveRDS(HR_shapes, file=file.path(PROJECT_FOLDER, "Classifications/HR_shapes.rda"))

# HR_shapes <- readRDS(file.path(PROJECT_FOLDER, "Classifications/HR_shapes.rda"))
# 
# Total_Case_Data <- fread(sprintf("%s/CaseDataTables/Total_Case_Data.csv", PROJECT_FOLDER))
# 
# p_table <- Total_Case_Data %>%
#     dplyr::filter(province == "Alberta") %>%
#     merge(Total_Case_Data %>% expand(HRUID2018, date), by=c("HRUID2018", "date"), all=TRUE) %>%
#     dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
#     dplyr::mutate(
#         cum_cases = ave(.$cases, .$HRUID2018, FUN=cumsum),
#         date=as.Date(date)
#     ) %>%
#     merge(HR_shapes, by=c("HRUID2018")) %>%
#     dplyr::select(-HR, -cases)

# cl <- makeCluster(1L)
# registerDoParallel(cl)
# 
# print_all <- foreach(i=1:5, .packages=c("data.table", "sf", "dplyr")) %dopar% # as.numeric(p_table[, max(date)-min(date)]))
# {
#     p <- ggplot(data = st_sf( p_table[date == as.Date(min(p_table$date)+i)] )) +
#         geom_sf(aes(fill=cum_cases)) +
#         theme_minimal() +
#         theme(
#             panel.grid = element_blank(),
#             axis.text = element_blank(),
#             axis.ticks = element_blank(),
#             legend.text = element_text(size=25),
#             legend.title = element_text(size=25),
#             plot.title = element_text(size=25),
#             # legend.key.height = unit(1, 'cm'),
#             plot.background = element_rect(fill="white",colour="white"),
#             legend.key.size = unit(2, 'cm')
#         ) +
#         coord_sf(datum=NA) +
#         labs(x="", y="", fill="Cumulative\nIncidence") + #, title="Date: {closest_state}") +
#         guides(colour = guide_legend(legend.title.align=0.5), fill=guide_colourbar(barheight = 30)) +
#         # transition_states(date) + # , transition_length=1, state_length=30) +
#         scale_fill_viridis(limits=c(0, max(p_table$cum_cases)))
#     
#     ggsave(p, file=sprintf("%s/Animation/ontario_anim_%i.png", PROJECT_FOLDER, i), height=16, width=16)
# }
# 
# stopCluster(cl)
# 
# p <- ggplot(data = st_sf( p_table )) +
#     geom_sf(aes(fill=cum_cases)) +
#     theme_minimal() +
#     theme(
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.text = element_text(size=25),
#         legend.title = element_text(size=25),
#         plot.title = element_text(size=25),
#         # legend.key.height = unit(1, 'cm'),
#         plot.background = element_rect(fill="white",colour="white"),
#         legend.key.size = unit(2, 'cm')
#     ) +
#     coord_sf(datum=NA) +
#     labs(x="", y="", fill="Cumulative\nIncidence") + #, title="Date: {closest_state}") +
#     guides(colour = guide_legend(legend.title.align=0.5), fill=guide_colourbar(barheight = 30)) +
#     transition_states(date) + # , transition_length=1, state_length=30) +
#     scale_fill_viridis(limits=c(0, max(p_table$cum_cases)))
# 
# print("about to render")
# 
# animate(p,
#     nframes = 5, # nrow(p_table),
#     renderer = file_renderer(
#         file.path(PROJECT_FOLDER, "Animation"),
#         prefix = "Ontario_plot",
#         overwrite = TRUE
#     )
# )
# 
# future::plan("multicore", workers = 12L)
# 
# start_time <- Sys.time()
# # animate(p,
# #         renderer=gifski_renderer(),
# #         height=1600, width=1600 # ,
# #         # fps = ceiling(filled_in_table %>% pull(episode_date) %>% unique %>% length %>% .[]/10)
# #     )
# anim_save(
#         file.path(PROJECT_FOLDER, "Graphs/canada_cumulative.gif"),
#         p,
#         height=1000, width=1600,
#         fps = ceiling(p_table %>% pull(date) %>% unique %>% length %>% .[]/30)
#     )
# # animate(p, fps = 25, renderer = av_renderer("out.mkv"))
# print(Sys.time() - start_time)

##### BREAK POINT ANALYSIS

# weekly_moving_average <- function(x) stats::filter(x, rep(1,7), sides = 1)/7
# 
# canada_temp <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=canada")$cases %>%
#     data.table() %>%
#     dplyr::mutate(
#         date = as.Date(date_report, format="%d-%m-%Y"),
#         moving_avg_seven_days = weekly_moving_average(cases),
#         index = as.numeric(date)
#     ) %>%
#     filter(cases != 0)
# 
# seg_reg <- segmented(
#     lm(log(cases) ~ index, data=canada_temp),
#     seg.Z = ~ index,
#     npsi=7
# )
# # saveRDS(seg_reg, file=file.path(PROJECT_FOLDER, "Classifications/all_breaks_waves.rda"))
# 
# # this makes sure that the dates stay the same during repeated runs
# # seg_reg <- readRDS(file.path(PROJECT_FOLDER, "Classifications/all_breaks_waves.rda"))
# 
# canada <- rbind(
#         canada_temp %>% dplyr::mutate(fit = fitted(seg_reg), cases=log(cases), type="log(Incidence)"),
#         canada_temp %>% dplyr::mutate(fit = exp(fitted(seg_reg)), type="Incidence")
#     )
# 
# break_dates <- canada_temp[index %in% floor(data.frame(seg_reg$psi)$`Est.`)]
# breaks_waves <- break_dates$date[c(2, 5, 7)]
# 
# all_breaks_waves <- rbind(
#         break_dates %>% dplyr::arrange(date) %>% dplyr::mutate(type="Incidence", label=1:nrow(.)),
#         break_dates %>% dplyr::arrange(date) %>% dplyr::mutate(type="log(Incidence)", cases=log(cases), label=1:nrow(.))
#     ) %>%
#     merge(canada %>%
#     select(date, fit, type), by=c("date", "type"))
# 
# canada_wave_break_point_analysis <- ggplot(canada, aes(x=date)) +
#     geom_point(aes(y=cases), size=0.5) +
#     geom_line(aes(y=fit), size=0.75, colour="blue") +
#     geom_point(data=all_breaks_waves, aes(x=date, y=fit), pch=21, fill=NA, size=5, colour="red", stroke=1) +
#     geom_point(data=all_breaks_waves, aes(x=date, y=fit), pch=21, fill=NA, size=6, colour="red", stroke=1) +
#     labs(x="Date", y="Value") +
#     theme_bw() +
#     theme(
#         axis.text = element_text(size=12),
#         axis.title = element_text(size=17),
#         axis.text.x = element_text(angle=45, vjust=1, hjust=1),
#         strip.text = element_text(size=15)
#     ) +
#     scale_x_date(date_breaks = "1 month" , date_labels = "%b %Y", expand=c(0,0)) +
#     facet_wrap(~type, scale="free", ncol=2) +
#     geom_vline(xintercept=breaks_waves[1], linetype="dashed", color = "blue", size=0.5) +
#     geom_vline(xintercept=breaks_waves[2], linetype="dashed", color = "blue", size=0.5) +
#     geom_vline(xintercept=breaks_waves[3], linetype="dashed", color = "blue", size=0.5) +
#     geom_text(data=all_breaks_waves, aes(x=date, y=fit, label=label), size=7, colour="red", nudge_y=1.5)
# 
# ggsave(canada_wave_break_point_analysis, file=file.path(PROJECT_FOLDER, "Graphs/canada_wave_break_points.png"), width=20, height=7)
# 
# Canada_waves <-  ggplot(canada, aes(date, cases)) +
#     theme_bw() +
#     theme(
#         axis.text = element_text(size=13),
#         axis.text.x = element_text(angle=45, vjust=1, hjust=1),
#         axis.title = element_text(size=13)
#     ) +
#     labs(y="Daily Incidence", x="") +
#     scale_x_date(date_breaks = "1 month" , date_labels = "%d-%b-%Y", expand=c(0,0)) +
#     scale_y_continuous(expand=c(0,0), breaks=seq(0, 15000, by=3000)) +
#     annotate(geom = "rect", xmin=min(canada_temp$date), xmax=breaks_waves[1], ymin=-Inf, ymax=Inf, fill="orange", alpha=0.1) +
#     annotate(geom = "rect", xmin=breaks_waves[1], xmax=breaks_waves[2], ymin=-Inf, ymax=Inf, fill="orange", alpha=0.3) +
#     annotate(geom = "rect", xmin=breaks_waves[2], xmax=breaks_waves[3], ymin=-Inf, ymax=Inf, fill="orange", alpha=0.5) +
#     geom_vline(xintercept=breaks_waves[1], linetype="dashed", color = "red", size=0.5) +
#     geom_vline(xintercept=breaks_waves[2], linetype="dashed", color = "red", size=0.5) +
#     geom_vline(xintercept=breaks_waves[3], linetype="dashed", color = "red", size=0.5) +
#     geom_bar(stat="identity", fill="lightblue") +
#     geom_line(aes(y=moving_avg_seven_days), colour="red", size=1)
# 
#     ggsave(Canada_waves, file=sprintf("%s/Graphs/canada_waves.png", PROJECT_FOLDER), height=5, width=10)
#     system(sprintf("convert %s/Graphs/canada_waves.png -trim %s/Graphs/canada_waves.png", PROJECT_FOLDER, PROJECT_FOLDER))

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
