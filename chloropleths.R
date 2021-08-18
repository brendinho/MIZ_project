rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(stringr)
library(sf)

source("function_header.R")

setwd('/home/bren/Documents/GitHub/MIZ_project/')
dir.create(file.path(".", "CaseDataFiles"), showWarnings=FALSE)

Regions <- readRDS("CaseDataTables/Regions.rda")
Total_Data <- fread("CaseDataTables/Total_Data.csv")
Total_Toronto_Data <- readRDS("CaseDataFiles/Total_Toronto_Data_with_odds_ratios.rda")

scaling_factor <- Regions[, .(num_HR=factor(length(unique(HR))), num_csd=sum(num_csds), population=sum(population)), by=.(province)][, max(num_csd)/max(population)]
SAC_distribution <- ggplot(
        Regions[, .(num_HR=factor(length(unique(HR))), num_csd=sum(num_csds), population=sum(population)), by=.(province)],
        aes(x=reorder(str_wrap(province, width=15), -population), group=province)
    ) +
    geom_bar(aes(y=num_csd, fill=num_HR), position="dodge", stat="identity") +
    geom_line(aes(y=population*scaling_factor), size=2, group=1, colour="blue") +
    labs(x="Province", y="Census Subdivisions (CSDs)") +
    scale_y_continuous(sec.axis = sec_axis(~./scaling_factor, name="Provincial population"), expand=c(0,0)) +
    theme_bw() +
    theme(
        axis.text.y = element_text(size=10),
        axis.title = element_text(size=14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.5, size=10),
        legend.position = "top",
        legend.direction="horizontal",
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
    ) +
    scale_color_viridis()+
    scale_fill_viridis_d() +
    guides(fill=guide_legend(title="Reporting Region count:", nrow=1))
    # ggsave(SAC_distribution, file="Graphs/SAC_dist.png", width=10, height=4)

SAC_map_with_census_info <- ggplot(st_sf(Regions)) +
    geom_sf(aes(fill = class)) +
    scale_fill_viridis_d("SAC") + 
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.key.height= unit(1.5, 'cm')
    ) +
    coord_sf(datum=NA)
    ggsave(SAC_map, file="Graphs/SAC_map.png", height=10, width=10)
    system(sprintf("convert %s -trim %s", "Graphs/SAC_map.png", "Graphs/SAC_map.png"))
    
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

SAC_map <- ggplot(st_sf(full_csds)) +
    geom_sf(aes(fill = class), lwd = 0) +
    scale_fill_viridis_d("Statistical\nArea\nClassification") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=13),
        legend.title = element_text(size=13),
        legend.key.height= unit(1.5, 'cm')
    ) +
    coord_sf(datum=NA)
    # ggsave(SAC_map, file="Graphs/SAC_map.png", height=11, width=10)
    # system(sprintf("convert %s -trim %s", "Graphs/SAC_map.png", "Graphs/SAC_map.png"))


# plots of the distribution of remoteness and MIZ scores
Remoteness_map <- ggplot(st_sf(Regions)) +
    geom_sf(aes(fill = index_of_remoteness), lwd = 0) +
    scale_fill_viridis("Remoteness", na.value = "black") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
    ) +
    coord_sf(datum=NA)
    # ggsave(Remoteness_map, file="Graphs/Remoteness_map.png", height=10, width=10)

aR_map <- ggplot(st_sf(Regions %>% dplyr::mutate(aR_score = factor(aR_score)))) +
    geom_sf(aes(fill = aR_score), lwd = 0) +
    scale_fill_viridis_d("aR score",  na.value = "black") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")
    ) +
    coord_sf(datum=NA)
    # ggsave(aR_map, file="Graphs/aR_map.png", height=10, width=10)

# get the geo boundaries of each health region for plotting the map
class_map <- ggplot()
# for(hr_code in Regions[, unique(HRUID2018)])
# {
#     class_map <- class_map +
#         geom_sf(
#             data = st_union(Regions[HRUID2018 == hr_code, geometry]),
#             mapping = aes_(fill = Total_Data[HRUID2018==hr_code, aR_score])
#         )
# }
for(hr_code in unique(full_csds$HRUID2018))
{
    class_map <- class_map +
        geom_sf(
            data = st_union(full_csds[HRUID2018 == hr_code, geometry]),
            mapping = aes_(fill = Total_Data[HRUID2018==hr_code, aR_score]),
            lwd = 0
        )
}

class_map <- class_map +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
    ) +
    scale_fill_viridis("aMIZ", na.value = "black") +
    # scale_colour_viridis("aMIZ") +
    coord_sf(datum=NA) #+
    # ggsave(class_map, "Graphs/class_map_c2.png", device='png', dpi=600)

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
                        HRUID2018==hr_code,
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

################ toronto chloropleths

toronto_nbds_cases <- ggplot(st_sf(Total_Toronto_Data)) +
    geom_sf(aes(fill = num_reports)) +
    scale_fill_viridis("Case Reports") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.height= unit(2.5, 'cm')
    ) +
    coord_sf(datum=NA)
    # ggsave(toronto_nbds_cases, file="Graphs/toronto_case_reports_map.png", height=10, width=10)
    # system(sprintf("convert %s -trim %s", "Graphs/toronto_case_reports_map.png", "Graphs/toronto_case_reports_map.png"))

toronto_nbds_odds_ratios <- ggplot(st_sf(Total_Toronto_Data) %>% dplyr::filter(odds_ratio != 1.0)) +
    geom_sf(aes(fill = odds_ratio)) +
    scale_fill_viridis("Odds Ratios") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.height= unit(2.5, 'cm'),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
    ) +
    coord_sf(datum=NA)
    # ggsave(toronto_nbds_odds_ratios, file="Graphs/toronto_odds_ratios_map.png", height=10, width=10)
    # system(sprintf("convert %s -trim %s", "Graphs/toronto_odds_ratios_map.png", "Graphs/toronto_odds_ratios_map.png"))


