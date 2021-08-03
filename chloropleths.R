rm(list=ls())

library(data.table)
library(ggplot2)
# library(viridis)
# library(CanCovidData)
# library(stringr)
# library(forcats)
library(sf)

setwd('/home/bren/Documents/GitHub/MIZ_project/')
dir.create(file.path(".", "CaseDataFiles"), showWarnings=FALSE)

Regions <- readRDS("CaseDataTables/Regions.rda")
Total_Data <- fread("CaseDataTables/Total_Data.csv")

SAC_map <- ggplot(st_sf(Regions)) +
    geom_sf(aes(fill = class)) +
    scale_fill_viridis_d("SAC") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
    ) +
    coord_sf(datum=NA)
ggsave(SAC_map, file="Graphs/SAC_map.png", height=10, width=10)

# plots of the distribution of remoteness and MIZ scores
Remoteness_map <- ggplot(st_sf(Regions)) +
    geom_sf(aes(fill = index_of_remoteness)) +
    scale_fill_viridis("Remoteness") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
    ) +
    coord_sf(datum=NA)
ggsave(Remoteness_map, file="Graphs/Remoteness_map.png", height=10, width=10)

aR_map <- ggplot(st_sf(Regions |> dplyr::mutate(aR_score = factor(aR_score)))) +
    geom_sf(aes(fill = aR_score)) +
    # scale_fill_viridis_d("aR score") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")
    ) +
    coord_sf(datum=NA)
ggsave(aR_map, file="Graphs/aR_map.png", height=10, width=10)

# get the geo boundaries of each health region for plotting the map
class_map <- ggplot()
for(hr_code in Regions[, unique(HRUID2018)])
{
    class_map <- class_map +
        geom_sf(
            data = st_union(Regions[HRUID2018 == hr_code, geometry]),
            mapping = aes_(fill = Total_Data[HRUID2018==hr_code, aR_score])
        )
}
class_map <- class_map +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
    ) +
    scale_fill_viridis("aMIZ") +
    scale_colour_viridis("aMIZ") +
    coord_sf(datum=NA) #+
ggsave(class_map, "Graphs/class_map_c2.png", device='png', dpi=600)

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

