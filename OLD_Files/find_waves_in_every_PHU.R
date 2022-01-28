rm(list=ls())

library(data.table)
library(ggplot2)
# library(viridis)
# library(RColorBrewer)
# library(stringr)
# library(sf)
# library(forcats)
# library(ggpubr)
# library(gganimate)
# library(tidyr)
# library(future)

# PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
PROJECT_FOLDER <- '/home/brendon/Documents/GitHub/MIZ_project'

source(file.path(PROJECT_FOLDER, "function_header.R"))
setwd(PROJECT_FOLDER)

# data.tables parallel by HRUID2018

Total_Case_Data <- fread(sprintf("%s/CaseDataTables/Total_Case_Data.csv", PROJECT_FOLDER))
Regions <- data.table(readRDS("CaseDataTables/Regions.rda"))

# saveRDS(st_union(Regions$geometry), sprintf("%s/Spatial_Features/All_Canada.rds", PROJECT_FOLDER))
# saveRDS(Regions[, .(geometry=st_union(geometry)%>% st_cast("MULTIPOLYGON")), by=.(province)], sprintf("%s/Spatial_Features/All_Provinces.rds", PROJECT_FOLDER))
# saveRDS(
#   Regions %>%
#     dplyr::group_by(HRUID2018) %>%
#     dplyr::summarise(geometry=st_union(geometry) %>% st_cast("MULTIPOLYGON")) %>%
#     data.table(),  
#   file.path(PROJECT_FOLDER, "Spatial_Features/All_HRs.rds")
# )

All_Canada <- st_sf(readRDS(file.path(PROJECT_FOLDER, "Spatial_Features/All_Canada.rds")))
All_Provinces <- st_sf(readRDS(file.path(PROJECT_FOLDER, "Spatial_Features/All_Provinces.rds"))) %>%
    merge(., province_LUT %>% dplyr::select(province, abbreviations, alpha), all=TRUE, by="province")
HR_shapes <- st_sf(readRDS(file.path(PROJECT_FOLDER, "Spatial_Features/All_HRs.rds")))

wave_dates <- list()
wave_plots <- list()

spline_parameter <- 0.75

# as opposed to the Canada dates, this calculates with a epicurve
find_wave_indices_here <- function(timeSeries)
{
    data_table <- data.table(cases = timeSeries) %>%
        dplyr::mutate(
            index = 1:nrow(.),
            spline = predict(smooth.spline(index, cases, spar=spline_parameter))$y
        )
    
    valley_indices <- localMaxima(-data_table$spline) %>%
        .[! . %in% c(1:50, (nrow(data_table)-15):nrow(data_table))]
    
    return(valley_indices)
}

# calculate the wave dates and plot for each PHU in each province
for(prov in unique(Total_Case_Data$province))
{
    print(prov)
    temp <- Total_Case_Data %>% dplyr::filter(province == prov)
    for(PHU in unique(temp$HR) %>% .[!grepl("reported|total| ", tolower(.))] %>% .[which(nchar(.) != 0)])
    {
        writeLines(sprintf("\t%s", PHU))
        case_data <- temp %>%
            dplyr::filter(HR == PHU) %>%
            dplyr::mutate(cases = ifelse(cases<0, NA, cases)) %>%
            dplyr::mutate(
                cases = ifelse(cases<0 | is.na(cases), 0, cases),
                index = as.numeric(date),
                loes = predict(loess(cases~index)),
                spline = predict(smooth.spline(index, cases, spar=spline_parameter))$y # o.83
            ) %>%
            dplyr::arrange(date)
        
        temp_waves <- case_data[find_wave_indices_here(case_data$spline)] %>%
            dplyr::mutate(wave=2:(nrow(.)+1))
        
        pl_waves <- ggplot(case_data, aes(x=date)) +
            geom_line(aes(y=cases)) +
            geom_point(aes(y=cases)) +
            geom_line(aes(y=spline), colour="blue", size=2) +
            geom_ribbon(aes(
                ymin=pmax(0, spline-sd(spline)),ymax=spline+sd(spline)),
                fill="blue", alpha=0.1
            ) +
            geom_vline(
                xintercept = temp_waves$date,
                colour = "darkgreen", size=1, linetype = "dashed"
            ) +
            theme_bw() +
            theme(
                axis.text = element_text(size=15),
                axis.title = element_text(size=15),
                axis.text.x = element_text(angle=45, hjust=1)
            ) +
            labs(x="Date", y=sprintf("Incidence (%s, %s)", prov, PHU)) +
            scale_x_date(breaks="1 month", date_labels="%b %Y", expand=c(0,0)) +
            scale_y_continuous(expand = c(0, 0))
        
        wave_dates[[paste(prov, PHU, sep="_") %>% gsub(" +|-", "_", .)]] <- temp_waves
        wave_plots[[paste(prov, PHU, sep="_") %>% gsub(" +|-", "_", .)]] <- pl_waves
    }
}