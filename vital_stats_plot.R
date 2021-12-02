rm(list=ls())

library(data.table)
library(ggplot2)

PROJECT_FOLDER <- "/home/bren/Documents/GitHub/MIZ_project"

Total_Data <- fread(file.path(PROJECT_FOLDER, "temp_total_data_for_regression.csv"))
Total_Data_hist <- Total_Data %>% dplyr::select(starts_with("wave")) %>% melt() %>% suppressWarnings

divisor <- 4

bw <- Total_Data_hist[!is.na(value), 2*IQR(value)/length(value)^(1/3), by=.(variable)][, max(V1)/divisor]
num_bins <- Total_Data_hist[!is.na(value), (max(value)-min(value))/bw, by=.(variable)][, ceiling(max(V1))]

AR_dist <-ggplot(
        Total_Data %>% 
            dplyr::select(starts_with("wave")) %>% 
            melt() %>% 
            suppressWarnings %>%
        dplyr::filter(!is.na(value)) %>% 
        dplyr::mutate(
            variable = dplyr::recode(
                variable, 
                "wave_1_attack_rate"="Wave 1", "wave_2_attack_rate"="Wave 2", "wave_3_attack_rate"="Wave 3"
            )
        ),
        aes(x=value)
    ) +
    geom_histogram(aes(y=..density..), binwidth=bw, fill="lightblue", colour="black") +
    theme_bw() +
    theme(
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        strip.text = element_text(size=15)
    ) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x="Attack Rate", y="Count") +
    facet_grid(variable~., scale="free_y")
    ggsave(AR_dist, file=paste0(PROJECT_FOLDER, "/Graphs/AR_dists.png"), height=7, width=8)
    
# Pop_plot <- 
    
    ggplot(
        Total_Data %>% 
            dplyr::select(province, hr, hruid, population) %>% 
            unique() %>% 
            dplyr::filter(!is.na(population)) %>%
            dplyr::select(population) %>% 
            dplyr::mutate(lev=1), 
        aes(y=population, x=lev)
    ) +
    geom_boxplot(fill="lightblue", outlier.colour="red", outlier.shape=16, outlier.size=3, notch=TRUE) +
    labs(y="Population", x="") +
    theme_bw() +
    theme(
        axis.text.y = element_text(size=15),
        axis.text.x = element_blank(),
        axis.title = element_text(size=15)
    ) +
    scale_y_continuous(breaks = seq(0, max(Total_Data$population, na.rm=T), by = 5e5))
    ggsave(Pop_plot, file=paste0(PROJECT_FOLDER, "/Graphs/Pop_plot.png"), height=7, width=8)

