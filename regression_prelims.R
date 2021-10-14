##################################################
## Project: COVID-19 regression
## Script purpose: assemble and filter data sets for the regression
## Date written: 2021-09-08
## Last edited: 2021-09-08
## Script author: Brendon Phillips
## Institution: York University
## Lab: ABM-Lab
## Principal Investigator: Seyed M. Moghadas
##################################################

rm(list=ls())

library(data.table)
library(stringr)
library(forcats)
library(lme4)
library(car)
library(digest)
library(rstudioapi)
library(sf)

# PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
PROJECT_FOLDER <- "/home/bren/Documents/GitHub/MIZ_project"

setwd(PROJECT_FOLDER)
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source(sprintf("%s/function_header.R", PROJECT_FOLDER))

##########################################################################################

Total_Case_Data <- fread(sprintf("%s/CaseDataTables/Total_Case_Data.csv", PROJECT_FOLDER)) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(wave=4) %>%
    dplyr::mutate(wave = case_when(
        date < as.Date("2020-07-15") ~ 1,
        date < as.Date("2021-02-15") ~ 2,
        date < as.Date("2021-07-15") ~ 3,
        date < max(date) ~ 4
    )) %>%
    dplyr::filter(wave < 4) %>%
    dplyr::rename(hruid = hruid2018)

Regions <- readRDS(sprintf("%s/CaseDataTables/Regions.rda", PROJECT_FOLDER)) %>%
    data.table %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename(hruid = hruid2018) %>%
    dplyr::filter(!is.na(population))

All_Provinces_Shapes <- readRDS(sprintf("%s/Classifications/All_Province_Shapes.rds", PROJECT_FOLDER)) %>% st_sf()

Demographic_Data <- Regions %>%
    dplyr::mutate(csdname = coalesce(region, csdname)) %>%
    dplyr::select(-population_density, -geometry, -num_csds, -mr_score, -class, -index_of_remoteness, -region, -pr_uid, -title) %>%
    dplyr::mutate(
        total_commuters = people_commuting_within_csd + people_commuting_within_cd_but_not_csd + 
        people_commuting_within_province_but_not_cd + people_commuting_outside_province,
    
        F0 = couples_with_0_children + people_not_in_census_families,
        F3 = singles_with_1_child + singles_with_2_children + couples_with_1_child,
        F4 = couples_with_2_children + couples_with_3_or_more_children + singles_with_3_or_more_children,
        
        F0_prop = F0/(F0+F3+F4),
        F3_prop = F3/(F0+F3+F4),
        F4_prop = F4/(F0+F3+F4),
        
        F_wo_C = F0,
        F_with_C = F3 + F4
    ) %>%
    dplyr::filter(!is.na(F0_prop) & !is.na(F3_prop) & !is.na(F4_prop)) %>%
    .[, 
        lapply(.SD, sum, na.rm=TRUE),
        .SDcols = setdiff(names(.), c("csduid", "csdname", "province", "pruid", "hr", "hruid")),
        by=.(hr, hruid, province, pruid)
    ] %>%
    dplyr::mutate(population_density = population/area_sq_km)

Remoteness <- Regions[, .(
        mean_index = meann(index_of_remoteness),
        sum_index = summ(index_of_remoteness),
        weighted_index = summ(index_of_remoteness*population)/summ(population),
        mean_mr = meann(mr_score),
        sum_mr = summ(mr_score),
        weighted_mr = summ(mr_score*population)/summ(population),
        num_csds = summ(num_csds)
    ), by=.(province, pruid, hr, hruid)]

Cumulative_Cases <- Total_Case_Data[hr != "Not Reported", .(
        wave_1_cumulative = summ(.SD[wave==1]$cases),
        wave_2_cumulative = summ(.SD[wave==2]$cases),
        wave_3_cumulative = summ(.SD[wave==3]$cases),
        total_cumulative = summ(cases)
    ), by=.(hr, province, hruid)] %>%
    dplyr::filter(
        !is.na(wave_1_cumulative) &
        !is.na(wave_2_cumulative) &
        !is.na(wave_3_cumulative)
    )

Total_Data <- Reduce(
    function(...) merge(..., all = TRUE, by = c("hr", "province", "hruid")),
    list(
        Remoteness,
        Cumulative_Cases,
        Demographic_Data
    )) %>%
    dplyr::mutate(
        hr = factor(hr), hruid = factor(hruid), province = factor(province), pruid = factor(pruid.x),

        wave_1_attack_rate = wave_1_cumulative/population,
        wave_2_attack_rate = wave_2_cumulative/population,
        wave_3_attack_rate = wave_3_cumulative/population,
        all_waves_attack_rate = total_cumulative/population,

        F0_standardised = z_transform(F0),
        F3_standardised = z_transform(F3),
        F4_standardised = z_transform(F4),

        F0_prop_standardised = z_transform(F0_prop),
        F3_prop_standardised = z_transform(F3_prop),
        F4_prop_standardised = z_transform(F4_prop),

        mean_index_standardised = z_transform(mean_index),
        sum_index_standardised = z_transform(sum_index),
        weighted_index_standardised = z_transform(weighted_index),

        mean_mr_standardised = z_transform(mean_mr),
        sum_mr_standardised = z_transform(sum_mr),
        weighted_mr_standardised = z_transform(weighted_mr),

        population_density_standardised = z_transform(population_density),
        total_commuters_standardised = z_transform(total_commuters)
        
    ) %>%
    dplyr::select(-pruid.x, -pruid.y, -wave_1_cumulative, -wave_2_cumulative, -wave_3_cumulative, -total_cumulative) %>%
    dplyr::relocate(province, pruid, hr, hruid)
