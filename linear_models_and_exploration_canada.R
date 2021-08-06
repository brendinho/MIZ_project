rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(CanCovidData)
library(stringr)
library(forcats)
library(sf)

setwd('/home/bren/Documents/GitHub/MIZ_project/')
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source("function_header.R")

Total_Data <- fread("CaseDataTables/Total_Data.csv") |>
    dplyr::mutate( # special columns we create for the regressions
        families_with_no_children = couples_with_0_children + people_not_in_census_families,
        families_with_children_of_size_2_or_smaller = singles_with_1_child,
        families_with_children_of_size_3_or_larger = couples_with_1_child + couples_with_2_children + couples_with_3_or_more_children +
            singles_with_2_children + singles_with_3_or_more_children,
        families_with_children_of_size_3_or_smaller = singles_with_1_child + singles_with_2_children + couples_with_1_child,
        families_with_children_of_size_4_or_larger = couples_with_2_children + couples_with_3_or_more_children + singles_with_3_or_more_children,
        HR = factor(HR),
        HRUID2018 = factor(HRUID2018),
    )

######################################3 CANADA-WIDE REGRESSIONS

print(summary(lm(
    formula = first_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters + 
        aR_score,
        # index_of_remoteness,
    data = Total_Data
)))
# for the second wave
print(summary(lm(
    formula = second_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters + aR_score,
    data = Total_Data
)))
# the sum of cases over all days
print(summary(lm(
    formula = total_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters + aR_score,
    data = Total_Data
)))
# creating a table with case numbers broken up by wave, so that I
print(summary(lm(
    formula = proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
    total_commuters + aR_score + wave,
    # formula = proportion ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
        # aR_score + wave,
    data = rbind(
        Total_Data |>
            dplyr::select(!starts_with("second_wave_")) |>
            dplyr::select(-total_cases, -interpeak_distance) |>
            dplyr::rename_with(\(x) gsub("first_wave_", "", x)) |>
            dplyr::mutate(wave=1),
        Total_Data |>
            dplyr::select(!starts_with("first_wave_")) |>
            dplyr::select(-total_cases, -interpeak_distance) |>
            dplyr::rename_with(\(x) gsub("second_wave_", "", x)) |>
            dplyr::mutate(wave=2)
    )
)))

######################################## PROVINCIAL REGRESSIONS

# original scheme in the paper
# THIS WORKS FOR ONTARIO, AND NO OTHER PROVINCE

PROVINCE <- "Ontario"

# for the first wave of infection
print(summary(lm(
    formula = first_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters + 
        aR_score,
    # index_of_remoteness,
    data = Total_Data[province == PROVINCE]
)))
# for the second wave
print(summary(lm(
    formula = second_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters + aR_score,
    data = Total_Data[province == PROVINCE]
)))
# the sum of cases over all days
print(summary(lm(
    formula = total_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
       total_commuters + aR_score,
    data = Total_Data[province == PROVINCE]
)))
# creating a table with case numbers broken up by wave, so that I can include the wave number in the regression
print(summary(lm(
    formula = proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
    # formula = proportion ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
        # total_commuters + 
        aR_score + 
        wave,
    aR_score + wave,
    data = rbind(
        Total_Data |>
            # dplyr::filter(province == PROVINCE) |>
            dplyr::select(!starts_with("second_wave_")) |>
            dplyr::select(-total_cases, -interpeak_distance) |>
            dplyr::rename_with(\(x) gsub("first_wave_", "", x)) |>
            dplyr::mutate(wave=1),
        Total_Data |>
            # dplyr::filter(province == PROVINCE) |>
            dplyr::select(!starts_with("first_wave_")) |>
            dplyr::select(-total_cases, -interpeak_distance) |>
            dplyr::rename_with(\(x) gsub("second_wave_", "", x)) |>
            dplyr::mutate(wave=2)
    )
)))
