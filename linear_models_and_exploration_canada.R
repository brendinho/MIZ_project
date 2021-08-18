rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(CanCovidData)
library(stringr)
library(forcats)
library(sf)
library(xtable)

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
        population_density = population/area_sq_km,
    )

######################################3 CANADA-WIDE REGRESSIONS

canada_first_wave <- summary(lm(
    formula = first_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters +
        population_density +
        aR_score,
        # index_of_remoteness,
    data = Total_Data
))

canada_second_wave <- summary(lm(
    formula = second_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters +
        population_density +
        aR_score,
    data = Total_Data
))

canada_total <- summary(lm(
    formula = proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters + 
        aR_score + 
        population_density +
        wave,
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
))

canada_regress_results <- Reduce(
    function(x,y) merge(x=x, y=y, by="vars", all=TRUE),
    list(
        data.table(vars=rownames(canada_first_wave$coefficients), canada_first_wave$coefficients) |> 
            dplyr::select(-`Std. Error`, -`t value`) |> 
            dplyr::rename(first_wave_value=Estimate, first_wave_p=`Pr(>|t|)`),
        data.table(vars=rownames(canada_second_wave$coefficients), canada_second_wave$coefficients) |> 
            dplyr::select(-`Std. Error`, -`t value`) |> 
            dplyr::rename(second_wave_value=Estimate, second_wave_p=`Pr(>|t|)`),
        data.table(vars=rownames(canada_total$coefficients), canada_total$coefficients) |> 
            dplyr::select(-`Std. Error`, -`t value`) |> 
            dplyr::rename(total_value=Estimate, total_p=`Pr(>|t|)`)
        )
    ) |> 
    dplyr::mutate(
        first_wave_value = round(first_wave_value, 4),
        first_wave_p = round(first_wave_p, 2),
        second_wave_value = round(second_wave_value, 4),
        second_wave_p = round(second_wave_p, 2),
        total_value = round(total_value, 4),
        total_p = round(total_p, 2),
    ) %>%
    print(xtable(.), include.rownames=F)
    fwrite(canada_regress_results, file="canada_regress_results.csv")


# # the sum of cases over all days
# print(summary(lm(
#     formula = total_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
#         total_commuters + aR_score,
#     data = Total_Data
# )))
# creating a table with case numbers broken up by wave, so that I

######################################## PROVINCIAL REGRESSIONS

# original scheme in the paper
# THIS WORKS FOR ONTARIO, AND NO OTHER PROVINCE
    
# num HRS: Ontario 34, BC 16, Quebec 18, Alberta 5, Saskatchewan 13, Manitoba 5, New Brunswick 7, Nova Scotia 4, NL 4
    
provinces_regressions <- data.table()

for(PROVINCE in c("Ontario", "British Columbia", "Quebec", "Saskatchewan")) # , "Alberta", "Manitoba", "New Brunswick"
{
    first_wave_regression <- summary(lm(
        formula = first_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
            total_commuters + 
            # population_density +
            aR_score,
        # index_of_remoteness,
        data = Total_Data[province == PROVINCE]
    ))
    
    second_wave_regression <- summary(lm(
        formula = second_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
            total_commuters + 
            # population_density +
            aR_score,
        data = Total_Data[province == PROVINCE]
    ))
    
    total_regression <- summary(lm(
        formula = proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
            aR_score + 
            total_commuters +
            # population_density +
            wave,
        data = rbind(
            Total_Data |>
                dplyr::filter(province == PROVINCE) |>
                dplyr::select(!starts_with("second_wave_")) |>
                dplyr::select(-total_cases, -interpeak_distance) |>
                dplyr::rename_with(\(x) gsub("first_wave_", "", x)) |>
                dplyr::mutate(wave=1),
            Total_Data |>
                dplyr::filter(province == PROVINCE) |>
                dplyr::select(!starts_with("first_wave_")) |>
                dplyr::select(-total_cases, -interpeak_distance) |>
                dplyr::rename_with(\(x) gsub("second_wave_", "", x)) |>
                dplyr::mutate(wave=2)
            )
    ))
    
    provinces_regressions <- rbind(
        provinces_regressions,
        Reduce(
            function(x,y) merge(x=x, y=y, by="vars", all=TRUE),
            list(
                data.table(province=PROVINCE, vars=rownames(first_wave_regression$coefficients), first_wave_regression$coefficients) |> 
                    dplyr::select(-`Std. Error`, -`t value`) |> 
                    dplyr::rename(first_wave_value=Estimate, first_wave_p=`Pr(>|t|)`),
                data.table(province=PROVINCE, vars=rownames(second_wave_regression$coefficients), second_wave_regression$coefficients) |> 
                    dplyr::select(-`Std. Error`, -`t value`) |> 
                    dplyr::rename(second_wave_value=Estimate, second_wave_p=`Pr(>|t|)`),
                data.table(province=PROVINCE, vars=rownames(total_regression$coefficients), total_regression$coefficients) |> 
                    dplyr::select(-`Std. Error`, -`t value`) |> 
                    dplyr::rename(total_value=Estimate, total_p=`Pr(>|t|)`)
            )
        )
    )
}

var_names <- function(x)
{
    if(x == "aR_score") return("$\\aR$")
    if(x == "families_with_children_of_size_2_or_smaller") return("$F^c_{\\le2}$")
    if(x == "families_with_children_of_size_3_or_larger") return("$F^c_{\\ge3}$")
    if(x == "families_with_no_children") return("$F_0$")
    if(x == "population_density") return("$D$")
    if(x == "total_commuters") return("$L$")
    if(x == "wave") return("$x$")
    if(x == "(Intercept)") return("Intercept")
    return(x)
}
provinces_regressions <- provinces_regressions |>
    dplyr::mutate(
        first_wave_value = round(first_wave_value, 4),
        first_wave_p = round(first_wave_p, 3),
        second_wave_value = round(second_wave_value, 4),
        second_wave_p = round(second_wave_p, 3),
        total_value = round(total_value, 4),
        total_p = round(total_p, 3),
        province = coalesce(province, province.x, province.y)
    ) |>
    dplyr::rename(
        Parameters = vars,
        Province = province
    ) |>
    dplyr::select(-province.x, -province.y) |>
    dplyr::relocate(Province, Parameters) %>%
    rbind(list("Province", "Parameters", "$\\beta_i$", "$p$-value","$\\beta_i$", "$p$-value","$\\beta_i$", "$p$-value"), .) %>%
    dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
    print(xtable(.), include.rownames=F)
    fwrite(provinces_regressions, file="provinces_regressions_results.csv")


# for the first wave of infection
print()
# for the second wave
print()
# # the sum of cases over all days
# print(summary(lm(
#     formula = total_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
#        total_commuters + aR_score,
#     data = Total_Data[province == PROVINCE]
# )))
# creating a table with case numbers broken up by wave, so that I can include the wave number in the regression
print()
