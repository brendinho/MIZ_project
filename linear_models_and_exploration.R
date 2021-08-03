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
        total_commuters + aR_score,
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
        total_commuters + aR_score,
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
    # formula = proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
    # # total_commuters + 
    #     aR_score + wave,
    formula = proportion ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
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

############################################################################ SCRAPYARD ############################################################################

# powerset functions so that I can automate a search for all possible linear models giving significance to the aR score

# # Vincent Zoonekynd's answer to the question "Algorithm to calculate power set (all possible subsets) of a set in R
# # https://stackoverflow.com/questions/18715580/algorithm-to-calculate-power-set-all-possible-subsets-of-a-set-in-r
# powerset <- function(set) {
#     n <- length(set)
#     masks <- 2^(1:n-1)
#     the_list <- lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
#     # the_list[[1]] <- NULL # toggle whether the empty set is returned or not
#     return(the_list)
# }

# # BRUTE FORCE SEARCH FOR LINEAR MODELS WHERE THE aR SCORE IS SIGNIFICANT
# start_time <- Sys.time()
# 
# # table that we can use to include the wave in the linear regression
# Total_Data_waves_stacked_on_each_other = rbind(
#     Total_Data |>
#         dplyr::select(!starts_with("second_wave_")) |> 
#         dplyr::select(-total_cases, -interpeak_distance) |> 
#         dplyr::rename_with(\(x) gsub("first_wave_", "", x)) |> 
#         dplyr::mutate(wave=1),
#     Total_Data |>
#         dplyr::select(!starts_with("first_wave_")) |> 
#         dplyr::select(-total_cases, -interpeak_distance) |> 
#         dplyr::rename_with(\(x) gsub("second_wave_", "", x)) |> 
#         dplyr::mutate(wave=2)
# )
# 
# # variables that we will combine for the lm formulas
# measures_of_remoteness <- c("index_of_remoteness", "aR_score")
# numbers_of_couples <- c("couples_with_children", "couples_with_0_children", "couples_with_1_child", "couples_with_2_children", "couples_with_3_or_more_children")
# numbers_of_singles <- c("singles_with_children", "singles_with_1_child", "singles_with_2_children", "singles_with_3_or_more_children", "people_not_in_census_families")
# numbers_of_commuters <- c("total_commuters", "people_commuting_within_province", "people_commuting_within_csd", "people_commuting_within_cd_but_not_csd" , "people_commuting_within_province_but_not_cd", "people_commuting_outside_province", "people_commuting_outside_their_csd")
# 
# writeLines("\n\nGenerating combinations")
# 
# regress_combinations <- list()
# 
# # generating a list of combinations, with redundancies removed
# for(commuters in powerset(numbers_of_commuters)){
#     for(singles in powerset(numbers_of_singles)){
#         for(couples in powerset(numbers_of_couples))
#         {
#             if(length(couples)>1) couples <- couples[which(couples != "couples_with_children")]
#             if(length(singles)>1) singles <- singles[which(singles != "singles_with_children")]
#             if(length(commuters)>1) commuters <- commuters[which(commuters != "total_commuters")]
#             regress_combinations <- append(regress_combinations, list(c(couples, singles, commuters)))
#         }}}
# regress_combinations <- unique(regress_combinations)
# 
# 
# print("\n\nStarting search")
# 
# cl <- parallel::makeCluster(8)
# doParallel::registerDoParallel(cl)
# 
# LIST_OF_PROVINCES <- c(
#     "Canada", # 108 health regions
#     "Ontario", # 34 health regions
#     "British Columbia", # 16 health regions
#     "Saskatchewan", # 13 health regions
#     "Alberta", # 5 health regions
#     "Manitoba", # 5 health regions
#     "Quebec", # 18 health regions
#     "New Brunswick"# 7 health regions
# )
# 
# # checking Canada total as well as all the provinces with sufficient number of HRs
# # for(province_name in LIST_OF_PROVINCES)
# foreach(province_index = 1:length(LIST_OF_PROVINCES), .packages = c("data.table")) %dopar%
# {
#     province_name <- LIST_OF_PROVINCES[province_index]
#     
#     # a table to hold the information for the regressions where aR was significant
#     all_possible_linear_models <- data.table(
#         province = as.character(),
#         number_of_variables = as.numeric(),
#         wave = as.numeric(),
#         command=as.character(),
#         num_signif=as.integer(),
#         names_signif=as.character(),
#         remoteness_signif=logical()
#     )
#     
#     if(province_name == "Canada"){
#         PROVINCES <- Total_Data[, unique(province)]
#     } else {
#         PROVINCES = c(province_name)
#     }
# 
#     # writeLines(paste("\n\n\t", province_name))
# 
#     # lm for the variable combination chosen
#     for(combo in regress_combinations)
#     {
#         for(remote in measures_of_remoteness)
#         {
#             # FIRST WAVE #
#             the_combination <- c(combo, remote)
#             
#             if(length(the_combination) < 3) next
#             
#             string <- sprintf("first_wave_proportion ~ %s", paste(the_combination, collapse=" + "))
#             regress <- lm(formula = as.formula(string), data = Total_Data[province %in% PROVINCES])
# 
#             p_vals <- summary(regress)$coefficients[, 4]
# 
#             if(remote %in% names(which(p_vals <= 0.05)))
#             {
#                 all_possible_linear_models <- rbind(all_possible_linear_models, list(
#                     province = province_name,
#                     number_of_variables = length(the_combination),
#                     wave = 1,
#                     command = string,
#                     num_signif = length(which(p_vals <= 0.05)),
#                     remoteness_signif = TRUE,
#                     names_signif = paste0(names(which(p_vals <= 0.05)), collapse=",")
#                 ))
#             }
# 
#             # SECOND WAVE #
#             string <- sprintf("second_wave_proportion ~ %s", paste(the_combination, collapse=" + "))
#             regress <- lm(formula = as.formula(string), data = Total_Data[province %in% PROVINCES])
# 
#             p_vals <- summary(regress)$coefficients[, 4]
# 
#             if(remote %in% names(which(p_vals <= 0.05)))
#             {
#                 all_possible_linear_models <- rbind(all_possible_linear_models, list(
#                     province = province_name,
#                     number_of_variables = length(the_combination),
#                     wave = 2,
#                     command = string,
#                     num_signif = length(which(p_vals <= 0.05)),
#                     remoteness_signif = TRUE,
#                     names_signif = paste0(names(which(p_vals <= 0.05)), collapse=",")
#                 ))
#             }
# 
#             # TOTAL CASE NUMBERS - WAVES NOT CONSIDERED #
#             string <- sprintf("total_proportion ~ %s", paste(the_combination, collapse=" + "))
#             regress <- lm(formula = as.formula(string), data = Total_Data[province %in% PROVINCES])
# 
#             p_vals <- summary(regress)$coefficients[, 4]
# 
#             if(remote %in% names(which(p_vals <= 0.05)))
#             {
#                 all_possible_linear_models <- rbind(all_possible_linear_models, list(
#                     province = province_name,
#                     number_of_variables = length(the_combination),
#                     wave = 0,
#                     command = string,
#                     num_signif = length(which(p_vals <= 0.05)),
#                     remoteness_signif = TRUE,
#                     names_signif = paste0(names(which(p_vals <= 0.05)), collapse=",")
#                 ))
#             }
# 
#             # WAVE NUMBER IS PART OF THE REGRESSION #
#             string <- sprintf("proportion ~ %s", paste(c(the_combination, "wave"), collapse=" + "))
#             regress <- lm(formula = as.formula(string), data = Total_Data_waves_stacked_on_each_other[province %in% PROVINCES])
# 
#             p_vals <- summary(regress)$coefficients[, 4]
# 
#             if(remote %in% names(which(p_vals <= 0.05)))
#             {
#                 all_possible_linear_models <- rbind(all_possible_linear_models, list(
#                     province = province_name,
#                     number_of_variables = length(the_combination)+1,
#                     wave = NA,
#                     command = string,
#                     num_signif = length(which(p_vals <= 0.05)),
#                     remoteness_signif = remote %in% names(which(p_vals <= 0.05)),
#                     names_signif = paste0(names(which(p_vals <= 0.05)), collapse=",")
#                 ))
#             }
#         }
#     }
#     
#     fwrite(
#         unique(all_possible_linear_models), 
#         sprintf("all_possible_linear_models_%s.csv", gsub(" ", "_", province_name))
#     )
# }
# print(Sys.time() - start_time)
# 
# haha <- rbind(
#     fread("all_possible_linear_models_Canada.csv"),
#     fread("all_possible_linear_models_Ontario.csv"),
#     fread("all_possible_linear_models_British_Columbia.csv"),
#     fread("all_possible_linear_models_Saskatchewan.csv"),
#     fread("all_possible_linear_models_Alberta.csv"),
#     fread("all_possible_linear_models_Manitoba.csv"),
#     fread("all_possible_linear_models_Quebec.csv"),
#     fread("all_possible_linear_models_New_Brunswick.csv")
# )