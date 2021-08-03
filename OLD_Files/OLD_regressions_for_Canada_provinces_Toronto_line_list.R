rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(CanCovidData)
library(stringr)
library(sf)
library(forcats)

# powerset functions so that I can automate a search for all possible linear models giving significance to the aR score

# Vincent Zoonekynd's answer to the question "Algorithm to calculate power set (all possible subsets) of a set in R
# https://stackoverflow.com/questions/18715580/algorithm-to-calculate-power-set-all-possible-subsets-of-a-set-in-r
powerset <- function(set) {
    n <- length(set)
    masks <- 2^(1:n-1)
    the_list <- lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
    # the_list[[1]] <- NULL # toggle whether the empty set is returned or not
    return(the_list)
}

setwd('/home/bren/Documents/GitHub/MIZ_project/')
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source("function_header.R")

CSD_score_class <- function(x)
{
    if(grepl("cma", tolower(x))) return(1)
    if(grepl("ca", tolower(x))) return(1)
    if(grepl("strong", tolower(x))) return(2)
    if(grepl("moderate", tolower(x))) return(2)
    if(grepl("weak", tolower(x))) return(3)
    if(grepl("none", tolower(x))) return(3)
    return(NA)
}

CSD_score_normal <- function(x)
{
    if(grepl("cma", tolower(x))) return(1)
    if(grepl("ca", tolower(x))) return(2)
    if(grepl("strong", tolower(x))) return(3)
    if(grepl("moderate", tolower(x))) return(4)
    if(grepl("weak", tolower(x))) return(5)
    if(grepl("none", tolower(x))) return(6)
    return(NA)
}

# read the MIZ score calculation, here using the classification and aggregation given in the MIZ paper from Seyed
Regions <- readRDS("Classifications/Total_CSD_Info.rda") |>
    dplyr::filter(population != 0) |>
    dplyr::select(-csd_type, -cd_uid) |>
    dplyr::mutate(
        index_of_remoteness = unlist(lapply(index_of_remoteness, \(x) if(x==".") NA else as.numeric(x) )),
        num_csds = 1,
        aR_score = as.integer(unlist(lapply(class, CSD_score_class))),
        province = factor(province), 
        region = factor(region),
        # for ordered bar chart plots
        class = fct_relevel(class, "CMA", "CA", "Strong", "Moderate", "Weak", "None", "NA"),
        csduid2016 = factor(csduid2016),
    )

# plots of the distribution of remoteness and MIZ scores
Remoteness_map <- ggplot(st_sf(Regions)) +
    geom_sf(aes(fill = index_of_remoteness)) +
    scale_fill_viridis_c("Remoteness") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        ) +
    coord_sf(datum=NA)
    ggsave(Remoteness_map, file="Graphs/Remoteness_map.jpg", height=10, width=10)

aR_map <- ggplot(st_sf(Regions)) +
    geom_sf(aes(fill = aR_score)) +
    scale_fill_viridis_c("aR") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")
    ) +
    coord_sf(datum=NA)

Case_Data <- fread("CaseDataTables/Total_Case_Data.csv") |> dplyr::mutate(date = as.Date(date))

Cumul_Cases <- Case_Data[
        HR!="Not Reported",
        .(
            first_wave_cases=sum(.SD[wave==1]$cases, na.rm=T),
            second_wave_cases=sum(.SD[wave==2]$cases, na.rm=T),
            total_cases=sum(cases, na.rm=T)
        ),
        by=.(HR, province, HRUID2018)
    ]

Time_to_Peak <- merge(
        Case_Data[HR!="Not Reported", .SD[wave==1][which.max(cases)], by=.(HRUID2018)] |>
            dplyr::mutate(first_wave_days_since=date-as.Date('2020-01-23')) |>
            dplyr::rename(first_wave_peak_date=date, first_wave_peak_cases=cases) |>
            dplyr::select(-wave),
        Case_Data[HR!="Not Reported", .SD[wave==2][which.max(cases)], by=.(HRUID2018)] |>
            dplyr::mutate(second_wave_days_since=date-as.Date('2020-01-23'))|>
            dplyr::rename(second_wave_peak_date=date, second_wave_peak_cases=cases) |>
            dplyr::select(-wave),
        by=c("HRUID2018", "HR", "province")
    ) |>
    dplyr::mutate(interpeak_distance=as.numeric(second_wave_days_since-first_wave_days_since))

Remoteness <- Regions[,
        lapply(.SD, sum, na.rm=TRUE),
        .SDcols = setdiff(
            names(Regions),
            c("csduid2016", "region", "province", "HR", "class", "population_density", "HRUID2018", "csd_type", "pr_uid", "cd_uid", "geometry")
        ),
        by=.(HR, HRUID2018, province, pr_uid)
    ]

Total_Data <- Reduce(
        function(...) merge(..., all = TRUE, by = c("HR", "HRUID2018", "province")),
        list(Remoteness, Time_to_Peak, Cumul_Cases)
    ) |>
    dplyr::mutate(
        first_wave_proportion = first_wave_cases/population,
        second_wave_proportion = second_wave_cases/population,
        total_proportion = first_wave_proportion + second_wave_proportion,
        total_commuters = people_commuting_within_csd + people_commuting_within_cd_but_not_csd + people_commuting_within_province_but_not_cd +
            people_commuting_outside_province,
        people_commuting_outside_their_csd = people_commuting_within_cd_but_not_csd + people_commuting_within_province_but_not_cd + people_commuting_outside_province,
        people_commuting_within_province = people_commuting_within_csd + people_commuting_within_cd_but_not_csd + people_commuting_within_province_but_not_cd,
        families_with_no_children = couples_with_0_children + people_not_in_census_families,
        families_with_children_of_size_2_or_smaller = singles_with_1_child,
        families_with_children_of_size_3_or_larger = couples_with_1_child + couples_with_2_children + couples_with_3_or_more_children + 
            singles_with_2_children + singles_with_3_or_more_children,
        families_with_children_of_size_3_or_smaller = singles_with_1_child + singles_with_2_children + couples_with_1_child,
        families_with_children_of_size_4_or_larger = couples_with_2_children + couples_with_3_or_more_children + singles_with_3_or_more_children,
        HR = factor(HR),
        HRUID2018 = factor(HRUID2018),
    )

# the model Seyed and I agreed to
# THIS WORKS FOR ONTARIO, AND NO OTHER PROVINCE
# comment the [province == PROVINCE] statement to get the regressions for all of Canada

PROVINCE <- "Ontario"

# # for the first wave of infection
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
# creating a table with case numbers broken up by wave, so that I
print(summary(lm(
    # formula = proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger + 
        # total_commuters + aR_score + wave,
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

# brute force search loop for linear models - braced for code folding in RStudio
{
# ############################################3
# #
# # BRUTE FORCE SEARCH FOR LINEAR MODELS WHERE THE aR SCORE IS SIGNIFICANT
# #
# # with the variables we have available to us, generate a set of all possible
# # combinations of variables (using the powwerset function at the start), and 
# # lm each one, taking only the models where aR is significant (at the 5%
# # level). The table can be grepped through to find a model that we like.
# 
# # 8 cores, 5GHz, 32GB ram- took ~30 mins
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
# # generating a list of all sensible combinations, with redundancies removed
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
#     fwrite( # write a file in each thread
#         unique(all_possible_linear_models), 
#         sprintf("all_possible_linear_models_%s.csv", gsub(" ", "_", province_name))
#     )
# }
# print(Sys.time() - start_time)
# 
# stopCluster(cl)
#     
# All_Linear_Regressions <- rbind(
#     fread("all_possible_linear_models_Canada.csv"),
#     fread("all_possible_linear_models_Ontario.csv"),
#     fread("all_possible_linear_models_British_Columbia.csv"),
#     fread("all_possible_linear_models_Saskatchewan.csv"),
#     fread("all_possible_linear_models_Alberta.csv"),
#     fread("all_possible_linear_models_Manitoba.csv"),
#     fread("all_possible_linear_models_Quebec.csv"),
#     fread("all_possible_linear_models_New_Brunswick.csv")
# )
}
# end of linear regression brute force search

# get the geo boundaries of each health region for plotting a map of province health regions
class_map <- ggplot()
for(hr_code in Regions[, unique(HRUID2018)][1:109])
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
    coord_sf(datum=NA)
    ggsave(class_map, "Graphs/class_map_c2.png", dpi=600)

province_map <- function(prov_names)
{
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
                        sprintf("%s - %s", pr_uid, HR)
                    ])
                )
        }
    }

    prov_HR_map <- prov_HR_map +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none"
        ) +
        coord_sf(datum=NA)

    return(prov_HR_map)
}