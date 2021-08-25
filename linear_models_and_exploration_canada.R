rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(CanCovidData)
library(stringr)
library(forcats)
library(sf)
library(xtable)
library(lme4)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(PROJECT_FOLDER)
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source(sprintf("%s/function_header.R", PROJECT_FOLDER))

##########################################################################################

# weekly_moving_average <- function(x) stats::filter(x, rep(1,7), sides = 1)/7
# 
# canada_temp <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=canada") %>%
#     .$cases %>%
#     dplyr::mutate(
#         date = as.Date(date_report, format="%d-%m-%Y"),
#         avg = weekly_moving_average(cases)
#     ) %>%
#     data.table()
# 
# ggplot(canada_temp, aes(date, cases)) +
#     geom_line() +
#     geom_line(aes(y=avg)) +
#     scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
#     geom_vline(xintercept=as.Date("2021-07-15"), linetype="dashed", color = "red", size=0.5) +
#     geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "red", size=0.5) +
#     geom_vline(xintercept=as.Date("2020-07-15"), linetype="dashed", color = "red", size=0.5)

##########################################################################################

Total_Case_Data <- fread(sprintf("%s/CaseDataTables/Total_Case_Data.csv", PROJECT_FOLDER))
Total_Case_Data[, wave:=4]
Total_Case_Data[date<as.Date("2021-07-15"), wave:=3]
Total_Case_Data[date<as.Date("2021-02-15"), wave:=2]
Total_Case_Data[date<as.Date("2020-07-15"), wave:=1]

Cumul_Cases <- Total_Case_Data[
    HR!="Not Reported",
    .(
        wave_1_cumulative = sum(.SD[wave==1]$cases, na.rm=T),
        wave_2_cumulative = sum(.SD[wave==2]$cases, na.rm=T),
        wave_3_cumulative = sum(.SD[wave==3]$cases, na.rm=T),
        total_cumulative = sum(cases, na.rm=T) # ,
        # wave_4_cumulative = sum(.SD[wave==4]$cases, na.rm=T)
    ),
    by=.(HR, province, HRUID2018)
]

Regions <- readRDS(sprintf("%s/CaseDataTables/Regions.rda", PROJECT_FOLDER))

Remoteness <- data.table(Regions)[,
    lapply(.SD, sum, na.rm=TRUE),
    .SDcols = setdiff(
      names(Regions),
      c(
        "CSDUID", "CSDNAME", "PRUID", "province", "HR", "class", "population_density", "HRUID2018", "csd_type", "pr_uid", "cd_uid", "geometry", "region"
      )
    ),
    by=.(HR, HRUID2018, province, pr_uid)
]

writeLines("\nTotal Data - adding HRs and saving")
Total_Data <- Reduce(
        function(...) merge(..., all = TRUE, by = c("HR", "province")),
        list(
            Remoteness,
            Cumul_Cases,
            data.table(Regions)[, .(mR_weighted_by_pop = sum(mR_score*population, na.rm=TRUE)/sum(population, na.rm=TRUE)), by=.(province, HR)]
        )
    ) %>%
    dplyr::mutate(
        HRUID = factor(HRUID2018.x),
        total_commuters = people_commuting_within_csd + people_commuting_within_cd_but_not_csd + people_commuting_within_province_but_not_cd +
            people_commuting_outside_province,
        HR = factor(HR),
        
        F0 = couples_with_0_children + people_not_in_census_families,
        Fs = singles_with_1_child + singles_with_2_children + singles_with_3_or_more_children,
        Fc = couples_with_1_child + couples_with_2_children + couples_with_3_or_more_children,
        
        F_with_C = singles_with_1_child + singles_with_2_children + singles_with_3_or_more_children + couples_with_1_child + couples_with_2_children + couples_with_3_or_more_children,
        
        families_with_no_children = couples_with_0_children + people_not_in_census_families,
        
        families_with_children_of_size_2_or_smaller = singles_with_1_child,
        families_with_children_of_size_3_or_larger = couples_with_1_child + couples_with_2_children + couples_with_3_or_more_children +
            singles_with_2_children + singles_with_3_or_more_children,
        families_with_children_of_size_3_or_smaller = singles_with_1_child + singles_with_2_children + couples_with_1_child,
        families_with_children_of_size_4_or_larger = couples_with_2_children + couples_with_3_or_more_children + singles_with_3_or_more_children,
        
        population_density = population/area_sq_km,
        
        log_pop = log(population),
        
        F0 = couples_with_0_children + people_not_in_census_families,
        F3 = singles_with_1_child + singles_with_2_children + couples_with_1_child,
        F4 = couples_with_2_children + couples_with_3_or_more_children + singles_with_3_or_more_children,
        
    ) %>%
  dplyr::mutate(
        log_pop_density = log(population_density),
        log_F0 = log(F0), log_F3 = log(F3), log_F4 = log(F4),
        
        F0_normalised = (F0 - mean(F0, na.rm=TRUE)) / sd(F0, na.rm=TRUE),
        F3_normalised = (F3 - mean(F3, na.rm=TRUE)) / sd(F3, na.rm=TRUE),
        F4_normalised = (F4 - mean(F4, na.rm=TRUE)) / sd(F4, na.rm=TRUE),
        
        mR_normalised = (mR_weighted_by_pop - mean(mR_weighted_by_pop, na.rm=TRUE)) / sd(mR_weighted_by_pop, na.rm=TRUE),
        pop_density_normalised = (population_density - mean(population_density, na.rm=TRUE)) / sd(population_density, na.rm=TRUE)
  ) %>%
 #  dplyr::filter(
 #    !is.na(population) & log_pop_density>=1 & !is.na(wave_1_cumulative)
 # & !is.na(wave_2_cumulative) & !is.na(wave_3_cumulative)    ) %>%
    dplyr::select(-HRUID2018.x, -HRUID2018.y)
    fwrite(Total_Data, sprintf("%s/CaseDataTables/Total_Data.csv", PROJECT_FOLDER))

###################################### CANADA-WIDE REGRESSIONS
    
    PROVINCE <- "Alberta"

# canada_first_wave <- summary(lm(
#     formula = wave_1_cumulative ~ mR_weighted_by_pop +
#       F0 + F3 +F4 +
#       population_density,
#     # log_pop,
#     data = Total_Data
# ))

# canada_second_wave <- summary(lm(
#   formula = wave_2_cumulative ~ mR_weighted_by_pop +
#     F0 + F3 +F4 +
#     population_density,
#     # log_pop,
#   data = Total_Data
# ))
# 
# canada_third_wave <- summary(lm(
#   formula = wave_3_cumulative ~ mR_weighted_by_pop +
#     F0 + F3 + F4 +
#     population_density,
#   data = Total_Data
# ))
# 
# canada_total <- summary(lm(
#     formula = cases ~ mR_weighted_by_pop +
#       F0 + F3 + F4 +
#       population_density +
#       wave,
#     data = rbind(
#         Total_Data %>%
#             dplyr::select(-wave_2_cumulative, -wave_3_cumulative, -total_cumulative) %>%
#             dplyr::rename(cases = wave_1_cumulative) %>%
#             dplyr::mutate(wave=1),
#         Total_Data %>%
#             dplyr::select(-wave_1_cumulative, -wave_3_cumulative, -total_cumulative) %>%
#             dplyr::rename(cases = wave_2_cumulative) %>%
#             dplyr::mutate(wave=2),
#         Total_Data %>%
#             dplyr::select(-wave_1_cumulative, -wave_2_cumulative, -total_cumulative) %>%
#             dplyr::rename(cases = wave_3_cumulative) %>%
#             dplyr::mutate(wave=3)
#     ) 
# ))

# canada_first_wave_z_transformed <- lm(
#     formula = wave_1_cumulative ~ mR_normalised +
#       F0_normalised + F3_normalised +F4_normalised +
#       pop_density_normalised,
#     data = Total_Data %>% dplyr::filter(province == "Ontario")
# )
# 
# canada_second_wave_z_transformed <- summary(lm(
#   formula = wave_2_cumulative ~ mR_normalised +
#     F0_normalised + F3_normalised + F4_normalised +
#     pop_density_normalised,
#   data = Total_Data
# ))
# 
# canada_third_wave_z_transformed <- summary(lm(
#   formula = wave_3_cumulative ~ mR_normalised +
#     F0_normalised + F3_normalised + F4_normalised +
#     pop_density_normalised,
#   data = Total_Data
# ))
# 
# canada_total_z_transformed <- summary(lm(
#     formula = cases ~ mR_normalised +
#       F0_normalised + F3_normalised + F4_normalised +
#       pop_density_normalised +
#       wave,
#     data = rbind(
#         Total_Data %>%
#             dplyr::select(-wave_2_cumulative, -wave_3_cumulative, -total_cumulative) %>%
#             dplyr::rename(cases = wave_1_cumulative) %>%
#             dplyr::mutate(wave=1),
#         Total_Data %>%
#             dplyr::select(-wave_1_cumulative, -wave_3_cumulative, -total_cumulative) %>%
#             dplyr::rename(cases = wave_2_cumulative) %>%
#             dplyr::mutate(wave=2),
#         Total_Data %>%
#             dplyr::select(-wave_1_cumulative, -wave_2_cumulative, -total_cumulative) %>%
#             dplyr::rename(cases = wave_3_cumulative) %>%
#             dplyr::mutate(wave=3)
#     )
# ))

pretty_print_coeff_CI <- function(form, data_table)
{
  regress <- lm(
    formula = parse(text=form),
    data = data.table
  )
  
  return(regress)
  
  
  
  # data.table(vars=names(canada_first_wave_z_transformed$coefficients), canada_first_wave_z_transformed$coefficients, confint(canada_first_wave_z_transformed)) %>% dplyr::rename(coeff=V2, CI25=`2.5 %`, CI975=`97.5 %`)
}


canada_total_mixed <- summary(lmer(
    formula = cases ~ mR_weighted_by_pop +
      F0 + F3 + F4 +
      population_density +
      wave +
      (1|province),
    data = rbind(
        Total_Data %>%
            dplyr::select(-wave_2_cumulative, -wave_3_cumulative, -total_cumulative) %>%
            dplyr::rename(cases = wave_1_cumulative) %>%
            dplyr::mutate(wave=1),
        Total_Data %>%
            dplyr::select(-wave_1_cumulative, -wave_3_cumulative, -total_cumulative) %>%
            dplyr::rename(cases = wave_2_cumulative) %>%
            dplyr::mutate(wave=2),
        Total_Data %>%
            dplyr::select(-wave_1_cumulative, -wave_2_cumulative, -total_cumulative) %>%
            dplyr::rename(cases = wave_3_cumulative) %>%
            dplyr::mutate(wave=3)
    ) 
    #%>% dplyr::filter(province == PROVINCE)
))

canada_regress_results <- Reduce(
    function(x,y) merge(x=x, y=y, by="vars", all=TRUE),
    list(
        data.table(vars=rownames(canada_first_wave$coefficients), canada_first_wave$coefficients) %>%
            dplyr::select(-`Std. Error`, -`t value`) %>%
            dplyr::rename(first_wave_value=Estimate, first_wave_p=`Pr(>|t|)`),
        data.table(vars=rownames(canada_second_wave$coefficients), canada_second_wave$coefficients) %>%
            dplyr::select(-`Std. Error`, -`t value`) %>%
            dplyr::rename(second_wave_value=Estimate, second_wave_p=`Pr(>|t|)`),
        data.table(vars=rownames(canada_third_wave$coefficients), canada_third_wave$coefficients) %>%
            dplyr::select(-`Std. Error`, -`t value`) %>%
            dplyr::rename(third_wave_value=Estimate, third_wave_p=`Pr(>|t|)`),
        data.table(vars=rownames(canada_total$coefficients), canada_total$coefficients) %>%
            dplyr::select(-`Std. Error`, -`t value`) %>%
            dplyr::rename(total_value=Estimate, total_p=`Pr(>|t|)`)
        )
    ) %>%
    dplyr::mutate(
        first_wave_value = round(first_wave_value, 3),
        first_wave_p = round(first_wave_p, 4),
        second_wave_value = round(second_wave_value, 3),
        second_wave_p = round(second_wave_p, 4),
        total_value = round(total_value, 3),
        total_p = round(total_p, 4),
    ) %>%
    print(xtable(.), include.rownames=F)
    fwrite(canada_regress_results, file="canada_regress_results.csv")
    
var_names <- function(x)
{
    if(x == "aR_score") return("aR")
    if(x == "families_with_children_of_size_2_or_smaller") return("F2")
    if(x == "families_with_children_of_size_3_or_larger") return("F3")
    if(x == "families_with_children_of_size_3_or_smaller") return("F3")
    if(x == "families_with_children_of_size_4_or_larger") return("F4")
    if(x == "families_with_no_children") return("F0")
    if(x == "population_density") return("D")
    if(x == "total_commuters") return("L")
    if(x == "wave") return("x")
    if(x == "(Intercept)") return("Intercept")
    if(x == "mR_weighted_by_pop") return("mR_pop")
    if(x == "aR_weighted_by_pop") return("aR_pop")
    return(x)
}
    
print(canada_regress_results %>%
    dplyr::mutate(
        first_wave_value = round(first_wave_value, 4),
        first_wave_p = round(first_wave_p, 4),
        second_wave_value = round(second_wave_value, 4),
        second_wave_p = round(second_wave_p, 4),
        third_wave_value = round(third_wave_value, 4),
        third_wave_p = round(third_wave_p, 4),
        total_value = round(total_value, 4),
        total_p = round(total_p, 4)
    ) %>%
    dplyr::rename(
        Parameters = vars,
        `first wave` = first_wave_value,
        `first wave ` = first_wave_p,
        `second wave` = second_wave_value,
        `second wave ` = second_wave_p
    ) %>%
    rbind(list("Parameters", "beta", "p-value","beta", "p-value","beta", "p-value", "beta", "p-value"), .) %>%
    dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
    dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
    xtable(.)
         , include.rownames=F)


# 
# # the sum of cases over all days
# summary(lm(
#     formula = total_cases ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
#         total_commuters +  population_density + mR_weighted_by_pop,
#     data = Total_Data
# ))
# 
# ######################################## PROVINCIAL REGRESSIONS
# 
# provinces_regressions <- data.table()
# 
# for(PROVINCE in c("Saskatchewan")) #c("Ontario","British Columbia", "Quebec", "Saskatchewan")) #  
# {
#     Total_Data_Here <- Total_Data %>% dplyr::filter(province == PROVINCE)
#     
#     first_wave_regression <- summary(lm(
#         formula = first_wave_cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
#             families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
#             # mR_weighted_by_pop,
#             aR_weighted_by_pop,
#         
#         data = Total_Data_Here
#     ))
#     
#     second_wave_regression <- summary(lm(
#         formula = second_wave_cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
#             # mR_weighted_by_pop,
#             aR_weighted_by_pop,
#         data = Total_Data_Here
#     ))
# 
#     total_regression <- summary(lm(
#         formula = cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
#             # mR_weighted_by_pop +
#             aR_weighted_by_pop +
#             wave,
#         data = rbind(
#             Total_Data_Here %>%
#                 dplyr::select(!starts_with("second_wave_")) %>%
#                 dplyr::select(-total_cases, -interpeak_distance) %>%
#                 dplyr::rename_with(function(x) gsub("first_wave_", "", x)) %>%
#                 dplyr::mutate(wave=1),
#             Total_Data_Here %>%
#                 dplyr::select(!starts_with("first_wave_")) %>%
#                 dplyr::select(-total_cases, -interpeak_distance) %>%
#                 dplyr::rename_with(function(x) gsub("second_wave_", "", x)) %>%
#                 dplyr::mutate(wave=2)
#         )
#     ))
# 
#     provinces_regressions <- rbind(
#         provinces_regressions,
#         Reduce(
#             function(x,y) merge(x=x, y=y, by="vars", all=TRUE),
#             list(
#                 data.table(province=PROVINCE, vars=rownames(first_wave_regression$coefficients), first_wave_regression$coefficients) %>%
#                     dplyr::select(-`Std. Error`, -`t value`) %>%
#                     dplyr::rename(first_wave_value=Estimate, first_wave_p=`Pr(>|t|)`),
#                 data.table(province=PROVINCE, vars=rownames(second_wave_regression$coefficients), second_wave_regression$coefficients) %>%
#                     dplyr::select(-`Std. Error`, -`t value`) %>%
#                     dplyr::rename(second_wave_value=Estimate, second_wave_p=`Pr(>|t|)`),
#                 data.table(province=PROVINCE, vars=rownames(total_regression$coefficients), total_regression$coefficients) %>%
#                     dplyr::select(-`Std. Error`, -`t value`) %>%
#                     dplyr::rename(total_value=Estimate, total_p=`Pr(>|t|)`)
#             )
#         )
#     )
# }
# 
# 
# print(provinces_regressions %>%
#     dplyr::mutate(
#         first_wave_value = round(first_wave_value, 4),
#         first_wave_p = round(first_wave_p, 3),
#         second_wave_value = round(second_wave_value, 4),
#         second_wave_p = round(second_wave_p, 3),
#         total_value = round(total_value, 4),
#         total_p = round(total_p, 3),
#         province = coalesce(province, province.x, province.y)
#     ) %>%
#     dplyr::rename(
#         Parameters = vars,
#         Province = province,
#         `first wave` = first_wave_value,
#         `first wave ` = first_wave_p,
#         `second wave` = second_wave_value,
#         `second wave ` = second_wave_p
#     ) %>%
#     dplyr::select(-province.x, -province.y) %>%
#     dplyr::relocate(Province, Parameters) %>%
#     rbind(list("Province", "Parameters", "beta", "p-value","beta", "p-value","beta", "p-value"), .) %>%
#     dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
#     xtable(.), include.rownames=F)
#     # print(xtable(.), include.rownames=F)
#     # fwrite(provinces_regressions, file="provinces_regressions_results.csv")
# 
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(prov)
#     print(summary(Total_Data[tolower(province) == prov]$mR_score))
#     writeLines("\n")
# }
# 
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(prov)
#     print(summary(Total_Data[tolower(province) == prov]$aR_score))
#     writeLines("\n")
# }
# 
# 
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(sprintf("%s, %f", prov, Total_Data[tolower(province)==prov, sum(area_sq_km)]))
# }
# 
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(sprintf("%s, %f", prov, Total_Data[tolower(province)==prov, sum(population)]))
# }
# 
# Regions <- readRDS(sprintf("%s/CaseDataTables/Regions.rda", PROJECT_FOLDER)) %>% data.table()
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(sprintf("%s, %i", prov, Regions[tolower(province)==prov, sum(num_csds)]))
# }
# 
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(sprintf("%s, %f", prov, Regions[tolower(province)==prov, sum(population, na.rm=T)/sum(num_csds)]))
# }
# 
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(sprintf("%s, %f", prov, Regions[tolower(province)==prov, length(unique(HR))]))
# }
# 
# 
# # for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# # {
# #     print(sprintf("%s, %f", prov, Regions[tolower(province)==prov, sum(population, na.rm=T)/length(unique(HR))]))
# # }
# 
# 
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(sprintf("%s, %f", prov, Total_Data[tolower(province)==prov, sum(population, na.rm=T)/length(unique(HR))]))
# }
# 
# 
# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(sprintf("%s, %f", prov, Regions[, length(unique(CSDUID))/length(unique(HR))]))
# }
# 
# 
# # # for the first wave of infection
# # print()
# # # for the second wave
# # print()
# # # # the sum of cases over all days
# # # print(summary(lm(
# # #     formula = total_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
# # #        total_commuters + aR_score,
# # #     data = Total_Data[province == PROVINCE]
# # # )))
# # # creating a table with case numbers broken up by wave, so that I can include the wave number in the regression
# # print()
# 
# # 
# # # original scheme in the paper
# # # THIS WORKS FOR ONTARIO, AND NO OTHER PROVINCE
# # 
# # # num HRS: Ontario 34, BC 16, Quebec 18, Alberta 5, Saskatchewan 13, Manitoba 5, New Brunswick 7, Nova Scotia 4, NL 4
# # 
# # provinces_regressions <- data.table()
# # 
# # the_bins <- sort(c(0, optbin(Total_Data$aR_score, 5, na.rm=TRUE)$thr))
# # the_bins <- cbind(the_bins[-length(the_bins)], the_bins[-1])
# # 
# # for(bin in 1:nrow(the_bins))
# # {
# #     bot_thr <- the_bins[bin,1]
# #     top_thr <- the_bins[bin,2]
# # 
# #     Total_Data_Here <- Total_Data %>% dplyr::filter(province == PROVINCE) %>% dplyr::filter(bot_thr<=mR_score & mR_score<=top_thr)
# # 
# #     if(nrow(Total_Data_Here) == 0) next
# # 
# #     for(PROVINCE in c("Ontario", "British Columbia", "Quebec", "Saskatchewan"))
# #     {
# #         first_wave_regression <- summary(lm(
# #             formula = first_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
# #                 total_commuters +
# #                 # population_density +
# #                 aR_score,
# #             data = Total_Data_Here
# #         ))
# # 
# #         second_wave_regression <- summary(lm(
# #             formula = second_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
# #                 total_commuters +
# #                 # population_density +
# #                 aR_score,
# #             data = Total_Data_Here
# #         ))
# # 
# #         total_regression <- summary(lm(
# #             formula = proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
# #                 aR_score +
# #                 total_commuters +
# #                 # population_density +
# #                 wave,
# #             data = rbind(
# #                 Total_Data_Here %>%
# #                     dplyr::select(!starts_with("second_wave_")) %>%
# #                     dplyr::select(-total_cases, -interpeak_distance) %>%
# #                     dplyr::rename_with(function(x) gsub("first_wave_", "", x)) %>%
# #                     dplyr::mutate(wave=1),
# #                 Total_Data_Here %>%
# #                     dplyr::select(!starts_with("first_wave_")) %>%
# #                     dplyr::select(-total_cases, -interpeak_distance) %>%
# #                     dplyr::rename_with(function(x) gsub("second_wave_", "", x)) %>%
# #                     dplyr::mutate(wave=2)
# #                 )
# #         ))
# # 
# #         provinces_regressions <- rbind(
# #             provinces_regressions,
# #             cbind(
# #                 Reduce(
# #                     function(x,y) merge(x=x, y=y, by="vars", all=TRUE),
# #                     list(
# #                         data.table(province=PROVINCE, vars=rownames(first_wave_regression$coefficients), first_wave_regression$coefficients) %>%
# #                             dplyr::select(-`Std. Error`, -`t value`) %>%
# #                             dplyr::rename(first_wave_value=Estimate, first_wave_p=`Pr(>|t|)`),
# #                         data.table(province=PROVINCE, vars=rownames(second_wave_regression$coefficients), second_wave_regression$coefficients) %>%
# #                             dplyr::select(-`Std. Error`, -`t value`) %>%
# #                             dplyr::rename(second_wave_value=Estimate, second_wave_p=`Pr(>|t|)`),
# #                         data.table(province=PROVINCE, vars=rownames(total_regression$coefficients), total_regression$coefficients) %>%
# #                             dplyr::select(-`Std. Error`, -`t value`) %>%
# #                             dplyr::rename(total_value=Estimate, total_p=`Pr(>|t|)`)
# #                     )
# #                 ),
# #                 data.table(bin_num=bin, low_thr=bot_thr, hig_thr=top_thr)
# #             )
# #         )
# #     }
# # }
# #  
