rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(CanCovidData)
library(stringr)
library(forcats)
library(sf)
library(xtable)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(PROJECT_FOLDER)
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source(sprintf("%s/function_header.R", PROJECT_FOLDER))

Total_Data <- fread("CaseDataTables/Total_Data.csv") %>%
    dplyr::mutate( # special columns we create for the regressions
        families_with_no_children = couples_with_0_children + people_not_in_census_families,
        families_with_children_of_size_2_or_smaller = singles_with_1_child,
        families_with_children_of_size_3_or_larger = couples_with_1_child + couples_with_2_children + couples_with_3_or_more_children +
            singles_with_2_children + singles_with_3_or_more_children,
        families_with_children_of_size_3_or_smaller = singles_with_1_child + singles_with_2_children + couples_with_1_child,
        families_with_children_of_size_4_or_larger = couples_with_2_children + couples_with_3_or_more_children + singles_with_3_or_more_children,
        HR = factor(HR),
        HRUID2018 = factor(HRUID2018),
        population_density = population/area_sq_km
    )

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


######################################3 CANADA-WIDE REGRESSIONS

canada_first_wave <- summary(lm(
    formula = first_wave_cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
        total_commuters +
        population_density +
        mean_aR,
    data = Total_Data
))

canada_second_wave <- summary(lm(
    formula = second_wave_cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
        total_commuters +
        population_density +
        mean_aR,
    data = Total_Data
))

canada_total <- summary(lm(
    formula = cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
        total_commuters +
        population_density +
        mean_aR +
        wave,
    data = rbind(
        Total_Data %>%
            dplyr::select(!starts_with("second_wave_")) %>%
            dplyr::select(-total_cases, -interpeak_distance) %>%
            dplyr::rename_with(function(x) gsub("first_wave_", "", x)) %>%
            dplyr::mutate(wave=1),
        Total_Data %>%
            dplyr::select(!starts_with("first_wave_")) %>%
            dplyr::select(-total_cases, -interpeak_distance) %>%
            dplyr::rename_with(function(x) gsub("second_wave_", "", x)) %>%
            dplyr::mutate(wave=2)
    )
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
    
    
    print(canada_regress_results %>%
        dplyr::mutate(
            first_wave_value = round(first_wave_value, 4),
            first_wave_p = round(first_wave_p, 4),
            second_wave_value = round(second_wave_value, 4),
            second_wave_p = round(second_wave_p, 4),
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
        rbind(list("Parameters", "beta", "p-value","beta", "p-value","beta", "p-value"), .) %>%
        dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
        dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
        xtable(.)
             , include.rownames=F)
    


# the sum of cases over all days
summary(lm(
    formula = total_cases ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
        total_commuters +  population_density + mR_weighted_by_pop,
    data = Total_Data
))

######################################## PROVINCIAL REGRESSIONS

provinces_regressions <- data.table()

for(PROVINCE in c("Saskatchewan")) #c("Ontario","British Columbia", "Quebec", "Saskatchewan")) #  
{
    Total_Data_Here <- Total_Data %>% dplyr::filter(province == PROVINCE)
    
    first_wave_regression <- summary(lm(
        formula = first_wave_cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
            families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
            # mR_weighted_by_pop,
            aR_weighted_by_pop,
        
        data = Total_Data_Here
    ))
    
    second_wave_regression <- summary(lm(
        formula = second_wave_cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
            # mR_weighted_by_pop,
            aR_weighted_by_pop,
        data = Total_Data_Here
    ))

    total_regression <- summary(lm(
        formula = cases ~ families_with_no_children + families_with_children_of_size_3_or_smaller + families_with_children_of_size_4_or_larger +
            # mR_weighted_by_pop +
            aR_weighted_by_pop +
            wave,
        data = rbind(
            Total_Data_Here %>%
                dplyr::select(!starts_with("second_wave_")) %>%
                dplyr::select(-total_cases, -interpeak_distance) %>%
                dplyr::rename_with(function(x) gsub("first_wave_", "", x)) %>%
                dplyr::mutate(wave=1),
            Total_Data_Here %>%
                dplyr::select(!starts_with("first_wave_")) %>%
                dplyr::select(-total_cases, -interpeak_distance) %>%
                dplyr::rename_with(function(x) gsub("second_wave_", "", x)) %>%
                dplyr::mutate(wave=2)
        )
    ))

    provinces_regressions <- rbind(
        provinces_regressions,
        Reduce(
            function(x,y) merge(x=x, y=y, by="vars", all=TRUE),
            list(
                data.table(province=PROVINCE, vars=rownames(first_wave_regression$coefficients), first_wave_regression$coefficients) %>%
                    dplyr::select(-`Std. Error`, -`t value`) %>%
                    dplyr::rename(first_wave_value=Estimate, first_wave_p=`Pr(>|t|)`),
                data.table(province=PROVINCE, vars=rownames(second_wave_regression$coefficients), second_wave_regression$coefficients) %>%
                    dplyr::select(-`Std. Error`, -`t value`) %>%
                    dplyr::rename(second_wave_value=Estimate, second_wave_p=`Pr(>|t|)`),
                data.table(province=PROVINCE, vars=rownames(total_regression$coefficients), total_regression$coefficients) %>%
                    dplyr::select(-`Std. Error`, -`t value`) %>%
                    dplyr::rename(total_value=Estimate, total_p=`Pr(>|t|)`)
            )
        )
    )
}


print(provinces_regressions %>%
    dplyr::mutate(
        first_wave_value = round(first_wave_value, 4),
        first_wave_p = round(first_wave_p, 3),
        second_wave_value = round(second_wave_value, 4),
        second_wave_p = round(second_wave_p, 3),
        total_value = round(total_value, 4),
        total_p = round(total_p, 3),
        province = coalesce(province, province.x, province.y)
    ) %>%
    dplyr::rename(
        Parameters = vars,
        Province = province,
        `first wave` = first_wave_value,
        `first wave ` = first_wave_p,
        `second wave` = second_wave_value,
        `second wave ` = second_wave_p
    ) %>%
    dplyr::select(-province.x, -province.y) %>%
    dplyr::relocate(Province, Parameters) %>%
    rbind(list("Province", "Parameters", "beta", "p-value","beta", "p-value","beta", "p-value"), .) %>%
    dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
    xtable(.), include.rownames=F)
    # print(xtable(.), include.rownames=F)
    # fwrite(provinces_regressions, file="provinces_regressions_results.csv")

for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(prov)
    print(summary(Total_Data[tolower(province) == prov]$mR_score))
    writeLines("\n")
}

for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(prov)
    print(summary(Total_Data[tolower(province) == prov]$aR_score))
    writeLines("\n")
}


for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(sprintf("%s, %f", prov, Total_Data[tolower(province)==prov, sum(area_sq_km)]))
}

for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(sprintf("%s, %f", prov, Total_Data[tolower(province)==prov, sum(population)]))
}

Regions <- readRDS(sprintf("%s/CaseDataTables/Regions.rda", PROJECT_FOLDER)) |> data.table()
for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(sprintf("%s, %i", prov, Regions[tolower(province)==prov, sum(num_csds)]))
}

for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(sprintf("%s, %f", prov, Regions[tolower(province)==prov, sum(population, na.rm=T)/sum(num_csds)]))
}

for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(sprintf("%s, %f", prov, Regions[tolower(province)==prov, length(unique(HR))]))
}


# for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
# {
#     print(sprintf("%s, %f", prov, Regions[tolower(province)==prov, sum(population, na.rm=T)/length(unique(HR))]))
# }


for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(sprintf("%s, %f", prov, Total_Data[tolower(province)==prov, sum(population, na.rm=T)/length(unique(HR))]))
}


for(prov in c("british columbia", "ontario", "saskatchewan", "quebec"))
{
    print(sprintf("%s, %f", prov, Regions[, length(unique(CSDUID))/length(unique(HR))]))
}


# # for the first wave of infection
# print()
# # for the second wave
# print()
# # # the sum of cases over all days
# # print(summary(lm(
# #     formula = total_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
# #        total_commuters + aR_score,
# #     data = Total_Data[province == PROVINCE]
# # )))
# # creating a table with case numbers broken up by wave, so that I can include the wave number in the regression
# print()

# 
# # original scheme in the paper
# # THIS WORKS FOR ONTARIO, AND NO OTHER PROVINCE
# 
# # num HRS: Ontario 34, BC 16, Quebec 18, Alberta 5, Saskatchewan 13, Manitoba 5, New Brunswick 7, Nova Scotia 4, NL 4
# 
# provinces_regressions <- data.table()
# 
# the_bins <- sort(c(0, optbin(Total_Data$aR_score, 5, na.rm=TRUE)$thr))
# the_bins <- cbind(the_bins[-length(the_bins)], the_bins[-1])
# 
# for(bin in 1:nrow(the_bins))
# {
#     bot_thr <- the_bins[bin,1]
#     top_thr <- the_bins[bin,2]
# 
#     Total_Data_Here <- Total_Data %>% dplyr::filter(province == PROVINCE) %>% dplyr::filter(bot_thr<=mR_score & mR_score<=top_thr)
# 
#     if(nrow(Total_Data_Here) == 0) next
# 
#     for(PROVINCE in c("Ontario", "British Columbia", "Quebec", "Saskatchewan"))
#     {
#         first_wave_regression <- summary(lm(
#             formula = first_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
#                 total_commuters +
#                 # population_density +
#                 aR_score,
#             data = Total_Data_Here
#         ))
# 
#         second_wave_regression <- summary(lm(
#             formula = second_wave_proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
#                 total_commuters +
#                 # population_density +
#                 aR_score,
#             data = Total_Data_Here
#         ))
# 
#         total_regression <- summary(lm(
#             formula = proportion ~ families_with_no_children + families_with_children_of_size_2_or_smaller + families_with_children_of_size_3_or_larger +
#                 aR_score +
#                 total_commuters +
#                 # population_density +
#                 wave,
#             data = rbind(
#                 Total_Data_Here %>%
#                     dplyr::select(!starts_with("second_wave_")) %>%
#                     dplyr::select(-total_cases, -interpeak_distance) %>%
#                     dplyr::rename_with(function(x) gsub("first_wave_", "", x)) %>%
#                     dplyr::mutate(wave=1),
#                 Total_Data_Here %>%
#                     dplyr::select(!starts_with("first_wave_")) %>%
#                     dplyr::select(-total_cases, -interpeak_distance) %>%
#                     dplyr::rename_with(function(x) gsub("second_wave_", "", x)) %>%
#                     dplyr::mutate(wave=2)
#                 )
#         ))
# 
#         provinces_regressions <- rbind(
#             provinces_regressions,
#             cbind(
#                 Reduce(
#                     function(x,y) merge(x=x, y=y, by="vars", all=TRUE),
#                     list(
#                         data.table(province=PROVINCE, vars=rownames(first_wave_regression$coefficients), first_wave_regression$coefficients) %>%
#                             dplyr::select(-`Std. Error`, -`t value`) %>%
#                             dplyr::rename(first_wave_value=Estimate, first_wave_p=`Pr(>|t|)`),
#                         data.table(province=PROVINCE, vars=rownames(second_wave_regression$coefficients), second_wave_regression$coefficients) %>%
#                             dplyr::select(-`Std. Error`, -`t value`) %>%
#                             dplyr::rename(second_wave_value=Estimate, second_wave_p=`Pr(>|t|)`),
#                         data.table(province=PROVINCE, vars=rownames(total_regression$coefficients), total_regression$coefficients) %>%
#                             dplyr::select(-`Std. Error`, -`t value`) %>%
#                             dplyr::rename(total_value=Estimate, total_p=`Pr(>|t|)`)
#                     )
#                 ),
#                 data.table(bin_num=bin, low_thr=bot_thr, hig_thr=top_thr)
#             )
#         )
#     }
# }
#  
