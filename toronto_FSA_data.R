# Brendon Phillips
# ABM_Lab
# York University
# supervisor: Professor Seyed M. Moghadas

rm(list=ls())

library(rightTruncation)
library(ggplot2)
library(CanCovidData)
library(MASS)
library(data.table)
library(dplyr)
library(stringr)

setwd('/home/bren/Documents/GitHub/MIZ_project/')

source("NOW_function_header.R")

# raw_toronto_fsa_data <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv")
# raw_toronto_fsa_data <- get_toronto_neighbourhood_cases()
# raw_toronto_fsa_data |>
#     dplyr::rename(ID = "_id") |>
#     dplyr::rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
#     dplyr::mutate(
#         reporting_delay = as.numeric(reported_date-episode_date, units="days"),
#         # lower_age = unlist(lapply(age_group, age_range_min)),
#         # upper_age = unlist(lapply(age_group, age_range_max)),
#         currently_hospitalized = (currently_hospitalized == "Yes"),
#         currently_in_icu = (currently_in_icu == "Yes"),
#         currently_intubated = (currently_intubated == "Yes"),
#         ever_hospitalized = (ever_hospitalized == "Yes"),
#         ever_in_icu = (ever_in_icu == "Yes"),
#         ever_intubated  = (ever_intubated == "Yes"),
#         num_case = TRUE
#     ) |>
#     dplyr::filter(reporting_delay >= 0 & reporting_delay <= 60) |>
#     dplyr::filter(classification == "CONFIRMED") |>
#     dplyr::select(id, neighbourhood_name, fsa, age_group, classification, client_gender, age_group, episode_date, reported_date, reporting_delay) |>
#     write.csv("Toronto_FSA_Reporting_Delay_Data.csv", row.names = FALSE)

toronto_fsa_data <- fread("Toronto_FSA_Reporting_Delay_Data.csv")

# # TO ESTABLISH/CONFIRM THAT EVERY FSA HERE IS FROM THE TORONTO AREA
# # THIS CORRESPONDENCE TABLE IS OUTDATED
# # Public health units to postal code correspondence table
# # https://mdl.library.utoronto.ca/collections/numeric-data/statistical-reference-tools/geographic-codes-classification
# # text find and replace for "Leeds,Grenville,Lanark" -> "Leeds Grenville Lanark"
# haha <- fread("data") |>
#     dplyr::mutate(POSTCD = substr(POSTCD, 1, 3)) |>
#     dplyr::filter(POSTCD %in% toronto_fsa_data[, unique(fsa)]) |>
#     unique()

# exp_fit <- fitdistr(Toronto_FSA_Data$reporting_delay, "exponential") 
# dweib <- data.table(x = seq(0, Toronto_FSA_Data[, max(reporting_delay)], 0.001))
# dweib$top <- dweibull(dweib$x, shape=MLE_res$estimate[1], scale=MLE_res$estimate[2]) # * max(unname(table(Toronto_FSA_Data$reporting_delay))) * 7
# delay_distro <- ggplot(Toronto_FSA_Data, aes(x=reporting_delay)) +
#     geom_histogram(binwidth=1, aes(y=..density..), colour="black", fill="grey") +
#     theme_bw() +
#     # geom_line(data=dweib, aes(x=x, y=top))
#     stat_function(fun=dweibull, args=list(shape=MLE_res$estimate[1], scale=MLE_res$estimate[2]), col="blue", size=1) +
#     stat_function(fun=dexp, args=(mean=exp_fit$estimate[["rate"]]), col="red", size=1)

# # Sources of Infection
# sources_of_infection <- ggplot(Toronto_FSA_Data, aes(x=str_wrap(source_of_infection,20))) +
#     geom_bar(fill="blue") +
#     labs(x="Sources of infection", y="Case count") +
#     theme_bw()
# 
# # infections by age group
# case_distribution_by_age <- ggplot(
#         Toronto_FSA_Data |> filter(!is.na(age_group)), 
#         aes(x=str_wrap(age_group,20))
#     ) +
#     geom_bar(fill="blue") +
#     labs(x="Age Group", y="Case count") +
#     theme_bw()
# 
# outbreak_type <- ggplot(
#         Toronto_FSA_Data,
#         aes(x=str_wrap(outbreak_associated, 20))
#     ) +
#     geom_bar(fill="blue") +
#     labs(x="Context", y="Case count") +
#     theme_bw()
# 
# distrib_by_FSA <- ggplot(
#         Toronto_FSA_Data,
#         aes(x=str_wrap(fsa, 20))
#     ) +
#     geom_bar(fill="blue") +
#     labs(x="Context", y="Case count") +
#     theme_bw()
# 
# # there's no overlap between the list of CSDs and these neighbourhood names, so
# # there's no remoteness information here
# CSD_info <- fread("Classifications/Classification_HR_Households_Commute.csv")
# overlap <- intersect(unique(Toronto_FSA_Data$neighbourhood_name), unique(CSD_info$region))
# 
# # Toronto - table of neighbourhood crime rates - this gives us the population and maybe a crude indication of QOL
# Toronto_crime_rates <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/4754baf5-3715-4166-a347-eda9813521de?format=csv&projection=4326") |>
#     select(-"_id", -geometry, -OBJECTID) |>
#     select(contains("2020") | !contains("20")) |>
#     rename(neighbourhood_name = Neighbourhood)
# 
# # toronto_testing_sites <- fread("grep -v '^#' Classifications/toronto_testing_centres.csv") |> 
# #     select(-address, -neighbourhood_number) |> 
# #     mutate(
# #         appointment_required = (tolower(appointment_required) == "yes"),
# #         tests_for_those_with_symptoms_exposure = (tolower(tests_for_those_with_symptoms_exposure) == "yes")
# #     ) |>
# #     mutate(min_age_years = unlist(lapply(requirement, \(x) if(grepl("mth", x)) parse_number(x)/12 else if(grepl("yr", x)) parse_number(x) else 0))) |>
# #     select(-requirement)
# 
# 
# raw_toronto_vaccination_data <- read("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/00ebee09-a602-4f82-978b-ad0a40f3846f?format=csv&projection=4326")
# Toronto_vaccination_sites <- raw_toronto_vaccination_data |>
#     select(which(unname(colSums(is.na(raw_toronto_vaccination_data)) < nrow(raw_toronto_vaccination_data)))) |>
#     select(-geometry, -"_id", -locationId, -address, -long, -hours, -info, -phone, -phoneExt, -phoneAlt, -Data_Source, -OBJECTID)
#     
#     # select(locationName, locationType, sameDayAppt, province, PHU, appointments, walk_ins, symptomatic, general_population, children_under_2, public_transit, Date_Added, wardName)
#     
# 
# 
# 
# 
# 
# toronto_testing_sites <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/308efc49-5428-439a-b504-e327ab632c4f?format=csv&projection=4326")
# 
# # because of FSA, some neighbourhoods are split into two, so just sum over the neighbourhood names to
# # get sensible numbers for correlation testing
# Total_Toronto_Info <- Reduce(
#     function(...) merge(..., by="neighbourhood_name", all = TRUE),
#     list(
#         Toronto_FSA_Data[, 
#                          lapply(.SD, sum), 
#                          .SDcols=c("num_case", "currently_hospitalized", "currently_in_icu", "currently_intubated", 
#                                    "ever_hospitalized", "ever_in_icu", "ever_intubated"), 
#                          by=.(neighbourhood_name) #, fsa)
#         ], 
#         Toronto_crime_rates |> rename_with( \(x) tolower(sub("_$|F_", "", gsub("2020", "", x))) ) #,
#        #  toronto_testing_sites[,
#        #                          c(.N, lapply(.SD, sum, na.rm=T)),
#        #                         .SDcols=c("appointment_required", "tests_for_those_with_symptoms_exposure"),
#        #                         by=.(neighbourhood_name) # , fsa)
#        # ] |> rename(num_testing_sites=N)
#     )
# ) # |>
# # mutate(num_testing_sites = unlist(lapply(num_testing_sites, \(x) if(is.na(x)) 0 else x))) |>
# # rename(
# #     num_testing_sites_requiring_appmt = appointment_required,
# #     num_testing_sites_for_symptoms_exposure = tests_for_those_with_symptoms_exposure,
# #     cases_to_date = num_case
# # )


