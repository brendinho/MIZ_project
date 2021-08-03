


















# ########################################## FOR THE TORONTO LINE LIST
# 
# # fwrite(fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/ef0239b1-832b-4d0b-a1f3-4153e53b189e?format=csv"), "temp_toronto_nbd_profile.csv")
# 
# # lookup table to get the neighbourhood number based on the name
# # the name "Pellam" is misspelt in the file as "Pelham"
# toronto_nbd_name_number_lookup <- fread("grep -v '^#' Classifications/toronto_neighbourhood_name_number_correspondence_file.csv") |>
#     dplyr::mutate(look = NeighbourhoodName %>% tolower() %>% gsub("-|\\(|\\)|\\/", " ", .) %>% gsub("'|st\\.| st ", "", .) %>%
#                       gsub(" +", " ", .) %>% trimws() %>% gsub("pelham", "pellam", .))
# 
# get_toronto_nbd_number <- function(x)
# {
#     if(x == "") return(-1)
#     clean_name <- x %>% tolower() %>% gsub("-|\\(|\\)|\\/", " ", .) %>% gsub("'", "", .) %>% gsub("'|st\\.| st ", "", .) %>% gsub(" +", " ", .) %>% trimws()
#     name_matches <- str_extract(toronto_nbd_name_number_lookup$look, clean_name)
#     if( all(is.na(name_matches)) ) return(-1)
#     prospectives <- toronto_nbd_name_number_lookup[which(!is.na(name_matches)), look]
#     if(length(prospectives) == 1){
#         the_chosen_one <- prospectives
#     } else {
#         the_chosen_one <- prospectives[amatch(clean_name, prospectives)]
#     }
#     return(toronto_nbd_name_number_lookup[look == the_chosen_one, get("Hood#")])
# }
# 
# parse_strings_as_numbers <- function(theList)
# {
#     unlist(lapply(theList, \(x){ if(x == "") return (0); is_percentage <- grepl("\\%", x); x <- gsub(',|%', '', x); return(as.numeric(x)/(if(is_percentage) 100 else 1)) } ))
# }
# 
# start_of_the_second_wave <- add_wave_numbers(fread("CaseDataTables/ON_cases.csv") |> group_by(date) |> tally(cases), case_col="n", date_col="date")$date
# 
# fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv") |>
#     dplyr::rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
#     dplyr::select(-`_id`, -assigned_id, -outbreak_associated, -source_of_infection, -outcome) |>
#     dplyr::mutate(
#         currently_hospitalized = (currently_hospitalized == "Yes"),
#         currently_in_icu = (currently_in_icu == "Yes"),
#         currently_intubated = (currently_intubated == "Yes"),
#         ever_hospitalized = (ever_hospitalized == "Yes"),
#         ever_in_icu = (ever_in_icu == "Yes"),
#         ever_intubated  = (ever_intubated == "Yes"),
#         reporting_delay = as.numeric(reported_date-episode_date, units="days"),
#         lower_age = unlist(lapply(age_group, age_range_min)),
#         upper_age = unlist(lapply(age_group, age_range_max)),
#         neighbourhood_number = unlist(lapply(neighbourhood_name, get_toronto_nbd_numbers)),
#         wave = unlist(lapply(episode_date, \(x) if(x < start_of_the_second_wave) 1 else 2)),
#         num_reports = 1,
#         age_group = factor(age_group), neighbourhood_name=factor(neighbourhood_name), fsa=factor(fsa), client_gender=factor(client_gender),
#         neighbourhood_number=factor(neighbourhood_number)
#     ) |>
#     dplyr::filter(reporting_delay >= 0 & reporting_delay <= 60) |>
#     dplyr::filter(classification == "CONFIRMED") |>
#     fwrite("temp_raw_toronto_fsa_data.csv")
# 
# raw_toronto_fsa_data <- fread("temp_raw_toronto_fsa_data.csv") |>
#     dplyr::filter(neighbourhood_number!=-1 & age_group!="") |>
#     dplyr::mutate(
#         age_group = as.factor(age_group),
#         neighbourhood_name = as.factor(neighbourhood_name),
#         fsa = as.factor(fsa),
#         client_gender = as.factor(client_gender),
#         classification = as.factor(classification),
#         wave = factor(wave)
#     )
# 
# testing_sites_by_neighbourhood <- fread("grep -v '^#' toronto_testing_centres.csv") |>
#     dplyr::select(fsa, neighbourhood_name, neighbourhood_number) |>
#     dplyr::mutate(N=1) |>
#     dplyr::group_by(neighbourhood_name, neighbourhood_number) |>
#     dplyr::tally() |>
#     dplyr::rename(testing_sites=n) |>
#     as.data.table()
# 
# vaccination_sites_by_neighbourhood <- fread("grep -v '^#' covid-19-immunization-clinics.csv") |>
#     dplyr::mutate(neighbourhood_number=city) |>
#     group_by(neighbourhood_number) |>
#     dplyr::tally() |>
#     dplyr::rename(vaccination_sites=n) |>
#     dplyr::mutate(neighbourhood_number=as.integer(neighbourhood_number))
# 
# # Toronto - table of neighbourhood crime rates - this gives us the population and maybe a crude indication of QOL
# toronto_crime_rates <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/4754baf5-3715-4166-a347-eda9813521de?format=csv&projection=4326") |>
#     dplyr::select(-"_id", -geometry, -OBJECTID) |>
#     dplyr::select(contains("2020") | !contains("20")) |>
#     dplyr::rename(
#         neighbourhood_name = Neighbourhood,
#         neighbourhood_number = Hood_ID
#     )
# 
# 
# ##########################################################
# 
# # SURVEY VALUES
# {
#     average_values <- data.table(attribute=as.character(), value=as.character(), wave_1_report_delay=as.character(), wave_2_report_delay=as.character(), total_report_delay=as.character())
#     # AGE
#     for(age_band in unique(raw_toronto_fsa_data$age_group))
#     {
#         average_values <- rbind(average_values, list(
#             attribute = "age", value = age_band,
#             wave_1_report_delay = raw_toronto_fsa_data[(age_group == age_band) & (wave == 1), mean(reporting_delay)],
#             wave_2_report_delay = raw_toronto_fsa_data[(age_group == age_band) & (wave == 2), mean(reporting_delay)],
#             total_report_delay = raw_toronto_fsa_data[(age_group == age_band), mean(reporting_delay)]
#         ))
#     }
#     # SEX
#     for(sex in unique(raw_toronto_fsa_data$client_gender))
#     {
#         average_values <- rbind(
#             average_values,
#             list(
#                 attribute = "sex", value = sex,
#                 wave_1_report_delay = raw_toronto_fsa_data[(client_gender == sex) & (wave == 1), mean(reporting_delay)],
#                 wave_2_report_delay = raw_toronto_fsa_data[(client_gender == sex) & (wave == 2), mean(reporting_delay)],
#                 total_report_delay = raw_toronto_fsa_data[(client_gender == sex), mean(reporting_delay)]
#             )
#         )
#     }
#     # NEIGHBOURHOOD
#     for(neighbourhood in unique(raw_toronto_fsa_data$neighbourhood_name))
#     {
#         average_values <- rbind(
#             average_values,
#             list(
#                 attribute = "neighbourhood", value = neighbourhood,
#                 wave_1_report_delay = raw_toronto_fsa_data[(neighbourhood_name == neighbourhood) & (wave == 1), mean(reporting_delay)],
#                 wave_2_report_delay = raw_toronto_fsa_data[(neighbourhood_name == neighbourhood) & (wave == 2), mean(reporting_delay)],
#                 total_report_delay = raw_toronto_fsa_data[(neighbourhood_name == neighbourhood), mean(reporting_delay)]
#             )
#         )
#     }
#     # FSA
#     for(fsa_code in unique(raw_toronto_fsa_data$fsa))
#     {
#         average_values <- rbind(
#             average_values,
#             list(
#                 attribute = "fsa", value = fsa_code,
#                 wave_1_report_delay = raw_toronto_fsa_data[(fsa == fsa_code) & (wave == 1), mean(reporting_delay)],
#                 wave_2_report_delay = raw_toronto_fsa_data[(fsa == fsa_code) & (wave == 2), mean(reporting_delay)],
#                 total_report_delay = raw_toronto_fsa_data[(fsa == fsa_code), mean(reporting_delay)]
#             )
#         )
#     }
#     # find whether the wave number makes a difference
#     temp <- rbind(
#             data.table(wave=rep(1, nrow(average_values)), delay=average_values$wave_1_report_delay),
#             data.table(wave=rep(2, nrow(average_values)), delay=average_values$wave_2_report_delay)
#         ) |> dplyr::filter(!is.na(delay)) |> dplyr::mutate(wave = factor(wave), delay = round(as.numeric(delay), 2))
# 
#     wave_kruskal_test <- kruskal.test(wave ~ delay, data = temp)
# 
#     wave_2_delay_longer_than_wave_1 <- sum(average_values[attribute %in% c("age_band", "sex", "neighbourhood"), wave_1_report_delay > wave_2_report_delay] == FALSE, na.rm=T)
# 
#     # KRUSKAL TESTS
# 
#     age_kruskal <- list(
#         "1" = kruskal.test(age_group ~ reporting_delay, data=raw_toronto_fsa_data[wave==1]),
#         "2" = kruskal.test(age_group ~ reporting_delay, data=raw_toronto_fsa_data[wave==2]),
#         "both" = kruskal.test(age_group ~ reporting_delay, data=raw_toronto_fsa_data)
#     )
#     sex_kruskal <- list(
#         "1" = kruskal.test(client_gender ~ reporting_delay, data=raw_toronto_fsa_data[wave==1 & tolower(client_gender) %in% c("male", "female")]),
#         "2" = kruskal.test(client_gender ~ reporting_delay, data=raw_toronto_fsa_data[wave==2 & tolower(client_gender) %in% c("male", "female")]),
#         "both" = kruskal.test(client_gender ~ reporting_delay, data=raw_toronto_fsa_data[tolower(client_gender) %in% c("male", "female")])
#     )
#     nbd_kruskal <- list(
#         "1" = kruskal.test(neighbourhood_number ~ reporting_delay, data=raw_toronto_fsa_data[wave==1]),
#         "2" = kruskal.test(neighbourhood_number ~ reporting_delay, data=raw_toronto_fsa_data[wave==2]),
#         "both" = kruskal.test(neighbourhood_number ~ reporting_delay, data=raw_toronto_fsa_data)
#     )
#     fsa_kruskal <- list(
#         "1" = kruskal.test(fsa ~ reporting_delay, data=raw_toronto_fsa_data[wave==1]),
#         "2" = kruskal.test(fsa ~ reporting_delay, data=raw_toronto_fsa_data[wave==2]),
#         "both" = kruskal.test(fsa ~ reporting_delay, data=raw_toronto_fsa_data)
#     )
# 
#     # # KULLBACK-LEIBLER DIVERGENCES
#     # male_delays <- raw_toronto_fsa_data[client_gender=="MALE", reporting_delay]
#     # female_delays <- raw_toronto_fsa_data[client_gender=="FEMALE", reporting_delay]
#     # sex_female_male_JSD <- JSD(rbind( sample(male_delays, 1000000, replace=T), sample(male_delays, 1000000, replace=T) ))
#     #
#     #
#     #
#     # male_delays_wave_1 <- raw_toronto_fsa_data[client_gender=="MALE" & wave==1, reporting_delay]
#     # female_delays_wave_1 <- raw_toronto_fsa_data[client_gender=="FEMALE" & wave==1, reporting_delay]
#     # MAT_wave_1 <- rbind(tabulate(male_delays_wave_1), tabulate(female_delays_wave_1))
#     # sex_chi_wave_1 <- chisq.test(MAT_wave_1)
#     # sex_JSD_wave_1 <- JSD(MAT_wave_1)
#     #
#     # male_delays_wave_2 <- raw_toronto_fsa_data[client_gender=="MALE" & wave==2, reporting_delay]
#     # female_delays_wave_2 <- raw_toronto_fsa_data[client_gender=="FEMALE" & wave==2, reporting_delay]
#     # MAT_wave_2 <- rbind(tabulate(male_delays_wave_2), tabulate(female_delays_wave_2))
#     # sex_chi_wave_2 <- chisq.test(MAT_wave_2[, colSums(MAT_wave_2 != 0) > 1])
#     # sex_JSD_wave_2 <- JSD(MAT_wave_2)
# 
# 
#     # sex_female_male_wave_1_JSD <- JSD(rbind( sample(male_delays_wave_1, 1000000, replace=T), sample(female_delays_wave_1, 1000000, replace=T) ))
#     #
#     # sex_female_male_wave_1_kld <- KLD(sample(male_delays_wave_1, 100000, replace=T), sample(female_delays_wave_1, 100000, replace=T))
#     #
#     # male_delays_wave_2 <- raw_toronto_fsa_data[client_gender=="MALE" & wave==2, reporting_delay]
#     # female_delays_wave_2 <- raw_toronto_fsa_data[client_gender=="FEMALE" & wave==2, reporting_delay]
#     # sex_female_male_wave_2_kld <- KLD(sample(male_delays, 1000000, replace=T), sample(male_delays, 1000000, replace=T))
#     #
#     # child_delays <-  raw_toronto_fsa_data[age_group=="19 and younger", reporting_delay]
#     # adult_delays <-  raw_toronto_fsa_data[age_group!="19 and younger", reporting_delay]
#     # age_child_adult_kld <- KLD(sample(child_delays, 1000000, replace=T), sample(adult_delays, 1000000, replace=T))
# 
# }
# 
# hospitalisation_regression <- glm(
#     formula = ever_hospitalized ~ age_group + client_gender + wave + reporting_delay,
#     data=raw_toronto_fsa_data, #[tolower(client_gender) %in% c("male", "female", "unknown")],
#     family = binomial
# )
# # odds ratio and confidence interval
# hospitalisation_regression_OR_CI <- exp(cbind("Odds ratio" = coef(hospitalisation_regression), confint.default(hospitalisation_regression, level = 0.95)))
# 
# icu_given_hospital <- glm(
#     formula = ever_in_icu ~ age_group + client_gender + wave + reporting_delay,
#     data=raw_toronto_fsa_data[client_gender != "NOT LISTED, PLEASE SPECIFY"],
#     family = binomial
# )
# icu_given_hospital_OR_CI <- exp(cbind("Odds ratio" = coef(icu_given_hospital), confint.default(icu_given_hospital, level = 0.95)))
# 
# intubated_given_icu <- glm(
#     formula = ever_intubated ~ age_group + client_gender + wave + reporting_delay,
#     data=raw_toronto_fsa_data[ever_in_icu & tolower(client_gender) %in% c("male", "female", "unknown")],
#     family = binomial
# )
# intubated_given_icu_OR_CI <- exp(cbind("Odds ratio" = coef(intubated_given_icu), confint.default(intubated_given_icu, level = 0.95)))
# 
# ###################################################################################
# 
# toronto_neighbourhood_data <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/ef0239b1-832b-4d0b-a1f3-4153e53b189e?format=csv") |>
#     dplyr::rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
#     dplyr::filter(topic %in% c(
#         "Neighbourhood Information", "Population and dwellings", "Age characteristics", "Household and dwelling characteristics",
#         "Family characteristics", "Household type", "Family characteristics of adults", "Highest certificate, diploma or degree",
#         "Commuting destination", "Visible minority population")
#     ) |>
#     dplyr::select(-`_id`, -category, -data_source, -category)
# 
# # because of the f*cking ignorant way StatCan/Toronto/whichever-intern labelled the "characteristic" row on Friday at 4:30pm, naive
# # transposition doesn't work, since some columns will have the same names, the conflict of which is avoided by data.table by appending
# # .\d to the end of the column name, so it no longer matches. so we'll build the table and the names vector at the same time.
# 
# # rows that will eventually be duplicately named will have spaces. so, store the last name without leading whitespace, and any column
# # name following that starts with a space or number will get the stored no-leading-whitespace title last seen:
# #
# # for example: the name series "Census families in private households by family size, 2 persons, 3 persons, 4 persons, 5 or more persons"
# # will become "Census families in private households by family size,
# # Census families in private households by family size - 2 persons,
# # Census families in private households by family size - 3 persons,
# # Census families in private households by family size - 4 persons,
# # Census families in private households by family size - 5 or more persons
# 
# is_number <- \(x) grepl("\\d", x)
# is_space <- \(x) grepl(" ", x)
# is_subclass <- \(x) is_number(substr(x,1,1)) | is_space(substr(x,1,1))
# 
# nbd_data <- data.table(matrix(ncol=0, nrow=ncol(toronto_neighbourhood_data |> dplyr::select(-topic, -characteristic)), NA))
# nbd_data$neighbourhood <- names(toronto_neighbourhood_data |> dplyr::select(-topic, -characteristic))
# current_category <- "a" # must seed with an actual charascter, else the algo will fail - ok to assume that the first one is a main class
# 
# for(i in 1:nrow(toronto_neighbourhood_data))
# {
#     here <- toronto_neighbourhood_data[i]
#     column <- here$characteristic
# 
#     if( is_subclass(column))
#     {
#         column <- sprintf("%s - %s", current_category, column)
#     } else {
#         current_category <- column
#     }
#     column <- column %>% gsub("-", " ", .) %>% gsub(" +|,|:", "_", .) %>% gsub("_+", "_", .) %>% tolower()
#     nbd_data[, eval(column) := parse_strings_as_numbers(unname(unlist(here |> dplyr::select(-topic, -characteristic))))]
# }
# 
# toronto_neighbourhood_data <- nbd_data
# rm(nbd_data)
# 
# 
# # The neighbourhood name mapping for the testing sites needs to be fixed
# # right now, it identifies
# # Banbury-Don Mills <-> Bridle Path-Sunnybrook-York Mills
# # Danforth <->  Daills <-> Bridle Path-Sunnybrook-York Mills
# # Danforth <->  Danforth East York
# 
# summary_fsa_data <- raw_toronto_fsa_data[,
#         lapply(.SD, sum, na.rm=T),
#         .SDcols=c("ever_hospitalized", "ever_in_icu", "num_reports", "ever_intubated"),
#         by=.(neighbourhood_name, neighbourhood_number) # ][client_gender %in% c("MALE", "FEMALE") # age_group, client_gender,
#     ]
# 
# total_data <- Reduce(
#         function(...) merge(..., all = TRUE, by = "neighbourhood_number"),
#         list(summary_fsa_data, testing_sites_by_neighbourhood, vaccination_sites_by_neighbourhood, toronto_neighbourhood_data, toronto_crime_rates)
#     ) |>
#     dplyr::mutate(
#         p_ever_hospitalised = ever_hospitalized/num_reports,
#         p_ever_in_icu = ever_in_icu/num_reports,
#         p_ever_intubated = ever_intubated/num_reports,
#         neighbourhood_number = factor(neighbourhood_number),
#         total_commuters = `commute_within_census_subdivision_(csd)_of_residence` + `commute_to_a_different_census_subdivision_(csd)_within_census_division_(cd)_of_residence` +
#             `commute_to_a_different_census_subdivision_(csd)_and_census_division_(cd)_within_province_or_territory_of_residence` + `commute_to_a_different_province_or_territory`,
#         edu_trades = `secondary_(high)_school_diploma_or_equivalency_certificate` + `trades_certificate_or_diploma_other_than_certificate_of_apprenticeship_or_certificate_of_qualification` +
#             `certificate_of_apprenticeship_or_certificate_of_qualification`,
#         edu_uni = `university_certificate_or_diploma_below_bachelor_level` + `bachelor's_degree` + `university_certificate_or_diploma_above_bachelor_level` + `earned_doctorate`,
#        `younger_seniors_(65_84_years)` = `seniors_(65+_years)` - `older_seniors_(85+_years)`,
#        total_crimes = Assault_2020 + AutoTheft_2020 + BreakAndEnter_2020 + Robbery_2020 + TheftOver_2020 + Homicide_2020 + Shootings_2020,
#        total_crime_rate = total_crimes/population_2016
#     ) |>
#     dplyr::filter(
#         !is.na(neighbourhood_number) & (neighbourhood_number != 0) # neighbourhood 0 is the stats for all Toronto
#     ) |>
#     dplyr::select(
#         -neighbourhood, -neighbourhood_name.x, -neighbourhood_name.y, -neighbourhood_name, -F2020_Population_Projection
#     )
# 
# 
# # measures_of_testing_vaccination <- c("testing_sites", "vaccination_sites")
# # measures_of_people <- c("population_2016")
# # measures_of_age <- c("children_(0_14_years)", "youth_(15_24_years)", "working_age_(25_54_years)", "pre_retirement_(55_64_years)", "seniors_(65+_years)", "older_seniors_(85+_years)")
# # measures_of_household_composition <- c("private_households_by_household_size_1_person", "private_households_by_household_size_2_persons", "private_households_by_household_size_3_persons", "private_households_by_household_size_4_persons", "private_households_by_household_size_5_or_more_persons")
# #
# # measures_of_race <- c("total_visible_minority_population")
# #
# # measures_of_education <- c(
# #     # "apprenticeship_or_trades_certificate_or_diploma", "trades_certificate_or_diploma_other_than_certificate_of_apprenticeship_or_certificate_of_qualification", "certificate_of_apprenticeship_or_certificate_of_qualification", "university_certificate_or_diploma_below_bachelor_level",
# #
# #                            "bachelor's_degree", "university_certificate_or_diploma_above_bachelor_level", "earned_doctorate", "master's_degree")
# #
# # measures_of_commuters <- c("total_commuters")
# #
# # probabilitiies <- c("p_ever_hospitalised", "p_ever_in_icu", "p_ever_intubated")
# #
# #
# # regress_combinations2 <- list()
# #
# #
# # for(test_vacc in powerset(measures_of_testing_vaccination)){
# # for(pop in powerset(measures_of_people)){
# # for(ages in powerset(measures_of_age)){
# # for(household_dist in powerset(measures_of_household_composition)){
# # for(race in powerset(measures_of_race)){
# #     if(length(race) > 1) race <- race[which(race != "total_visible_minority_population")]
# # for(edu in powerset(measures_of_education)){
# # for(commuters in powerset(measures_of_commuters))
# # {
# #     regress_combinations2 <- append(regress_combinations2, list(c(test_vacc, pop, commuters)))
# #     # regress_combinations <- regress_combinations + 1
# #
# # }}}}}}}
# # regress_combinations2 <- unique(regress_combinations2)
# #
# # results2 <- data.table(command=as.character(), num_signif=as.integer(), names_signif=as.character(), remoteness_signif=logical())
# #
# # for(combo in regress_combinations){
# # for(remote in measures_of_remoteness)
# # {
# #     string <- sprintf("second_wave_proportion ~ %s", paste(c(combo, remote), collapse=" + "))
# #     regress2 <- lm(formula = as.formula(string), data = Total_Data)
# #
# #     lm(second_wave_proportion ~ couples_with_0_children + couples_with_1_child + couples_with_2_children + sgls_w_ch + people_commuting_within_province + people_commuting_within_CSD + people_commuting_within_CD_but_not_CSD + people_commuting_within_province_but_not_CD + people_commuting_outside_province + aR_score, data=Total_Data)
# #
# #     p_vals <- summary(regress)$coefficients[, 4]
# #
# #     results2 <- rbind(results2, list(
# #         command = string,
# #         num_signif = length(which(p_vals <= 0.05)),
# #         remoteness_signif = remote %in% names(which(p_vals <= 0.05)),
# #         names_signif = paste0(names(which(p_vals <= 0.05)), collapse=",")
# #     ))
# # }}
# 
# summary_data <- raw_toronto_fsa_data[
#     classification=="CONFIRMED",
#     lapply(.SD, sum, na.rm=T),
#     .SDcols=c("ever_hospitalized", "ever_in_icu", "ever_intubated", "num_reports"),
#     b=.(age_group, neighbourhood_number, client_gender)
# ] |>
#     dplyr::mutate(
#         proportion_ever_hospitalised=ever_hospitalized/num_reports,
#         proportion_ever_in_icu=ever_in_icu/num_reports,
#         proportion_ever_intubated=ever_intubated/num_reports,
#     )
# 
# total_hospitalised <- lm(
#     p_ever_hospitalised ~
#         # `vaccination_sites` +
#         # `testing_sites` +
#         total_crime_rate +
#         # `population_2016` +
#         `children_(0_14_years)` +
#         `youth_(15_24_years)` +
#         `working_age_(25_54_years)` +
#         `pre_retirement_(55_64_years)` +
#         `younger_seniors_(65_84_years)` +
#         `older_seniors_(85+_years)` +
#         `private_households_by_household_size_1_person` +
#         `private_households_by_household_size_2_persons` +
#         `private_households_by_household_size_3_persons` +
#         `private_households_by_household_size_4_persons` +
#         `private_households_by_household_size_5_or_more_persons` +
#         `total_visible_minority_population`, # +
#         # `apprenticeship_or_trades_certificate_or_diploma` +
#         # `edu_trades`+
#         # `edu_uni` +
#         # `trades_certificate_or_diploma_other_than_certificate_of_apprenticeship_or_certificate_of_qualification` +
#         # `certificate_of_apprenticeship_or_certificate_of_qualification` +
#         # `university_certificate_or_diploma_below_bachelor_level` +
#         # `bachelor's_degree` +
#         # `university_certificate_or_diploma_above_bachelor_level` +
#         # `earned_doctorate`, # +
#         # `total_commuters`,
#     data = total_data
# )
# 
# total_icu <- lm(
#     p_ever_in_icu ~
#         # `vaccination_sites` +
#         # `testing_sites` +
#         # `population_2016` +
#         `children_(0_14_years)` +
#         `youth_(15_24_years)` +
#         `working_age_(25_54_years)` +
#         `pre_retirement_(55_64_years)` +
#         `younger_seniors_(65_84_years)` +
#         `older_seniors_(85+_years)` +
#         `private_households_by_household_size_1_person` +
#         `private_households_by_household_size_2_persons` +
#         `private_households_by_household_size_3_persons` +
#         `private_households_by_household_size_4_persons` +
#         `private_households_by_household_size_5_or_more_persons` +
#         `total_visible_minority_population`, # +
#         # `apprenticeship_or_trades_certificate_or_diploma` +
#         # `edu_trades`+
#         # `edu_uni` +
#         # `trades_certificate_or_diploma_other_than_certificate_of_apprenticeship_or_certificate_of_qualification` +
#         # `certificate_of_apprenticeship_or_certificate_of_qualification` +
#         # `university_certificate_or_diploma_below_bachelor_level` +
#         # `bachelor's_degree` +
#         # `university_certificate_or_diploma_above_bachelor_level` +
#         # `earned_doctorate`, # +
#         # `total_commuters`,
#     data = total_data
# )
# 
# total_intubation <- lm(
#     p_ever_intubated ~ `vaccination_sites` +
#         `testing_sites` +
#         # `population_2016` +
#         `children_(0_14_years)` +
#         `youth_(15_24_years)` +
#         `working_age_(25_54_years)` +
#         `pre_retirement_(55_64_years)` +
#         `younger_seniors_(65_84_years)` +
#         `older_seniors_(85+_years)` +
#         `private_households_by_household_size_1_person` +
#         `private_households_by_household_size_2_persons` +
#         `private_households_by_household_size_3_persons` +
#         `private_households_by_household_size_4_persons` +
#         `private_households_by_household_size_5_or_more_persons`, # +
#         # `total_visible_minority_population` +
#         # `apprenticeship_or_trades_certificate_or_diploma` +
#         # `edu_trades`+
#         # `edu_uni` +
#         # `trades_certificate_or_diploma_other_than_certificate_of_apprenticeship_or_certificate_of_qualification` +
#         # `certificate_of_apprenticeship_or_certificate_of_qualification` +
#         # `university_certificate_or_diploma_below_bachelor_level` +
#         # `bachelor's_degree` +
#         # `university_certificate_or_diploma_above_bachelor_level` +
#         # `earned_doctorate`, # +
#         # `total_commuters`,
#     data = total_data
# )
# 
# 
# 
# # Canada_Line_List <- fread("canada_complete_COVID19_line_list.csv") |>
# #     dplyr::filter(record_type=="Infection") |>
# #     dplyr::mutate(date_report=as.Date(date_report, format="%d-%m-%Y")) |>
# #     dplyr::group_by(sex, health_region, province, date_report) |>
# #     dplyr::tally() |>
# #     dplyr::rename(num_cases=n)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 





















# library(reticulate)

################################## KARLEN FILES

# karlen_cases <- fread("https://github.com/pypm/data/raw/master/covid19/Canada/ca-pypm.csv")
# karlen_vaxes <- fread("https://github.com/pypm/data/raw/master/covid19/Canada/ca-vacc-pypm.csv")
# 
# fwrite(karlen_cases, "ca-pypm.csv")
# fwrite(karlen_vaxes, "ca-vacc-pypm.csv")
# 
# source_python("karlen_generate_data_headers.py")
# 
# haha <- lapply(unlist(get_data_description(), recursive = TRUE), `[[`, 1)
# haha <- haha[grepl("header", names(haha))]
# haha <- as.data.table(haha)
# 
# naming_conventions <- data.table(
#     description = names(haha) %>% gsub("regional.data.|.header", "", .) %>% gsub(" ", "_", .),
#     header = as.character(unname(haha))
# )
# 
# karlen_rename_long_name <-  \(x) if(x %in% naming_conventions$header) naming_conventions[header==x, description] else x
# 
# names(karlen_cases) <- unlist(lapply(names(karlen_cases), karlen_rename_long_name))
# names(karlen_vaxes) <- unlist(lapply(names(karlen_vaxes), karlen_rename_long_name))
# 
# fwrite(karlen_cases, "CaseDataTables/Karlen_Case_Data.csv")
# fwrite(karlen_vaxes, "CaseDataTables/Karlen_Vacc_Data.csv")


################# CANADA BRUTE FORCE REGRESSIONS

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
