rm(list=ls())

library(data.table)
library(ggplot2)
library(viridis)
library(CanCovidData)
library(stringr)
library(forcats)
library(sf)
library(stringdist)
library(MASS)
library(tidyr)
library(gsubfn)
library(xtable)

setwd('/home/bren/Documents/GitHub/MIZ_project/')
dir.create(file.path(".", "Graphs"), showWarnings=FALSE)

source("function_header.R")

# fwrite(fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/ef0239b1-832b-4d0b-a1f3-4153e53b189e?format=csv"), "temp_toronto_nbd_profile.csv")

# lookup table to get the neighbourhood number based on the name
# the name "Pellam" is misspelt in the file as "Pelham"
toronto_nbd_name_number_lookup <- fread("grep -v '^#' Classifications/toronto_neighbourhood_name_number_correspondence_file.csv") |>
    dplyr::mutate(look = NeighbourhoodName %>% tolower() %>% gsub("-|\\(|\\)|\\/", " ", .) %>% gsub("'|st\\.| st ", "", .) %>%
                      gsub(" +", " ", .) %>% trimws() %>% gsub("pelham", "pellam", .))

get_toronto_nbd_number <- function(x)
{
    if(x == "") return(-1)
    clean_name <- x %>% tolower() %>% gsub("-|\\(|\\)|\\/", " ", .) %>% gsub("'", "", .) %>% gsub("'|st\\.| st ", "", .) %>% gsub(" +", " ", .) %>% trimws()
    name_matches <- str_extract(toronto_nbd_name_number_lookup$look, clean_name)
    if( all(is.na(name_matches)) ) return(-1)
    prospectives <- toronto_nbd_name_number_lookup[which(!is.na(name_matches)), look]
    if(length(prospectives) == 1){
        the_chosen_one <- prospectives
    } else {
        the_chosen_one <- prospectives[stringdist::amatch(clean_name, prospectives)]
    }
    return(toronto_nbd_name_number_lookup[look == the_chosen_one, get("Hood#")])
}

parse_strings_as_numbers <- function(theList)
{
    unlist(lapply(
        theList,
        \(x)
        {
            if(x == "") return (0);
            is_percentage <- grepl("\\%", x); x <- gsub(',|%', '', x);
            return(as.numeric(x)/(if(is_percentage) 100 else 1))
        }
    ))
}

start_of_the_second_wave <- add_wave_numbers(fread("CaseDataTables/ON_cases.csv") |> group_by(date) |> tally(cases), case_col="n", date_col="date")$date

if(! file.exists("CaseDataFiles/raw_toronto_fsa_data.csv"))
{
    writeLines("\nfetching Toronto FSA data")
    fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv") |>
        dplyr::rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
        dplyr::select(-`_id`, -assigned_id, -outbreak_associated, -source_of_infection, -outcome) |>
        dplyr::mutate(
            currently_hospitalized = (currently_hospitalized == "Yes"),
            currently_in_icu = (currently_in_icu == "Yes"),
            currently_intubated = (currently_intubated == "Yes"),
            ever_hospitalized = (ever_hospitalized == "Yes"),
            ever_in_icu = (ever_in_icu == "Yes"),
            ever_intubated  = (ever_intubated == "Yes"),
            reporting_delay = as.numeric(reported_date-episode_date, units="days"),
            lower_age = unlist(lapply(age_group, age_range_min)),
            upper_age = unlist(lapply(age_group, age_range_max)),
            neighbourhood_number = unlist(lapply(neighbourhood_name, get_toronto_nbd_number)),
            wave = unlist(lapply(episode_date, \(x) if(x < start_of_the_second_wave) 1 else 2)),
            num_reports = 1,
            age_group = factor(age_group), neighbourhood_name=factor(neighbourhood_name), fsa=factor(fsa), client_gender=factor(client_gender),
            neighbourhood_number=factor(neighbourhood_number)
        ) |>
        dplyr::filter(reporting_delay >= 0 & reporting_delay <= 60) |>
        dplyr::filter(classification == "CONFIRMED") |>
        fwrite("CaseDataFiles/raw_toronto_fsa_data.csv")
}

writeLines("\nfixing Toronto data")
not_cis <- \(x) if(x %in% c("MALE", "FEMALE")) return(as.character(x)) else return("NONCIS")
raw_toronto_fsa_data <- fread("CaseDataFiles/raw_toronto_fsa_data.csv") |>
    dplyr::filter(neighbourhood_number!=-1 & age_group!="") |>
    dplyr::mutate(
        age_group = as.factor(age_group),
        neighbourhood_name = as.factor(neighbourhood_name),
        fsa = as.factor(fsa),
        client_gender = as.factor(client_gender),
        classification = as.factor(classification),
        wave = factor(wave),
        client_gender = unlist(lapply(FUN=not_cis, client_gender))
    )

# writeLines("\nToronto testing site by neighbourhood")
# testing_sites_by_neighbourhood <- fread("grep -v '^#' Classifications/toronto_testing_centres.csv") |>
#     dplyr::select(fsa, neighbourhood_name, neighbourhood_number) |>
#     dplyr::mutate(N=1) |>
#     dplyr::group_by(neighbourhood_name, neighbourhood_number) |>
#     dplyr::tally() |>
#     dplyr::rename(testing_sites=n) |>
#     as.data.table()

# writeLines("\nToronto vaccination sites by neighbourhood")
# vaccination_sites_by_neighbourhood <- fread("grep -v '^#' Classifications/covid-19-immunization-clinics.csv") |>
#     dplyr::mutate(neighbourhood_number=city) |>
#     group_by(neighbourhood_number) |>
#     dplyr::tally() |>
#     dplyr::rename(vaccination_sites=n) |>
#     dplyr::mutate(neighbourhood_number=as.integer(neighbourhood_number))

writeLines("\nToronto crime rates")
# Toronto - table of neighbourhood crime rates - this gives us the population and maybe a crude indication of QOL
toronto_crime_rates <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/4754baf5-3715-4166-a347-eda9813521de?format=csv&projection=4326") |>
    dplyr::select(-"_id", -geometry, -OBJECTID) |>
    dplyr::select(contains("2020") | !contains("20")) |>
    dplyr::rename(
        neighbourhood_name = Neighbourhood,
        neighbourhood_number = Hood_ID
    )

writeLines("\nToronto neighbourhood data")
toronto_neighbourhood_data <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/ef0239b1-832b-4d0b-a1f3-4153e53b189e?format=csv") |>
    dplyr::rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
    dplyr::filter(topic %in% c(
        "Neighbourhood Information", "Population and dwellings", "Age characteristics", "Household and dwelling characteristics",
        "Family characteristics", "Household type", "Family characteristics of adults", "Highest certificate, diploma or degree",
        "Commuting destination", "Visible minority population")
    ) |>
    dplyr::select(-`_id`, -category, -data_source, -category)

######################################## NEIGHBOURHOOD DATA

# because of the f*cking ignorant way StatCan/Toronto/whichever-intern labelled the "characteristic" row on Friday at 4:30pm, naive
# transposition doesn't work, since some columns will have the same names, the conflict of which is avoided in data.table by appending
# .\d to the end of the column name, so it no longer matches. so we'll build the table and the names vector at the same time.
#
# I assume thatrows that will eventually be duplicately named will have spaces at the beginning. so, store the last name without 
# leading whitespace, and append column name following that starts with a space or number will get the stored no-leading-whitespace 
# title last seen:
#
# for example: the name series "Census families in private households by family size, 2 persons, 3 persons, 4 persons, 5 or more persons"
# will become "Census families in private households by family size,
#       Census families in private households by family size - 2 persons,
#       Census families in private households by family size - 3 persons,
#       Census families in private households by family size - 4 persons,
#       Census families in private households by family size - 5 or more persons

is_number <- \(x) grepl("\\d", x)
is_space <- \(x) grepl(" ", x)
is_subclass <- \(x) is_number(substr(x,1,1)) | is_space(substr(x,1,1))

nbd_data <- data.table(matrix(ncol=0, nrow=ncol(toronto_neighbourhood_data |> dplyr::select(-topic, -characteristic)), NA))
nbd_data$neighbourhood <- names(toronto_neighbourhood_data |> dplyr::select(-topic, -characteristic))
current_category <- "a" # must seed with an actual character, else the algo will fail - ok to assume that the first one is a main class

for(i in 1:nrow(toronto_neighbourhood_data))
{
    here <- toronto_neighbourhood_data[i]
    column <- here$characteristic
    
    if( is_subclass(column))
    {
        column <- sprintf("%s - %s", current_category, column)
    } else {
        current_category <- column
    }
    column <- column %>% gsub("-", " ", .) %>% gsub(" +|,|:", "_", .) %>% gsub("_+", "_", .) %>% tolower()
    nbd_data[, eval(column) := parse_strings_as_numbers(unname(unlist(here |> dplyr::select(-topic, -characteristic))))]
}

toronto_neighbourhood_data <- nbd_data
rm(nbd_data)

# The neighbourhood name mapping for the testing sites needs to be fixed
# right now, it identifies
# Banbury-Don Mills <-> Bridle Path-Sunnybrook-York Mills
# Danforth <->  Daills <-> Bridle Path-Sunnybrook-York Mills
# Danforth <->  Danforth East York

summary_fsa_data <- raw_toronto_fsa_data[,
                                         lapply(.SD, sum, na.rm=T),
                                         .SDcols=c("ever_hospitalized", "ever_in_icu", "num_reports", "ever_intubated"),
                                         by=.(neighbourhood_name, neighbourhood_number)
                                         # ignoring the fsa for now - it just causes problems with data duplication
                                         # ][client_gender %in% c("MALE", "FEMALE") # age_group, client_gender,
]


writeLines("\nToronto neighbourhood geographies")
toronto_nbd_geo_field_names <- fread("Toronto_Neighbourhoods/Neighbourhoods_fields.csv")
toronto_renaming <- function(x)
{
    if(! x %in% unique(toronto_nbd_geo_field_names$field)){ return(NA) } 
    if(x == "geometry") { return(x) } 
    return(toronto_nbd_geo_field_names[field==x, name])
}
    
toronto_neighbourhood_geometries <- st_read("Toronto_Neighbourhoods/Neighbourhoods.shp") |>
    dplyr::rename_with(\(theList) unlist(lapply(theList, toronto_renaming)) |> tolower() %>% gsub("_+", "_", .)) |>
    dplyr::select(
        -`_id`, -area_id, -area_attr_id, -parent_area_id, -x, -y, -area_short_code, -shape_area,
        -area_desc, -longitude, -latitude, -classification, -classification_code, -objectid,
        -shape_length
    ) |>
    dplyr::mutate(area_name = unlist(lapply(area_name, \(x) trimws(str_split(x, '\\(')[[1]][1])))) |>
    dplyr::rename(neighbourhood_number = area_long_code)

total_data <- Reduce(
        function(...) merge(..., all = TRUE, by = "neighbourhood_number"),
        list(
            summary_fsa_data, 
            # testing_sites_by_neighbourhood, 
            # vaccination_sites_by_neighbourhood, 
            toronto_neighbourhood_data, 
            toronto_crime_rates, 
            toronto_neighbourhood_geometries
        )
    ) |>
    dplyr::mutate(
        p_ever_hospitalised = ever_hospitalized/num_reports,
        p_ever_in_icu = ever_in_icu/num_reports,
        p_ever_intubated = ever_intubated/num_reports,
        neighbourhood_number = factor(neighbourhood_number),
        total_commuters = `commute_within_census_subdivision_(csd)_of_residence` + 
            `commute_to_a_different_census_subdivision_(csd)_within_census_division_(cd)_of_residence` +
            `commute_to_a_different_census_subdivision_(csd)_and_census_division_(cd)_within_province_or_territory_of_residence` + 
            `commute_to_a_different_province_or_territory`,
        edu_trades = `secondary_(high)_school_diploma_or_equivalency_certificate` + 
            `trades_certificate_or_diploma_other_than_certificate_of_apprenticeship_or_certificate_of_qualification` +
            `certificate_of_apprenticeship_or_certificate_of_qualification`,
        edu_uni = `university_certificate_or_diploma_below_bachelor_level` + 
            `bachelor's_degree` + `earned_doctorate` +
            `university_certificate_or_diploma_above_bachelor_level`,
        `younger_seniors_(65_84_years)` = `seniors_(65+_years)` - `older_seniors_(85+_years)`,
        total_crimes = Assault_2020 + AutoTheft_2020 + BreakAndEnter_2020 + Robbery_2020 + TheftOver_2020 + Homicide_2020 + Shootings_2020,
        total_crime_rate = total_crimes/population_2016
    ) |>
    dplyr::filter(
        !is.na(neighbourhood_number) & (neighbourhood_number != 0) # neighbourhood 0 is the stats for all Toronto
    ) |>
    dplyr::select( -neighbourhood_name.x, -neighbourhood_name.y, -neighbourhood, -F2020_Population_Projection) |>
    dplyr::rename(neighbourhood_name = area_name) |>
    st_as_sf()
    saveRDS(total_data, "CaseDataFiles/Total_Toronto_Data.rda")

summary_data <- raw_toronto_fsa_data[
        classification == "CONFIRMED",
        lapply(.SD, sum, na.rm=T),
        .SDcols = c("ever_hospitalized", "ever_in_icu", "ever_intubated", "num_reports"),
        by = .(age_group, neighbourhood_number, client_gender)
    ] |>
    dplyr::mutate(
        proportion_ever_hospitalised=ever_hospitalized/num_reports,
        proportion_ever_in_icu=ever_in_icu/num_reports,
        proportion_ever_intubated=ever_intubated/num_reports,
    )

average_values <- data.table(attribute=as.character(), value=as.character(), wave_1_report_delay=as.character(), wave_2_report_delay=as.character(), total_report_delay=as.character())

for(age_band in unique(raw_toronto_fsa_data$age_group))
{
    average_values <- rbind(average_values, list(
        attribute = "age", value = age_band,
        wave_1_report_delay = raw_toronto_fsa_data[(age_group == age_band) & (wave == 1), mean(reporting_delay)],
        wave_2_report_delay = raw_toronto_fsa_data[(age_group == age_band) & (wave == 2), mean(reporting_delay)],
        total_report_delay = raw_toronto_fsa_data[(age_group == age_band), mean(reporting_delay)]
    ))
}
for(sex in unique(raw_toronto_fsa_data$client_gender))
{
    average_values <- rbind(
        average_values,
        list(
            attribute = "sex", value = sex,
            wave_1_report_delay = raw_toronto_fsa_data[(client_gender == sex) & (wave == 1), mean(reporting_delay)],
            wave_2_report_delay = raw_toronto_fsa_data[(client_gender == sex) & (wave == 2), mean(reporting_delay)],
            total_report_delay = raw_toronto_fsa_data[(client_gender == sex), mean(reporting_delay)]
        )
    )
}
for(neighbourhood in unique(raw_toronto_fsa_data$neighbourhood_name))
{
    average_values <- rbind(
        average_values,
        list(
            attribute = "neighbourhood", value = neighbourhood,
            wave_1_report_delay = raw_toronto_fsa_data[(neighbourhood_name == neighbourhood) & (wave == 1), mean(reporting_delay)],
            wave_2_report_delay = raw_toronto_fsa_data[(neighbourhood_name == neighbourhood) & (wave == 2), mean(reporting_delay)],
            total_report_delay = raw_toronto_fsa_data[(neighbourhood_name == neighbourhood), mean(reporting_delay)]
        )
    )
}
for(fsa_code in unique(raw_toronto_fsa_data$fsa))
{
    average_values <- rbind(
        average_values,
        list(
            attribute = "fsa", value = fsa_code,
            wave_1_report_delay = raw_toronto_fsa_data[(fsa == fsa_code) & (wave == 1), mean(reporting_delay)],
            wave_2_report_delay = raw_toronto_fsa_data[(fsa == fsa_code) & (wave == 2), mean(reporting_delay)],
            total_report_delay = raw_toronto_fsa_data[(fsa == fsa_code), mean(reporting_delay)]
        )
    )
}

age_and_sex_average_delays <- ggplot(
        average_values |>
            dplyr::rename(
                "First" = wave_1_report_delay, 
                "Second" = wave_2_report_delay, 
                "Both" = total_report_delay, 
                quant=value
            ) %>% 
            melt(., id.vars=c("quant", "attribute")) |> 
            dplyr::relocate(attribute, quant) |>
            dplyr::filter(attribute %in% c("age", "sex") & variable!="Both" & quant!="NOT LISTED, PLEASE SPECIFY") |>
            dplyr::mutate(value = as.numeric(value)),
        aes(x=stringr::str_wrap(quant, 10), y=value, fill=variable, group=quant)
    ) +
    geom_bar(stat="identity",position = "identity", na.rm=T) +
    theme_bw() +
    theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle=45, vjust=0.5),
        legend.position = "top" # "right"
    ) +
    labs(x="Group", y="Average Reporting Delay") +
    # scale_fill_viridis_d() +
    facet_grid(.~attribute, scales = "free_x") +
    guides(fill = guide_legend(title="Wave"))
    ggsave(age_and_sex_average_delays, file="Graphs/age_and_sex_average_days.png", height=3.5, width=10)
    system(sprintf("convert %s -trim %s", "Graphs/age_and_sex_average_days.png", "Graphs/age_and_sex_average_days.png"))

{ # KRUSKAL TESTS
    # find whether the wave number makes a difference
    temp <- rbind(
            data.table(wave=rep(1, nrow(average_values)), delay=average_values$wave_1_report_delay),
            data.table(wave=rep(2, nrow(average_values)), delay=average_values$wave_2_report_delay)
        ) |> dplyr::filter(!is.na(delay)) |> dplyr::mutate(wave = factor(wave), delay = round(as.numeric(delay), 2))

    wave_kruskal_test <- kruskal.test(wave ~ delay, data = temp)

    wave_2_delay_longer_than_wave_1 <- sum(
        average_values[attribute %in% c("age_band", "sex", "neighbourhood")][wave_1_report_delay > wave_2_report_delay] == FALSE, 
        na.rm=T
    )

    age_kruskal <- list(
        "1" = kruskal.test(age_group ~ reporting_delay, data=raw_toronto_fsa_data[wave==1]),
        "2" = kruskal.test(age_group ~ reporting_delay, data=raw_toronto_fsa_data[wave==2]),
        "both" = kruskal.test(age_group ~ reporting_delay, data=raw_toronto_fsa_data)
    )
    sex_kruskal <- list(
        "1" = kruskal.test(client_gender ~ reporting_delay, data=raw_toronto_fsa_data[wave==1 & tolower(client_gender) %in% c("male", "female")]),
        "2" = kruskal.test(client_gender ~ reporting_delay, data=raw_toronto_fsa_data[wave==2 & tolower(client_gender) %in% c("male", "female")]),
        "both" = kruskal.test(client_gender ~ reporting_delay, data=raw_toronto_fsa_data[tolower(client_gender) %in% c("male", "female")])
    )
    nbd_kruskal <- list(
        "1" = kruskal.test(neighbourhood_number ~ reporting_delay, data=raw_toronto_fsa_data[wave==1]),
        "2" = kruskal.test(neighbourhood_number ~ reporting_delay, data=raw_toronto_fsa_data[wave==2]),
        "both" = kruskal.test(neighbourhood_number ~ reporting_delay, data=raw_toronto_fsa_data)
    )
    fsa_kruskal <- list(
        "1" = kruskal.test(fsa ~ reporting_delay, data=raw_toronto_fsa_data[wave==1]),
        "2" = kruskal.test(fsa ~ reporting_delay, data=raw_toronto_fsa_data[wave==2]),
        "both" = kruskal.test(fsa ~ reporting_delay, data=raw_toronto_fsa_data)
    )
}

########################################## delay distributions plot

exp_fit <- fitdistr(raw_toronto_fsa_data[reporting_delay!=0]$reporting_delay, "exponential")
weib_fit <- fitdistr(raw_toronto_fsa_data[reporting_delay>0, reporting_delay], "weibull")
delay_distro <- ggplot(raw_toronto_fsa_data, aes(x=reporting_delay)) +
    geom_histogram(binwidth=1, aes(y=..density..), colour="black", fill="grey") +
    theme_bw() +
    theme(
        axis.text = element_text(size=12), 
        axis.title=element_text(size=13)
    ) +
    labs(x="Reporting Delay", y="Density") +
    stat_function(fun=dweibull, args=list(shape=weib_fit$estimate[["shape"]], scale=weib_fit$estimate[["scale"]]), col="blue", size=1) +
    stat_function(fun=dexp, args=(mean=exp_fit$estimate[["rate"]]), col="red", size=1) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))
    ggsave(delay_distro, file="Graphs/entire_delay_distribution_plot.jpg", width=10, height=3)
    system(sprintf("convert %s -trim %s", "Graphs/entire_delay_distribution_plot.jpg", "Graphs/entire_delay_distribution_plot.jpg"))

#################################### toronto area linear regressions

prtty_print_logit_table <- function(logit_regress, raw = FALSE)
{
    OR_CI <- exp(cbind("odds_ratio" = coef(logit_regress), confint.default(logit_regress, level = 0.95)))
    
    table_here <- merge(
            data.table(Parameters=names(logit_regress$coefficients), summary(logit_regress)$coefficients),
            data.table(Parameters=row.names(OR_CI), OR_CI),
            by="Parameters"
        )
    
    if(raw) return(table_here)

    table_here <- table_here |>
        dplyr::select(-`Std. Error`, -`z value`) %>%
        dplyr::mutate(
            Parameters = unlist(lapply(
                Parameters,
                \(x) gsubfn(
                    "age_group|client_gender|reporting_delay|wave2|MALE|NONCIS",
                    list("age_group"="Age: ", "client_gender"="Sex: ", "reporting_delay"="Reporting Delay", "wave2"="Wave 2", "MALE"="Male", "NONCIS"="Other"),
                    x
                )
            )),
            or_95_ci = sprintf(" %.3f (%.3f, %.3f)", .$odds_ratio, .$`2.5 %`, .$`97.5 %`),
            Estimate = sprintf("%.3f", Estimate),
            p_value = sprintf("%.3f", `Pr(>|z|)`),
        ) |>
        dplyr::select(-`odds_ratio`, -`2.5 %`, -`97.5 %`, -`Pr(>|z|)`) |>
        dplyr::relocate(Parameters, Estimate, or_95_ci) |>
        dplyr::rename(
            "beta i" = Estimate,
            "p value" = p_value,
            "OR (95 CI)" = or_95_ci
        )
    
    if(raw) return(table_here)
    return(print(xtable(table_here), include.rownames=F))
}

hospitalisation_regression <- glm(
    formula = ever_hospitalized ~ age_group + client_gender + wave + reporting_delay, # + neighbourhood_number
    data=raw_toronto_fsa_data |> dplyr::mutate(neighbourhood_number = factor(neighbourhood_number)),
    family = binomial
)
hosp_table <- prtty_print_logit_table(hospitalisation_regression)

icu_given_hospital <- glm(
    formula = ever_in_icu ~ age_group + client_gender + wave + reporting_delay,
    data=raw_toronto_fsa_data,
    family = binomial
)
icu_table <- prtty_print_logit_table(icu_given_hospital)

intubated_given_icu <- glm(
    formula = ever_intubated ~ age_group + client_gender + wave + reporting_delay,
    data=raw_toronto_fsa_data[ever_in_icu == T],
    family = binomial
)
intubation_table <- prtty_print_logit_table(intubated_given_icu)

############################### logit on the neighbourhoods

hospitalisation_nbd_regression <- glm(
    formula = ever_hospitalized ~ age_group + client_gender + wave + reporting_delay + neighbourhood_number,
    data=raw_toronto_fsa_data |> dplyr::mutate(neighbourhood_number = factor(neighbourhood_number)),
    family = binomial
)

nbds_odds_ratios <- prtty_print_logit_table(hospitalisation_nbd_regression, TRUE) |> 
    dplyr::filter(grepl("neighbourhood", Parameters)) |>  
    dplyr::mutate(Parameters = unlist(lapply(Parameters, \(x) gsub("neighbourhood_number", "", x)))) |> 
    dplyr::mutate(Parameters = as.numeric(Parameters)) |> 
    dplyr::rename(neighbourhood_number = Parameters) |> 
    dplyr::select(neighbourhood_number, odds_ratio)

baseline_nbd <- as.numeric(setdiff(total_data$neighbourhood_number, nbds_odds_ratios$neighbourhood_number))

nbds_odds_ratios <- rbind(nbds_odds_ratios, list(neighbourhood_number=baseline_nbd, odds_ratio=1.0))

total_data <- merge(
        total_data,
        rbind(nbds_odds_ratios, list(neighbourhood_number=baseline_nbd, odds_ratio=1.0)) |> 
            dplyr::mutate(neighbourhood_number = factor(neighbourhood_number)),
        by="neighbourhood_number"
    ) |> st_as_sf()
    saveRDS(total_data, "CaseDataFiles/Total_Toronto_Data_with_odds_ratios.rda")


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
#         `total_visible_minority_population`,
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
#         `private_households_by_household_size_5_or_more_persons`,
#         # `total_visible_minority_population`, # +
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
# # melt(average_values |> dplyr::filter(attribute=="age") |> dplyr::rename('First Wave'=wave_1_report_delay, "Second Wave"=wave_2_report_delay, "Both Waves"=total_report_delay, quant=value), id.vars=c("attribute", "quant"))
    
    
    
    
    
    
    [1] "neighbourhood_number"                                                                                                                                        
    [2] "ever_hospitalized"                                                                                                                                           
    [3] "ever_in_icu"                                                                                                                                                 
    [4] "num_reports"                                                                                                                                                 
    [5] "ever_intubated"                                                                                                                                              
    [6] "tsns2020_designation"                                                                                                                                        
    [7] "population_2016"                                                                                                                                             
    [8] "population_2011"                                                                                                                                             
    [9] "population_change_2011_2016"                                                                                                                                 
    [10] "total_private_dwellings"                                                                                                                                    
    [11] "private_dwellings_occupied_by_usual_residents"                                                                                                              
    [12] "population_density_per_square_kilometre"                                                                                                                    
    [13] "land_area_in_square_kilometres"                                                                                                                             
    [14] "children_(0_14_years)"                                                                                                                                      
    [15] "youth_(15_24_years)"                                                                                                                                        
    [16] "working_age_(25_54_years)"                                                                                                                                  
    [17] "pre_retirement_(55_64_years)"                                                                                                                               
    [18] "seniors_(65+_years)"                                                                                                                                        
    [19] "older_seniors_(85+_years)"                                                                                                                                  
    [20] "male_0_to_04_years"                                                                                                                                         
    [21] "male_05_to_09_years"                                                                                                                                        
    [22] "male_10_to_14_years"                                                                                                                                        
    [23] "male_15_to_19_years"                                                                                                                                        
    [24] "male_20_to_24_years"                                                                                                                                        
    [25] "male_25_to_29_years"                                                                                                                                        
    [26] "male_30_to_34_years"                                                                                                                                        
    [27] "male_35_to_39_years"                                                                                                                                        
    [28] "male_40_to_44_years"                                                                                                                                        
    [29] "male_45_to_49_years"                                                                                                                                        
    [30] "male_50_to_54_years"                                                                                                                                        
    [31] "male_55_to_59_years"                                                                                                                                        
    [32] "male_60_to_64_years"                                                                                                                                        
    [33] "male_65_to_69_years"                                                                                                                                        
    [34] "male_70_to_74_years"                                                                                                                                        
    [35] "male_75_to_79_years"                                                                                                                                        
    [36] "female_10_to_14_years"                                                                                                                                      
    [37] "male_80_to_84_years"                                                                                                                                        
    [38] "male_85_to_89_years"                                                                                                                                        
    [39] "male_90_to_94_years"                                                                                                                                        
    [40] "male_95_to_99_years"                                                                                                                                        
    [41] "male_100_years_and_over"                                                                                                                                    
    [42] "female_0_to_04_years"                                                                                                                                       
    [43] "female_05_to_09_years"                                                                                                                                      
    [44] "female_15_to_19_years"                                                                                                                                      
    [45] "female_20_to_24_years"                                                                                                                                      
    [46] "female_25_to_29_years"                                                                                                                                      
    [47] "female_30_to_34_years"                                                                                                                                      
    [48] "female_35_to_39_years"                                                                                                                                      
    [49] "female_40_to_44_years"                                                                                                                                      
    [50] "female_45_to_49_years"                                                                                                                                      
    [51] "female_50_to_54_years"                                                                                                                                      
    [52] "female_55_to_59_years"                                                                                                                                      
    [53] "female_60_to_64_years"                                                                                                                                      
    [54] "female_65_to_69_years"                                                                                                                                      
    [55] "female_70_to_74_years"                                                                                                                                      
    [56] "female_75_to_79_years"                                                                                                                                      
    [57] "female_80_to_84_years"                                                                                                                                      
    [58] "female_85_to_89_years"                                                                                                                                      
    [59] "female_90_to_94_years"                                                                                                                                      
    [60] "female_95_to_99_years"                                                                                                                                      
    [61] "female_100_years_and_over"                                                                                                                                  
    [62] "occupied_private_dwellings_by_structural_type_of_dwelling"                                                                                                   
    [63] "single_detached_house"                                                                                                                                       
    [64] "apartment_in_a_building_that_has_five_or_more_storeys"                                                                                                       
    [65] "other_attached_dwelling"                                                                                                                                     
    [66] "semi_detached_house"                                                                                                                                         
    [67] "row_house"                                                                                                                                                   
    [68] "apartment_or_flat_in_a_duplex"                                                                                                                               
    [69] "apartment_in_a_building_that_has_fewer_than_five_storeys"                                                                                                    
    [70] "other_single_attached_house"                                                                                                                                 
    [71] "movable_dwelling"                                                                                                                                            
    [72] "private_households_by_household_size"                                                                                                                        
    [73] "private_households_by_household_size_1_person"                                                                                                               
    [74] "private_households_by_household_size_2_persons"                                                                                                              
    [75] "private_households_by_household_size_3_persons"                                                                                                              
    [76] "private_households_by_household_size_4_persons"                                                                                                              
    [77] "private_households_by_household_size_5_or_more_persons"                                                                                                      
    [78] "number_of_persons_in_private_households"                                                                                                                     
    [79] "average_household_size"                                                                                                                                      
    [80] "census_families_in_private_households_by_family_size"                                                                                                        
    [81] "census_families_in_private_households_by_family_size_2_persons"                                                                                              
    [82] "census_families_in_private_households_by_family_size_3_persons"                                                                                              
    [83] "census_families_in_private_households_by_family_size_4_persons"                                                                                              
    [84] "census_families_in_private_households_by_family_size_5_or_more_persons"                                                                                      
    [85] "couples_without_children"                                                                                                                                    
    [86] "total_number_of_census_families_in_private_households"                                                                                                       
    [87] "total_couple_families"                                                                                                                                       
    [88] "married_couples"                                                                                                                                             
    [89] "common_law_couples"                                                                                                                                          
    [90] "total_lone_parent_families_by_sex_of_parent"                                                                                                                 
    [91] "female_parent"                                                                                                                                               
    [92] "male_parent"                                                                                                                                                 
    [93] "couple_census_families_in_private_households"                                                                                                                
    [94] "couples_with_children"                                                                                                                                       
    [95] "couples_with_children_1_child"                                                                                                                               
    [96] "couples_with_children_2_children"                                                                                                                            
    [97] "couples_with_children_3_or_more_children"                                                                                                                    
    [98] "lone_parent_census_families_in_private_households"                                                                                                           
    [99] "lone_parent_census_families_in_private_households_1_child"                                                                                                   
    [100] "lone_parent_census_families_in_private_households_2_children"                                                                                                
    [101] "non_census_family_households"                                                                                                                                
    [102] "non_census_family_households_3_or_more_children"                                                                                                             
    [103] "persons_not_in_census_families_in_private_households"                                                                                                        
    [104] "private_households_by_household_type"                                                                                                                        
    [105] "one_census_family_households"                                                                                                                                
    [106] "without_children_in_a_census_family"                                                                                                                         
    [107] "with_children_in_a_census_family"                                                                                                                            
    [108] "multiple_census_family_households"                                                                                                                           
    [109] "one_person_households"                                                                                                                                       
    [110] "two_or_more_person_non_census_family_households"                                                                                                             
    [111] "total_population_(age_15+)_by_family_characteristics"                                                                                                        
    [112] "persons_living_alone_(total)"                                                                                                                                
    [113] "persons_living_alone_(per_cent)"                                                                                                                             
    [114] "population_age_65+_by_family_characteristics"                                                                                                                
    [115] "persons_age_65+_living_alone_(total)"                                                                                                                        
    [116] "persons_age_65+_living_alone_(per_cent)"                                                                                                                     
    [117] "population_age_85+_by_family_characteristics"                                                                                                                
    [118] "persons_age_85+_living_alone_(total)"                                                                                                                        
    [119] "persons_age_85+_living_alone_(per_cent)"                                                                                                                     
    [120] "population_age_20_34_by_family_characteristics"                                                                                                              
    [121] "single_young_adults_(age_20_34)_living_at_home_(total)"                                                                                                      
    [122] "single_young_adults_(age_20_34)_living_at_home_(per_cent)"                                                                                                   
    [123] "latin_american"                                                                                                                                              
    [124] "master's_degree"                                                                                                                                             
    [125] "total_visible_minority_for_the_population_in_private_households_25%_sample_data"                                                                             
    [126] "total_visible_minority_population"                                                                                                                           
    [127] "south_asian"                                                                                                                                                 
    [128] "chinese"                                                                                                                                                     
    [129] "black"                                                                                                                                                       
    [130] "filipino"                                                                                                                                                    
    [131] "arab"                                                                                                                                                        
    [132] "southeast_asian"                                                                                                                                             
    [133] "west_asian"                                                                                                                                                  
    [134] "korean"                                                                                                                                                      
    [135] "japanese"                                                                                                                                                    
    [136] "visible_minority;_n.i.e."                                                                                                                                    
    [137] "multiple_visible_minorities"                                                                                                                                 
    [138] "not_a_visible_minority"                                                                                                                                      
    [139] "not_a_visible_minority_postsecondary_certificate_diploma_or_degree"                                                                                          
    [140] "apprenticeship_or_trades_certificate_or_diploma"                                                                                                             
    [141] "total_highest_certificate_diploma_or_degree_for_the_population_aged_15_years_and_over_in_private_households_25%_sample_data"                                 
    [142] "total_highest_certificate_diploma_or_degree_for_the_population_aged_15_years_and_over_in_private_households_25%_sample_data_no_certificate_diploma_or_degree"
    [143] "secondary_(high)_school_diploma_or_equivalency_certificate"                                                                                              
    [144] "trades_certificate_or_diploma_other_than_certificate_of_apprenticeship_or_certificate_of_qualification"                                                  
    [145] "certificate_of_apprenticeship_or_certificate_of_qualification"                                                                                           
    [146] "certificate_of_apprenticeship_or_certificate_of_qualification_college_cegep_or_other_non_university_certificate_or_diploma"                              
    [147] "university_certificate_or_diploma_below_bachelor_level"                                                                                                  
    [148] "university_certificate_or_diploma_below_bachelor_level_university_certificate_diploma_or_degree_at_bachelor_level_or_above"                              
    [149] "bachelor's_degree"                                                                                                                                       
    [150] "university_certificate_or_diploma_above_bachelor_level"                                                                                                  
    [151] "university_certificate_or_diploma_above_bachelor_level_degree_in_medicine_dentistry_veterinary_medicine_or_optometry"                                    
    [152] "earned_doctorate"                                                                                                                                        
    [153] "total_highest_certificate_diploma_or_degree_for_the_population_aged_25_to_64_years_in_private_households_25%_sample_data"                                
    [154] "total_highest_certificate_diploma_or_degree_for_the_population_aged_25_to_64_years_in_private_households_25%_sample_data_no_certificate_diploma_or_degree" 
    [155] "secondary_(high)_school_diploma_or_equivalency_certificate_postsecondary_certificate_diploma_or_degree"                                                  
    [156] "total_commuting_destination_for_the_employed_labour_force_aged_15_years_and_over_in_private_households_with_a_usual_place_of_work_25%_sample_data"       
    [157] "commute_within_census_subdivision_(csd)_of_residence"                                                                                                    
    [158] "commute_to_a_different_census_subdivision_(csd)_within_census_division_(cd)_of_residence"                                                                
    [159] "commute_to_a_different_census_subdivision_(csd)_and_census_division_(cd)_within_province_or_territory_of_residence"                                      
    [160] "commute_to_a_different_province_or_territory"                                                                                                            
    [161] "Assault_2020"                                                                                                                                            
    [162] "Assault_Rate2020"                                                                                                                                        
    [163] "AutoTheft_2020"                                                                                                                                          
    [164] "AutoTheft_Rate2020"                                                                                                                                      
    [165] "BreakAndEnter_2020"                                                                                                                                      
    [166] "BreakAndEnter_Rate2020"                                                                                                                                  
    [167] "Robbery_2020"                                                                                                                                            
    [168] "Robbery_Rate2020"                                                                                                                                        
    [169] "TheftOver_2020"                                                                                                                                          
    [170] "TheftOver_Rate2020"                                                                                                                                      
    [171] "Homicide_2020"                                                                                                                                           
    [172] "Homicide_Rate2020"                                                                                                                                       
    [173] "Shootings_2020"                                                                                                                                          
    [174] "Shootings_Rate2020"                                                                                                                                      
    [175] "neighbourhood_name"                                                                                                                                      
    [176] "p_ever_hospitalised"                                                                                                                                     
    [177] "p_ever_in_icu"                                                                                                                                           
    [178] "p_ever_intubated"                                                                                                                                        
    [179] "total_commuters"                                                                                                                                         
    [180] "edu_trades"                                                                                                                                              
    [181] "edu_uni"                                                                                                                                                 
    [182] "younger_seniors_(65_84_years)"                                                                                                                           
    [183] "total_crimes"                                                                                                                                            
    [184] "total_crime_rate"                                                                                                                                        
    [185] "odds_ratio"                                                                                                                                              
    [186] "geometry"   
    
    
    
    
    
    
    
    
    
    
    
    
    
    