library(data.table)
library(dplyr)
library(CanCovidData)
library(ggplot2)
library(rightTruncation)
library(fitdistrplus)

PROJECT_FOLDER <- "~/Documents/GitHub/MIZ_project/"
setwd(PROJECT_FOLDER)

# canada_complete_line_list <- rbind(
# 
#     rbind(
#         fread("https://github.com/ccodwg/Covid19Canada/raw/master/retired_datasets/individual_level/cases_2020.csv"),
#         fread("https://github.com/ccodwg/Covid19Canada/raw/master/retired_datasets/individual_level/cases_2021_1.csv"),
#         fread("https://github.com/ccodwg/Covid19Canada/raw/master/retired_datasets/individual_level/cases_2021_2.csv")
#     ) |> mutate(record_type="Infection"),
# 
#     rbind(
#         fread("https://github.com/ccodwg/Covid19Canada/raw/master/retired_datasets/individual_level/mortality_2020.csv"),
#         fread("https://github.com/ccodwg/Covid19Canada/raw/master/retired_datasets/individual_level/mortality_2021.csv")
#     ) |> mutate(record_type="Death"),
# 
#     fill=TRUE
# 
# ) |> relocate(record_type) |> write.csv("canada_complete_COVID19_line_list.csv")

canada_complete_line_list <- fread("canada_complete_COVID19_line_list.csv")

raw_toronto_fsa_data <- get_toronto_neighbourhood_cases()
toronto_line_list <- raw_toronto_fsa_data |>
    # select(-"_id") |>
    rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
    mutate(
        reporting_delay = as.numeric(reported_date-episode_date, units="days"),
        lower_age = unlist(lapply(age_group, age_range_min)),
        upper_age = unlist(lapply(age_group, age_range_max)),
        currently_hospitalized = (currently_hospitalized == "Yes"),
        currently_in_icu = (currently_in_icu == "Yes"),
        currently_intubated = (currently_intubated == "Yes"),
        ever_hospitalized = (ever_hospitalized == "Yes"),
        ever_in_icu = (ever_in_icu == "Yes"),
        ever_intubated  = (ever_intubated == "Yes"),
        num_case = TRUE,
        episode_weekday = weekdays(episode_date),
        reported_weekday = weekdays(reported_date),
        episode_on_weekend = unlist(lapply(episode_weekday, \(x) if(x %in% c("Saturday", "Sunday")) "YES" else "NO"))
    ) |>
    filter(reporting_delay >= 0 & reporting_delay <= 60) |>
    data.table()

######################### PLOT THE DELAY DISTRIBUTIONS

# doing negative Exponential, untruncated Weibull and right-truncated Weibull fits for each distrbution: total, weekdays and weekends
# I want to see how the reporting distrutions change for weekend episodes and whether they need to be discounted

parameter_table <- data.table( 
    attribute=character(),
    value=character(),
    negexp_rate=numeric(), 
    plain_weib_shape=numeric(), 
    plain_weib_scale=numeric() #, 
    # trunc_weib_shape=numeric(),
    # trunk_weib_scale=numeric()
)

for(vari in c("episode_on_weekend", "client_gender", "lower_age", "fsa"))
{
    choices <- na.omit(unique(toronto_line_list[[vari]]))
    
    for(case in choices)
    {
        here_table <- toronto_line_list[get(vari)==case]
        
        print(sprintf("%s: %s", vari, case))
        
        exp_fit <- fitdistr(here_table$reporting_delay, "exponential")
        plain_weib_fit <- fitdistr(here_table[reporting_delay>0, reporting_delay], "weibull")
        ggplot(here_table, aes(x=reporting_delay)) +
            geom_histogram(binwidth=1, aes(y=..density..), colour="black", fill="grey") +
            theme_bw() + 
            stat_function(
                fun=dweibull, 
                args=list(
                    shape=plain_weib_fit$estimate[["shape"]],
                    scale=plain_weib_fit$estimate[["scale"]]
                ), 
                col="red", size=1
            ) + 
            stat_function(
                fun=dexp,
                args=(mean=exp_fit$estimate[["rate"]]), 
                col="black", 
                size=1
            ) +
            labs(
                x = "Reporting delay (days)", 
                y = "Density",
                title = sprintf(
                    "NegExp (rate=%f), untruncated Weibull (shape=%f, scale=%f)", #  truncated Weibull (shape=%f, scale=%f)", 
                    exp_fit$estimate[["rate"]], plain_weib_fit$estimate[["shape"]], plain_weib_fit$estimate[["scale"]] # , MLE_res$estimate[1], MLE_res$estimate[2]
                )
            ) +
            ggsave(sprintf("Graphs/Delay_Distros/toronto_line_list_reporting_distribution_%s_%s.png", vari, case), width=13)

        parameter_table <- rbind(
            parameter_table,
            list(
                value = case,
                attribute=vari,
                negexp_rate = exp_fit$estimate[["rate"]],
                plain_weib_shape = plain_weib_fit$estimate[["shape"]],
                plain_weib_scale = plain_weib_fit$estimate[["scale"]]
            )
        )
        
        # stop()
    }
}

write.csv(parameter_table, "delay_distro_fits.csv", row.names=F)

#########################################################################

# toronto_testing <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/308efc49-5428-439a-b504-e327ab632c4f?format=csv&projection=4326")
toronto_testing_sites <- toronto_testing |>
    select(locationName, locationType, dates, sameDayAppt, active, PHU, dates, hours) |>
    mutate(sameDayAppt = sameDayAppt == "Y") |>
    mutate(active = active == "Y") |>
    filter(active)
