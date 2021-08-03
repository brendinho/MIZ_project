rm(list = ls())

library(nimble)
library(ggplot2)
library(data.table)
library(dplyr)
library(fitdistrplus)
# library(rightTruncation)
library(stringr)
library(segmented)
library(lubridate)
library(XLConnect)
library(directlabels)

PROJECT_FOLDER <- "~/Documents/GitHub/MIZ_project/"
MAX_REPORTING_DELAY <- 60

source(sprintf("%s/NOW_function_header.R", PROJECT_FOLDER))


intervention_dates <- readWorksheetFromFile(
        # "https://www.cihi.ca/sites/default/files/document/covid-19-intervention-timeline-in-canada-en.xlsx",
        sprintf("%s/%s", PROJECT_FOLDER, "Classifications/covid-19-intervention-timeline-in-canada-en.xlsx"),
        sheet="Tool interventions", 
        startRow=3, 
        endCol=8
    ) |> 
    dplyr::select(-Entry.ID, -Organization) |> 
    dplyr::filter(
        Jurisdiction. == "Ont." &
            grepl("closures|openings|distancing|travel", tolower(Intervention.Category))
    ) |>
    dplyr::mutate(
        Action = as.character(Action),
        Weekend.Effective.Date = lubridate::ceiling_date(Start.date, "week")
    ) |>
    data.table()

back_calculated_ON_case_data <- fread("Summ.csv")[var=="I"]

ON_case_data <- fread("CaseDataTables/ON_cases.csv")[, .(cases=sum(cases, na.rm=TRUE)), by=.(date, wave)]
karlen_case_data <- fread("CaseDataTables/Karlen_Case_Data.csv")
karlen_vacc_data <- fread("CaseDataTables/Karlen_Vacc_Data.csv")

ON_vacc_official <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=avaccine")$avaccine |> 
    dplyr::filter(province == "Ontario") |> 
    dplyr::rename(date=date_vaccine_administered, vaccines=avaccine) |> 
    dplyr::mutate(log_vaccines = log(vaccines)) |> 
    dplyr::select(-cumulative_avaccine, -province) |>
    dplyr::mutate(date=as.Date(date, format="%d-%m-%Y"), index=as.numeric(date)) |>
    dplyr::filter(vaccines != 0)

# first_date <- min(ON_case_data$date, ON_vacc_official$date, as.Date(intervention_dates$Weekend.Effective.Date), karlen_case_data$date, karlen_vacc_data$date)
# last_date <- max(ON_case_data$date, ON_vacc_official$date, as.Date(intervention_dates$Weekend.Effective.Date), karlen_case_data$date, karlen_vacc_data$date)
first_date <- min(ON_case_data$date)
last_date <- max(ON_case_data$date)

start_of_wave_2 <- ON_case_data[which(diff(wave) == 1), date]

gathering_interventions <- aggregate(
        intervention_dates, 
        by=list(intervention_dates$Weekend.Effective.Date, 
                intervention_dates$Action), 
        \(x) c(unique(x))
    ) |>
    dplyr::select(-Group.1, -Group.2, -Jurisdiction., -Start.date) |>
    dplyr::filter(
        Weekend.Effective.Date >= back_calculated_ON_case_data[, min(date)] &
            Weekend.Effective.Date <= back_calculated_ON_case_data[, max(date)]
    ) |>
    dplyr::relocate(Weekend.Effective.Date) |>
    data.table()

################################### GENERAL CASES ###################################

results_I <- ggplot(
        ON_case_data |> dplyr::mutate(wave=as.character(wave)), 
        aes(x=date, y=cases)
    ) +
    geom_rect(
        inherit.aes = FALSE,
        data = data.frame(x = 0, y = 0),
        aes(
            xmin = as.Date(first_date),
            xmax = as.Date(start_of_wave_2),
            ymin=-Inf, ymax=Inf
        ),
        fill="orange", alpha=0.1
    ) +
    geom_rect(
        inherit.aes = FALSE,
        data = data.frame(x = 0, y = 0),
        aes(
            xmin = as.Date(start_of_wave_2),
            xmax = as.Date(last_date),
            ymin=-Inf, ymax=Inf
        ),
        fill="red", alpha=0.1
    ) +
    geom_vline(xintercept=start_of_wave_2, linetype="dashed") +
    # geom_text(aes(x=start_of_wave_2, label="the_date", y=3000), colour="blue", angle=90) +
    geom_label(aes(x=start_of_wave_2, label=start_of_wave_2, y=3000), colour="blue") +
    geom_label(aes(x=as.Date("2020-05-01"), label="first wave", y=4500), colour="blue") +
    geom_label(aes(x=as.Date("2021-06-15"), label="second wave", y=4500), colour="blue") +
    geom_line(size = 0.75, colour="blue") +
    # geom_line(data=karlen_case_data, aes(y=Ontario.in_hospital.total), colour="red", size=1) +
    # geom_line(data=karlen_case_data, aes(y=Ontario.in_icu.total), colour="orange", size=1.15) +
    # geom_line(data=karlen_case_data, aes(y=Ontario.deaths.daily), colour="black", size=1.15) +
    # geom_vline(xintercept=as.numeric(gathering_interventions[grepl("New", Action), as.Date(Weekend.Effective.Date)]), linetype="dashed", colour="red") +
    # geom_vline(xintercept=as.numeric(gathering_interventions[grepl("Ease", Action), as.Date(Weekend.Effective.Date)]), linetype="dashed", colour="green") +
    theme_bw() +
    theme(
        legend.position="right"
    ) +
    labs(
        x="Date",
        y="Daily COVID-19 case incidence" # ,
        # title="Semi-log plot: Ontario
        # COVID-19 daily cases (blue curve), daily vaccinations (red curve), segmented regressions (black lines),
        # tightened restrictions (vertical red dashed lines), some restrictions loosened (green vertical dashed lines),
        # period where both infections and vaccinations increase roughly exponentially (orange shaded area)"
    ) +
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y", limits=c(first_date, last_date)) +
    ggsave(sprintf("%s/ON_I_plot_diff_waves.png", PROJECT_FOLDER), height=5, width=10)

print(results_I)

# print(plot_grid(vacc_timeline_plot, results_I, align = "hv", ncol = 1))
