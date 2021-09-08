rm(list = ls())

library(nimble)
library(ggplot2)
library(data.table)
library(dplyr)
library(fitdistrplus)
library(cowplot)
library(stringr)
library(segmented)
library(lubridate)
library(XLConnect)
library(directlabels)
library(ggrepel)
library(CanCovidData)
library(ggforce)
library(foreach)

# https://stackoverflow.com/questions/12945687/read-all-worksheets-in-an-excel-workbook-into-an-r-list-with-data-frames
# "Jeromy Anglim"s answer to "Read all worksheets in an Excel workbook into an R list with data.frames"
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
}

PROJECT_FOLDER <- "~/Documents/GitHub/MIZ_project/"
MAX_REPORTING_DELAY <- 60

source(sprintf("%s/NOW_function_header.R", PROJECT_FOLDER))

# fwrite(get_toronto_neighbourhood_cases(), "toronto_neighbourhood_data.csv")
    
raw_toronto_fsa_data <- fread("toronto_neighbourhood_data.csv")
# fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv")
delay_data <- raw_toronto_fsa_data |>
    # rename(ID = "_id") |>
    dplyr::rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
    dplyr::mutate(
        reporting_delay = as.numeric(reported_date-episode_date, units="days"),
        lower_age = unlist(lapply(age_group, age_range_min)),
        upper_age = unlist(lapply(age_group, age_range_max)),
        currently_hospitalized = (currently_hospitalized == "Yes"),
        currently_in_icu = (currently_in_icu == "Yes"),
        currently_intubated = (currently_intubated == "Yes"),
        ever_hospitalized = (ever_hospitalized == "Yes"),
        ever_in_icu = (ever_in_icu == "Yes"),
        ever_intubated  = (ever_intubated == "Yes")
    ) |>
    dplyr::filter(classification == "CONFIRMED") |>
    write.csv(sprintf("%s/toronto_delay_data.csv", PROJECT_FOLDER), row.names = FALSE)

raw_reporting_delays <- fread(sprintf("%s/toronto_delay_data.csv", PROJECT_FOLDER)) |>
    dplyr::filter(reporting_delay >= 0) |>
    dplyr::filter(reporting_delay <= MAX_REPORTING_DELAY) |>
    dplyr::mutate(num_cases = 1)

# downloading the big-asss intervention file from the website
# it surpasses the max number of rows
download.file(
        "https://www.cihi.ca/sites/default/files/document/covid-19-intervention-scan-data-tables-en.xlsx", 
        destfile="covid-19-intervention-scan-data-tables-en.xlsx"
    )
worksheet_names <- names(read_excel_allsheets("covid-19-intervention-scan-data-tables-en.xlsx"))
intervention_scan <- readWorksheetFromFile("covid-19-intervention-scan-data-tables-en.xlsx", sheet="Intervention scan")

columns <- list()

for(cols in names(intervention_scan))
{
    beam <- intervention_scan[[cols]]
    columns[[beam[2]]] <- beam[3:length(beam)]
}
date_cleaner <- function(x)
{
    if(is.na(x) | x=="NA") return("")
    if(grepl("no", tolower(x))) return("")
    if(!grepl(":", x)) return(paste0(x, "-01"))
    return(gsub(" 00:00:00", "", x))
}
All_Interventions <- as.data.frame(columns) |>
    tibble() |>
    dplyr::rename_with(\(x) x |> tolower() |> trimws() %>% gsub("\\.+", ".", .) %>% sub('\\.$', '', .)) |>
    dplyr::filter(!grepl("end of worksheet", tolower(entry.id))) |>
    dplyr::mutate(
        date_announced_temp = as.Date(""), 
        date_implemented_temp = as.Date(""),
        intervention.who = "", 
        intervention.what = "", 
        effective.until = ""
    ) |>
    dplyr::mutate(
        date_announced_temp = as.Date(unlist(lapply(date.announced, date_cleaner))),
        date_implemented_temp = as.Date(unlist(lapply(date.implemented, date_cleaner))),
        intervention.who = unlist(lapply(
            intervention.summary,
            \(x) regmatches(x, regexec("Who:\\s*(.*?)\\s*\\n", x))[[1]][2]
        )),
        intervention.what = unlist(lapply(
            intervention.summary,
            \(x) regmatches(x, regexec("What:\\s*(.*?)\\s*\\n", x))[[1]][2]
        )),
        effective.until = as.Date(unlist(lapply(
            intervention.summary,
            \(x) regmatches(x, regexec("until:\\s*(.*?)\\s*\\n", x))[[1]][2]
        ))),
        indigenous.population.group = unlist(lapply(
            indigenous.population.group,
            \(x) if(tolower(x)=="na" | is.na(x)) NA else grepl("yes", tolower(x))
        ))
    ) |>
    dplyr::select(-date.announced, -date.implemented, -intervention.summary) |>
    dplyr::rename(
        date.announced = date_announced_temp,
        date.implemented = date_implemented_temp
    )



# sprintf("%s/%s", PROJECT_FOLDER, "covid-19-intervention-timeline-in-canada-en.xlsx"
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
        Weekend.Effective.Date = lubridate::ceiling_date(Start.date, "week"),
        index = 1:length(Jurisdiction.)
    ) |>
    data.table()

back_calculated_ON_case_data <- fread("Summ.csv")[var=="I"]

ON_vacc_data_official <- fread("CaseDataTables/ON_vaxes.csv") |>
    dplyr::rename(vaccines=previous_day_total_doses_administered) |>
    dplyr::filter(!is.na(vaccines)) |>
    dplyr::mutate(date=as.Date(report_date)-1) |>
    dplyr::mutate(log_vaccines=log(vaccines), index=as.numeric(date)) |>
    dplyr::select(date, vaccines, log_vaccines, index)

ON_vaxes_by_age <- fread("CaseDataTables/ON_vaxes_by_age.csv")

ON_vacc_data_UofT <- jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=avaccine")$avaccine |> 
    dplyr::filter(province == "Ontario") |> 
    dplyr::rename(date=date_vaccine_administered, vaccines=avaccine) |> 
    dplyr::mutate(log_vaccines = log(vaccines)) |> 
    dplyr::select(-cumulative_avaccine, -province) |>
    dplyr::mutate(date=as.Date(date, format="%d-%m-%Y"), index=as.numeric(date)) |>
    dplyr::filter(vaccines != 0)

VHA_total_pop_break_indices <- c(
    back_calculated_ON_case_data[date %in% seq(lubridate::ymd('2020-08-01'), lubridate::ymd('2021-09-01'), '1 day')][which(log_meann==min(log_meann, na.rm=T)), index],
    back_calculated_ON_case_data[date %in% seq(lubridate::ymd('2021-01-01'), lubridate::ymd('2021-02-01'), '1 day')][which(log_meann==max(log_meann, na.rm=T)), index],
    back_calculated_ON_case_data[date %in% seq(lubridate::ymd('2021-02-01'), lubridate::ymd('2021-04-01'), '1 day')][which(log_meann==min(log_meann, na.rm=T)), index],
    back_calculated_ON_case_data[date %in% seq(lubridate::ymd('2021-04-01'), lubridate::ymd('2021-05-01'), '1 day')][which(log_meann==max(log_meann, na.rm=T)), index]
)

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

total_cases_seg_regression <- segmented(
    lm(log_meann ~ index, data=back_calculated_ON_case_data),
    seg.Z = ~ index,
    # psi = NA
    # psi = VHA_breakpoint_dates
    psi = VHA_total_pop_break_indices
)

cases_fitted_data <- data.table(
        date = back_calculated_ON_case_data$date,
        cases_line = unname(fitted(total_cases_seg_regression))
    ) |>
    filter(cases_line > 0)

total_vacc_seg_regression <- lm(log_vaccines ~ index, data=ON_vacc_data_official)

vacc_fitted_data <- data.table(
        date = ON_vacc_data_official$date,
        vacc_line = unname(fitted(total_vacc_seg_regression))
    ) |>
    filter(vacc_line > 0)

vacc_and_cases <- merge(cases_fitted_data, vacc_fitted_data, by=c("date"))[date>"2021-01-15"]
increasing_indices <- intersect(which(diff(vacc_and_cases$vacc_line)>0), which(diff(vacc_and_cases$cases_line)>0))
start_date_both_cases_vacc_increase <- vacc_and_cases[min(increasing_indices), date]
end_date_both_cases_vacc_increase <- vacc_and_cases[max(increasing_indices), date]

ggplot(back_calculated_ON_case_data, aes(x=date)) + 
    geom_point(aes(y=meann)) +
    geom_circle(inherit.aes=F, aes(x0=as.Date("2021-01-06"), y0=3658, r=20, colour="red")) +
    geom_circle(inherit.aes=F, aes(x0=as.Date("2021-04-14"), y0=4470, r=20, colour="red")) +
    theme_bw() +
    theme(legend.position="none") +
    labs(x="Date", y="Cases") +
    ggsave(sprintf("%s/circles.png", PROJECT_FOLDER), height=5, width=10)

vacc_timeline_plot <- ggplot(
        ON_vaxes_by_age |> 
            group_by(Agegroup) |> 
            dplyr::mutate(label = if_else(Percent_at_least_one_dose == max(Percent_at_least_one_dose), as.character(Agegroup), NA_character_)), 
        aes(Date, Percent_at_least_one_dose, group=Agegroup, colour=Agegroup)
    ) +
    geom_rect(
        inherit.aes = F,
        data = vacc_and_cases,
        aes(
            xmin=start_date_both_cases_vacc_increase,
            xmax=end_date_both_cases_vacc_increase,
            ymin=-Inf,
            ymax=Inf
        ),
        fill="pink", alpha=0.9
    ) +
    geom_line(size=0.75) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(
        y="percentage with first dose administered"
        # title="Ontario vaccination trend: both case numbers and administered vaccines
        #      increasing exponentially in shaded section (16 Jan to 21 Apr)"
    ) + 
    guides(colour=guide_legend(title="Age Group")) + 
    geom_label_repel(aes(label=label), nudge_x=1, box.padding=0.02, label.size=0.25) +
    ggsave(sprintf("%s/ON_vacc_timeline.png", PROJECT_FOLDER), height=5, width=10)

results_I <- ggplot(back_calculated_ON_case_data, aes(x=date)) +
    geom_rect(
        data = vacc_and_cases,
        aes(
            xmin=start_date_both_cases_vacc_increase, 
            xmax=end_date_both_cases_vacc_increase, 
            ymin=-Inf, 
            ymax=Inf
        ), 
        fill="pink", alpha=0.1
    ) +
    geom_ribbon(aes(ymin=log_meann-log_sdn, ymax=log_meann+log_sdn), fill="grey70") +
    geom_line(data=cases_fitted_data, aes(date, cases_line), colour="black") +
    geom_line(data=vacc_fitted_data, aes(date, vacc_line), colour="black") +
    geom_line(data=ON_vacc_data_official, aes(date, log_vaccines), colour="red") +
    geom_line(aes(y=log_meann), colour="blue") +
    geom_vline(xintercept=as.numeric(gathering_interventions[grepl("New", Action), as.Date(Weekend.Effective.Date)]), linetype="dashed", colour="red") +
    geom_vline(xintercept=as.numeric(gathering_interventions[grepl("Ease", Action), as.Date(Weekend.Effective.Date)]), linetype="dashed", colour="green") +
    theme_bw() +
    theme(
        legend.position="right"
    ) +
    labs(
        x="Date", 
        y="log(New Cases), log(Vaccinations)"
        # title="Semi-log plot: Ontario
        # COVID-19 daily cases (blue curve), daily vaccinations (red curve), segmented regressions (black lines),
        # tightened restrictions (vertical red dashed lines), some restrictions loosened (green vertical dashed lines),
        # period where both infections and vaccinations increase roughly exponentially (orange shaded area)"
    ) +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
    ggsave(sprintf("%s/ON_I_plot_interventions.png", PROJECT_FOLDER), height=5, width=10)

print(plot_grid(vacc_timeline_plot, results_I, align = "hv", ncol = 1))


