rm(list = ls())

library(ggplot2)
library(data.table)
library(dplyr)
library(segmented)
library(lubridate)
library(XLConnect)
library(directlabels)

PROJECT_FOLDER <- "~/Documents/GitHub/MIZ_project/"

source(sprintf("%s/NOW_function_header.R", PROJECT_FOLDER))

intervention_dates <- readWorksheetFromFile(
        sprintf("%s/%s", PROJECT_FOLDER, "Classifications/covid-19-intervention-timeline-in-canada-en.xlsx"),
        sheet="Tool interventions", 
        startRow=3, 
        endCol=8
    ) |> 
    dplyr::select(-Entry.ID, -Organization) |> 
    dplyr::filter(
        Jurisdiction. == "Alta." &
            grepl("closures|openings|distancing|travel", tolower(Intervention.Category))
    ) |>
    dplyr::mutate(
        Action = as.character(Action),
        Weekend.Effective.Date = lubridate::ceiling_date(Start.date, "week")
    ) |>
    data.table()

AB_true_data <- fread("CaseDataTables/AB_cases.csv") |>
    group_by(date) |>
    tally(cases) |>
    dplyr::rename(cases=n) |>
    dplyr::filter(!is.na(cases) & cases!=0) |>
    dplyr::mutate(log_cases=log(cases), index=as.numeric(date)) |>
    data.table()

AB_vacc_data <- fread("CaseDataTables/UofT_vaccines.csv") |>
    dplyr::filter(province == "Alberta") |>
    dplyr::rename(vaccines=avaccine, date=date_vaccine_administered) |>
    dplyr::filter(!is.na(vaccines) & vaccines!=0) |>
    dplyr::mutate(log_vaccines=log(vaccines), index=as.numeric(date)) |>
    dplyr::select(-cumulative_avaccine, -province) |>
    data.table()

VHA_total_pop_break_indices <- c(
    AB_true_data[date %in% seq(lubridate::ymd('2020-04-15'), lubridate::ymd('2020-05-01'), '1 day')][which(log_cases==max(log_cases, na.rm=T)), index],
    AB_true_data[date %in% seq(lubridate::ymd('2020-05-15'), lubridate::ymd('2020-06-01'), '1 day')][which(log_cases==min(log_cases, na.rm=T)), index],
    AB_true_data[date %in% seq(lubridate::ymd('2020-11-15'), lubridate::ymd('2020-12-15'), '1 day')][which(log_cases==max(log_cases, na.rm=T)), index],
    AB_true_data[date %in% seq(lubridate::ymd('2021-01-15'), lubridate::ymd('2021-02-15'), '1 day')][which(log_cases==min(log_cases, na.rm=T)), index],
    AB_true_data[date %in% seq(lubridate::ymd('2021-04-15'), lubridate::ymd('2021-05-15'), '1 day')][which(log_cases==max(log_cases, na.rm=T)), index]
)

gathering_interventions <- aggregate(
        intervention_dates, 
        by=list(intervention_dates$Weekend.Effective.Date, 
                intervention_dates$Action), 
        \(x) c(unique(x))
    ) |>
    dplyr::select(-Group.1, -Group.2, -Jurisdiction., -Start.date) |>
    dplyr::filter(
        Weekend.Effective.Date >= AB_true_data[, min(date)] &
        Weekend.Effective.Date <= AB_true_data[, max(date)]
    ) |>
    dplyr::relocate(Weekend.Effective.Date) |>
    data.table()

################################### GENERAL CASES ###################################

total_cases_seg_regression <- segmented(
    lm(log_cases ~ index, data=AB_true_data),
    seg.Z = ~ index,
    psi = VHA_total_pop_break_indices
)

cases_fitted_data <- data.table(
    date = AB_true_data$date,
    cases_line = unname(fitted(total_cases_seg_regression))
) |>
    filter(cases_line > 0)

total_vacc_seg_regression <- lm(log_vaccines ~ index, data=AB_vacc_data)

vacc_fitted_data <- data.table(
        date = AB_vacc_data$date,
        vacc_line = unname(fitted(total_vacc_seg_regression))
    ) |>
    filter(vacc_line > 0)

vacc_and_cases <- merge(cases_fitted_data, vacc_fitted_data, by=c("date"))[date>"2021-01-15"]
increasing_indices <- intersect(which(diff(vacc_and_cases$vacc_line)>0), which(diff(vacc_and_cases$cases_line)>0))
start_date_both_cases_vacc_increase <- vacc_and_cases[min(increasing_indices), date]
end_date_both_cases_vacc_increase <- vacc_and_cases[max(increasing_indices), date]

# vacc_timeline_plot <- ggplot(
#     ON_vaxes_by_age |> 
#         group_by(Agegroup) |> 
#         dplyr::mutate(label = if_else(Percent_at_least_one_dose == max(Percent_at_least_one_dose), as.character(Agegroup), NA_character_)), 
#     aes(Date, Percent_at_least_one_dose, group=Agegroup, colour=Agegroup)
# ) +
#     geom_rect(
#         inherit.aes = F,
#         data = vacc_and_cases,
#         aes(
#             xmin=start_date_both_cases_vacc_increase,
#             xmax=end_date_both_cases_vacc_increase,
#             ymin=-Inf,
#             ymax=Inf
#         ),
#         fill="pink", alpha=0.9
#     ) +
#     geom_line(size=0.75) + 
#     theme_bw() + 
#     theme(legend.position = "none") +
#     labs(
#         y="percentage with first dose administered", 
#         title="Ontario vaccination trend: both case numbers and administered vaccines
#              increasing exponentially in shaded section (16 Jan to 21 Apr)"
#     ) + 
#     guides(colour=guide_legend(title="Age Group")) + 
#     geom_label_repel(aes(label=label), nudge_x=1, box.padding=0.02, label.size=0.25) +
#     ggsave(sprintf("%s/ON_vacc_timeline_%s.png", PROJECT_FOLDER, gsub(" ", "_", as.character(Sys.time()))), height=5, width=10)

results_I <- ggplot(AB_true_data, aes(x=date)) +
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
    # geom_ribbon(aes(ymin=log_meann-log_sdn, ymax=log_meann+log_sdn), fill="grey70") +
    geom_line(data=cases_fitted_data, aes(date, cases_line), colour="black") +
    geom_line(data=vacc_fitted_data, aes(date, vacc_line), colour="black") +
    geom_line(data=AB_vacc_data, aes(date, log_vaccines), colour="red") +
    geom_line(aes(y=log_cases), colour="blue") +
    geom_vline(xintercept=as.numeric(gathering_interventions[grepl("New", Action), as.Date(Weekend.Effective.Date)]), linetype="dashed", colour="red") +
    geom_vline(xintercept=as.numeric(gathering_interventions[grepl("Ease", Action), as.Date(Weekend.Effective.Date)]), linetype="dashed", colour="green") +
    theme_bw() +
    theme(
        legend.position="right"
    ) +
    labs(
        x="Date", 
        y="log(New Cases), log(Vaccinations)"
        # title="Semi-log plot: Alberta
        # COVID-19 daily cases (blue curve), daily vaccinations (red curve), segmented regressions (black lines),
        # tightened restrictions (vertical red dashed lines), some restrictions loosened (green vertical dashed lines),
        # period where both infections and vaccinations increase roughly exponentially (orange shaded area)"
    ) +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
    ggsave(sprintf("%s/AB_I_plot.png", PROJECT_FOLDER), height=5, width=10)

print(results_I)
