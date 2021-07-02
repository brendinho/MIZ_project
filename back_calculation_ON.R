rm(list = ls())

library(nimble)
library(ggplot2)
library(data.table)
library(dplyr)
# library(CanCovidData)
library(fitdistrplus)
# library(rightTruncation)
library(stringr)
library(segmented)
library(lubridate)
library(XLConnect)

PROJECT_FOLDER <- "~/Documents/GitHub/MIZ_project/"
MAX_REPORTING_DELAY <- 60

# source(sprintf("%s/function_header.R", PROJECT_FOLDER))

Case_File <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv")

New_Cases <- Case_File |> pull(Total)
First_Record_Date <- Case_File |> pull(Date) |> min() |> as.Date()
num_observations <- length(New_Cases)

# raw_toronto_fsa_data <- fread("https://ckan0.cf.opendata.inter.prod-toronto.ca/download_resource/e5bf35bc-e681-43da-b2ce-0242d00922ad?format=csv")
# delay_data <- raw_toronto_fsa_data |>
#     # rename(ID = "_id") |>
#     rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
#     mutate(
#         reporting_delay = as.numeric(reported_date-episode_date, units="days"),
#         # lower_age = unlist(lapply(age_group, age_range_min)),
#         # upper_age = unlist(lapply(age_group, age_range_max)),
#         currently_hospitalized = (currently_hospitalized == "Yes"),
#         currently_in_icu = (currently_in_icu == "Yes"),
#         currently_intubated = (currently_intubated == "Yes"),
#         ever_hospitalized = (ever_hospitalized == "Yes"),
#         ever_in_icu = (ever_in_icu == "Yes"),
#         ever_intubated  = (ever_intubated == "Yes")
#     ) |>
#     filter(classification == "CONFIRMED") |>
#     write.csv(sprintf("%s/toronto_delay_data.csv", PROJECT_FOLDER), row.names = FALSE)

raw_reporting_delays <- fread(sprintf("%s/toronto_delay_data.csv", PROJECT_FOLDER)) |>
    filter(reporting_delay >= 0) |>
    filter(reporting_delay <= MAX_REPORTING_DELAY)

intervention_dates <- readWorksheetFromFile(
        sprintf("%s/%s", PROJECT_FOLDER, "covid-19-intervention-timeline-in-canada-en.xlsx"), 
        sheet="Tool interventions", 
        startRow=3, 
        endCol=8
    ) |> 
    dplyr::select(-Entry.ID, -Organization) |> 
    dplyr::filter(
        Jurisdiction. == "Ont." & 
            Action == "New" &
            Intervention.Category %in% c("Closures/openings", "Distancing")
    )



# breakpoints_for_mean_infectios_exp_growth <- c(
#     Summ[date %in% seq(ymd('2020-08-01'), ymd('2021-09-01'), '1 day'), min(log_meann, na.rm=T)],
#     Summ[date %in% seq(ymd('2021-01-01'), ymd('2021-02-01'), '1 day'), max(log_meann, na.rm=T)],
#     Summ[date %in% seq(ymd('2021-02-01'), ymd('2021-04-01'), '1 day'), min(log_meann, na.rm=T)],
#     Summ[date %in% seq(ymd('2021-04-01'), ymd('2021-05-01'), '1 day'), max(log_meann, na.rm=T)]
# )
# 
# seg_regression <- segmented(
#     lm(log_meann ~ index, data=Summ),
#     seg.Z = ~ index,
#     # psi = NA
#     # psi = Summ[log_meann %in% breakpoints_for_mean_infectios_exp_growth, index]
# )
# 
# fitted_data <- data.table(
#     date = Summ$date,
#     the_line = unname(fitted(seg_regression))
# ) |>
#     filter(the_line > 0)
# 
# results_I <- ggplot(Summ, aes(x=date)) +
#     # geom_ribbon(aes(ymin=log_minn, ymax=log_maxn), fill="grey70") +
#     geom_ribbon(aes(ymin=log_meann-log_sdn, ymax=log_meann+log_sdn), fill="grey70") +
#     # geom_point(aes(y=log_meann), colour="blue") +
#     geom_line(aes(y=log_meann), colour="blue") +
#     geom_line(data=fitted_data, aes(date, the_line), colour="black") +
#     theme_bw() +
#     labs(x="Date", y="New Cases") # +
# # ggsave(sprintf("%s/ON_I_plot_%s.png", PROJECT_FOLDER, gsub(" ", "_", as.character(Sys.time()))), height=5, width=10)
# 
# print(results_I)


# reporting_delay_probs <- raw_reporting_delays |>
#     count(reporting_delay) |>
#     mutate(f = n/sum(n)) |>
#     setkey(reporting_delay)
# 
# prob_vec <- rep(0, num_observations)
# for(i in 1:num_observations)
# {
#     if((i-1) %in% reporting_delay_probs$reporting_delay)
#     {
#         prob_vec[i] <- reporting_delay_probs[reporting_delay == (i-1), f]
#     }
# }
# 
# exp_fit <- fitdistr(raw_reporting_delays |> pull(reporting_delay), "exponential")
# plain_weib_fit <- fitdistr(raw_reporting_delays |> filter(reporting_delay>0) |> pull(reporting_delay), "weibull")
# ggplot(raw_reporting_delays, aes(x=reporting_delay)) +
#     geom_histogram(binwidth=1, aes(y=..density..), colour="black", fill="grey") +
#     theme_bw() +
#     stat_function(
#         fun=dweibull,
#         args=list(
#             shape=plain_weib_fit$estimate[["shape"]],
#             scale=plain_weib_fit$estimate[["scale"]]
#         ),
#         col="red", size=1
#     ) +
#     stat_function(
#         fun=dexp,
#         args=(mean=exp_fit$estimate[["rate"]]),
#         col="black",
#         size=1
#     ) +
#     labs(
#         x = "Reporting delay (days)",
#         y = "Density",
#         title = sprintf(
#             "NegExp (rate=%f), untruncated Weibull (shape=%f, scale=%f)",
#             exp_fit$estimate[["rate"]], plain_weib_fit$estimate[["shape"]], plain_weib_fit$estimate[["scale"]]
#         )
#     ) +
#     ggsave(sprintf("%s/ON_reporting_distribution.png", PROJECT_FOLDER), width=13)
# 
# BackCalc_code <- nimbleCode({
#     for(i in 1:n)
#     {
#         I[i] ~ dpois(lambda[i])
#         for(j in max(i-max_delay,1):i)
#         {
#             Z[i,j] <- f[(i-j)+1] * I[j]
#         }
#         D[i] ~ dpois(sum(Z[i,]))
#         lambda[i+1] ~ dnorm(lambda[i], sd=varsigma*varsigma)
#     }
# })
# 
# BackCalc_constants <- list(
#     n = num_observations,
#     varsigma = 5,
#     f = prob_vec,
#     max_delay = MAX_REPORTING_DELAY
# )
# 
# BackCalc_data = list(
#     Z = matrix(nrow = num_observations, ncol = num_observations, 0),
#     D = New_Cases
# )
# 
# model <- nimbleModel(
#     code = BackCalc_code,
#     constants = BackCalc_constants,
#     data = BackCalc_data,
#     inits = list(
#         lambda = sample(0:10000, num_observations+1),
#         I = New_Cases
#     )
# )
# 
# compiled_model <- compileNimble(model, resetFunctions = TRUE, showCompilerOutput = TRUE)
# 
# mcmc_conf <- configureMCMC(compiled_model, monitors=c("I"), print=T)
# 
# mcmc <- buildMCMC(mcmc_conf)
# 
# compiled_mcmc <- compileNimble(mcmc, project = model, showCompilerOutput = TRUE)
# 
# start_time <- Sys.time()
# 
# mcmc_samples = runMCMC(
#     compiled_mcmc,
#     nchains = 2, #5,
#     nburnin = 500, # 10000,
#     niter = 2000, # 50000,
#     thin = 1,
#     samplesAsCodaMCMC = TRUE,
#     summary = T,
#     setSeed = sample.int(100000, 1)
# )
# 
# print(Sys.time() - start_time)
# 
# mcmc_samples_file_name <- sprintf("%s/ON_mcmc_samples_%s.rds", PROJECT_FOLDER, gsub(" ", "_", as.character(Sys.time())))
# 
# saveRDS(mcmc_samples, mcmc_samples_file_name)

# mcmc_samples <- readRDS(mcmc_samples_file_name)
# mcmc_samples <- readRDS("ON_mcmc_samples_2021-06-21_06:31:35.rds")
# 
# DT <- data.table(do.call('rbind', mcmc_samples$samples))
# 
# Summ <- data.table()
# for(timestep in names(DT))
# {
#     step <- as.numeric(str_extract(timestep, "[[:digit:]]+"))
#     obs_variable <- strsplit(timestep, '\\[')[[1]][1]
# 
#     pointset <- DT[, get(timestep)]
#     
#     log_pointset <- DT[, log(get(timestep))]
#     log_pointset <- log_pointset[which(log_pointset>0)]
# 
#     n <- length(pointset)
# 
#     Summ <- rbind( Summ,
#         list(
#             index = step,
#             date = First_Record_Date + step - 1,
#             var = obs_variable,
#             
#             meann = mean(pointset),
#             sdn = sd(pointset),
#             maxn = max(pointset),
#             minn = min(pointset),
#             
#             log_meann = mean(log_pointset),
#             log_sdn = sd(log_pointset),
#             log_maxn = max(log_pointset),
#             log_minn = min(log_pointset)
#         ),
#         fill = TRUE
#    )
# }
# Summ <- Summ[var=="I" & date>=min(Summ$date)+MAX_REPORTING_DELAY]
# fwrite(Summ, "Summ.csv")

# intervention_dates <- readWorksheetFromFile(
#         sprintf("%s/%s", PROJECT_FOLDER, "covid-19-intervention-timeline-in-canada-en.xlsx"), 
#         sheet="Tool interventions", 
#         startRow=3, 
#         endCol=8
#     ) |> 
#     dplyr::select(-Entry.ID, -Organization) |> 
#     dplyr::filter(
#         Jurisdiction. == "Ont." & 
#         Action == "New" &
#         Intervention.Category %in% c("Closures/openings", "Distancing")
#     )
# 
# breakpoints_for_mean_infectios_exp_growth <- c(
#     Summ[date %in% seq(ymd('2020-08-01'), ymd('2021-09-01'), '1 day'), min(log_meann, na.rm=T)],
#     Summ[date %in% seq(ymd('2021-01-01'), ymd('2021-02-01'), '1 day'), max(log_meann, na.rm=T)],
#     Summ[date %in% seq(ymd('2021-02-01'), ymd('2021-04-01'), '1 day'), min(log_meann, na.rm=T)],
#     Summ[date %in% seq(ymd('2021-04-01'), ymd('2021-05-01'), '1 day'), max(log_meann, na.rm=T)]
# )
# 
# seg_regression <- segmented(
#     lm(log_meann ~ index, data=Summ),
#     seg.Z = ~ index,
#     # psi = NA
#     # psi = Summ[log_meann %in% breakpoints_for_mean_infectios_exp_growth, index]
# )
# 
# fitted_data <- data.table(
#         date = Summ$date,
#         the_line = unname(fitted(seg_regression))
#     ) |>
#     filter(the_line > 0)
# 
# results_I <- ggplot(Summ, aes(x=date)) +
#     # geom_ribbon(aes(ymin=log_minn, ymax=log_maxn), fill="grey70") +
#     geom_ribbon(aes(ymin=log_meann-log_sdn, ymax=log_meann+log_sdn), fill="grey70") +
#     # geom_point(aes(y=log_meann), colour="blue") +
#     geom_line(aes(y=log_meann), colour="blue") +
#     geom_line(data=fitted_data, aes(date, the_line), colour="black") +
#     theme_bw() +
#     labs(x="Date", y="New Cases") # +
#     # ggsave(sprintf("%s/ON_I_plot_%s.png", PROJECT_FOLDER, gsub(" ", "_", as.character(Sys.time()))), height=5, width=10)
# 
# print(results_I)
