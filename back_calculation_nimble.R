rm(list = ls())

library(nimble)
# library(mgcv)
library(ggplot2)
library(data.table)
library(dplyr)
library(CanCovidData)

source("~/Documents/GitHub/MIZ_project/function_header.R")

MAX_DELAY <- 60

Case_File <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv")

New_Cases <- Case_File |> pull(Total)
First_Record_Date <- Case_File |> pull(Date) |> min() |> as.Date()
num_observations <- length(New_Cases)

# raw_toronto_fsa_data <- get_toronto_neighbourhood_cases()
# delay_data <- raw_toronto_fsa_data |>
#     # rename(ID = "_id") |>
#     rename_with(~ tolower(gsub(" ", "_", .x, fixed = TRUE))) |>
#     mutate(
#         reporting_delay = as.numeric(reported_date-episode_date, units="days"),
#         lower_age = unlist(lapply(age_group, age_range_min)),
#         upper_age = unlist(lapply(age_group, age_range_max)),
#         currently_hospitalized = (currently_hospitalized == "Yes"),
#         currently_in_icu = (currently_in_icu == "Yes"),
#         currently_intubated = (currently_intubated == "Yes"),
#         ever_hospitalized = (ever_hospitalized == "Yes"),
#         ever_in_icu = (ever_in_icu == "Yes"),
#         ever_intubated  = (ever_intubated == "Yes")
#     ) |>
#     filter(classification == "CONFIRMED") |>
#     write.csv("~/Documents/GitHub/MIZ_project/toronto_delay_data.csv", row.names = FALSE)

raw_reporting_delays <- fread("~/Documents/GitHub/MIZ_project/toronto_delay_data.csv") |>
    filter(reporting_delay >= 0) |> 
    filter(reporting_delay <= MAX_DELAY)

reporting_delay_probs <- raw_reporting_delays |>
    count(reporting_delay) |>
    mutate(f = n/sum(n)) |>
    setkey(reporting_delay)

prob_vec <- rep(0, num_observations)
for(i in 1:num_observations)
{
    if((i-1) %in% reporting_delay_probs$reporting_delay)
    {
        prob_vec[i] <- reporting_delay_probs[reporting_delay == (i-1), f]
    }
}

exp_fit <- fitdistr(raw_reporting_delays |> pull(reporting_delay), "exponential")
plain_weib_fit <- fitdistr(raw_reporting_delays |> filter(reporting_delay>0) |> pull(reporting_delay), "weibull")
ggplot(raw_reporting_delays, aes(x=reporting_delay)) +
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
    ggsave(sprintf("~/Documents/GitHub/MIZ_project/Graphs/Delay_Distros/ON_reporting_distribution.png"), width=13)

BackCalc_code <- nimbleCode({
    for(i in 1:n)
    {
        I[i] ~ dpois(lambda[i])
        for(j in max(i-max_delay,1):i)
        {
            Z[i,j] <- f[(i-j)+1] * I[j]
        }
        D[i] ~ dpois(sum(Z[i,]))
        lambda[i+1] ~ dnorm(lambda[i], sd=varsigma*varsigma)

    }
})

BackCalc_constants <- list(
    n = num_observations,
    varsigma = 5,
    f = prob_vec,
    max_delay = MAX_DELAY
)

BackCalc_data = list(
    Z = matrix(nrow = num_observations, ncol = num_observations, 0),
    D = New_Cases
)

model <- nimbleModel(
    code = BackCalc_code,
    constants = BackCalc_constants,
    data = BackCalc_data,
    inits = list(
        lambda = sample(0:10000, num_observations+1),
        I = New_Cases
    )
)

compiled_model <- compileNimble(model, resetFunctions = TRUE, showCompilerOutput = TRUE)

mcmc_conf <- configureMCMC(compiled_model, monitors=c("I", "D"), print=T)

mcmc <- buildMCMC(mcmc_conf)

compiled_mcmc <- compileNimble(mcmc, project = model, showCompilerOutput = TRUE)

start_time <- Sys.time()

mcmc_samples = runMCMC(
    compiled_mcmc,
    nchains = 5,
    nburnin = 10000,
    niter = 50000,
    thin = 1,
    samplesAsCodaMCMC = TRUE,
    summary = T,
    setSeed = sample.int(100000, 1)
)

print(Sys.time() - start_time)

mcmc_samples_file_name <- paste0("~/Documents/GitHub/MIZ_project/ON_mcmc_samples_", gsub(" ", "_", as.character(Sys.time())), ".rds")

saveRDS(mcmc_samples, mcmc_samples_file_name)

mcmc_samples <- readRDS(mcmc_samples_file_name)

DT <- data.table(do.call('rbind', mcmc_samples$samples))

Summ <- data.table(date=character(), var=as.character(), mean=numeric(), sd=numeric(), max=integer(), min=integer(), me90=numeric(), me95=numeric(), me99=numeric())
Summ[, date:=as.Date(date)]

for(timestep in names(DT))
{
    step <- as.numeric(str_extract(timestep, "[[:digit:]]+"))

    pointset <- DT[, get(timestep)]

    n <- length(pointset)
    sdn <- sd(pointset)

    Summ <- rbind( Summ,
        list(
            date = First_Record_Date + step - 1,
            var = strsplit(timestep, '\\[')[[1]][1],
            mean = mean(pointset),
            sd = sdn,
            max = max(pointset),
            min = min(pointset),
            me90 = qt(.90, n-1)*sdn/sqrt(n),
            me95 = qt(.95, n-1)*sdn/sqrt(n),
            me99 = qt(.99, n-1)*sdn/sqrt(n)
        )
   )
}

results_D <- ggplot(Summ[var=="D"], aes(x=date)) +
    geom_ribbon(aes(ymin=min, ymax=max), fill="grey70") +
    geom_line(aes(y=mean), colour="blue") +
    geom_line(data=Case_File, aes(Date, Total), colour="black") +
    theme_bw() +
    labs(x="Date", y="New Cases") +
    ggsave(paste0("~/Documents/GitHub/MIZ_project/ON_D_plot_", gsub(" ", "_", as.character(Sys.time())), ".png" ), height=5, width=10)

results_I <- ggplot(Summ[var=="I"], aes(x=date)) +
    geom_ribbon(aes(ymin=min, ymax=max), fill="grey70") +
    geom_line(aes(y=mean), colour="blue") +
    geom_line(data=Case_File, aes(Date, Total), colour="black") +
    theme_bw() +
    labs(x="Date", y="New Cases") +
    ggsave(paste0("~/Documents/GitHub/MIZ_project/ON_I_plot_", gsub(" ", "_", as.character(Sys.time())), ".png" ), height=5, width=10)



