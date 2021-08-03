rm(list = ls())
library(nimble)
# library(mgcv)
library(ggplot2)
library(data.table)
library(dplyr)

source("~/Documents/GitHub/MIZ_project/function_header.R")

# MAX_DELAY <- 60

# ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") |> 
# write.csv("~/Documents/GitHub/MIZ_project/ON_cases.csv", row.names=FALSE)

Case_File <- fread("~/Documents/GitHub/MIZ_project/ON_cases.csv")

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

toronto_reporting_delay_probs <- fread("~/Documents/GitHub/MIZ_project/toronto_delay_data.csv") |>
    filter(reporting_delay >= 0) |>
    filter(reporting_delay <= MAX_DELAY) |>
    count(reporting_delay) |>
    mutate(f = n/sum(n)) |>
    select(-n) |> 
    setkey(reporting_delay)

prob_vec <- rep(0, num_observations)
for(i in 1:num_observations)
{
    if((i-1) %in% toronto_reporting_delay_probs$reporting_delay)
    {
        prob_vec[i] <- toronto_reporting_delay_probs[reporting_delay == (i-1), f]
    }
}

BackCalc_constants <- list(
    n = num_observations, 
    varsigma = 5, 
    f = prob_vec, 
    max_delay = MAX_DELAY
)

BackCalc_data = list(
    Z = matrix(nrow = num_observations+MAX_DELAY, ncol = num_observations+MAX_DELAY, 0),
    D = rep(New_Cases, num_observations+MAX_DELAY)
)

BackCalc_code <- nimbleCode({
    
    lambda[1] ~ dunif(0, 1e4)
    for(i in 2:(n+max_delay)){ lambda[i] ~ dnorm(lambda[i-1], varsigma) }
    
    for(i in (max_delay+1):(n+max_delay))
    {
        I[i-max_delay] ~ dpois(lambda[i])
        for(j in (i-max_delay):i)
        {
            Z[i,j] <- f[(i-j)+1] * I[j]
        # }
        D[i] ~ dpois(sum(Z[i,]))
    }
})

model <- nimbleModel(
    code = BackCalc_code,
    constants = BackCalc_constants,
    data = BackCalc_data,
    inits = list(
        lambda = sample(0:10000, num_observations+MAX_DELAY),
        I = c(New_Cases, rep(0, MAX_DELAY)) # padded with zeroes so we can do some nowcasting
    )
)

compiled_model <- compileNimble(model, resetFunctions = TRUE, showCompilerOutput = TRUE)

mcmc_conf <- configureMCMC(compiled_model, monitors=c("I", "D", "lambda"), print=T) # I", 

mcmc <- buildMCMC(mcmc_conf)

compiled_mcmc <- compileNimble(mcmc, project = model, showCompilerOutput = TRUE)

start_time <- Sys.time()

mcmc_samples = runMCMC(
    compiled_mcmc,
    nchains = 2, # 5,
    nburnin = 500, # 10000,
    niter = 2000, # 40000,
    thin = 1,
    samplesAsCodaMCMC = TRUE,
    summary = T,
    setSeed = sample.int(1e4, 1)
)

print(Sys.time() - start_time)

mcmc_samples_file_name <- paste0("~/Documents/GitHub/MIZ_project/mcmc_samples_", gsub(" ", "_", as.character(Sys.time())), ".rds")

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

# results_D <- ggplot(Summ[var=="D"], aes(x=date)) +
#     geom_ribbon(aes(ymin=min, ymax=max), fill="grey70") +
#     geom_line(aes(y=mean), colour="blue") +
#     geom_line(data=Case_File, aes(Date, Total), colour="black") +
#     theme_bw() +
#     labs(x="Date", y="New Cases") +
#     ggsave(paste0("~/Documents/GitHub/MIZ_project/D_plot_", gsub(" ", "_", as.character(Sys.time())), ".png" ), height=5, width=10)
# 
# results_I <- ggplot(Summ[var=="I"], aes(x=date)) +
#     geom_ribbon(aes(ymin=min, ymax=max), fill="grey70") +
#     geom_line(aes(y=mean), colour="blue") +
#     geom_line(data=Case_File, aes(Date, Total), colour="black") +
#     theme_bw() +
#     labs(x="Date", y="New Cases") +
#     ggsave(paste0("~/Documents/GitHub/MIZ_project/I_plot_", gsub(" ", "_", as.character(Sys.time())), ".png" ), height=5, width=10)
# 
# 
# 
