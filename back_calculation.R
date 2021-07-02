rm(list = ls())

library(CanCovidData)
library(dplyr)
library(nimble)
# library(coda) # For manipulation of MCMC results.
# library(mgcv)
# library(ggplot2)
# library(ggpubr)
# library(ggmcmc)
# library(data.table)
# library(reshape)

source("~/Documents/GitHub/MIZ_project/function_header.R")

# ON_cases <- fread("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/8a88fe6d-d8fb-41a3-9d04-f0550a44999f/download/daily_change_in_cases_by_phu.csv") |> 
# write.csv("~/Documents/GitHub/MIZ_project/ON_cases.csv", row.names=FALSE)

ON_cases <- fread("~/Documents/GitHub/MIZ_project/ON_cases.csv") |> pull(Total)

num_observations <- length(ON_cases)

# raw_toronto_fsa_data <- get_toronto_neighbourhood_cases()
# delay_data <- raw_toronto_fsa_data |>
#     rename(ID = "_id") |>
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
#     filter(reporting_delay >= 0 & reporting_delay <= ) |>
#     filter(classification == "CONFIRMED") |>
#     write.csv("~/Documents/GitHub/MIZ_project/toronto_delay_data.csv", row.names = FALSE)

toronto_reporting_delay_probs <- fread("~/Documents/GitHub/MIZ_project/toronto_delay_data.csv") |> 
    # filter(reporting_delay <= max_delay) |>
    count(reporting_delay) |> 
    mutate(f = n/sum(n)) |> 
    pull(f) 

prob_vec = rep(0, nobs)
for(i in 1:60){
    prob_vec[i] = toronto_reporting_delay_probs[i]
}

BackCalc_code = nimbleCode({
    for(i in 1:n)
    {
        I[i] ~ dpois(lambda[i])

        for(j in max(i-max_delay, 1):i) # we've set the max delay to 60, so no need to sum a bunch of zeroes
        {
            D[i,j] <- f[i-j]*I[j] # R is 1-indexed, so no need to add the 1
        }

        Expected[i] ~ dpois(sum(D[i,]))

        lambda[i+1] ~ T(dnorm(lambda[i], varsigma), 0,) # NOTE: I may need to change the variance here...
    }
})

chains_inits <- list(
    chain1 = list(lambda = rep(1.0, num_observations+1), I = ON_cases),
    chain2 = list(lambda = rep(0.5, num_observations+1), I = ON_cases),
    chain3 = list(lambda = rep(2.0, num_observations+1), I = ON_cases),
    chain4 = list(lambda = rep(1.5, num_observations+1), I = ON_cases),
    chain5 = list(lambda = rep(2.5, num_observations+1), I = ON_cases)
)

BackCalc_constants <- list(
    n = num_observations, 
    varsigma = 1, 
    f = prob_vec, #toronto_reporting_delay_probs, 
    D = matrix(nrow = num_observations, ncol = num_observations, 0),
    max_delay = 60
)

BackCalc_data <- list(Expected = ON_cases)

model <- nimbleModel(
    code = BackCalc_code, 
    constants = BackCalc_constants, 
    data = BackCalc_data, 
    inits = chains_inits,
    name = "toronto_true_incidence"
)

compiled_model <- compileNimble(model, resetFunctions = TRUE)

# mcmc_conf <- configureMCMC(compiled_model, monitors=c("I"), print=T)
# 
# 
# compiled_mcmc <- compileNimble(mcmc, project = model)

# mcmc_samples = runMCMC(compiled_mcmc, inits=inits, nchains=5, nburnin=10000, niter=20000, thin=1,
#                        samplesAsCodaMCMC = TRUE, summary=T, setSeed=c(1, 2, 3, 4, 5))
# 
# df = data.table(do.call('rbind', mcmc_samples$samples))
# colMeans(df) 
# sum(colMeans(df))
# 
# write.table(df,file = "Results.csv",sep = ",", row.names = FALSE, col.names = FALSE)
# 
# 
# p<-ggplot(df, aes(x=as.factor(get("I[30]")))) + 
#   geom_histogram(color="black", fill="white", stat="count")
# p
# 
# 
# 
# 
# lstr = c("I[20]", "I[21]", "I[22]", "I[23]")
# trace_plots = df[, ..lstr]
# chainsPlot(trace_plots,file = "myplot.pdf",width = 7)

