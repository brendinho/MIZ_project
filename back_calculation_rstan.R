rm(list=ls())
options(mc.cores = 5)

library(rstan)

source("~/Documents/GitHub/MIZ_project/function_header.R")

MAX_DELAY <- 60

# library(tidyverse)
# library(lubridate)
# library(zoo)

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
    count(reporting_delay) |>
    mutate(f = n/sum(n)) |>
    select(-n) |> 
    setkey(reporting_delay)

prob_vec <- rep(0, MAX_DELAY)
for(i in 1:num_observations)
{
    if((i-1) %in% toronto_reporting_delay_probs$reporting_delay)
    {
        prob_vec[i] <- toronto_reporting_delay_probs[reporting_delay == (i-1), f]
    }
}

stan_code <- "
    data {
        int<lower=0> N; // number of observations
        int<lower=0> I[N]; // number of new cases per day
        real<lower=0> varsigma; // the standard deviation of the random walk progress 
        vector[N] f; // vector of probabilities of reporting delay (f[x] = Pr(reporting_delay=x))
        int<lower=0> max_delay; // longest reporting delay we're willing to consider
    }
    
    parameters {
        row_vector<lower=0>[N+max_delay] lambda; // the incidence rates of infection
        // real<lower=0>[N+max_delay] accumulator; // dummy for the discrete convolution
    }
    
    model {
    
        lambda[1] ~ uniform(0, 1e4);
        for(i in 2:(N+max_delay)) { lambda[i] ~ normal(lambda[i-1], varsigma); }
        
        for(i in (max_delay+1):(N+max_delay)) // replacement of I in place
        {
            // I[i-max_delay] ~ poisson(lambda[i]);
           
            // for(j in max(i-max_delay, 1):i)
            // {
            //     accumulator[i] += I[j]*f[(i-j)+1];
            // }
            
            // for(j in (i-max_delay):i)
            // {
            //     accumulator[i] = accumulator[i] + poisson(lambda[j])*f[(i-j)+1];
            // }
            
            real D_i = poisson(lambda[(i-max_delay):i ])*f[1:max_delay];
            
            // D[i] ~ poisson(accumulator[i]);
            I[i-max_delay] ~ poisson(D_i);
        }
    }
"

sample_file_name <- paste0("~/Documents/GitHub/MIZ_project/rstan_samples_", gsub(" ", "_", as.character(Sys.time())), ".rds")

res_icu_ger = sampling(
    stan_model(model_code = stan_code),
    data = list(
        N = num_observations,
        I = New_Cases,
        varsigma = 30,
        f = prob_vec,
        max_delay = MAX_DELAY
    ),
    iter = 50000,
    chains = 5,
    warmup = 10000,
    verbose = TRUE,
    thin = 1,
    seed = sample.int(100000, 1),
    inits = list(
        lambda = sample(0:10000, num_observations+MAX_DELAY) # ,
        # accumulator = rep(0, num_observations+MAX_DELAY)
    ),
    sample_file = sample_file_name
)
# 
# 
# res_icu_new_post = extract(res_icu_ger)
# mod_res = tibble(date=seq(min(dat$date)-(length(rev_surv_s)-1), max(dat$date), by="1 day"),
#                  new_icu_med = apply(exp(res_icu_new_post$log_lambda),2,median),
#                  new_icu_ci_lwr = apply(exp(res_icu_new_post$log_lambda),2,function(x) quantile(x, 0.025)),
#                  new_icu_ci_upr = apply(exp(res_icu_new_post$log_lambda),2,function(x) quantile(x, 0.975)))
# write_tsv(mod_res, path = paste0("../results/",day, "_backpro_ger_res.csv"))

# theme_set(theme_bw())
# date = ymd("2021-04-27")
# 
# dat_ger = read_tsv(paste0("../data/", date, "_divi_ger_reports.csv"))
# dat_ger_res = read_tsv(paste0("../results/", date, "_backpro_ger_res.csv"))
# dat_fed = read_tsv(paste0("../results/", date, "_backpro_fed_state.csv"))
# 
# # Estimate smooth model for daily new admissions in Germany as a whole
# bel_smooth = mgcv::gam(icu_new ~ s(t), family = "poisson", data = dat_ger %>% mutate(t=1:n()))
# 
# dat_ger_sm = dat_ger %>% select(date) %>%
#     mutate(icu_new_smooth_ex = exp(predict(bel_smooth, newdata = tibble(t=1:n()))),
#            icu_new_smooth_ex_lwr = exp(log(icu_new_smooth_ex) - 
#                                            2*predict(bel_smooth, newdata = tibble(t=1:n()),
#                                                      se.fit = T)[[2]]),
#            icu_new_smooth_ex_upr = exp(log(icu_new_smooth_ex) + 
#                                            2*predict(bel_smooth, newdata = tibble(t=1:n()),
#                                                      se.fit = T)[[2]]))
# 
# dat_ger_sm %>% rename(est=icu_new_smooth_ex, q025=icu_new_smooth_ex_lwr, q975=icu_new_smooth_ex_upr) %>%
#     write_tsv(paste0("../results/", date, "_ger_new_sm.csv"))
# # Create Plot for Germany
# plot_bel = dat_ger %>% ggplot() + geom_line(aes(date, icu)) + ylab("Belegung")
# plot_new = dat_ger %>% select(date, icu=icu_new) %>%
#     mutate(icu_lwr=NA, icu_upr=NA, type="Beobachtet") %>%
#     rbind(dat_ger_res %>% select(date, icu=new_icu_med, icu_lwr=new_icu_ci_lwr,
#                                  icu_upr=new_icu_ci_upr) %>% mutate(type="Aus Belegung")) %>%
#     rbind(dat_ger_sm %>% select(date, icu=icu_new_smooth_ex, icu_lwr=icu_new_smooth_ex_lwr,
#                                 icu_upr = icu_new_smooth_ex_upr) %>% mutate(type="Aus beobachteten\nNeuaufnahmen")) %>%
#     filter(date>=ymd("2020-10-23")) %>%
#     ggplot() + 
#     geom_line(aes(date, icu, col=type, lty=type)) +
#     geom_ribbon(aes(date, ymin=icu_lwr, ymax=icu_upr, fill = type), alpha=.2) +
#     scale_linetype_manual(values = c("Aus beobachteten\nNeuaufnahmen"=1, "Aus Belegung"=1, "Beobachtet"=2)) +
#     scale_color_manual(values = c("Aus beobachteten\nNeuaufnahmen"="darkgreen", "Aus Belegung"="purple", "Beobachtet"="black")) +
#     scale_fill_manual(values = c("Aus beobachteten\nNeuaufnahmen"="darkgreen", "Aus Belegung"="purple", "Beobachtet"="black")) +
#     ylab("Neuaufnahmen") + theme(legend.position = "bottom", legend.title = element_blank()) +
#     xlab("Datum") + guides(linetype = FALSE, fill=FALSE)
# 
# ggpubr::ggarrange(plot_bel, plot_new, ncol=2, labels = "AUTO", common.legend = T, legend = "bottom")
# 
# # Perform adjustment for federal states
# # Derive correction factor lambda_t
# dat_ger = dat_ger %>% left_join(dat_ger_res) %>% left_join(dat_ger_sm) %>%
#     mutate(lambda_t = icu_new_smooth_ex_lwr/new_icu_med)
# 
# # Adjust estimates for federal states
# dat_fed = dat_fed %>% right_join(dat_ger %>% select(date, lambda_t)) %>%
#     mutate(med_adj=med*lambda_t,
#            q025_adj = q025*lambda_t,
#            q975_adj = q975*lambda_t)
# 
# # Plot results
# dat_fed %>% select(date, state, med, q025, q975) %>%
#     mutate(type = "unadj") %>%
#     rbind(dat_fed %>% select(date, state, med=med_adj, q025=q025_adj, q975=q975_adj) %>%
#               mutate(type = "adj")) %>%
#     mutate(type=factor(type, levels = rev(c("adj", "unadj")), labels = rev(c("Model Belegung adjusted",
#                                                                              "Model Belegung")))) %>%
#     ggplot() +
#     geom_vline(aes(xintercept=date), 
#                data = tibble(date=c(ymd("2021-03-01"),
#                                     ymd("2021-02-01"),
#                                     ymd("2021-01-01"),
#                                     ymd("2020-12-01"),
#                                     ymd("2020-11-01"),
#                                     ymd("2020-10-01"),
#                                     ymd("2020-09-01"))), 
#                lty=1, col="lightgrey") +
#     geom_line(aes(date, med, col = type)) +
#     geom_ribbon(aes(date, ymin=q025, max=q975, fill = type), alpha=.3) +
#     facet_wrap(~state, scales = "free_y") +
#     ylab("Neuaufnahmen") +
#     theme(legend.position = "bottom", legend.title = element_blank())