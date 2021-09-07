# library(data.table)
# library(MASS)
# library(dplyr)
# library(ggplot2)
# 
# 
# PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(PROJECT_FOLDER)
# 
variant_table <- fread("variant_dataset.csv") %>%
    dplyr::filter(!is.na(new_tests) & new_tests>0 & !is.na(alpha)) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(
        rescaled_population = (popestimate2019 - mean(popestimate2019))/sd(popestimate2019),
        rescaled_new_tests = (new_tests - mean(new_tests))/sd(new_tests),
        state = factor(state)
    )
    dplyr::mutate(
        log_new_tests = log(new_tests),
        log_popestimate2019 = log(POPESTIMATE2019),
    ) %>%
    dplyr::mutate(
        rescaled_log_new_tests = (log_new_tests - mean(log_new_tests))/sd(new_tests),
        rescaled_log_population = (log_popestimate2019 - mean(log_popestimate2019))/sd(log_popestimate2019)
    )

# fit = glmer(new_case ~ alpha +
#         # offset(log_new_tests) +
#             rescaled_new_tests +
#         # offset(log_popestimate2019) +
#             rescaled_population +
#         (1|state),
#     family = "poisson",
#     data = variant_table,
#     control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

library("lme4")
library("numDeriv")
library("RCurl") ## to source() from Github
library("ggplot2"); theme_set(theme_bw())
library("reshape2")
library("plyr")
library("RColorBrewer")

source("SO_23478792_dat.R")
df$SUR.ID <- factor(df$SUR.ID)
df$replicate <- factor(df$replicate)
Rdet <- cbind(df$ValidDetections,df$FalseDetections)
Unit <- factor(1:length(df$ValidDetections))