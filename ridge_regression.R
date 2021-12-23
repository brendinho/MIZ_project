library(data.table)
library(dplyr)
library(glmnet)
library(caret)
library(ggplot2)

## things to fix:
## 
## 1) change the add_HRs function to equally divide the numerical columns instead of copying the values directly (maybe through an optional function argument)
## 4) include interaction terms in the model
## 5) figure out graphing for the results
## 7) make it a penalised regression (ridge, for example)
## 8) clarify with Seyed whether we're looking at 'only 1 dose' or 'full vaccination' for the vaccine regressor

# calculate r**2
r2 <- function(truth, prediction) {
    sse <- sum((truth - prediction)^2)
    sst <- sum((truth - mean(truth))^2)
    return(1 - sse/sst)
}

factor_to_num <- function(factor_sequence)
{
    return(unlist(lapply(
        factor_sequence,
        \(x){
            if(grepl("none|absent|no|false|0", tolower(x))) return(0)
            if(grepl("partial|some|most", tolower(x))) return(1)
            if(grepl("whole|entire|complete|present|yes|true|1", tolower(x))) return(2)
            return(NA)
        } 
    )))
}

Import <- readRDS(file.path(PROJECT_FOLDER, "CaseDataFiles/regression_data.rda")) %>%
    data.table() %>%
    dplyr::select(-geometry) %>%
    dplyr::mutate(EIs = factor_to_num(EIs), LIs = factor_to_num(LIs), VIs = factor_to_num(VIs)) %>%
    # adding interaction terms
    dplyr::mutate(
        school_closure_child_interaction = EIs*cohort_0_to_4,
        # movement restriction intervention
        MRI_cohort_0_to_4 = LIs*cohort_0_to_4,
        MRI_cohort_5_to_19 = LIs*cohort_5_to_19,
        MRI_cohort_20_to_44 = LIs*cohort_20_to_44,
        MRI_cohort_45_to_64 = LIs*cohort_45_to_64,
        MRI_cohort_65_and_older = LIs*cohort_65_and_older
    )

# add the log of the incidence from the previous wave
Data <- rbind(
    
    # first wave is unadulterated
    Import %>% dplyr::filter(wave==1) %>% dplyr::mutate(log_previous_wave_incidence=0),
    
    # second wave with the log(incidence) of the first wave attached
    merge(
        Import %>% dplyr::filter(wave == 2),
        Import %>% 
            dplyr::filter(wave == 1) %>% 
            dplyr::mutate(log_previous_wave_incidence = ifelse(incidence==0, 0, log(incidence))) %>%
            dplyr::select(pruid, HRUID2018, log_previous_wave_incidence),
        by=c("pruid", "HRUID2018"),
        all = TRUE
    ),
    
    # third wave with the log(incidence) of the second wave attached
    merge(
        Import %>% dplyr::filter(wave == 3),
        Import %>% 
            dplyr::filter(wave == 2) %>% 
            dplyr::mutate(log_previous_wave_incidence = ifelse(incidence==0, 0, log(incidence))) %>%
            dplyr::select(pruid, HRUID2018, log_previous_wave_incidence),
        by=c("pruid", "HRUID2018"),
        all = TRUE
    ),
    
    # fourth wave with the log(incidence) of the third wave attached
    merge(
        Import %>% dplyr::filter(wave == 4),
        Import %>% 
            dplyr::filter(wave == 3) %>% 
            dplyr::mutate(log_previous_wave_incidence = ifelse(incidence==0, 0, log(incidence))) %>%
            dplyr::select(pruid, HRUID2018, log_previous_wave_incidence),
        by=c("pruid", "HRUID2018"),
        all = TRUE
    )
)

# # MUST DO - check why Saskatchewan as no wave 1 information at all
# merge(
#     Data %>% dplyr::filter(wave==1) %>% dplyr::select(province, HR) %>%
#         dplyr::mutate(wave=1),
#     Data %>% dplyr::filter(wave==2) %>% dplyr::select(province, HR) %>%
#         dplyr::mutate(wave=2),
#     by=c("province", "HR"),
#     all = TRUE
# ) %>% filter(is.na(wave.x) | is.na(wave.y))

Coefficients <- data.table()

for(wave_number in 1:4)
{
    reg_data <- Data %>%
        dplyr::filter(wave == wave_number) %>%
        # information not needed ofr debugging anymore
        dplyr::select(-province, -HRUID2018, -HR, -pruid) %>%
        # take out the columns with only a sing e value, since we can't regress on those
        select(where(~n_distinct(.) > 1)) %>%
        # for columns that were all the same value, then any interaction columns using that
        # regressor will be duplicated, so we remove them from the end of the table
        .[, which(! duplicated( t( . ) ) ), with = FALSE]
    
    tIndices <- sample(1:nrow(Wave_Data), 0.7*nrow(Wave_Data))

    training_set <- reg_data[tIndices,]
    testing_set  <- reg_data[-tIndices,]

    dummies <- dummyVars(
        incidence ~ .,
        data = reg_data
    )

    dummies_for_training <- predict(dummies, newdata = training_set)
    dummies_for_testing  <- predict(dummies, newdata = testing_set)

    # proceed with training the model
    x_train <- as.matrix(dummies_for_training)
    y_train <- training_set$incidence

    # for testing the predictions afterwards
    x_test <- as.matrix(dummies_for_testing)
    y_test <- testing_set$incidence

    # find the optimal lambda hyperparameter value
    # alpha = 0 for ridge regression
    lambda_options <- 10**seq(3, -3, by = -.1)
    ridge_regressions <- glmnet(
        x_train, y_train,
        nlambda = length(lambda_options),
        alpha = 0,
        family = 'gaussian',
        lambda = lambda_options
    )

    glm_fit <- cv.glmnet(x_train, y_train, alpha=0, lambda=lambda_options)

    optimal_lambda = glm_fit$lambda.min

    # predictions with the training data
    training_predictions <- predict(ridge_regressions, s=optimal_lambda, newx = x_train)

    # predictions with the testing data
    testing_predictions <- predict(ridge_regressions, s=optimal_lambda, newx = x_test)

    r2(y_train, training_predictions)
    r2(y_test,  testing_predictions)

    coeffs <- coef(glm_fit, s="lambda.min")
    
    Coefficients <- rbind(
        Coefficients,
        data.table(
            regressor = coeffs@Dimnames[[1]], 
            coefficient = coeffs@x, 
            wave = wave_number
        )
    ) %>% suppressWarnings
}

pl_regression <- ggplot(
        Coefficients %>% dplyr::mutate( regressor = dplyr::recode(regressor,
            "(Intercept)" = "Intercept",
            "cohort_0_to_4" = "0-4",
            "cohort_5_to_19" = "5-19",
            "cohort_20_to_44" = "20-44",
            "cohort_45_to_64" = "45-64",
            "cohort_65_and_older" = "65+",
            "total_population" = "Population",
            "total_dwellings" = "Dwellings",
            "is_cma" = "CMAs",
            "is_ca" = "CAs",
            "is_miz_strong" = "Strong",
            "is_miz_moderate" = "Moderate",
            "is_miz_weak" = "Weak",
            "is_miz_none" = "None",
            "airports" = "Airports",
            "school_closure_child_interaction" = "EIs : 5-19",
            "MRI_cohort_0_to_4" = "LIs : 0-4",
            "MRI_cohort_5_to_19" = "LIs : 5-19",
            "MRI_cohort_20_to_44" = "LIs : 20-44",
            "MRI_cohort_45_to_64" = "LIs : 45-66",
            "MRI_cohort_65_and_older" = "LIs : 65+",
            "log_previous_wave_incidence" = "log(Y[i-1])"
        )),
        aes(x=str_wrap(regressor,20), y=log(coefficient))
    ) +
    geom_point(size=5) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_grid(wave~.) +
    theme_bw() +
    theme(
        axis.text = element_text(size=15),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=15),
        strip.text = element_text(size=15)
    ) +
    labs(x="Regressor", y="log(Coefficient)")

ggsave(
        pl_regression, 
        file = file.path(PROJECT_FOLDER, "Graphs/regression_coefficients.png"),
        width=15, height=10
    )


 # the_model <- lm(
#     incidence ~ is_cma + is_ca + is_miz_strong + is_miz_moderate + is_miz_weak + is_miz_none +
#         airports + LTCHs + 
#         VIs_AL1D + EIs + LIs +
#         # total_dwellings + I(log(total_population)) +
#         cohort_0_to_4 + cohort_5_to_19 + cohort_20_to_44 + cohort_45_to_64 + cohort_65_and_older +
#         total_dwellings,
#     data = Data[wave == 1]
#     
# )
