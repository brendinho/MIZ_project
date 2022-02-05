rm(list=ls())

library(data.table)
library(dplyr)
library(glmnet)
library(ggplot2)
library(stringr)
library(ppcor)
library(hdi)
library(xtable)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)

source(sprintf("%s/function_header.R", PROJECT_FOLDER))

# calculate r**2
r2 <- function(truth, prediction) {
    sse <- sum((truth - prediction)^2)
    sst <- sum((truth - mean(truth))^2)
    return(1 - sse/sst)
}
 # for None/Partial/Entire factors, convert them to 0/1/2
factor_to_num <- function(factor_sequence)
{
     return(unlist(lapply(
        factor_sequence,
        \(x){
            if(grepl("none|absent|no|false|0", tolower(x))) 
                return(0)
            if(grepl("partial|some|most", tolower(x))) 
                return(1)
            if(grepl("whole|entire|complete|present|yes|true|1", tolower(x))) 
                return(2)
            return(NA)
        } 
    )))
}

cohorts <- function(start_age, end_age, ...)
{ # getting the names of added cohorts - I'm just lazy 
    
    col_names = c(
        sprintf(
            "cohort_%s_to_%s", 
            seq(start_age, end_age-4, by=5), 
            seq(start_age+4, end_age, by=5)
        ),
        unlist(list(...))
    )
    return(parse(text = paste(col_names, collapse=" + ")))
}

# don't do the regression again if a results file already exists
# if(!file.exists( file.path(PROJECT_FOLDER, "Classifications/regression_results.rda") ))
{
    # import the pre-prepared regression data
    Imported <- readRDS(file.path(
            PROJECT_FOLDER, 
            "Classifications/regression_data.rda"
        )) %>%
        data.table() %>%
        dplyr::mutate(
            daycares = factor_to_num(daycares),
            education = factor_to_num(education),
            work_from_home = factor_to_num(work_from_home)
        ) %>%
        # changing the None/Partial/Entire values to 0/1/2
        dplyr::mutate(
            group_0_to_4 = eval(cohorts(0, 4)),
            group_5_to_19 = eval(cohorts(5, 19)),
            group_20_to_49 = eval(cohorts(20, 49)),
            group_50_to_64 = eval(cohorts(50, 64)),
            group_65_plus = eval(cohorts(65, 99, "cohort_100_plus"))
        ) %>%
        # adding interaction terms
        dplyr::mutate(
            interaction_babies_daycare_closures = daycares*group_0_to_4,
            
            interaction_children_school_closures = education*group_5_to_19,

            # should we discount work from home for essential workers
            interaction_20_to_49_WFH = work_from_home*group_20_to_49,
            interaction_50_to_64_WFH = work_from_home*group_50_to_64,
            interaction_65_plus_WFH = work_from_home*group_65_plus
        ) %>%
        dplyr::select(
            -geometry, -PROV_vaxx_FULL, -starts_with("cohort"),
            -PHU_area_km2, -PHU_dwellings
        )
        
    # add the incidence and log(incidence) from the previous wave
    Regression_Data <- rbind(
        # first wave is unadulterated
        Imported %>% dplyr::filter(wave==1) %>%
            dplyr::mutate(
                previous_wave_incidence = 0,
                interaction_vaccination_5_to_19 = 0,
                interaction_vaccination_20_to_49 = 0,
                interaction_vaccination_50_to_64 = 0,
                interaction_vaccination_65_plus = 0,
            ),
        # second wave with the incidence of the first wave attached
        merge(
            Imported %>% dplyr::filter(wave == 2) %>%
                dplyr::group_by(province, pruid) %>%
                dplyr::mutate(
                interaction_vaccination_5_to_19 = 0,
                interaction_vaccination_20_to_49 = 
                    PROV_vaxx_AL1D*group_20_to_49/PROV_population,
                interaction_vaccination_50_to_64 = 
                    PROV_vaxx_AL1D*group_50_to_64/PROV_population,
                interaction_vaccination_65_plus = 
                    PROV_vaxx_AL1D*group_65_plus/PROV_population
                ),
            Imported %>% 
                dplyr::filter(wave == 1) %>% 
                dplyr::rename(previous_wave_incidence = incidence) %>%
                dplyr::select(pruid, HRUID2018, previous_wave_incidence),
            by=c("pruid", "HRUID2018"),
            all = TRUE
        ),
        # third wave with the incidence of the second wave attached
        merge(
            Imported %>% dplyr::filter(wave == 3) %>%    
                dplyr::mutate(
                interaction_vaccination_5_to_19 = 
                    PROV_vaxx_AL1D*group_5_to_19/PROV_population,
                interaction_vaccination_20_to_49 = 
                    PROV_vaxx_AL1D*group_20_to_49/PROV_population,
                interaction_vaccination_50_to_64 = 
                    PROV_vaxx_AL1D*group_50_to_64/PROV_population,
                interaction_vaccination_65_plus = 
                    PROV_vaxx_AL1D*group_65_plus/PROV_population
                ),
            Imported %>% 
                dplyr::filter(wave == 2) %>% 
                dplyr::rename(previous_wave_incidence = incidence) %>%
                dplyr::select(pruid, HRUID2018, previous_wave_incidence),
            by=c("pruid", "HRUID2018"),
            all = TRUE
        ),
        # fourth wave with the incidence of the third wave attached
        merge(
            Imported %>% dplyr::filter(wave == 4) %>%    
                dplyr::mutate(
                interaction_vaccination_5_to_19 = 
                    PROV_vaxx_AL1D*group_5_to_19/PROV_population,
                interaction_vaccination_20_to_49 = 
                    PROV_vaxx_AL1D*group_20_to_49/PROV_population,
                interaction_vaccination_50_to_64 = 
                    PROV_vaxx_AL1D*group_50_to_64/PROV_population,
                interaction_vaccination_65_plus = 
                    PROV_vaxx_AL1D*group_65_plus/PROV_population
                ),
            Imported %>% 
                dplyr::filter(wave == 3) %>% 
                dplyr::rename(previous_wave_incidence = incidence) %>%
                dplyr::select(pruid, HRUID2018, previous_wave_incidence),
            by=c("pruid", "HRUID2018"),
            all = TRUE
        )
    ) %>%
    dplyr::mutate(
        # if there was zero incidence, just set the log incidence to zero
        log_previous_wave_incidence=ifelse(
            previous_wave_incidence==0, 
            0, 
            log(previous_wave_incidence)
        )
    )
    
    # tables for descriptive data
    covariates <- list()
    Information <- data.table()
    r2s <- data.table()
    VIFs <- data.table()
    optimal_lambdas <- data.table()
    MSEs <- data.table()
    predictions <- data.table()
    cv_models <- list()
    fits <- list()
    correlations <- list()
    added_var_data <- list()
    
    start_time <- Sys.time()
    
    for(wave_number in 1:4)
    {
        writeLines(sprintf("Wave: %s", wave_number))
        
        reg_data_here <- Regression_Data %>%
            dplyr::filter(wave == wave_number) %>%
            # information not needed for debugging anymore
            dplyr::select(
                -province, -HRUID2018, -HR, -pruid, -PROV_vaxx_AL1D, 
                -log_previous_wave_incidence, -PROV_population, -PHU_population, 
                -wave
            ) %>%
            # take out the columns with only a single value
            dplyr::select(where(~n_distinct(.) > 1)) %>%
            # for columns that were all the same value, then any interaction 
            # columns using that regressor will be duplicated, so we remove them
            # from the end of the table
            .[, which(! duplicated( t( . ) ) ), with = FALSE] %>%
            # the Saskatchewan official; data starts quite a few months behind 
            # the other data, so that the previous_wave_incidence columns for 
            # the second wave are not filled. so, we'll have to kill those rows
            # so that they don't unduly influence the regression
            dplyr::filter(
                if("previous_wave_incidence" %in% names(.)) 
                    !is.na(previous_wave_incidence) else TRUE 
            )
        
        outlying_incidences <- unique(boxplot(reg_data_here$incidence)$out)
        
        aliased_columns <- unique(rownames(which(
            alias(lm(incidence ~ ., data = reg_data_here))[["Complete"]] != "0", 
            arr.ind=TRUE
        )))
        
        reg_data_here <- reg_data_here %>% 
            dplyr::select(-any_of(aliased_columns)) %>%
            dplyr::filter(! incidence %in% outlying_incidences)
        
        correlations[[wave_number]] <- Reduce(
            function(x, y) merge(x, y, by="regressor", all=TRUE),
            list(
                reg_data_here %>% 
                    dplyr::select(-dplyr::starts_with("interaction")) %>% 
                    pcor(.) %>% 
                    .$estimate %>%
                    data.table(regressor=rownames(.), .) %>% 
                    dplyr::select(regressor, incidence) %>%
                    dplyr::rename(partial_corr.val = incidence),
                reg_data_here %>% 
                    dplyr::select(-dplyr::starts_with("interaction")) %>% 
                    pcor(.) %>% 
                    .$p.value %>% 
                    round(., 4) %>%
                    data.table(regressor=rownames(.), .) %>% 
                    dplyr::select(regressor, incidence) %>%
                    dplyr::rename(partial_corr.p = incidence),
                reg_data_here %>% 
                    dplyr::select(-dplyr::starts_with("interaction")) %>% 
                    cor(.) %>% 
                    data.table(regressor=rownames(.), .) %>% 
                    dplyr::select(regressor, incidence) %>% 
                    dplyr::rename(normal_cor.val = incidence),
                reg_data_here %>%
                    .[, 
                        lapply(.SD, \(x) round(cor.test(incidence, x)$p.value, 4)), 
                        .SDcols = names(reg_data_here) %>% 
                          .[!grepl("interaction", .)]
                    ] %>% 
                    data.table(regressor = names(.), transpose(.)) %>% 
                    dplyr::rename(normal_corr.p = V1) %>% 
                    dplyr::select(regressor, normal_corr.p)
                )) %>% 
            dplyr::mutate(wave = wave_number) %>%
            dplyr::relocate(wave)
        
        x_full <- as.matrix(reg_data_here %>% dplyr::select(-incidence))
        y_full <- as.matrix(reg_data_here$incidence, ncol=1)
        
        fits[[wave_number]] <- glmnet(x_full, y_full, alpha = 0)
        
        cv_model <- cv.glmnet(
            x = x_full,
            y = y_full,
            alpha = 0,
            family = "gaussian",
            lower = 0,
            upper = 1
        )

        optim.lambda <- cv_model$lambda.min
    
        glm_fit <- glmnet(x_full, y_full, alpha = 0, lambda = optim.lambda)
        ridge_predictions <- predict(glm_fit, s = optim.lambda, newx = x_full)
        
        Information.Ridge.glmnet <- data.table(
            type = "Ridge.glmnet",
            wave = wave_number,
            regressor = glm_fit$beta@Dimnames[[1]],
            coefficient = glm_fit$beta@x
        )
        
        ridge_fit = ridge.proj(
            x = x_full,
            y = y_full,
            family="gaussian",
            lambda = optim.lambda
        )
    
        Information.Ridge.hdi <- cbind(
                ridge_fit$bhat, ridge_fit$se, ridge_fit$pval, ridge_fit$sds
            ) %>%
            data.table(regressor = rownames(.)) %>%
            dplyr::rename(coefficient=V1, se=V2, p.value=V3, sds=V4) %>%
            dplyr::mutate(
                type = "Ridge.hdi",
                wave = wave_number,
                CI025 = coefficient-1.96*se,
                CI975 = coefficient+1.96*se
            ) %>%
            dplyr::relocate(wave, regressor)
        
        # comparison with OLS regression
        OLS_model <- lm(
            incidence ~ ., 
            data = reg_data_here %>% dplyr::select(-any_of(aliased_columns))
        )
        
        OLS.confidence <- confint(OLS_model, level=0.95) %>%
            data.table(regressor=rownames(.)) %>%
            dplyr::rename(OLS.CI025=`2.5 %`, OLS.CI975=`97.5 %`) %>%
            dplyr::mutate(wave = wave_number) %>%
            dplyr::relocate(wave, regressor)
    
        Information.OLS <- summary(OLS_model)$coefficients %>%
            data.table(regressor=rownames(.)) %>% 
            dplyr::rename(
                coefficient = Estimate, 
                se = `Std. Error`,
                p.value = `Pr(>|t|)`, 
                t.value = `t value`,
            ) %>%
            dplyr::mutate(
                type = "OLS",
                wave = wave_number
            ) %>%
            dplyr::relocate(wave, regressor) %>%
            merge(
                confint(OLS_model, level=0.95) %>% 
                    data.table(regressor=rownames(.)) %>% 
                    dplyr::rename(CI025=`2.5 %`, CI975=`97.5 %`)
            )
        
        RSS <- c(crossprod(OLS_model$residuals))
        MSE <- RSS / length(OLS_model$residuals)
        RMSE <- sqrt(MSE)
        
        vif <- car::vif(OLS_model)
        
        ##### COLLECTING DATA

        covariates[[wave_number]] <- paste(names(reg_data_here), collapse=", ")
    
        Information <- rbind(
            Information,
            Information.Ridge.glmnet,
            Information.Ridge.hdi, 
            Information.OLS,
            fill = TRUE
        )
    
        optimal_lambdas <- rbind(
            optimal_lambdas,
            data.table(wave=wave_number, optimal.lambda=optim.lambda)
        )
        
        VIFs <- rbind(VIFs, data.table(
            regressor = names(vif),
            value = as.numeric(vif),
            wave = wave_number
        ))
        
        MSEs <- rbind(MSEs, data.table(
            wave = wave_number, lambda = cv_model$lambda, meann = cv_model$cvm,
            sdd = cv_model$cvsd, upper = cv_model$cvup, lower = cv_model$cvlo
        ))
        
        r2s <- rbind(
            r2s,
            data.table(
                wave = wave_number,
                Ridge.r2 = r2(y_full, ridge_predictions),
                Ridge.RMSE = cv_model$cvm %>%
                    .[which(cv_model$lambda == optim.lambda)] %>%
                    sqrt(),
                OLS.r2 = summary(OLS_model)$r.squared,
                OLS.RMSE = RMSE
            )
        )
        
        predictions <- rbind(
            predictions,
            data.table(
                wave = wave_number,
                incidence = as.numeric(y_full), 
                glmnet = as.numeric(ridge_predictions), 
                lm = unname(predict(OLS_model))
            )
        )
        
        cv_models[[wave_number]] <- cv_model
    }
    
    print(Sys.time() - start_time)

    saveRDS(
        list(
            raw_data = Regression_Data,
            covariates = covariates,
            information = Information,
            optimal_lambdas = optimal_lambdas,
            MSE = MSEs,
            VIF = VIFs,
            goodness = r2s,
            predictions = predictions,
            fits = fits,
            cv = cv_models,
            correlations = correlations,
            added_var_data = added_var_data
        ),
        file=file.path(PROJECT_FOLDER, "Classifications/regression_results.rda")
    )
}

# added_variable_plot <- function(data_tab=reg_data_here, response="incidence", covar="is_miz_moderate")
# {
#     if(response == covar) return(data.table())
#     if(! response %in% names(data_tab)) return(data.table())
#     if(! covar %in% names(data_tab)) return(data.table())
#     
#     # writeLines(sprintf("\tCovariate: %s", covar))
#     
#     # the Y regression
#     Y_x_full <- as.matrix(data_tab %>% dplyr::select(-dplyr::any_of(c(response, covar))))
#     Y_y_full <- as.matrix(data_tab[[response]], ncol=1)
#     
#     Y_cv_model <- cv.glmnet(Y_x_full, Y_y_full, alpha = 0, family = "gaussian")
#     Y_glm_fit <- glmnet(Y_x_full, Y_y_full, alpha = 0, lambda = Y_cv_model$lambda.min)
#     Y_ridge_predictions <- predict(Y_glm_fit, s = Y_cv_model$lambda.min, newx = Y_x_full)
#     
#     Y_regression_residuals <- Y_y_full - Y_ridge_predictions
#     
#     # print(Y_regression_residuals)
#     # print("here")
#     
#     # the X regression
#     X_x_full <- as.matrix(data_tab %>% dplyr::select(-dplyr::any_of(c(covar, response))))
#     X_y_full <- as.matrix(data_tab[[covar]], ncol=1)
#     
#     output <- tryCatch(
#         {
#             X_cv_model <- cv.glmnet(X_x_full, X_y_full, alpha = 0, family = "gaussian")
#             X_glm_fit <- glmnet(X_x_full, X_y_full, alpha = 0, lambda = X_cv_model$lambda.min)
#             X_ridge_predictions <- predict(X_glm_fit, s = X_cv_model$lambda.min, newx = X_x_full)
#             
#             X_regression_residuals <- X_y_full - X_ridge_predictions
#             
#             all_resid <- data.table(
#                 regressor =  covar,
#                 wave = wave_number,
#                 x = as.numeric(X_regression_residuals),
#                 y = as.numeric(Y_regression_residuals)
#             )
#         },
#         error = function(e){ writeLines(sprintf(
#             "\twave: %s, covariate: %s,\n\t\terror: %s.\n", 
#             wave_number, covar, e
#         )) },
#         warning = function(w) message(w)
#     )
#     
#     return(output)
# }
# 
# added_var_data[[wave_number]] <- rbindlist(lapply(
#     reg_data_here %>% names %>% .[. != "incidence"],
#     \(xx) added_variable_plot(reg_data_here, "incidence", xx)
# ))

