rm(list=ls())

library(data.table)
library(dplyr)
library(glmnet)
library(cowplot)
library(ggplot2)
library(stringr)
library(ppcor)
library(hdi)
library(xtable)
library(reshape)

if(getElement(Sys.info(), "sysname") == "Windows"){
    PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
    PROJECT_FOLDER <- "/home/brendon/Documents/GitHub/MIZ_project"
}

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

# don't do the regression again if a file already exists, since the effect
# sizes are going to change every time
if(!file.exists( file.path(
    PROJECT_FOLDER,
    "Classifications/regression_results.rda")
))
{
    # import the pre-prepared regression data
    Imported <- readRDS(file.path(
            PROJECT_FOLDER, 
            "Classifications/regression_data.rda"
        )) %>%
        data.table() %>%
        # changing the None/Partial/Entire values to 0/1/2
        dplyr::mutate(
            EIs = factor_to_num(EIs),
            LIs = factor_to_num(LIs),
            VIs = factor_to_num(VIs),
            group_0_to_4 = eval(cohorts(0, 4)),
            group_5_to_19 = eval(cohorts(5, 19)),
            group_20_to_49 = eval(cohorts(20, 49)),
            group_50_to_64 = eval(cohorts(50, 64)),
            group_65_plus = eval(cohorts(65, 99, "cohort_100_plus"))
        ) %>%
        # adding interaction terms
        dplyr::mutate(
            interaction_children_school_closures = EIs*group_5_to_19,
            
            interaction_lockdown_0_to_4 = LIs*group_0_to_4,
            interaction_lockdown_5_to_19 = LIs*group_5_to_19,
            interaction_lockdown_20_to_49 = LIs*group_20_to_49,
            interaction_lockdown_50_to_64 = LIs*group_50_to_64,
            interaction_lockdown_65_plus = LIs*group_65_plus

        ) %>%
        dplyr::select(
            -geometry, -PROV_vaxx_FULL, -starts_with("cohort"), -airports, 
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
            alias(lm(incidence ~ ., data = reg_data_here))[["Complete"]] != "2", 
            arr.ind=TRUE
        )))
        
        reg_data_here <- reg_data_here %>% 
            dplyr::select(-any_of(aliased_columns)) %>%
            dplyr::filter(! incidence %in% outlying_incidences)
        
        added_variable_plot <- function(data_tab=reg_data_here, response="incidence", covar="is_miz_moderate")
        {
            if(response == covar) return(data.table())
            if(! response %in% names(data_tab)) return(data.table())
            if(! covar %in% names(data_tab)) return(data.table())
            
            # writeLines(sprintf("\tCovariate: %s", covar))
            
            # the Y regression
            Y_x_full <- as.matrix(data_tab %>% dplyr::select(-dplyr::any_of(c(response, covar))))
            Y_y_full <- as.matrix(data_tab[[response]], ncol=1)
            
            Y_cv_model <- cv.glmnet(Y_x_full, Y_y_full, alpha = 0, family = "gaussian")
            Y_glm_fit <- glmnet(Y_x_full, Y_y_full, alpha = 0, lambda = Y_cv_model$lambda.min)
            Y_ridge_predictions <- predict(Y_glm_fit, s = Y_cv_model$lambda.min, newx = Y_x_full)

            Y_regression_residuals <- Y_y_full - Y_ridge_predictions
            
            # print(Y_regression_residuals)
            # print("here")

            # the X regression
            X_x_full <- as.matrix(data_tab %>% dplyr::select(-dplyr::any_of(c(covar, response))))
            X_y_full <- as.matrix(data_tab[[covar]], ncol=1)
            
            output <- tryCatch(
                {
                    X_cv_model <- cv.glmnet(X_x_full, X_y_full, alpha = 0, family = "gaussian")
                    X_glm_fit <- glmnet(X_x_full, X_y_full, alpha = 0, lambda = X_cv_model$lambda.min)
                    X_ridge_predictions <- predict(X_glm_fit, s = X_cv_model$lambda.min, newx = X_x_full)
                    
                    X_regression_residuals <- X_y_full - X_ridge_predictions
                    
                    all_resid <- data.table(
                        regressor =  covar,
                        wave = wave_number,
                        x = as.numeric(X_regression_residuals),
                        y = as.numeric(Y_regression_residuals)
                    )
                },
                error = function(e){ writeLines(sprintf(
                    "\twave: %s, covariate: %s,\n\t\terror: %s.\n", 
                    wave_number, covar, e
                )) },
                warning = function(w) message(w)
            )
            
            return(output)
        }
        
        added_var_data[[wave_number]] <- rbindlist(lapply(
            reg_data_here %>% names %>% .[. != "incidence"],
            \(xx) added_variable_plot(reg_data_here, "incidence", xx)
        ))
        
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

Regression_Data <- readRDS(file.path(
    PROJECT_FOLDER,
    "Classifications/regression_results.rda"
))

stop()

############## INFO FOR LATEX TABLES

pretty_variable_names <- function(tab) return( tab %>% 
dplyr::mutate(pretty_regressor = factor(regressor, levels = c(
    "LTCHs", "previous_wave_incidence", "group_0_to_4", "group_5-19", 
    "group_20_to_49", "group_50_to_64", "group_65_plus",
    "interaction_vaccination_0_to_4", "interaction_vaccination_5_to_19", 
    "interaction_vaccination_20_to_49", "interaction_vaccination_50_to_64", 
    "interaction_vaccination_65_plus", "EIs",
    "interaction_children_school_closures", "is_cma", "is_ca", 
    "is_miz_strong", "is_miz_moderate", "is_miz_weak", "is_miz_none", 
    "PHU_pop_density", "interaction_popdensity_cma", 
    "interaction_popdensity_strong", "interaction_popdensity_moderate", 
    "interaction_popdensity_weak", "interaction_popdensity_none"
    ))) %>%
dplyr::mutate(pretty_regressor = dplyr::recode(regressor,
    "LTCHs" = "LTCH",
    "School Closures" = "$E$",
    "EIs" = "E",
    "group_0_to_4" = "$C_{0-4}$",
    "group_5_to_19" = "$C_{5-19}$",
    "group_20_to_49" = "$C_{20-49}$",
    "group_50_to_64" = "$C_{50-64}$",
    "group_65_plus" = "$C_{\\ge65}$",
    "PHU_pop_density" = "$D$",
    "interaction_children_school_closures" = "$E\\cdot C_{5-19}$",
    "is_cma" = "CMA",
    "is_ca" = "CA",
    "is_miz_strong" = "Strong",
    "is_miz_moderate" = "Moderate",
    "is_miz_weak" = "Weak",
    "is_miz_none" = "None",
    "previous_wave_incidence" = "$Y_{u-1}$",
    "interaction_popdensity_cma" = "$D\\cdot\\text{CMA}$",
    "interaction_popdensity_ca" = "$D\\cdot\\text{CA}$",
    "interaction_popdensity_moderate"="$D\\cdot\\text{MIZ}_{\\text{moderate}}$",
    "interaction_popdensity_strong" = "$D\\cdot\\text{MIZ}_{\\text{strong}}$",
    "interaction_popdensity_weak" = "$D\\cdot\\text{MIZ}_{\\text{weak}}$",
    "interaction_popdensity_none" = "$D\\cdot\\text{MIZ}_{\\text{none}}$",
    "interaction_vaccination_0_to_4" = "$v_{\\text{prov}}\\cdot C_{0-4}$",
    "interaction_vaccination_5_to_19" = "$v_{\\text{prov}}\\cdot C_{5-19}",
    "interaction_vaccination_20_to_49" = "$v_{\\text{prov}}\\cdot C_{20-49}$",
    "interaction_vaccination_50_to_64" = "$v_{\\text{prov}}\\cdot C_{50-64}$",
    "interaction_vaccination_50_to_64" = "$v_{\\text{prov}}\\cdot C_{50-64}$",
    "interaction_vaccination_65_plus" = "$v_{\\text{prov}}\\cdot C_{\\ge65}$",
    "interaction_LTCH_65_plus" = "\\text{LTCH}\\cdot C_{\\ge65}",
    "interaction_LTCHs_65_plus" = "\\text{LTCH}\\cdot C_{\\ge65}"
)))

latex_regression_coefficients <- Reduce(
    function(x, y) merge(x, y, by = c("wave", "regressor"), all = TRUE),
    list(
        Regression_Data$information %>% dplyr::select(regressor, wave) %>% 
            unique(),
        Regression_Data$information %>% filter(type == "Ridge.glmnet") %>%
            dplyr::select(wave, regressor, coefficient) %>%
            setNames(sprintf("Ridge.glmnet.%s", names(.))) %>%
            dplyr::rename(
                wave = Ridge.glmnet.wave,
                regressor = Ridge.glmnet.regressor
            ),
        Regression_Data$information %>% filter(type == "Ridge.hdi") %>%
            dplyr::select(-type) %>%
            setNames(sprintf("Ridge.hdi.%s", names(.))) %>%
            dplyr::rename(wave=Ridge.hdi.wave, regressor=Ridge.hdi.regressor),
        Regression_Data$information %>% filter(type == "OLS") %>% 
            dplyr::select(-type) %>%
            setNames(sprintf("OLS.%s", names(.))) %>%
            dplyr::rename(wave=OLS.wave, regressor=OLS.regressor)
    )) %>%
    dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
    dplyr::select(wave, regressor, matches("coefficient|CI|p.value")) %>%
    dplyr::mutate(
        Ridge.hdi = ifelse(
            Ridge.hdi.p.value < 0.001,
            sprintf(
                "$%.4f\\;(%.4f,\\;%.4f),\\;p < 0.001$",
                Ridge.hdi.coefficient, Ridge.hdi.CI025, Ridge.hdi.CI975
            ),
            sprintf("$%.4f\\;(%.4f,\\;%.4f),\\;p = %.4f$",
                Ridge.hdi.coefficient, Ridge.hdi.CI025, 
                Ridge.hdi.CI975, Ridge.hdi.p.value
            )
        ),
        Ridge.glmnet = sprintf("$%.3f$", Ridge.glmnet.coefficient),
        OLS = ifelse(
            OLS.p.value < 0.001,
            sprintf("$%.4f\\;(%.4f,\\;%.4f),\\;p < 0.001$", 
                OLS.coefficient, OLS.CI025, OLS.CI975
            ),
            sprintf("$%.4f\\;(%.4f,\\;%.4f),\\;p = %.4f$", 
                OLS.coefficient, OLS.CI025, OLS.CI975,
                OLS.p.value
            )
        )
    ) %>%
    dplyr::select(wave, regressor, Ridge.hdi, Ridge.glmnet, OLS)

# latex_regression_coefficients %>% pretty_variable_names() %>% dplyr::select(-regressor) %>% dplyr::rename(regressor = pretty_regressor) %>% dplyr::relocate(regressor) %>% dplyr::filter(wave == 2) %>% dplyr::select(-wave, -Ridge.glmnet) %>% xtable %>% print.xtable(include.rownames=FALSE)

fwrite(
    latex_regression_coefficients %>% 
        pretty_variable_names() %>%
        dplyr::select(-regressor) %>%
        dplyr::rename(regressor = pretty_regressor) %>%
        dplyr::relocate(regressor) %>%
        dplyr::filter(wave == 2) %>%
        dplyr::select(-wave, -Ridge.glmnet), 
    file = file.path(
        PROJECT_FOLDER, 
        "Classifications/latex_regression_coefficients_wave_2.csv"
    )
)

fwrite(
    latex_regression_coefficients %>% 
        pretty_variable_names() %>% 
        dplyr::arrange(regressor), 
    file = file.path(
        PROJECT_FOLDER, 
        "Classifications/latex_regression_coefficients.csv"
    )
)

latex_regression_descriptives <- Regression_Data$data %>%
    dplyr::select(
        -province, -HRUID2018, -HR, -pruid, -wave, -incidence, -PROV_population,
        -matches("interaction|previous|vaxx"), -VIs, -EIs, -LIs
    ) %>%
    unique() %>%
    dplyr::mutate(
        PHU_population = PHU_population/1000,
        group_0_to_4 = group_0_to_4/100,
        group_5_to_19 = group_5_to_19/100,
        group_20_to_49 = group_20_to_49/100,
        group_50_to_64 = group_50_to_64/100,
        group_65_plus = group_65_plus/100
    ) %>%
    dplyr::summarise(across(
        names(.),
        \(x) sprintf(
            "%.3f (%.3f), range: (%.3f,\\;%.3f), IQR: %.3f",
            mean(x), sd(x), min(x), max(x), IQR(x)
        )
    )) %>%
    as.list %>%
    data.table(regressor = names(.), description = .) %>%
    dplyr::mutate(regressor = dplyr::recode(regressor,
        "PHU_population (x 1000)" = "Population",
        "PHU_pop_density" = "Population Density",
        "group_0_to_4 (x 100)" = "$C_{0-4}$",
        "group_5_to_19 (x 100)" = "$C_{5-19}$",
        "group_20_to_49 (x 100)" = "$C_{20-49}$",
        "group_50_to_64 (x 100)" = "$C_{50-64}$",
        "group_65_plus (x 100)" = "$C_{\\ge65}$",
        "is_cma" = "CMA",
        "is_ca" = "CA",
        "is_miz_strong" = "MIZ textsubscript{strong}",
        "is_miz_moderate" = "MIZ textsubscript{mod}",
        "is_miz_weak" = "MIZ textsubscript{weak}",
        "is_miz_weak" = "MIZ textsubscript{weak}",
        "is_miz_none" = "MIZ textsubscript{none}"
    ))
fwrite(
    latex_regression_descriptives, 
    file = file.path(
        PROJECT_FOLDER, 
        "Classifications/latex_regression_descriptives.csv"
    )
)

latex_vif_tables <- Regression_Data$VIF %>%
    dplyr::mutate(value = sprintf("%.3f", value)) %>%
    dplyr::relocate(wave, regressor, value) %>%
    pretty_variable_names() %>%
    dplyr::select(-regressor) %>%
    dplyr::relocate(wave, pretty_regressor)
  
fwrite(
    latex_vif_tables, 
    file.path(PROJECT_FOLDER, "Classifications/regression_vif_tables.csv")
)

############# GRAPH PLOTTING CODE
    
Residuals <- rbind(
        data.table(
            wave = Regression_Data$predictions$wave, 
            regression = "Ridge Regression", 
            incidence = Regression_Data$predictions$incidence,
            value = Regression_Data$predictions[, incidence-glmnet]
        ),
        data.table(
            wave=Regression_Data$predictions$wave, 
            regression="OLS Regression", 
            incidence = Regression_Data$predictions$incidence,
            value=Regression_Data$predictions[, incidence-lm]
        )
    ) %>%
    dplyr::mutate( 
        # wave = paste0("Wave ", wave),
        full_title = sprintf("Wave %i - %s", wave, regression)
    )

Residual_plot <- ggplot(Residuals, aes(x=incidence, y=value)) +
    geom_hline(yintercept=0) +
    geom_point(colour="blue", size=2) +
    facet_wrap(full_title~., scale="free", ncol=2) +
    theme_bw() +
    theme(
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        strip.text=  element_text(size=13)
    ) +
    labs(x="Predicted Incidence", y="Residual")

ggsave(
    Residual_plot, 
    file = file.path(PROJECT_FOLDER, "Graphs/residuals_plot.png"), 
    width=9, height=8
)

pl_MSEs <- ggplot(
        Regression_Data$MSE %>% dplyr::mutate(wave = paste0("Wave ", wave)), 
        aes(x=log10(lambda), y=meann, ymin=lower, ymax=upper)
    ) +
    geom_ribbon(fill="red", alpha=0.1) +
    geom_vline(
        data = Regression_Data$optimal_lambdas %>% 
            dplyr::mutate(wave = paste0("Wave ", wave)), 
        aes(xintercept=log10(optimal.lambda)),
        colour="black"
    ) +
    geom_point(size=1, colour="red") +
    facet_grid(wave~., scale="free_y") +
    theme_bw() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 13)
    ) +
    labs(x="Log(lambda)", y="Ridge Regression MSE") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

ggsave(
    pl_MSEs, 
    file = file.path(PROJECT_FOLDER, "Graphs/MSE_plot.png"), 
    height=7, width=11
)

Coeff_vs_Lambda <- rbindlist(lapply (
    1:length(Regression_Data$fits),
    function(xx)
    {
        Regression_Data$fits[[xx]] %>% 
            coef %>% 
            as.matrix %>% 
            as.data.frame %>% 
            dplyr::mutate(regressor=rownames(.)) %>% 
            reshape::melt(id="regressor") %>%
            dplyr::mutate(
                variable = as.numeric(gsub('s', '', variable)),
                wave = paste0("Wave ", xx)
            ) %>%
            merge(Regression_Data$fits[[4]]$lambda %>% 
                    data.table(lambda=., variable=(1:length(.))-1)) %>%
            dplyr::filter(!grepl("intercept", tolower(regressor)))
    }
))

pl_coeff_lambda <- ggplot(
        Coeff_vs_Lambda, 
        aes(x=lambda, y=value, colour=regressor)
    ) +
    scale_x_log10() +
    geom_line() +
    labs(x="Lambda", y="Coefficients") +
    facet_grid(wave~., scale="free_y") +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 13)
    )
    
ggsave(
    pl_coeff_lambda, 
    file = file.path(PROJECT_FOLDER, "Graphs/coeffs_vs_lambda.png"), 
    height = 7, width = 11
)

# # to find the culprits for the large VIFs, check the table of correlations
# # pcor(x_full)

Coefficients <- Regression_Data$information %>%
    dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
    # dplyr::filter(type == "OLS") %>%
    # dplyr::filter(type == "Ridge.glmnet") %>%
    dplyr::mutate(signif=ifelse(p.value<0.05, TRUE, FALSE)) %>%
    dplyr::mutate(graph_factor = unlist(lapply(
        regressor,
        \(xx)
        {
            if(grepl("ca|cma|miz|strong|moderate|weak|none|density", tolower(xx))) 
                return("Remoteness")
            if(grepl("closure|Is", xx)) return("Interventions")
            if(grepl("group", tolower(xx))) return("Cohort")
            if(grepl("lockdown|vaccination", tolower(xx))) 
                return("Vaccination")
            return("General")
        }
    ))) %>%
    dplyr::mutate(graph_factor = factor(graph_factor)) %>%
    pretty_variable_names()

# example report
# https://arxiv.org/pdf/2111.12272.pdf

pl_regression <- ggplot(
        Coefficients %>% 
            dplyr::filter(type %in% c("Ridge.hdi")) %>% 
            # dplyr::filter(graph_factor == "Interventions") %>%# , "OLS"
            dplyr::mutate(wave = paste0("Wave ", wave)) %>%
            dplyr::mutate(type = dplyr::recode(type, "Ridge.hdi" = "Ridge")),
        aes(
            x=str_wrap(regressor, 20), y=abs(coefficient), 
            group=type, colour=type, shape=type, fill=type
        )
    ) +
    geom_point(size=2) +
    # geom_errorbar(aes(ymin=CI025, ymax=CI975)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_grid(wave~graph_factor, scales="free", space="free_x") +# graph_factor
    theme_bw() +
    theme(
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=15),
        strip.text = element_text(size=13),
        # legend.position = "none"
        # panel.spacing.y = unit(1, "lines")
    ) +
    labs(x="Regressor", y="Coefficients") +
    guides(
        fill = guide_legend(title = "Regression\nType"),
        colour = guide_legend(title = "Regression\nType"),
        shape = guide_legend(title = "Regression\nType")
    )

 ggsave(
    pl_regression,
    file = file.path(PROJECT_FOLDER, "Graphs/regression_coefficients_by_category.png"),
    width=10, height=7
)

pl_regression_by_wave <-  ggplot(
        Regression_Data$information %>% 
            dplyr::filter(type %in% c("Ridge.hdi")) %>% # "OLS", 
            dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
            pretty_variable_names() %>%
            dplyr::rename("Regression" = "type") %>%
            dplyr::mutate(
                Regression = dplyr::recode(Regression, "Ridge.hdi" = "Ridge")
            ),
        aes(
            x=wave, y=coefficient, ymin=CI025, ymax=CI975, 
            group=Regression, fill=Regression, colour=Regression
        )
    ) +
    geom_point() +
    geom_errorbar(aes(ymin=CI025, ymax=CI975)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_wrap(regressor~., scale="free", ncol=4) +
    theme_bw() +
    theme(
        
    ) +
    labs(x="Epidemiological Wave", y="Regression Coefficient")

ggsave(
    pl_regression_by_wave, 
    file = file.path(PROJECT_FOLDER, "Graphs/regression_by_wave.png"),
    height=15, width=10 
)

graph_parsing_sugar <- function(xx)
{return( xx %>%
    dplyr::mutate(pretty_regressor = dplyr::recode(regressor,
       "is_cma" ="(R1)~CMA", 
       "is_ca" = "(R2)~CA",
       "is_miz_strong" = "(R3)~Strong",
       "is_miz_moderate" = "(R4)~Moderate", 
       "is_miz_weak" = "(R5)~Weak",
       "is_miz_none" = "(R6)~None",
       "LTCHs" = "(R7)~LTCH",
       "group_0_to_4" = "(A1)~C['0-4']",
       "group_5_to_19" = "(A2)~C['5-19']",
       "group_20_to_49" = "(A3)~C['20-49']",
       "group_50_to_64" = "(A4)~C['50-64']",
       "group_65_plus" = "(A5)~C['\u2265 65']",
       # "(D1)~D\U00B7" - I can;t find out how to get the centre dot
       # without an error, so will fix this later on
       "previous_wave_incidence" = "(P)~Y['u-1']",
       "PHU_pop_density" = "(D0)~D",
       "interaction_popdensity_cma" = "(D1)~D:CMA", 
       "interaction_popdensity_ca" = "(D2)~D:CA",
       "interaction_popdensity_strong" = "(D3)~D:MIZ['strong']",
       "interaction_popdensity_moderate" = "(D4)~D:MIZ['mod']", 
       "interaction_popdensity_weak" = "(D5)~D:MIZ['weak']",
       "interaction_popdensity_none" = "(D6)~D:MIZ['none']", 
       "EIs" = "(S0)~E",
       "interaction_children_school_closures" = "(S1)~E:C['5-19']",
       "interaction_vaccination_0_to_4" = "(V1)~v['prov']:C['0-4']",
       "interaction_vaccination_5_to_19" = "(V2)~v['prov']:C['5-19']",
       "interaction_vaccination_20_to_49" = "(V3)~v['prov']:C['20-49']",
       "interaction_vaccination_50_to_64" = "(V4)~v['prov']:C['50-64']",
       "interaction_vaccination_65_plus" = "(V5)~v['prov']:C['\u2265 65']"
    ))
)}

pl_regression_by_wave_hdi_only <- ggplot(
        Regression_Data$information %>% 
            dplyr::filter(type %in% c("Ridge.hdi")) %>%
            dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
            graph_parsing_sugar() %>%
            dplyr::rename("Regression" = "type") %>%
            dplyr::mutate(
                wave = factor(wave, 1:4),
                Regression = dplyr::recode(Regression, "Ridge.hdi" = "Ridge")
            ),
        aes(x=wave, y=coefficient, ymin=CI025, ymax=CI975)
        ) +
    geom_point() +
    geom_errorbar(aes(ymin=CI025, ymax=CI975), colour="blue") +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_wrap(pretty_regressor~., scale="free_y", ncol=4, labeller=label_parsed) +
    theme_bw() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15)
    ) +
    labs(x="Epidemiological Wave", y="Regression Coefficient")

ggsave(
    pl_regression_by_wave_hdi_only, 
    file = file.path(PROJECT_FOLDER, "Graphs/regression_by_wave_hdi_only.png"),
    height=15, width=10 
)

Added_Variable_Data <- Regression_Data$added_var_data %>%
    rbindlist() %>%
    dplyr::group_by(wave, regressor) %>%
    dplyr::mutate(
        gradient = lm(y~x)$coefficients[["x"]],
        # # the intercept is always zero for these plots
        # intercept = lm(y~x)$coefficients[["(Intercept)"]]
    ) %>% 
    graph_parsing_sugar() %>%
    dplyr::mutate(pos_gradient = ifelse(gradient>0, "positive", "negative"))

latex_gradients <- Added_Variable_Data %>% 
    data.table %>% 
    dplyr::filter(wave==2) %>% 
    dplyr::select(-x, -y) %>% 
    unique %>% 
    dplyr::arrange(-abs(gradient)) %>%
    pretty_variable_names %>% 
    dplyr::select(pretty_regressor, gradient) %>% 
    # dplyr::mutate(gradient = round(gradient, 4)) %>%
    xtable(digits=4)

pl_added_variables <- ggplot(
        Added_Variable_Data %>% dplyr::filter(wave == 2),
        aes(x=x, y=y)
    ) +
    geom_point(aes(colour=pos_gradient)) + 
    # geom_smooth(method='lm', formula=y~x) +
    geom_abline(aes(intercept=intercept, slope=gradient)) +
    facet_wrap(pretty_regressor~., labeller=label_parsed, ncol=4) + # , scale="free_x"
    theme_bw() +
    scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
    labs(x=expression("e( X"['i']*" | X )"), y="e( Y | X )") +
    theme(
        # legend.key.size = unit(5, "line"),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=15),
        # legend.position = "bottom",
        legend.position = c(0.95,0.01),
        legend.justification = c(1,0),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        
    ) +
    guides(colour=guide_legend(title="Gradient"))

ggsave(
    pl_added_variables,
    file = file.path(PROJECT_FOLDER, "Graphs/regression_added_variable_plots.png"),
    height=15, width=10
)
 