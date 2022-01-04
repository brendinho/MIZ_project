rm(list=ls())

library(data.table)
library(dplyr)
library(glmnet)
# library(caret)
library(ggplot2)
# library(car)
library(stringr)
library(ppcor)
library(hdi)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
# PROJECT_FOLDER <- "/home/bren/Documents/GitHub/MIZ_project"

source(sprintf("%s/function_header.R", PROJECT_FOLDER))

## things to fix:
## 
## 1) change the add_HRs function to equally divide the numerical columns instead of copying the values directly (maybe through an optional function argument)
## 2) figure out why there is no Saskatchewan data for the first wave

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
            if(grepl("none|absent|no|false|0", tolower(x))) return(0)
            if(grepl("partial|some|most", tolower(x))) return(1)
            if(grepl("whole|entire|complete|present|yes|true|1", tolower(x))) return(2)
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

if(!file.exists( file.path(PROJECT_FOLDER, "CaseDataTables/temp_cleaned_regression_data.csv") ))
{
    # import the pre-prepared regression data
    Imported <- readRDS(file.path(PROJECT_FOLDER, "CaseDataFiles/regression_data.rda")) %>%
        data.table() %>%
        # changing the None/Partial/Entire values to 0/1/2
        dplyr::mutate(
            EIs = factor_to_num(EIs),
            LIs = factor_to_num(LIs),
            VIs = factor_to_num(VIs),
            # the 0-4 cohort is for reference
            group_0_to_4 = eval(cohorts(0, 4)), #/total_population,
            group_5_to_19 = eval(cohorts(5, 19)), #/PHU_population,
            group_20_to_49 = eval(cohorts(20, 49)), #/PHU_population,
            group_50_to_64 = eval(cohorts(50, 64)), #/PHU_population,
            group_65_plus = eval(cohorts(65, 99, "cohort_100_plus")) # /PHU_population
            # group_0_to_19 = eval(cohorts(0, 19)), #/PHU_population,
            # group_20_to_49 = eval(cohorts(20, 49)), #/PHU_population,
            # group_50_to_64 = eval(cohorts(50, 64)), #/PHU_population
        ) %>%
        # adding interaction terms
        dplyr::mutate(
            # interaction_children_school_closures = EIs*group_0_to_19,
            interaction_children_school_closures = EIs*group_5_to_19,
            
            interaction_popdensity_cma = PHU_pop_density*is_cma,
            interaction_popdensity_ca = PHU_pop_density*is_ca,
            interaction_popdensity_strong = PHU_pop_density*is_miz_strong,
            interaction_popdensity_moderate = PHU_pop_density*is_miz_moderate,
            interaction_popdensity_weak = PHU_pop_density*is_miz_weak,
            interaction_popdensity_none = PHU_pop_density*is_miz_none,
            
            # interaction_LTCH_65_plus = LTCHs*group_65_plus,
            
            interaction_lockdown_0_to_4 = LIs*group_0_to_4,
            interaction_lockdown_5_to_19 = LIs*group_5_to_19,
            interaction_lockdown_20_to_49 = LIs*group_20_to_49,
            interaction_lockdown_50_to_64 = LIs*group_50_to_64,
            interaction_lockdown_65_plus = LIs*group_65_plus
            
            # interaction_lockdown_0_to_19 = LIs*group_0_to_19,
            # interaction_lockdown_20_to_49 = LIs*group_20_to_49,
            # interaction_lockdown_50_to_64 = LIs*group_50_to_64,

        ) %>%
        # taking away the unnecessary columns. later we'll do regression on the entire
        # glmnet matrix, so no stray columns
        dplyr::select(
            -geometry, -PROV_vaxx_FULL, -starts_with("cohort"), -airports, 
            -PHU_area_km2, -PHU_dwellings
        )
        
    # add the incidence and log(incidence) from the previous wave
    Data <- rbind(
        # first wave is unadulterated
        Imported %>% dplyr::filter(wave==1) %>%
            dplyr::mutate(
                previous_wave_incidence = 0,
                # interaction_vaccination_0_to_19 = 0,
                interaction_vaccination_0_to_4 = 0,
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
                interaction_vaccination_0_to_4 = 0,
                interaction_vaccination_5_to_19 = 0,
                # interaction_vaccination_0_to_19 = 0,
                interaction_vaccination_20_to_49 = PROV_vaxx_AL1D*group_20_to_49/PROV_population,
                interaction_vaccination_50_to_64 = PROV_vaxx_AL1D*group_50_to_64/PROV_population,
                interaction_vaccination_65_plus = PROV_vaxx_AL1D*group_65_plus/PROV_population
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
                interaction_vaccination_0_to_4 = PROV_vaxx_AL1D*group_0_to_4/PROV_population,
                interaction_vaccination_5_to_19 = PROV_vaxx_AL1D*group_5_to_19/PROV_population,
                # interaction_vaccination_0_to_19 = PROV_vaxx_AL1D, #/PROV_population*group_0_to_19,
                interaction_vaccination_20_to_49 = PROV_vaxx_AL1D*group_20_to_49/PROV_population,
                interaction_vaccination_50_to_64 = PROV_vaxx_AL1D*group_50_to_64/PROV_population,
                interaction_vaccination_65_plus = PROV_vaxx_AL1D*group_65_plus/PROV_population
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
                interaction_vaccination_0_to_4 = PROV_vaxx_AL1D*group_0_to_4/PROV_population,
                interaction_vaccination_5_to_19 = PROV_vaxx_AL1D*group_5_to_19/PROV_population,
                # interaction_vaccination_0_to_19 = PROV_vaxx_AL1D, #/PROV_population*group_0_to_19,
                interaction_vaccination_20_to_49 = PROV_vaxx_AL1D*group_20_to_49/PROV_population,
                interaction_vaccination_50_to_64 = PROV_vaxx_AL1D*group_50_to_64/PROV_population,
                interaction_vaccination_65_plus = PROV_vaxx_AL1D*group_65_plus/PROV_population
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
    
    fwrite(Data, file=file.path(PROJECT_FOLDER, "CaseDataTables/temp_cleaned_regression_data.csv"))

}

Regression_Data <- fread(file.path(
    PROJECT_FOLDER, 
    "CaseDataTables/temp_cleaned_regression_data.csv"
))

# stop()

# tables for descriptive data
Information <- data.table()
r2s <- data.table()
VIFs <- data.table()
optimal_lambdas <- data.table()
MSEs <- data.table()

start_time <- Sys.time()
for(wave_number in 1:4)
{
    reg_data_here <- Regression_Data %>%
        dplyr::filter(wave == wave_number) %>%
        # information not needed for debugging anymore
        dplyr::select(
            -province, -HRUID2018, -HR, -pruid, -PROV_vaxx_AL1D, -log_previous_wave_incidence, 
            -PROV_population, -PHU_population, -wave
        ) %>%
        # take out the columns with only a single value, since we can't regress on those
        dplyr::select(where(~n_distinct(.) > 1)) %>%
        # for columns that were all the same value, then any interaction columns using that
        # regressor will be duplicated, so we remove them from the end of the table
        .[, which(! duplicated( t( . ) ) ), with = FALSE] %>%
        # the Saskatchewan official; data starts quite a few months behind the other data, so that
        # the previous_wave_incidence columns for the second wave are not filled. so, we'll have to 
        # kill those rows so that they don't unduly influence the regression
        dplyr::filter(
            if("previous_wave_incidence" %in% names(.)) !is.na(previous_wave_incidence) else TRUE 
        )
    
    aliased_columns <- unique(rownames(
        which(
            alias(lm(incidence ~ ., data = reg_data_here))[["Complete"]] != "2", 
            arr.ind=TRUE
        )
    ))
    
    reg_data_here <- reg_data_here %>% dplyr::select(-any_of(aliased_columns))
    
    x_full <- as.matrix(reg_data_here %>% dplyr::select(-incidence))
    y_full <- as.matrix(reg_data_here$incidence, ncol=1)
    
    ridge_models <- glmnet(x_full, y_full, alpha=0)
    
    cv_model <- cv.glmnet(
        x = x_full,
        y = y_full,
        alpha = 0,
        family = "gaussian"
    )
    
    optim.lambda <- cv_model$lambda.min
    
    # glm_fit <- glmnet(x_full, y_full, alpha = 0, lambda = optim.lambda)
    # 
    # y_predicted <- predict(ridge_models, s=optim.lambda, newx=x_full)
    
    # hybrid approach for StackExchange
    # https://stackoverflow.com/questions/60181310/standard-error-of-ridge-logistic-regression-coefficient-using-caret
    
    ridge_fit = ridge.proj(
        x = x_full,
        y = y_full,
        family="gaussian",
        lambda = optim.lambda
    )
    
    # Information.Ridge <- cbind(ridge_fit$bhat, ridge_fit$se, ridge_fit$pval, ridge_fit$sds) %>% 
    #     data.table(regressor = rownames(.)) %>% 
    #     dplyr::rename(Ridge.coefficient=V1, Ridge.se=V2, Ridge.p.value=V3, Ridge.sds=V4) %>% 
    #     dplyr::mutate(
    #         wave = wave_number,
    #         Ridge.CI025 = Ridge.coefficient-1.96*Ridge.se,
    #         Ridge.CI975 = Ridge.coefficient+1.96*Ridge.se
    #     ) %>%
    #     dplyr::relocate(wave, regressor)

    # uniform columns for ggplot grouping
    Information.Ridge <- cbind(ridge_fit$bhat, ridge_fit$se, ridge_fit$pval, ridge_fit$sds
        ) %>%
        data.table(regressor = rownames(.)) %>%
        dplyr::rename(coefficient=V1, se=V2, p.value=V3, sds=V4) %>%
        dplyr::mutate(
            type = "Ridge",
            wave = wave_number,
            CI025 = coefficient-1.96*se,
            CI975 = coefficient+1.96*se
        ) %>%
        dplyr::relocate(wave, regressor)
    
    # glm_fit$beta %>% as.matrix %>% data.table(regressor=rownames(.))
    
    OLS_model <- lm(
        incidence ~ ., 
        data = reg_data_here %>% dplyr::select(-any_of(aliased_columns))
    )
    
    # OLS.confidence <- confint(OLS_model, level=0.95) %>% 
    #     data.table(regressor=rownames(.)) %>% 
    #     dplyr::rename(OLS.CI025=`2.5 %`, OLS.CI975=`97.5 %`) %>% 
    #     dplyr::mutate(wave = wave_number) %>% 
    #     dplyr::relocate(wave, regressor)
    
    # Information.OLS <- summary(OLS_model)$coefficients %>%
    #     data.table(regressor=rownames(.)) %>% 
    #     dplyr::rename(
    #         OLS.coefficient=Estimate, 
    #         OLS.se=`Std. Error`,
    #         OLS.p.value=`Pr(>|t|)`, 
    #         OLS.t=`t value`,
    #     ) %>%
    #     dplyr::mutate(
    #         wave = wave_number,
    #         OLS.CI025 = OLS.coefficient-1.96*OLS.se,
    #         OLS.CI975 = OLS.coefficient+1.96*OLS.se
    #     ) %>%
    #     dplyr::relocate(wave, regressor)

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
            wave = wave_number,
            CI025 = coefficient-1.96*se,
            CI975 = coefficient+1.96*se
        ) %>%
        dplyr::relocate(wave, regressor)
    
    
    vif <- car::vif(OLS_model)
    
    ##### COLLECTING DATA FOR GRAPHS

    Information <- rbind(
        Information,
        Information.Ridge, 
        Information.OLS,
        fill = TRUE
    )

    optimal_lambdas <- rbind(
        optimal_lambdas,
        data.table(wave=wave_number, lambda=optim.lambda)
    )
    
    VIFs <- rbind(VIFs, data.table(
        regressor = names(vif),
        value = as.numeric(vif),
        wave = wave_number
    ))
    
    MSEs <- rbind(
        MSEs,
        data.table(
            wave = wave_number, lambda=cv_model$lambda, meann = cv_model$cvm,
            sdd = cv_model$cvsd, upper = cv_model$cvup, lower = cv_model$cvlo
        )
    )
    
    # r2s <- rbind(
    #     r2s,
    #     data.table(
    #         wave = wave_number,
    #         ridge = r2(y_full, y_predicted),
    #         OLS = summary(OLS_model)$r.squared
    #     )
    # )
}
print(Sys.time() - start_time)

# to find the culprits for the large VIFs, check the table of correlations
# pcor(x_full)

##### plot of the standardised regression coefficients

Coefficients <- Information %>%
    dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
    # dplyr::filter(type == "OLS") %>%
    dplyr::filter(type == "Ridge") %>%
    dplyr::mutate(signif=ifelse(p.value<0.05, TRUE, FALSE)) %>%
    dplyr::mutate(graph_factor = unlist(lapply(
        regressor,
        \(xx)
        {            
            if(grepl("density", tolower(xx))) return("Density")
            if(grepl("ca|cma|miz|strong|moderate|weak|none", tolower(xx))) return("Remoteness")
            if(grepl("closure|Is", xx)) return("Interventions")
            if(grepl("group", tolower(xx))) return("Cohort")
            if(grepl("lockdown", tolower(xx))) return("Lockdown")
            if(grepl("vaccination", tolower(xx))) return("Vaccination")
            return("General")
        }
    ))) %>%
    dplyr::mutate(graph_factor = factor(graph_factor)) %>%
    # dplyr::filter((abs(coefficient)<=100) & (wave != 4)) %>%
    dplyr::mutate( regressor = dplyr::recode(regressor,
         "group_0_to_4" = "0-4",
         "group_5_to_19" = "5-19",
         "group_20_to_49" = "20-49",
         "group_50_to_64" = "50-64",
         "group_65_plus" = "65+",
         
         "total_population" = "Population",
         "total_dwellings" = "Dwellings",
         
         "is_cma" = "CMAs",
         "is_ca" = "CAs",
         "is_miz_strong" = "MIZ Strong",
         "is_miz_moderate" = "MIZ Moderate",
         "is_miz_weak" = "MIZ Weak",
         "is_miz_none" = "MIZ None",
         
         "previous_wave_incidence" = "Y[i-1]",
         "log_previous_wave_incidence" = "log(Y[i-1])",
         
         "interaction_lockdown_5_to_19" = "LI : 05-19",
         "interaction_lockdown_20_to_49" = "LI : 20-49",
         "interaction_lockdown_50_to_64" = "LI : 50-64",
         "interaction_lockdown_65_plus" = "LI : 65+",
         
         "PHU_pop_density" = "Density",
         
         "interaction_popdensity_ca" = "Den : CAs",
         "interaction_popdensity_cma" = "Den : CMAs",
         "interaction_popdensity_strong" = "Den : MIZ Strong",
         "interaction_popdensity_moderate" = "Den : MIZ Moderate",
         "interaction_popdensity_weak" = "Den : MIZ Weak",
         "interaction_popdensity_none" = "Den : MIZ None",
         
         "interaction_children_school_closures" = "EI : 05-19",
         "interaction_LTCH_65_plus" = "LTCHs : 65+",
         
         "interaction_vaccination_20_to_49" = "Vaxx : 20-49",
         "interaction_vaccination_5_to_19" = "Vaxx : 05-19",
         "interaction_vaccination_0_to_4" = "Vaxx : 00-04",
         "interaction_vaccination_50_to_64" = "Vaxx : 50-64",
         "interaction_vaccination_65_plus" = "Vaxx : 65+"
    ))

pl_regression <- ggplot(
        Coefficients,
        aes(x=str_wrap(regressor,20), y=coefficient, group=type, colour=type, fill=type)
    ) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=CI025, ymax=CI975)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_grid(wave~graph_factor, scales="free", space="free_x") +
    theme_bw() +
    theme(
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=15),
        strip.text = element_text(size=13),
        panel.spacing.y = unit(1, "lines")
    ) +
    labs(
        x="Regressor", 
        y="Coefficients", 
        title="Colon (:) for interaction terms. Den (population density), LTCH (long-term care
            homes), Y[i-1] (previous wave incidence), EI (school closure), Vaxx (vaccination)"
    ) +
    guides(
        fill = guide_legend(title = "Regression\nType"), 
        colour = guide_legend(title = "Regression\nType")
    )

print(pl_regression)

stop()
    
ggsave(
        pl_regression,
        file = file.path(PROJECT_FOLDER, "Graphs/regression_coefficients.png"),
        width=15, height=9
    )

pl_significance_grid <- ggplot(Coefficients, aes(x=regressor, y=type)) +
    geom_tile(aes(fill=signif), colour="black") +
    theme(
        axis.title = element_text(size=15),
        axis.text = element_text(size=13),
        axis.text.x = element_text(angle=45, hjust=1),
    ) +
    labs(x="Regressor", y="Regression Type") + 
    guides(
        fill = guide_legend(title = "p<0.05")
    )

ggsave(
    pl_significance_grid,
    file = file.path(PROJECT_FOLDER, "Graphs/significance_grid.png"),
    width=15, height=7
)

##### plot of MSE vs lambda for the unstandardised regression

# pl_MSEs <- ggplot(MSEs, aes(x=log10(lambda), y=meann, ymin=lower, ymax=upper)) +
#     geom_ribbon(fill="red", alpha=0.1) +
#     geom_vline(aes(xintercept=log10(lambda)), optimal_lambdas, colour="black") +
#     geom_point(size=1, colour="red") +
#     facet_grid(wave~., scale="free_y") +
#     theme_bw() +
#     theme(
#         
#     ) +
#     labs(x="log10(lambda)", y="MSE") +
#     scale_x_continuous(expand = c(0,0)) +
#     scale_y_continuous(expand = c(0,0))
# 
# ggsave(pl_MSEs, file=file.path(PROJECT_FOLDER, "Graphs/MSE_plot.png"), height=9, width=12)
#         
        
##################################################

# # cross-validation to find the optimal lambda
# cv_fit <- cv.glmnet(
#     y = y_full,
#     x = x_full,
#     alpha = 0,
#     # lambdas = seq(0, 1000, by=0.1),
#     family= "gaussian",
#     type.measure = "mse"
# )
# 
# lambda_opt <- cv_fit$lambda.min
# 
# ridge_predictions <- predict(glm_fit, s=lambda_opt, newx=x_full)
# 
# print(r2(y_full, ridge_predictions))
# 
# lambda_0_model   <- glmnet(x=x_full, y=y_full, alpha=0, lambda=0)
# lambda_0_predictions  <- predict(lambda_0_model, s=0, newx=x_full)
# 
#     # # USEFUL FOR COMPARING THE COEFFICIENTS THAT WE GET FROM THESE TWO CODE SNIPPETS
# glm_fit <- glmnet(x=x_full, y=y_full, alpha=0, lambda=optim.lambda)
# 
# coeffs <- coef(glm_fit, s="lambda.min")
# Coefficients <- rbind(
#     Coefficients,
#     data.table(
#         wave = wave_number,
#         regressor = coeffs@Dimnames[[1]],
#         coefficient = coeffs@x
#     )
# ) %>% suppressWarnings
#
# # PRELIM CODE FRO THE SAME WEBPAGE
# 
# lambda <- 10^seq(-5,4,length.out=25)
# 
# Ridge1 <- train(
#     x = reg_data_here %>% dplyr::select(-incidence, -EIs),
#     y = y_full, 
#     method = 'glmnet',
#     family="binomial",
#     trControl = trainControl("cv", number = 10),
#     tuneGrid = expand.grid(alpha = 0, lambda = lambda)
# )

        
        
        
        
        
        
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
