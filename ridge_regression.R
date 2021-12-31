rm(list=ls())

library(data.table)
library(dplyr)
library(glmnet)
library(caret)
library(ggplot2)
library(car)
library(stringr)
library(ppcor)

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

# import the pre-prepared regression data
Imported <- readRDS(file.path(PROJECT_FOLDER, "CaseDataFiles/regression_data.rda")) %>%
    data.table() %>%
    # changing the None/Partial/Entire values to 0/1/2
    dplyr::mutate(
        EIs = factor_to_num(EIs),
        LIs = factor_to_num(LIs),
        VIs = factor_to_num(VIs),
        # the 0-4 cohort is for reference
        # group_0_to_4 = eval(cohorts(0,4))/total_population,
        group_5_to_19 = eval(cohorts(5, 19))/PHU_population,
        group_20_to_49 = eval(cohorts(20, 49))/PHU_population,
        group_50_to_64 = eval(cohorts(50, 64))/PHU_population,
        group_65_plus = eval(cohorts(65, 99, "cohort_100_plus"))/PHU_population
    ) %>%
    # adding interaction terms
    dplyr::mutate(
        interaction_children_school_closures = EIs*group_5_to_19,
        
        interaction_popdensity_cma = PHU_pop_density*is_cma,
        interaction_popdensity_ca = PHU_pop_density*is_ca,
        interaction_popdensity_strong = PHU_pop_density*is_miz_strong,
        interaction_popdensity_moderate = PHU_pop_density*is_miz_moderate,
        interaction_popdensity_weak = PHU_pop_density*is_miz_weak,
        interaction_popdensity_none = PHU_pop_density*is_miz_none,
        
        interaction_LTCH_65_plus = LTCHs*group_65_plus,
        
        # interaction_lockdown_0_to_4 = LIs*group_0_to_4,
        interaction_lockdown_5_to_19 = LIs*group_5_to_19,
        interaction_lockdown_20_to_49 = LIs*group_20_to_49,
        interaction_lockdown_50_to_64 = LIs*group_50_to_64,
        interaction_lockdown_65_plus = LIs*group_65_plus
    ) %>%
    # taking away the unnecessary columns. later we'll do regression on the entire\
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
            interaction_vaccination_20_to_49 = PROV_vaxx_AL1D/PROV_population*group_20_to_49,
            interaction_vaccination_50_to_64 = PROV_vaxx_AL1D/PROV_population*group_50_to_64,
            interaction_vaccination_65_plus = PROV_vaxx_AL1D/PROV_population*group_65_plus,
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
            interaction_vaccination_5_to_19 = PROV_vaxx_AL1D/PROV_population*group_5_to_19,
            interaction_vaccination_20_to_49 = PROV_vaxx_AL1D/PROV_population*group_20_to_49,
            interaction_vaccination_50_to_64 = PROV_vaxx_AL1D/PROV_population*group_50_to_64,
            interaction_vaccination_65_plus = PROV_vaxx_AL1D/PROV_population*group_65_plus,
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
            interaction_vaccination_5_to_19 = PROV_vaxx_AL1D/PROV_population*group_5_to_19,
            interaction_vaccination_20_to_49 = PROV_vaxx_AL1D/PROV_population*group_20_to_49,
            interaction_vaccination_50_to_64 = PROV_vaxx_AL1D/PROV_population*group_50_to_64,
            interaction_vaccination_65_plus = PROV_vaxx_AL1D/PROV_population*group_65_plus,
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
    log_previous_wave_incidence=ifelse(previous_wave_incidence==0, 0, log(previous_wave_incidence))
)

# tables for descriptive data
Coefficients <- data.table()
r2s <- data.table()
VIFs <- data.table()
MSEs <- data.table()
optimal_lambdas <- data.table()

start_time <- Sys.time()
for(wave_number in 1:4)
{
    ##### UNSTANDARDISED REGRESION
    
    reg_data <- Data %>%
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
        .[, which(! duplicated( t( . ) ) ), with = FALSE]
        
    # 70/30 split for training/testing
    # # getting random indices
    tIndices <- sample(1:nrow(reg_data), 0.7*nrow(reg_data))

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
    lambda_options <- 10**seq(5, -5, by = -.1)
    trace <- glmnet(
        x_train, y_train,
        nlambda = length(lambda_options),
        alpha = 0,
        family = 'gaussian',
        lambda = lambda_options
    )

    # n-fold cross-validation
    glm_fit <- cv.glmnet(x_train, y_train, alpha=0, lambda=lambda_options)

    optimal_lambda = glm_fit$lambda.min

    best_model <- glmnet(x_train, y_train, alpha=0, lambda=optimal_lambda)

    # predictions with the training data
    training_predictions <- predict(best_model, s=optimal_lambda, newx = x_train)

    # predictions with the testing data
    testing_predictions <- predict(best_model, s=optimal_lambda, newx = x_test)

    ##### COLLECTING DATA FOR GRAPHS
    
    coeffs <- coef(best_model, s="lambda.min")
    Coefficients <- rbind(
        Coefficients,
        data.table(
            regressor = coeffs@Dimnames[[1]],
            coefficient = coeffs@x,
            wave = wave_number
        )
    ) %>% suppressWarnings
    

    optimal_lambdas <- rbind(
        optimal_lambdas,
        data.table(wave=wave_number, lambda=optimal_lambda)
    )
    
    aliased_columns <- unique(rownames(
        which(
            alias(lm(incidence ~ ., data = reg_data))[["Complete"]] != "2", 
            arr.ind=TRUE
        )
    ))
    
    vif <- car::vif(lm(incidence ~ ., data = reg_data %>% dplyr::select(-any_of(aliased_columns))))
    
    VIFs <- rbind(VIFs, data.table(
        regressor = names(vif),
        value = as.numeric(vif),
        wave = wave_number
    ))
    
    MSEs <- rbind(
        MSEs,
        data.table(
            wave = wave_number, lambda=glm_fit$lambda, meann = glm_fit$cvm, 
            sdd = glm_fit$cvsd, upper = glm_fit$cvup, lower = glm_fit$cvlo
        )
    )
    
    r2s <- rbind(
        r2s,
        data.table(
            r2_60pc_training_fit = r2(y_train, training_predictions),
            r2_40pc_testing_fit = r2(y_test,  testing_predictions),
            wave = wave_number
        )
    )
}
print(Sys.time() - start_time)

##### plot of the standardised regression coefficients

Coefficients2 <- Coefficients %>%
    dplyr:: filter(!grepl("intercept", tolower(regressor))) %>%
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
         
         "pop_density" = "Density",
         
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
         "interaction_vaccination_50_to_64" = "Vaxx : 50-64",
         "interaction_vaccination_65_plus" = "Vaxx : 65+"
    ))

pl_regression <- ggplot(
        Coefficients2,
        aes(x=str_wrap(regressor,20), y=coefficient)
    ) +
    geom_point(size=4) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_grid(wave~graph_factor, scales="free", space="free_x") +
    theme_bw() +
    theme(
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=15),
        strip.text = element_text(size=13)
    ) +
    labs(
        x="Regressor", 
        y="Standardised Coefficient", 
        title="Colon (:) for interaction terms. Den (population density), LTCH (long-term care
            homes), Y[i-1] (previous wave incidence), EI (school closure), Vaxx (vaccination)"
    )

ggsave(
        pl_regression,
        file = file.path(PROJECT_FOLDER, "Graphs/regression_coefficients.png"),
        width=15, height=7
    )

##### plot of MSE vs lambda for the unstandardised regression

pl_MSEs <- ggplot(MSEs, aes(x=log10(lambda), y=meann, ymin=lower, ymax=upper)) +
    geom_ribbon(fill="red", alpha=0.1) +
    geom_vline(aes(xintercept=log10(lambda)), optimal_lambdas, colour="black") +
    geom_point(size=1, colour="red") +
    facet_grid(wave~., scale="free_y") +
    theme_bw() +
    theme(
        
    ) +
    labs(x="log10(lambda)", y="MSE") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

ggsave(pl_MSEs, file=file.path(PROJECT_FOLDER, "Graphs/MSE_plot.png"), height=9, width=12)
        
        
        
        
        
        
        
        
        
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
