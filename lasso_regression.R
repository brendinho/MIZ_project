library(glmnet)
library(caret)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)

source(sprintf("%s/regression_prelims.R", PROJECT_FOLDER))

########################## REGRESSION FUNCTIONS

do_linear_regression <- function(data_table, response_var, ...)
{
    variables <- unlist(list(...))
    
    linear.regress <- lm(
        formula = sprintf("%s~%s", response_var, paste(variables, collapse="+")),
        data = data_table
    )    
    
    linear.vif_table <- car::vif(linear.regress) %>% 
        data.frame() %>% 
        data.table(covariate=rownames(.)) %>% 
        dplyr::rename(vif=".") %>% 
        dplyr::relocate(covariate)
    
    linear.fstatistics <- summary(linear.regress)$fstatistic
    
    linear.table_with_CI <- data.table(
            covariate=names(linear.regress$coefficients),
            linear.regress$coefficients,
            confint(linear.regress)
        ) %>%
        dplyr::mutate(
            fstat_value = c(
                linear.fstatistics[1],
                rep(NA, times = length(names(linear.regress$coefficients))-1)
            ),
            fstat_p = c(
                pf(linear.fstatistics[1], linear.fstatistics[2], linear.fstatistics[3], lower.tail=FALSE),
                rep(NA, times = length(names(linear.regress$coefficients))-1)
            )
        ) %>%
        dplyr::rename(beta=V2, CI25=`2.5 %`, CI975=`97.5 %`) %>%
        dplyr::mutate(
            p.value = data.frame(summary(linear.regress)$coefficients)$`Pr...t.`,
            response = response_var, signif=as.character(p.value<0.05)
        ) %>%
        merge(linear.vif_table, by="covariate", all=TRUE)
    
    linear.points <- avPlots(linear.regress)
    linear.all_points <- data.table()
    
    for(thing in names(linear.points))
    {
        linear.all_points <- rbind(
            linear.all_points, 
            linear.points[[thing]] %>% 
                data.table() %>%
                dplyr::mutate(variable = names(.)[1]) %>%
                dplyr::rename(value = names(.)[1])
        )
    }
    
    linear.y_var <- names(linear.all_points) %>% .[which(! . %in% c("value", "variable"))]
    
    linear.the_graph <- ggplot(linear.all_points, aes_string(x="value", y=linear.y_var, group="variable")) +
        geom_point(size=2) +
        geom_smooth(method='lm', formula=y~x) +
        labs(x="value | others") + # , y=pretty_name(y_var) 
        theme_bw() +
        facet_grid(variable~., scales="free") # , labeller = label_bquote(.(pretty_name(variable)))
    
    return(list("data" = data_table, "coefficients" = linear.table_with_CI, "regression" = linear.regress, "residuals" = resid(linear.regress), 
        "av_table" = linear.all_points, "av_graph" = linear.the_graph))
}

do_lasso_regression <- function(data_table, response_var, ...)
{
    variables <- unlist(list(...))
    
    lasso.prelim <- data_table %>% dplyr::select(all_of(c(response_var, variables)))
    
    lasso.covariates <- model.matrix(
        eval(parse(text=paste0(response_var, "~."))),
        lasso.prelim
    )[,-1]
    
    lasso.response <- data_table[[response_var]]
    
    # set.seed(as.numeric(Sys.time()))
    set.seed(digest2int("Brendon Phillips"))
    
    lasso.training_rows = sample(1:nrow(lasso.covariates), 0.6*nrow(lasso.covariates))
    
    lasso.testing_rows = (-lasso.training_rows)
    
    lasso.cv_output <- cv.glmnet(
        x = lasso.covariates[lasso.training_rows,],
        y = lasso.response[lasso.training_rows],
        alpha = 1,
        type.measure = "mse",
        grouped = FALSE,
        standardize = FALSE, # the variables are already standardised
        # lambda = 10^seq(-2, 2, by = 0.1),
        nfolds = 10
    )
    
    lasso.lambda_best <- lasso.cv_output$lambda.min
    
    # second pass with the best lambda
    lasso.best_model <- glmnet(
        x = lasso.covariates[lasso.training_rows, ],
        y = lasso.response[lasso.training_rows],
        lambda = lasso.lambda_best
    )
    
    lasso.predictions <- predict(
        object = lasso.best_model,
        s = lasso.lambda_best,
        newx = lasso.covariates[lasso.testing_rows,]
    )
    
    lasso.final <- data.table(actual=lasso.response[lasso.testing_rows], prediction=as.numeric(lasso.predictions))
    lasso.r_sq <- calculate_r_squared(actual=lasso.final$actual, predicted=lasso.final$prediction)
    
    lasso.coefficients <- coef(lasso.best_model) %>% 
        as.matrix %>% data.table(covariate=rownames(.)) %>% 
        dplyr::rename(lasso_coefficient=s0)
    
    return(list("data" = data_table, "final" = lasso.final, "r_sq" = lasso.r_sq, "regression" = lasso.predictions, "coefficients" = lasso.coefficients))
}

################### EXPLORING THE WAVES

all_the_tables_with_CI <- data.table()

display_names = c(
    "mean_mr_standardised" = expression("R"[mean]),
    "sum_mr_standardised" = expression("R"[sum]),
    "weighted_mr_standardised" = expression("R"[weighted]),
    "mean_index_standardised" = expression("I"[mean]),
    "sum_index_standardised" = expression("I"[sum]),
    "weighted_index_standardised" = expression("I"[weighted]),
    
    "F0_standardised" = expression("F"[0]),
    "F3_standardised" = expression(F[{"\u2264 3"}]),
    "F4_standardised" = expression(F[{"\u2265 4"}]),
    
    "total_commuters_standardised" = "L",
    "population_density_standardised" = "D",
    "(Intercept)" = "Intercept",
    "wave_standardised" = "u",
    "wave" = "u"
    
    
)

for(remoteness_covariate in c("mean_mr_standardised", "sum_mr_standardised", "weighted_mr_standardised", 
                              "mean_index_standardised", "sum_index_standardised", "weighted_index_standardised")) 
{
    first_wave_info_linear <- do_linear_regression(
        Total_Data,
        "wave_1_attack_rate",
        remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"#, "total_commuters_standardised"
    )
    first_wave_info_lasso <- do_lasso_regression(
        Total_Data,
        "wave_1_attack_rate",
        remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"#, "total_commuters_standardised"
    )
    first_wave_coefficients <- merge(first_wave_info_linear$coefficients, first_wave_info_lasso$coefficients, by="covariate", all=TRUE)
    
    #####
    
    second_wave_info_linear <- do_linear_regression(
        Total_Data,
        "wave_2_attack_rate",
        remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"#, "total_commuters_standardised"
    )
    second_wave_info_lasso <- do_lasso_regression(
        Total_Data,
        "wave_2_attack_rate",
        remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"#, "total_commuters_standardised"
    )
    second_wave_coefficients <- merge(second_wave_info_linear$coefficients, second_wave_info_lasso$coefficients, by="covariate", all=TRUE)
    
    #####
    
    third_wave_info_linear <- do_linear_regression(
        Total_Data,
        "wave_3_attack_rate",
        remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"#, "total_commuters_standardised"
    )
    third_wave_info_lasso <- do_lasso_regression(
        Total_Data,
        "wave_3_attack_rate",
        remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"#, "total_commuters_standardised"
    )
    third_wave_coefficients <- merge(third_wave_info_linear$coefficients, third_wave_info_lasso$coefficients, by="covariate", all=TRUE)
    
    #####

    All_Waves_Data <- rbind(
            Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_1_standardised, wave=1),
            Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_2_standardised, wave=2),
            Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_3_standardised, wave=3)
        ) %>%
        dplyr::mutate(wave_standardised = z_transform(wave)) %>%
        dplyr::select(-wave_1_attack_rate, -wave_2_attack_rate, -wave_3_attack_rate, -wave_1_standardised, -wave_2_standardised, -wave_3_standardised)

    total_info_linear <- do_linear_regression(
        All_Waves_Data,
        "standardised_attack_rate",
        remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised", "wave"#, "total_commuters_standardised"
    )
    total_info_lasso <- do_lasso_regression(
        All_Waves_Data,
        "standardised_attack_rate",
        remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised", "wave"#, "total_commuters_standardised"
    )
    all_waves_coefficients <- merge(total_info_linear$coefficients, total_info_lasso$coefficients, by="covariate", all=TRUE)
    
    table_with_CI <- rbind(
            first_wave_coefficients,
            second_wave_coefficients,
            third_wave_coefficients # ,
            # all_waves_coefficients
        ) %>%
        dplyr::mutate(
            response.fancy = unlist(lapply(response, function(x){
                if(x == "cases") return("First Three Waves")
                if(x == "wave_1_attack_rate") return("First Wave")
                if(x == "wave_2_attack_rate") return("Second Wave")
                if(x == "wave_3_attack_rate") return("Third Wave")
                if(x == "attack_rate") return("First Three Waves")
                if(x == "wave_1_standardised") return("First Wave")
                if(x == "wave_2_standardised") return("Second Wave")
                if(x == "wave_3_standardised") return("Third Wave")
                if(x == "standardised_attack_rate") return("First Three Waves")
                return(x)
            })),
            response_var = remoteness_covariate
        )
    
    fstat_labels <- table_with_CI[,
        .(lab = sprintf(
            "F: %.2f\np: %s",
            na.omit(fstat_value),
            if(na.omit(fstat_p) > 0.01) round(na.omit(fstat_p), 2) else expression("\u003c0.01")
        ) ),
        by=.(response, response.fancy)
    ]
    
    lasso_scaling_factor <- max(table_with_CI[, max(abs(CI25))/max(abs(lasso_coefficient))], table_with_CI[, max(abs(CI975))/max(abs(lasso_coefficient))])
    Significance_diagram <- ggplot(
            table_with_CI %>% 
                dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))) %>%
                dplyr::mutate(covariate = factor(covariate, levels=c("(Intercept)", "F0_standardised", "F3_standardised", "F4_standardised", 
                    "population_density_standardised", "total_commuters_standardised", "mean_mr_standardised", "sum_mr_standardised", 
                    "weighted_mr_standardised", "mean_index_standardised", "sum_index_standardised", "weighted_index_standardised", 
                    "wave_standardised", "wave"))),
            aes(x=covariate, y=beta, group=response, colour=signif)
        ) +
        geom_hline(aes(yintercept=0), linetype="dashed", size=1, colour="grey") +
        geom_line(aes(x=covariate, y=lasso_coefficient*lasso_scaling_factor, group=1), inherit.aes=FALSE, size=1, colour="black") +
        geom_point(aes(x=covariate, y=lasso_coefficient*lasso_scaling_factor, group=1), inherit.aes=FALSE, size=3, colour="black") +
        scale_y_continuous("Standardised Regression Coefficient", sec.axis = sec_axis(~.*lasso_scaling_factor, name="Lasso Coefficient")) +
        geom_errorbar(aes(ymin=CI25, ymax=CI975), size=1, width=0.5) +
        geom_point(size=4) +
        geom_label(
            data=fstat_labels %>% 
                dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))),
            aes(label=lab),
            x=0.7, y=09*max(table_with_CI$CI975),
            inherit.aes=FALSE, size=3.5, fill="grey90"
        ) +
        geom_label(aes(label=sprintf("%.2f", vif)), y=min(table_with_CI$CI25), size=2.5, fill="grey90") +
        theme_bw() +
        theme(
            legend.position = "none",
            axis.text = element_text(size=13),
            axis.title = element_text(size=13),
            strip.text = element_text(size=13),
            axis.text.y.right = element_text(colour = "black")
        ) +
        labs(x="Covariates") +
        scale_colour_manual(name="signif", values=c(`TRUE`="blue", `FALSE`="red")) +
        facet_grid(response.fancy~.) + # , scale="free_y"
        scale_x_discrete(labels = display_names)
    
        ggsave(Significance_diagram, file=sprintf("%s/Graphs/new_coefficients_%s.png", PROJECT_FOLDER, remoteness_covariate), width=10, height=7)
    
    all_the_tables_with_CI <- rbind(all_the_tables_with_CI, table_with_CI)
}

remoteness_covariates_performance <- all_the_tables_with_CI %>% 
    dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))) %>%
    dplyr::filter(!grepl("f|population|commuters|intercept|wave", tolower(covariate)))

All_Remoteness_Variables_Performance <- ggplot(remoteness_covariates_performance, aes(x=covariate, y=beta, group=response, colour=signif)) +
    facet_grid(response.fancy~.,) +
    geom_hline(aes(yintercept=0), linetype="dashed", size=1, colour="grey") +
    geom_line(aes(x=covariate, y=lasso_coefficient*lasso_scaling_factor, group=1), inherit.aes=FALSE, size=1, colour="black") +
    geom_point(aes(x=covariate, y=lasso_coefficient*lasso_scaling_factor, group=1), inherit.aes=FALSE, size=3, colour="black") +
    scale_y_continuous("Standardised Regression Coefficient", sec.axis = sec_axis(~.*lasso_scaling_factor, name="Lasso Coefficient")) +
    geom_errorbar(aes(ymin=CI25, ymax=CI975), size=1, width=0.5) +
    geom_point(size=4) +
    geom_label(aes(label=sprintf("%.2f", vif)), y=min(remoteness_covariates_performance$CI25), size=4, fill="grey90") +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text = element_text(size=13),
        axis.title = element_text(size=13),
        strip.text = element_text(size=13),
        panel.spacing = unit(1, "lines")
    ) +
    labs(y="Standardised Regression Coefficient", x="Measures of Remoteness") +
    scale_colour_manual(name="signif", values=c(`TRUE`="blue", `FALSE`="red")) +
    facet_grid(response.fancy~.,) +
    scale_x_discrete(labels = display_names)
    ggsave(All_Remoteness_Variables_Performance, file=sprintf("%s/Graphs/remoteness_coefficients.png", PROJECT_FOLDER), width=10, height=8)




