##################################################
## Project: COVID-19 regression
## Script purpose: do linear regressions, return the coefficients of the standardised covariates, print graphs of the results
## Date written: 2021-09-08
## Last edited: 2021-09-08 
## Script author: Brendon Phillips  
## Institution: York University
## Lab: ABM_Lab
## Principal Investigator: Seyed M. Moghadas
##################################################

library(glmnet)
library(caret)
library(ggplot2)
library(viridis)
library(ungeviz)

# PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
PROJECT_FOLDER <- "/home/bren/Documents/GitHub/MIZ_project"

source(sprintf("%s/regression_prelims.R", PROJECT_FOLDER))

########################## REGRESSION FUNCTIONS

# function to carry through the linear regressions
do_linear_regression <- function(data_table, response_var, ...)
{
    variables <- unlist(list(...))
    
    # regression
    linear.regress <- lm(
        formula = sprintf("%s~%s", response_var, paste(variables, collapse="+")),
        data = data_table
    )    
    
    # calculate VIFs with the car library
    linear.vif_table <- car::vif(linear.regress) %>% 
        data.frame() %>% 
        data.table(covariate=rownames(.)) %>% 
        dplyr::rename(vif=".") %>% 
        dplyr::relocate(covariate)
    
    # f statistics of each regression
    linear.fstatistics <- summary(linear.regress)$fstatistic
    
    # table with all information: coefficients, p-values, F-statistic, confidence interval, VIF
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
    
    # points for added variable plots
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
    
    # graph of added variables
    linear.the_graph <- ggplot(linear.all_points, aes_string(x="value", y=linear.y_var, group="variable")) +
        geom_point(size=2) +
        geom_smooth(method='lm', formula=y~x) +
        labs(x="value | others") + # , y=pretty_name(y_var) 
        theme_bw() +
        facet_grid(variable~., scales="free") # , labeller = label_bquote(.(pretty_name(variable)))
    
    return(list("data" = data_table, "coefficients" = linear.table_with_CI, "regression" = linear.regress, "residuals" = resid(linear.regress), 
        "av_table" = linear.all_points, "av_graph" = linear.the_graph, "f_statistics"=linear.fstatistics))
}

# carry through the LASSO regression
do_lasso_regression <- function(data_table, response_var, ...)
{
    variables <- unlist(list(...))
    
    # get table with only the necessary information
    lasso.prelim <- data_table %>% dplyr::select(all_of(c(response_var, variables)))
    
    # model.matrix object out of the covariate data
    lasso.covariates <- model.matrix(
        eval(parse(text=paste0(response_var, "~."))),
        lasso.prelim
    )[,-1]
    
    # values of the specified response variable
    lasso.response <- data_table[[response_var]]
    
    # set.seed(as.numeric(Sys.time()))
    set.seed(digest2int("Brendon Phillips"))
    
    # we'll train on 60% iof the data
    lasso.training_rows = sample(1:nrow(lasso.covariates), 0.6*nrow(lasso.covariates))
    lasso.testing_rows = (-lasso.training_rows)
    
    # getting the best lambda to use with the model
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
    
    # the best value
    lasso.lambda_best <- lasso.cv_output$lambda.min
    
    # second pass with the best lambda
    lasso.best_model <- glmnet(
        x = lasso.covariates[lasso.training_rows, ],
        y = lasso.response[lasso.training_rows],
        lambda = lasso.lambda_best
    )
    
    # rpedictions of the response variable given the fit model
    lasso.predictions <- predict(
        object = lasso.best_model,
        s = lasso.lambda_best,
        newx = lasso.covariates[lasso.testing_rows,]
    )
    
    # putting the predictions and the actual response variable values side-by-side in a table
    lasso.final <- data.table(actual=lasso.response[lasso.testing_rows], prediction=as.numeric(lasso.predictions))
    # calculating the R^2 value given the actuall and predicted reponses
    lasso.r_sq <- calculate_r_squared(actual=lasso.final$actual, predicted=lasso.final$prediction)
    
    # generate a named table of the coefficients
    lasso.coefficients <- coef(lasso.best_model) %>% 
        as.matrix %>% data.table(covariate=rownames(.)) %>% 
        dplyr::rename(lasso_coefficient=s0)
    
    return(list("data" = data_table, "final" = lasso.final, "r_sq" = lasso.r_sq, "regression" = lasso.predictions, "coefficients" = lasso.coefficients))
}

################### EXPLORING THE WAVES

# pretty names for the graphs - variables will appear in the x-axis in the same order they do here
# all covariates must be given here, otherwise their labels will be NA
display_names = c(
    "fstat" = "",
    
    "(Intercept)" = "Intercept",
    
    "F0_standardised" = expression("F"[0]),
    "F3_standardised" = expression("F"[{"\u2264 3"}]),
    "F4_standardised" = expression("F"[{"\u2265 4"}]),
    
    "F0_prop_standardised" = expression("f"[0]),
    "F3_prop_standardised" = expression("f"[{"\u2264 3"}]),
    "F4_prop_standardised" = expression("f"[{"\u2265 4"}]),
    
    "mean_mr_standardised" = expression("R"[m]),
    "sum_mr_standardised" = expression("R"[s]),
    "weighted_mr_standardised" = expression("R"[w]),
    "mean_index_standardised" = expression("I"[m]),
    "sum_index_standardised" = expression("I"[s]),
    "weighted_index_standardised" = expression("I"[w]),

    "total_commuters_standardised" = "L",
    "population_density_standardised" = "D",
    
    "wave_standardised" = "u",
    "wave" = "u"
)

# table to aggregate the coefficients in all the regressions to draw a diagram showing the performance of remoteness measures in all regressions
tables_with_CI <- data.table()

fstatistics <- data.table()

all_waves_av_table <- data.table()

PROVINCE <- "Ontario"

remoteness_metrics_tested <- c("weighted_mr_standardised", "mean_index_standardised")

for(remoteness_covariate in remoteness_metrics_tested) 
{
    # linear regression for the first wave
    first_wave_info_linear <- do_linear_regression(
        Total_Data,
        "wave_1_attack_rate",
        remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"
        # remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"
    )
    
    second_wave_info_linear <- do_linear_regression(
        Total_Data,
        "wave_2_attack_rate",
        remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"
        # remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"
    )

    third_wave_info_linear <- do_linear_regression(
        Total_Data,
        "wave_3_attack_rate",
        remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"
        # remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"
    )
    
    all_three_waves_info_linear <- do_linear_regression(
        Total_Data,
        "all_waves_attack_rate",
        remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"
        # remoteness_covariate, "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised"
    )
    
    first_wave_coefficients <- first_wave_info_linear$coefficients
    second_wave_coefficients <- second_wave_info_linear$coefficients
    third_wave_coefficients <- third_wave_info_linear$coefficients
    all_waves_coefficients <- all_three_waves_info_linear$coefficients
    
    all_waves_av_table <- rbind(
        all_waves_av_table,
        first_wave_info_linear$av_table %>% dplyr::mutate(wave="First Wave", remoteness_var=remoteness_covariate) %>% dplyr::rename(attack_rate=wave_1_attack_rate),
        second_wave_info_linear$av_table %>% dplyr::mutate(wave="Second Wave", remoteness_var=remoteness_covariate) %>% dplyr::rename(attack_rate=wave_2_attack_rate),
        third_wave_info_linear$av_table %>% dplyr::mutate(wave="Third Wave", remoteness_var=remoteness_covariate) %>% dplyr::rename(attack_rate=wave_3_attack_rate),
        all_three_waves_info_linear$av_table %>% dplyr::mutate(wave="Waves Combined", remoteness_var=remoteness_covariate) %>% dplyr::rename(attack_rate=all_waves_attack_rate)
    )

    # big table with all the coefficients and prettified names
    Regression_Here <- rbind(
            first_wave_coefficients,
            second_wave_coefficients,
            third_wave_coefficients,
            all_waves_coefficients
        ) %>%
        dplyr::mutate(
            response.fancy = unlist(lapply(response, function(x){
                if(x == "cases") return("First Three Waves")
                if(x == "wave_1_attack_rate") return("First Wave")
                if(x == "wave_2_attack_rate") return("Second Wave")
                if(x == "wave_3_attack_rate") return("Third Wave")
                if(x == "all_waves_attack_rate") return("First Three Waves")
                if(x == "wave_1_standardised") return("First Wave")
                if(x == "wave_2_standardised") return("Second Wave")
                if(x == "wave_3_standardised") return("Third Wave")
                if(x == "standardised_attack_rate") return("First Three Waves")
                return(x)
            })),
            remoteness_var = remoteness_covariate,
            wave_number = unlist(lapply(response, function(x){
                if(grepl(1, x)) return(1)
                if(grepl(2, x)) return(2)
                if(grepl(3, x)) return(3)
                if(grepl("all", x)) return("all")
                retrun(x)
            }))
        ) %>%
        add_row(covariate="fstat", response.fancy="First Wave", remoteness_var = remoteness_covariate) %>%
        add_row(covariate="fstat", response.fancy="Second Wave", remoteness_var = remoteness_covariate) %>%
        add_row(covariate="fstat", response.fancy="Third Wave", remoteness_var = remoteness_covariate) %>%
        add_row(covariate="fstat", response.fancy="First Three Waves", remoteness_var = remoteness_covariate)

    # labels for each subplot showing what the F and p values were for that test
    fstat_labels <- Regression_Here[!grepl("fstat", tolower(covariate)),
        .(lab = sprintf(
                "atop('F'[%s]: %.2f, p%s)", #
                wave_number,
                na.omit(fstat_value),
                if(na.omit(fstat_p) > 0.01) round(na.omit(fstat_p), 2) else expression("\u003c0.01")
            ),
          remoteness_var = remoteness_covariate
          ),
        by=.(response, response.fancy)
    ]

    # Regression_Plot <- ggplot(
    #         Regression_Here %>%
    #             dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))) %>%
    #             dplyr::mutate(covariate = factor(covariate, levels=names(display_names))),
    #         aes(x=covariate, y=beta, group=response, colour=signif)
    #     ) +
    #     # zero line
    #     geom_hline(aes(yintercept=0), linetype="dashed", size=1, colour="grey") +
    #     # 95% confidence interval
    #     geom_errorbar(aes(ymin=CI25, ymax=CI975), size=1, width=0.5) +
    #     geom_point(size=4) +
    #     # F and p information for the test
    #     geom_label(
    #         data=fstat_labels %>%
    #             dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))),
    #         aes(label=lab),
    #         x=1, y=00, # 9*max(table_with_CI$CI975),
    #         inherit.aes=FALSE, size=4, fill="grey90", parse=TRUE
    #     ) +
    #     # VIF for each covariate in a box at the bottom
    #     # geom_label(aes(label=sprintf("%.2f", vif)), y=min(Regression_Here$CI25), size=2.5, fill="grey90") +
    #     theme_bw() +
    #     theme(
    #         legend.position = "none",
    #         axis.text = element_text(size=14),
    #         axis.title = element_text(size=14),
    #         strip.text = element_text(size=14),
    #         axis.text.y.right = element_text(colour = "black")
    #     ) +
    #     labs(x="Covariates", y="Standardised Regression Coefficient") +
    #     scale_colour_manual(name="signif", values=c(`TRUE`="blue", `FALSE`="red")) +
    #     facet_grid(response.fancy~. , scale="free_y") +
    #     scale_x_discrete(labels = display_names)
    # 
    #     ggsave(Regression_Plot, file=sprintf("%s/Graphs/new_coefficients_%s.png", PROJECT_FOLDER, remoteness_covariate), width=10, height=8)

    tables_with_CI  <- rbind(tables_with_CI, Regression_Here)
    fstatistics <- rbind(fstatistics, fstat_labels)
}

Altogether_Plot <- ggplot(
        tables_with_CI %>%
            dplyr::mutate(
                response.fancy = dplyr::recode(response.fancy,
                    "First Wave"="'First Wave'", "Second Wave"="'Second Wave'", "Third Wave"="'Third Wave'", "First Three Waves"="'First Three Waves'"),
                covariate = factor(covariate, levels=names(display_names)),
                remoteness_var = dplyr::recode(remoteness_var,
                    "weighted_index_standardised"="'(A) the model using I'[w]",
                    "weighted_mr_standardised"="'(A) the model using R'[w]",
                    "sum_index_standardised"="'(A) the model using I'[s]",
                    "mean_index_standardised"="'(B) the model using I'[m]"
                )
            ),
        aes(x=covariate, y=beta, group=response, colour=signif)
    ) +
    # zero line
    geom_hline(aes(yintercept=0), linetype="dashed", size=1, colour="grey") +
    # 95% confidence interval
    geom_errorbar(aes(ymin=CI25, ymax=CI975), size=1, width=0.25) +
    geom_point(size=4) +
    # F and p information for the test
    geom_label(
        data=fstatistics %>%
            dplyr::mutate(
                response.fancy = dplyr::recode(response.fancy,
                                        "First Wave"="'First Wave'", "Second Wave"="'Second Wave'", "Third Wave"="'Third Wave'", "First Three Waves"="'First Three Waves'"),
                remoteness_var = dplyr::recode(remoteness_var,
                    "sum_index_standardised"="'(A) the model using I'[s]",
                    "weighted_mr_standardised"="'(A) the model using R'[w]",
                    "weighted_index_standardised"="'(A) the model using I'[w]",
                    "mean_index_standardised"="'(B) the model using I'[m]"
                )
            ),
        aes(label=lab),
        x=1, y=00, # 9*max(table_with_CI$CI975),
        inherit.aes=FALSE, size=5, fill="grey90", parse=TRUE
    ) +
    # VIF for each covariate in a box at the bottom
    # geom_label(aes(label=sprintf("%.2f", vif)), y=min(table_with_CI$CI25), size=2.5, fill="grey90") +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text = element_text(size=18),
        axis.text.x = element_text(size=22),
        axis.title = element_text(size=18),
        strip.text = element_text(size=18),
        axis.text.y.right = element_text(colour = "black")
    ) +
    labs(x="Covariates", y="Standardised Regression Coefficient") +
    scale_colour_manual(name="signif", values=c(`TRUE`="blue", `FALSE`="red")) +
    facet_grid(
        factor(response.fancy, levels=c("'First Wave'", "'Second Wave'", "'Third Wave'", "'First Three Waves'")) ~ remoteness_var,
        scale="free", labeller=label_parsed
    ) +
    scale_x_discrete(labels = display_names)

ggsave(Altogether_Plot, file=sprintf("%s/Graphs/all_remoteness_together.png", PROJECT_FOLDER), width=13, height=11)

stop()

all_waves_av_table2 <- all_waves_av_table %>%
    dplyr::arrange(variable, wave) %>%
    dplyr::mutate(
        variable = dplyr::recode(variable,
            "mean_index_standardised" = "I'[m]",
            "weighted_mr_standardised" = "R'[w]",
            "population_density_standardised" = "D'",
            "total_commuters_standard" = "L'",
            "F0_prop_standardised" = "f'[0]",
            "F3_prop_standardised" = "f'[{'\u2264 3'}]",
            "F4_prop_standardised" = "f'[{'\u2265 4'}]"
        ),
        variable = factor(variable, levels=c("f'[0]", "f'[{'≤ 3'}]", "f'[{'≥ 4'}]", "R'[m]", "I'[m]", "I'[w]", "I'[s]", "R'[w]", "R'[s]", "D'")),
        whole_name = paste0(wave, " - ", variable)
    )

all_waves_av_table3 <- merge(
        all_waves_av_table2,
        all_waves_av_table2 %>%
            # dplyr::filter(!is.na(variable)) %>%
            dplyr::select(wave, variable, whole_name) %>%
            unique() %>%
            dplyr::mutate(
                fancy_name = paste0("'", letters[1:nrow(.)], ") ", whole_name)
            ),
        by=c("wave", "variable"),
    )

for(remoteness_covariate in remoteness_metrics_tested)
{
    large_partial_regression_plots <- ggplot(
            all_waves_av_table3 %>% dplyr::filter(remoteness_var ==  remoteness_covariate ),
            aes(x=value, y=attack_rate)
        ) +
        geom_point(size=0.5) +
        geom_smooth(method="lm", formula=y~x) +
        # facet_wrap(wave~variable,scales="free", labeller = label_parsed)
        facet_wrap(fancy_name~., scales="free", labeller = label_parsed, ncol=4) +
        labs(x="Value", y="Attack Rate") +
        theme_bw() +
        theme(
            plot.background = element_rect(fill = "azure2", colour = NA),
            panel.spacing = unit(2, "lines"),
            strip.text = element_text(size=16),
            axis.title = element_text(size=25),
            axis.text = element_text(size=17)
        )
    ggsave(large_partial_regression_plots, file=file.path(PROJECT_FOLDER, sprintf("Graphs/remoteness_av_plot_%s.png", remoteness_covariate)), width=15, height=19)
}

################ PRINT RESULTS TABLE FOR THE APPENDIX
tables_with_CI %>%
    filter(covariate != "fstat") %>% 
    mutate(CI = paste0("(", round(CI25,3), ",", round(CI975,3), ")")) %>% 
    select(remoteness_var, wave_number, covariate, beta, CI, p.value, fstat_value, fstat_p) %>%
    dplyr::mutate(
        p.value = unlist(lapply( p.value, \(x) if(x<0.01) "l0.01" else round(x, 2) )),
        fstat_p = unlist(lapply( fstat_p, \(x) if(is.na(x)) NA else if(x<0.01) "l0.01" else round(x, 2) )),
        beta = round(beta, 4),
        fstat_value = round(fstat_value, 4),
        covariate = dplyr::recode(covariate, 
            "(Intercept)"="intercept", 
            "F0_prop_standardised"="f0", 
            "F3_prop_standardised"="fle3", 
            "F4_prop_standardised"="fge4",
            "population_density_standardised"="D",
            "weighted_mr_standardised"="Rw",
            "mean_index_standardised"="Im"
        ),
        remoteness_var = dplyr::recode(remoteness_var, "weighted_mr_standardised"="Rw", "mean_index_standardised"="Im")
    ) %>%
    xtable %>%
    print(include.rownames=F)

















































