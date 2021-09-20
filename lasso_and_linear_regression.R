##################################################
## Project: COVID-19 regression
## Script purpose: do linear and lasso regressions, return the coefficients of the standardised covariates, print graphs of the results
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
PROJECT_FOLDER <- "/home/bren/Documents/GitHub/MIZ_project/"

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

# table to aggregate the coefficients in all the regressions to draw a diagram showing the performance of remoteness measures in all regressions
all_the_tables_with_CI <- data.table()

# pretty names for the graphs - variables will appear in the x-axis in the same order they do here
# all covariates must be given here, otherwise their labels will be NA
display_names = c(
    "fstat" = "F statistic",
    
    "(Intercept)" = "Intercept",
    
    "F0_standardised" = expression("f"[0]),
    "F3_standardised" = expression("f"[{"\u2264 3"}]),
    "F4_standardised" = expression("f"[{"\u2265 4"}]),
    
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

PROVINCE <- "Ontario"

for(remoteness_covariate in c("weighted_mr_standardised", "weighted_index_standardised")) 
# "sum_mr_standardised", "mean_mr_standardised", "mean_index_standardised", "sum_index_standardised")) # ,  
{
    # linear regression for the first wave
    first_wave_info_linear <- do_linear_regression(
        Total_Data, # %>% filter(province == PROVINCE),
        "wave_1_attack_rate",
        remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"
        #, "total_commuters_standardised"
    )
    
    second_wave_info_linear <- do_linear_regression(
        Total_Data, # %>% filter(province == PROVINCE),
        "wave_2_attack_rate",
        remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"
        #, "total_commuters_standardised"
    )

    third_wave_info_linear <- do_linear_regression(
        Total_Data, # %>% filter(province == PROVINCE),
        "wave_3_attack_rate",
        remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"
        #, "total_commuters_standardised"
    )

    # data table getting the attack rates over each of the three waves
    All_Waves_Data <- rbind(
            Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_1_standardised, wave=1),
            Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_2_standardised, wave=2),
            Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_3_standardised, wave=3)
        ) %>%
        dplyr::mutate(wave_standardised = z_transform(wave)) %>%
        dplyr::select(-wave_1_attack_rate, -wave_2_attack_rate, -wave_3_attack_rate, -wave_1_standardised, -wave_2_standardised, -wave_3_standardised)

    # regressions including the wave this time
    total_info_linear <- do_linear_regression(
        All_Waves_Data %>% filter(province == PROVINCE),
        "standardised_attack_rate",
        remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised", "wave_standardised"
        #, "total_commuters_standardised"
    )
    
    first_wave_coefficients <- first_wave_info_linear$coefficients
    second_wave_coefficients <- second_wave_info_linear$coefficients
    third_wave_coefficients <- third_wave_info_linear$coefficients
    all_waves_coefficients <- total_info_linear$coefficients

    # big table with all the coefficients and prettified names
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
            response_var = remoteness_covariate,
            wave_number = unlist(lapply(response, function(x){
                if(grepl(1, x)) return(1)
                if(grepl(2, x)) return(2)
                if(grepl(3, x)) return(3)
                if(grepl("stand", x)) return("all")
            }))
        ) %>% 
        add_row(covariate="fstat", response.fancy="First Wave") %>% 
        add_row(covariate="fstat", response.fancy="Second Wave") %>% 
        add_row(covariate="fstat", response.fancy="Third Wave") # %>% 
        # add_row(covariate="fstat", response.fancy="First Three Waves")

    # labels for each subplot showing what the F and p values were for that test
    fstat_labels <- table_with_CI[!grepl("fstat", tolower(covariate)),
        .(lab = sprintf(
            "atop('F'[%s]: %.2f, p%s)", # 
            wave_number,
            na.omit(fstat_value),
            if(na.omit(fstat_p) > 0.01) round(na.omit(fstat_p), 2) else expression("\u003c0.01")
        ) ),
        by=.(response, response.fancy)
    ]

    Significance_diagram <- ggplot(
            table_with_CI %>%
                dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))) %>%
                dplyr::mutate(covariate = factor(covariate, levels=names(display_names))),
            aes(x=covariate, y=beta, group=response, colour=signif)
        ) +
        # zero line
        geom_hline(aes(yintercept=0), linetype="dashed", size=1, colour="grey") +
        # visualizing the LASSO regression coefficients
        # geom_line(aes(x=covariate, y=lasso_coefficient*lasso_scaling_factor, group=1), inherit.aes=FALSE, size=1, colour="black", linetype="dashed") +
        # geom_point(aes(x=covariate, y=lasso_coefficient*lasso_scaling_factor, group=1), inherit.aes=FALSE, size=3, colour="black") +
        # scale_y_continuous("Standardised Regression Coefficient", sec.axis = sec_axis(~.*lasso_scaling_factor, name="Lasso Coefficient")) +
        # 95% confidence interval
        geom_errorbar(aes(ymin=CI25, ymax=CI975), size=1, width=0.5) +
        geom_point(size=4) +
        # F and p information for the test
        geom_label(
            data=fstat_labels %>%
                dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))),
            aes(label=lab),
            x=1, y=00, # 9*max(table_with_CI$CI975),
            inherit.aes=FALSE, size=4, fill="grey90", parse=TRUE
        ) +
        # VIF for each covariate in a box at the bottom
        # geom_label(aes(label=sprintf("%.2f", vif)), y=min(table_with_CI$CI25), size=2.5, fill="grey90") +
        theme_bw() +
        theme(
            legend.position = "none",
            axis.text = element_text(size=14),
            axis.title = element_text(size=14),
            strip.text = element_text(size=14),
            axis.text.y.right = element_text(colour = "black")
        ) +
        labs(x="Covariates", y="Standardised Regression Coefficient") +
        scale_colour_manual(name="signif", values=c(`TRUE`="blue", `FALSE`="red")) +
        facet_grid(response.fancy~. , scale="free_y") +
        scale_x_discrete(labels = display_names)

        ggsave(Significance_diagram, file=sprintf("%s/Graphs/new_coefficients_%s.png", PROJECT_FOLDER, remoteness_covariate), width=10, height=6)

    all_the_tables_with_CI <- rbind(all_the_tables_with_CI, table_with_CI)
}

# # gather the performance (linear and LASSO regression coefficients, VIFs, CIs, et) to plot them all on a single graph for comparison
# remoteness_covariates_performance <- all_the_tables_with_CI %>%
#     dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))) %>%
#     dplyr::filter(!grepl("f|population|commuters|intercept|wave", tolower(covariate)))
# 
# # graph with all the possible remoteness measures on the same graph
# All_Remoteness_Variables_Performance <- ggplot(remoteness_covariates_performance, aes(x=covariate, y=beta, group=response, colour=signif)) +
#     facet_grid(response.fancy~.,) +
#     geom_hline(aes(yintercept=0), linetype="dashed", size=1, colour="grey") +
#     geom_hpline(aes(x=covariate, y=lasso_coefficient*lasso_scaling_factor, group=1), inherit.aes=FALSE, size=1, colour="black", linetype="dashed") +
#     geom_point(aes(x=covariate, y=lasso_coefficient*lasso_scaling_factor, group=1), inherit.aes=FALSE, size=3, colour="black") +
#     scale_y_continuous("Standardised Regression Coefficient", sec.axis = sec_axis(~.*lasso_scaling_factor, name="Lasso Coefficient")) +
#     geom_errorbar(aes(ymin=CI25, ymax=CI975), size=1, width=0.5) +
#     geom_point(size=4) +
#     geom_label(aes(label=sprintf("%.2f", vif)), y=min(remoteness_covariates_performance$CI25), size=4, fill="grey90") +
#     theme_bw() +
#     theme(
#         legend.position = "none",
#         axis.text = element_text(size=13),
#         axis.title = element_text(size=13),
#         strip.text = element_text(size=13),
#         panel.spacing = unit(1, "lines")
#     ) +
#     labs(y="Standardised Regression Coefficient", x="Measures of Remoteness") +
#     scale_colour_manual(name="signif", values=c(`TRUE`="blue", `FALSE`="red")) +
#     facet_grid(response.fancy~.,scale="free_y") +
#     scale_x_discrete(labels = display_names)
#     ggsave(All_Remoteness_Variables_Performance, file=sprintf("%s/Graphs/remoteness_coefficients.png", PROJECT_FOLDER), width=10, height=8)

########### CODE FOR THE LASSO REGRESSIONS

# # LASSO regression for the first wave
# first_wave_info_lasso <- do_lasso_regression(
#     Total_Data %>% filter(province == PROVINCE),
#     "wave_1_attack_rate",
#     remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"#, "total_commuters_standardised"
# )
# # merge the two coefficient data.tables to plot them on the same graph for comparison (pattern, not value)
# first_wave_coefficients <- merge(first_wave_info_linear$coefficients, first_wave_info_lasso$coefficients, by="covariate", all=TRUE)
# 
# second_wave_info_lasso <- do_lasso_regression(
#     Total_Data %>% filter(province == PROVINCE),
#     "wave_2_attack_rate",
#     remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"#, "total_commuters_standardised"
# )
# second_wave_coefficients <- merge(second_wave_info_linear$coefficients, second_wave_info_lasso$coefficients, by="covariate", all=TRUE)
# 
# 
# third_wave_info_lasso <- do_lasso_regression(
#     Total_Data %>% filter(province == PROVINCE),
#     "wave_3_attack_rate",
#     remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised"#, "total_commuters_standardised"
# )
# third_wave_coefficients <- merge(third_wave_info_linear$coefficients, third_wave_info_lasso$coefficients, by="covariate", all=TRUE)
# 
# total_info_lasso <- do_lasso_regression(
#     All_Waves_Data %>% filter(province == PROVINCE),
#     "standardised_attack_rate",
#     remoteness_covariate, "F0_prop_standardised", "F3_prop_standardised", "F4_prop_standardised", "population_density_standardised", "wave_standardised"#, "total_commuters_standardised"
# )
# all_waves_coefficients <- merge(total_info_linear$coefficients, total_info_lasso$coefficients, by="covariate", all=TRUE)
# 
# # left and right axes, so scaling factor to make sure both patterns can be seen
# lasso_scaling_factor <- max(
#     table_with_CI[, max(abs(CI25))/max(abs(lasso_coefficient))], 
#     table_with_CI[, max(abs(CI975))/max(abs(lasso_coefficient))]
# )

