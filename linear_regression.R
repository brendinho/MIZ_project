library(caret)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)

source(sprintf("%s/regression_prelims.R", PROJECT_FOLDER))

pretty_print_coeff_CI <- function(data_table, response_var, ...)
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
  
  return(list(
    data="data_table", 
    "oefficients"=linear.table_with_CI, 
    "regression"=linear.regress, 
    "residuals"=resid(linear.regress), 
    "av_table"=linear.all_points, "
    av_graph"=linear.the_graph))
}

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

for(remoteness_covariate in c("mean_mr_standardised", "sum_mr_standardised", "weighted_mr_standardised", "mean_index_standardised", "sum_index_standardised", "weighted_index_standardised")) 
{
  first_wave_info <- pretty_print_coeff_CI(
    Total_Data,
    "wave_1_standardised",
    remoteness_covariate,
    "F0_standardised",
    "F3_standardised",
    "F4_standardised",
    "total_commuters_standardised",
    "population_density_standardised"
    
  )
  
  stop()
  # ggsave(
  #     first_wave_info$av_plot + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), strip.text = element_text(size = 15)),
  #     file = sprintf("%s/Graphs/first_wave_av_plot_%s.png", PROJECT_FOLDER, remoteness_covariate),
  #     width=10, height=10
  # )
  
  second_wave_info <- pretty_print_coeff_CI(
    Total_Data,
    "wave_2_standardised",
    remoteness_covariate,
    "F0_standardised",
    "F3_standardised",
    "F4_standardised",
    "total_commuters_standardised",
    "population_density_standardised"
    
  )
  # ggsave(
  #     second_wave_info$av_plot + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), strip.text = element_text(size = 15)),
  #     file = sprintf("%s/Graphs/second_wave_av_plots_%s.png", PROJECT_FOLDER, remoteness_covariate),
  #     width=10, height=10
  # )
  
  third_wave_info <- pretty_print_coeff_CI(
    Total_Data,
    "wave_3_standardised",
    remoteness_covariate,
    "F0_standardised",
    "F3_standardised",
    "F4_standardised",
    "total_commuters_standardised",
    "population_density_standardised"
    
  )
  # ggsave(
  #     third_wave_info$av_plot + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), strip.text = element_text(size = 15)),
  #     file = sprintf("%s/Graphs/third_wave_av_plots_%s.png", PROJECT_FOLDER, remoteness_covariate),
  #     width=10, height=10
  # )
  
  total_info <- pretty_print_coeff_CI(
    rbind(
      Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_1_standardised, wave=1),
      Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_2_standardised, wave=2),
      Total_Data %>% dplyr::mutate(standardised_attack_rate = wave_3_standardised, wave=3)
    ) %>% 
      dplyr::select(-wave_1_attack_rate, -wave_2_attack_rate, -wave_3_attack_rate, # -all_waves_attack_rate, 
                    -wave_1_standardised, -wave_2_standardised, -wave_3_standardised),
    "standardised_attack_rate",
    remoteness_covariate,
    "F0_standardised",
    "F3_standardised",
    "F4_standardised",
    "population_density_standardised",
    "total_commuters_standardised",
    "wave"
  )
  # ggsave(
  #     total_info$av_plot + theme(axis.text = element_text(size = 15), axis.title = element_text(size = 15), strip.text = element_text(size = 15)),
  #     file = sprintf("%s/Graphs/all_waves_av_plots_%s.png", PROJECT_FOLDER, remoteness_covariate),
  #     width=10, height=10
  # )
  
  table_with_CI <- rbind(
    first_wave_info$coefficients,
    second_wave_info$coefficients,
    third_wave_info$coefficients,
    total_info$coefficients
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
                                  "F-statistic: %.2f, p value: %s",
                                  na.omit(fstat_value),
                                  if(na.omit(fstat_p) > 0.01) round(na.omit(fstat_p), 2) else expression("\u003c0.01")
                                ) ),
                                by=.(response, response.fancy)]
  
  Significance_diagram <- ggplot(
    table_with_CI %>% 
      dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))) %>%
      dplyr::mutate(covariate = factor(covariate, levels=c("(Intercept)", "F0_standardised", "F3_standardised", "F4_standardised", "population_density_standardised",
                                                           "total_commuters_standardised", "mean_mr_standardised", "sum_mr_standardised", "weighted_mr_standardised",
                                                           "mean_index_standardised", "sum_index_standardised", "weighted_index_standardised"))),
    aes(x=covariate, y=beta, group=response, colour=signif)
  ) +
    geom_hline(aes(yintercept=0), linetype="dashed", size=1, colour="grey") +
    geom_errorbar(aes(ymin=CI25, ymax=CI975), size=1, width=0.5) +
    geom_point(size=4) +
    geom_label(
      data=fstat_labels %>% 
        dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))),
      aes(label=lab),
      x=1.6, y=0.9*max(table_with_CI$CI975),
      inherit.aes=FALSE, size=4, fill="grey90"
    ) +
    geom_label(aes(label=sprintf("%.2f", vif)), y=min(table_with_CI$CI25), size=2.5, fill="grey90") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text = element_text(size=13),
      axis.title = element_text(size=13),
      strip.text = element_text(size=13)
    ) +
    labs(y="Standardised Regression Coefficient", x="Covariates") +
    scale_colour_manual(name="signif", values=c(`TRUE`="blue", `FALSE`="red")) +
    facet_grid(response.fancy~.,) +
    scale_x_discrete(labels = display_names)
  
  ggsave(Significance_diagram, file=sprintf("%s/Graphs/coefficients_%s.png", PROJECT_FOLDER, remoteness_covariate), width=10, height=7)
  
  all_the_tables_with_CI <- rbind(all_the_tables_with_CI, table_with_CI)
}

remoteness_covariates_performance <- all_the_tables_with_CI %>% 
  dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))) %>%
  dplyr::filter(!grepl("f|population|commuters|intercept|wave", tolower(covariate)))

All_Remoteness_Variables_Performance <- ggplot(remoteness_covariates_performance, aes(x=covariate, y=beta, group=response, colour=signif)) +
  facet_grid(response.fancy~.,) +
  geom_hline(aes(yintercept=0), linetype="dashed", size=1, colour="grey") +
  geom_errorbar(aes(ymin=CI25, ymax=CI975), size=1, width=0.5) +
  geom_point(size=4) +
  geom_label(
    data=fstat_labels %>%
      dplyr::mutate(response.fancy = factor(response.fancy, levels=c("First Wave", "Second Wave", "Third Wave", "First Three Waves"))),
    aes(label=lab),
    x=1.6, y=0.9*max(table_with_CI$CI975),
    inherit.aes=FALSE, size=4, fill="grey90"
  ) +
  geom_label(aes(label=sprintf("%.2f", vif)), y=min(remoteness_covariates_performance$CI25), size=2.5, fill="grey90") +
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


# var_names <- function(x)
# {
#   if(x == "aR_score") return("aR")
#   if(x == "families_with_children_of_size_2_or_smaller") return("F2")
#   if(x == "families_with_children_of_size_3_or_larger") return("F3")
#   if(x == "families_with_children_of_size_3_or_smaller") return("F3")
#   if(x == "families_with_children_of_size_4_or_larger") return("F4")
#   if(x == "families_with_no_children") return("F0")
#   if(x == "population_density") return("D")
#   if(x == "total_commuters") return("L")
#   if(x == "wave") return("x")
#   if(x == "(Intercept)") return("Intercept")
#   if(x == "mR_weighted_by_pop") return("mR_pop")
#   if(x == "aR_weighted_by_pop") return("aR_pop")
#   return(x)
# }

# print(canada_regress_results %>%
#         dplyr::mutate(
#           first_wave_value = round(first_wave_value, 4),
#           first_wave_p = round(first_wave_p, 4),
#           second_wave_value = round(second_wave_value, 4),
#           second_wave_p = round(second_wave_p, 4),
#           third_wave_value = round(third_wave_value, 4),
#           third_wave_p = round(third_wave_p, 4),
#           total_value = round(total_value, 4),
#           total_p = round(total_p, 4)
#         ) %>%
#         dplyr::rename(
#           Parameters = vars,
#           `first wave` = first_wave_value,
#           `first wave ` = first_wave_p,
#           `second wave` = second_wave_value,
#           `second wave ` = second_wave_p
#         ) %>%
#         rbind(list("Parameters", "beta", "p-value","beta", "p-value","beta", "p-value", "beta", "p-value"), .) %>%
#         dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
#         dplyr::mutate(Parameters = unlist(lapply(Parameters, var_names))) %>%
#         xtable(.)
#       , include.rownames=F)

# print(xtable(table_with_CI %>% 
#                dplyr::relocate(response.fancy) %>% 
#                dplyr::select(-response, -signif) %>%
#                dplyr::mutate(p.value = round(p.value, 5))), include.rownames=FALSE)
