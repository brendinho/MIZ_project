library(data.table)
library(dplyr)

## things to fix:
## 
## 1) change the add_HRs function to equally divide the numerical columns instead of copying the values directly (maybe through an optional function argument)
## 4) include interaction terms in the model
## 5) figure out graphing for the results
## 7) make it a penalised regression (ridge, for example)
## 8) clarify with Seyed whether we're looking at 'only 1 dose' or 'full vaccination' for the vaccine regressor

## ISSUES - the vaccination factor values are all the same 

# # plot for the vaccination rates after the 
# ggplot(vaxx_info, aes(x=week_end, y=VIs_AL1D, group=province)) +
#     geom_line(size=2) +
#     geom_vline(xintercept=Canada_Wave_Dates)


Regression_Data <- readRDS(file.path(PROJECT_FOLDER, "CaseDataFiles/regression_data.rda"))

# the_model <- lm(
#     incidence ~ is_cma + is_ca + is_miz_strong + is_miz_moderate + is_miz_weak + is_miz_none +
#         airports + LTCHs + 
#         VIs_AL1D + EIs + LIs +
#         # total_dwellings + I(log(total_population)) +
#         cohort_0_to_4 + cohort_5_to_19 + cohort_20_to_44 + cohort_45_to_64 + cohort_65_and_older +
#         total_dwellings,
#     data = Regression_Data[wave == 1]
#     
# )
