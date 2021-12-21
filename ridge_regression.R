
## things to fix:
## 
## 1) change the add_HRs function to equally divide the numerical columns instead of copying the values directly (maybe through an optional function argument)
## 2) move the model code to a separate execution file, instead of here
## 3) change vaccination to categorical
## 4) include interaction terms in the model
## 5) figure out graphing for the results
## 6) tell Seyed the good news
## 7) make it a penalised regression (ridge, for example)
## 8) clarify with Seyed whether we're looking at 'only 1 dose' or 'full vaccination' for the vaccine regressor

file.path(PROJECT_FOLDER, "CaseDataFiles/regression_data.rda")

the_model <- lm(
    incidence ~ is_cma + is_ca + is_miz_strong + is_miz_moderate + is_miz_weak + is_miz_none +
        airports + LTCHs + 
        VIs_AL1D + EIs + LIs +
        # total_dwellings + I(log(total_population)) +
        cohort_0_to_4 + cohort_5_to_19 + cohort_20_to_44 + cohort_45_to_64 + cohort_65_and_older +
        total_dwellings,
    data = Regression_Data[wave == 1]
    
)
