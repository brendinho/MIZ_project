rm(list=ls())

library(data.table)
library(dplyr)
library(segmented)
library(ptest)

setwd("~/Documents/GitHub/MIZ_project/")

add_waves <- function(tab_here, case_col="cases", date_col="date", days_apart=50)
{ 
    # this function assumes that 1) there are only two waves, 2) exponential growth and decline of case numbers during each phase
    # perhaps this could be better handled with simple breakpoint analysis and the `npsi` argument to segmented (also allowing the 
    #    user to speciufy the number of waves they're looking for), but that's a problem for future me
    
    # get rid of all the extraneous information
    tab_here <- tab_here |>
        dplyr::filter( !is.na(cases) & cases>0) |>
        dplyr::rename(cases=case_col, date=date_col) |>
        dplyr::group_by(date) |> 
        dplyr::tally(cases) |> 
        dplyr::rename(cases=n) |> 
        dplyr::mutate(index=as.numeric(date), date=as.Date(date)) |>
        data.table()
    
    # at least one of the peaks will be an absolute maximum - choose the first one with the max number of cases
    first_peak <- tab_here[cases==max(cases)][1]
    # find the second peak as the absolute maximum on either `days_apart` side of the first peak identified
    second_peak <- tab_here[abs(date-first_peak$date)>days_apart][cases==max(cases)]
    
    # get the dates of the two peaks
    first_peak_date <- min(first_peak$date, second_peak$date)
    second_peak_date <- max(first_peak$date, second_peak$date)
    
    # do a breakpoint analysis to get the valley between the two peaks
    seg_reg <- segmented(
        lm(log(cases) ~ index, data=tab_here[date>=first_peak_date & date<=second_peak_date]),
        seg.Z = ~ index,
        npsi=1
    )
    # positively identify this valley at the start of the second wave
    start_of_second_wave <- tab_here[index == floor(data.table(seg_reg$psi)$Initial)]$date
    # add a column to the table with the number of the wave
    tab_here <- tab_here |> dplyr::mutate(wave = if_else(date>start_of_second_wave, 2, 1))
    
    # return both the date of the second wave and the case table with the information added
    return(list(
        "date"=start_of_second_wave, 
        "cases"=tab_here
    ))
}

print(add_waves(ON_cases))

print(ggplot(ON_cases[HR == "Toronto_Public_Health"], aes(date, cases)) + geom_point(colour="blue") + geom_vline(xintercept=add_waves(ON_cases)$date) + scale_x_date(date_breaks="1 month"))




