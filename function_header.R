##################################################
## Project: COVID-19 regression 
## Script purpose: header file for functions
## Date written: 2021-09-08
## Last edited: 2021-09-08
## Script author: Brendon Phillips 
## Institution: York University 
## Lab: ABM_Lab
## Principal Investigator: Seyed M. Moghadas
##################################################

library(dplyr)
library(data.table)
library(segmented)

if(getElement(Sys.info(), "sysname") == "Windows"){
    PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
    PROJECT_FOLDER <- "/home/bren/Documents/GitHub/MIZ_project"
}

# shorthand
meann <- function(...) mean(..., na.rm=TRUE)
summ  <- function(...) sum(...,  na.rm=TRUE)
sdd   <- function(...) sd(..., na.rm=TRUE)

symmetric.difference <- function(a,b) setdiff(union(a,b), intersect(a,b))

# used for standardising the regression variables
z_transform <- function(x) (x-meann(x))/sdd(x)

# calculating the weekly moving average to smooth the case incidence data
weekly_moving_average <- function(x) stats::filter(x, rep(1,7), sides = 1)/7

# convert vectors of integers to dates from the beginning of (Unix) time
get.Dates <- function(theList)
{
    do.call("c", lapply(
        theList,
        \(xx) as.Date(xx, origin="1970-01-01")
    ))
}

# calculating teh r^2 of the LASSO regression 
calculate_r_squared <- function(actual, predicted)
{
    rss <- summ((predicted - actual)**2)
    tss <- summ((actual - mean(actual))**2)
    return(1 - rss/tss)
}

### SCRUBBING AND PARSING THE VARIOUS DATA FILES

age_range <- function(string)
{
    if(is.na(string) || string=="" )
    {
        min_age <- NA
        max_age <- NA
        
    } else {
        
        string <- tolower(as.character(string))
        the_ages <- strsplit(string, " ")[[1]]
        
        if( grepl("to", string) )
        {
            min_age <- as.numeric(the_ages[1])
            max_age <- as.numeric(the_ages[3])
            
        } else if( grepl("younger", string) ){
            
            min_age <- 0
            max_age <- as.numeric(the_ages[1])
            
        } else if( grepl("older", string) ){
            
            min_age <-  as.numeric(the_ages[1])
            max_age <- NA
        }
    }
    
    return( list(lower=min_age, upper=max_age) )
}

age_range_min <- function(x) age_range(x)$lower
age_range_max <- function(x) age_range(x)$upper

select_year <- function(name, year) if(!grepl("20", name)) name else if(grepl(year, name)) name

replaceAccents <- function(string)
{
    accentedCharacters <- list("é"="e", "è"="e", "ô"="o", "Î"="I", "\xe9"="e", "\xe8"="e", "\x96"="-", "\x97"="-", "\xce"="I", "\xf4"="o")
    for(index in 1:length(accentedCharacters))
    {
        string <- gsub(names(accentedCharacters)[index], accentedCharacters[[index]], string)
    }
    return(string)
}
replace_accents <- function(strings) unlist(lapply(strings, replaceAccents))

trim_numbers <- function(strings) unlist(lapply(strings, function(x) trimws(tail(strsplit(x, " - ")[[1]], 1))))
underscore <- function(strings) unlist(lapply(strings, function(x) paste(strsplit(x, " ")[[1]], collapse="_")))

zone_scores <- function(vec)
{
    zones <- list("none"=0, "weak"=1, "moderate"=2, "strong"=3, "ca"=4, "cma"=5)
    unlist(lapply( tolower(vec), function(x) zones[[x]] ))
}

# extract the first 4 numbers in the geocode for matching CSD to health regions
extract_code <- function(x) as.numeric(paste0(head(strsplit(x, '')[[1]], 4), collapse=''))
extract_codes <- function(theList) unlist(lapply(as.character(theList), extract_code))

remove_CA_name <- function(x)
{
    if(grepl("cma", x)) return("cma")
    if(grepl("ca", x)) return("ca")
    return(x)
}
remove_CA_names <- function(theList) unlist(lapply(theList, remove_CA_name))

get_region_type <- function(x) gsub("\\(|\\)", "", str_extract(x, regex("\\(([^)]+)\\)")))
get_region_types <- function(theList) unlist(lapply(theList, get_region_type))

strip_region_type <- function(x) trimws(strsplit(as.character(x), '\\(')[[1]][1])
strip_region_types <- function(theList) unlist(lapply(theList, strip_region_type))

province_rename_helper <- function(x)
{
    if(x=="PEI") return("Prince Edward Island")
    if(x=="NWT") return("Northwest Territories")
    return(x)
}

# https://www150.statcan.gc.ca/n1/pub/92-195-x/2011001/geo/prov/tbl/tbl8-eng.htm
province_LUT <- data.table(rbind(
    c("Newfoundland and Labrador", "N.L.","NL", "10", "Atlantic"),
    c("Prince Edward Island", "P.E.I.",	"PE", "11", "Atlantic"),
    c("Nova Scotia", "N.S.", "NS", "12", "Atlantic"),
    c("New Brunswick", "N.B.", "NB", "13", "Atlantic"),
    c("Quebec", "Que.", "QC", "24", "Quebec"),
    c("Ontario", "Ont.", "ON", "35", "Ontario"),
    c("Manitoba", "Man.", "MB", "46", "Prairies"),
    c("Saskatchewan", "Sask.", "SK", "47", "Prairies"),
    c("Alberta", "Alta.", "AB", "48", "Prairies"),
    c("British Columbia", "B.C.", "BC", "59", "British Columbia"),
    c("Yukon", "Y.T.", "YT", "60", "Territories"),
    c("Northwest Territories", "N.W.T.", "NT", "61", "Territories"),
    c("Nunavut", "Nvt.", "NU", "62", "Territories")
)) %>% 
dplyr::rename(province=V1, abbreviations=V2, alpha=V3, SGC=V4, region=V5)

lookup_province <- function(x)
{
    province_number <- substr(as.character(x), 1, 2)
    if(province_number %in% province_LUT$SGC) return(province_LUT[SGC==province_number, province])
    return("")
}
lookup_provinces <- function(theList) unlist(lapply(theList, lookup_province))

lookup_alpha <- function(x)
{
    if(tolower(x) %in% tolower(province_LUT$province)) 
        return(province_LUT %>% dplyr::mutate(province=tolower(province)) %>% dplyr::filter(province==tolower(x)) %>% dplyr::pull(alpha))
    if(grepl("canada", tolower(x))) return("CAN")
    return("")
}
lookup_alphas <- function(theList) unlist(lapply(theList, lookup_alpha))

lookup_province_UID <- function(prov_name) province_LUT[province==prov_name, as.numeric(SGC)]
lookup_province_UIDs <- function(theList) unlist(lapply(theList, lookup_province_UID))

HA_crosswalk <- fread(sprintf("%s/Classifications/health_region_crosswalk.csv", PROJECT_FOLDER)) %>%
    unique() %>%
    dplyr::mutate(
        case_data_name = trimws(case_data_name),
        display_name = trimws(display_name)
    )

standard_HR_name <- function(x) if(trimws(x) %in% HA_crosswalk$case_data_name) HA_crosswalk[case_data_name==trimws(x), display_name] else trimws(x)
standard_HR_names <- function(theList) unlist(lapply(theList, standard_HR_name))

HR_info_mine <- fread(sprintf("%s/Classifications/HR_info_mine.csv", PROJECT_FOLDER))
HR_info_mine_lookup <- HR_info_mine %>% dplyr::select(-CSDUID2016) %>% unique()
lookup_HR_mine <- function(x)
{
    if(x %in% unique(HR_info_mine$CSDUID2016))
    {
        temp <- HR_info_mine %>% dplyr::filter(CSDUID2016 == x) %>% dplyr::select(HR, HRUID2018) %>% as.list()
        if(length(temp$`HR`) > 1) "duplicate" else temp
    } else { "missing" }
}

HR_info_StatCan <- fread(sprintf("%s/Classifications/HR_info_StatCan.csv", PROJECT_FOLDER))
HR_info_StatCan_lookup <- HR_info_StatCan %>% dplyr::select(-CSDUID2016) %>% unique()
lookup_HR_StatCan <- function(x)
{
    if(x %in% unique(HR_info_StatCan$CSDUID2016)) {
        temp <- HR_info_StatCan %>% dplyr::filter(CSDUID2016 == x) %>% dplyr::select(HR, HRUID2018) %>% as.list()
        if(length(temp$`HR`) > 1) "duplicate" else temp
    } else { "missing" }
}

add_HRs <- function(table, uid_column, province_column, raw_numbers=FALSE)
{
    here <- data.table(table)
    here[, c("HR", "HRUID2018"):=list("", -1)]
    
    raw_HRUIDs <- c()
    
    for(row_index in 1:nrow(here))
    {
        # print(row_index)
        
        UID <- here[row_index, get(uid_column)]
        
        if(! UID %in% unique(c(HR_info_mine$CSDUID2016, HR_info_StatCan$CSDUID2016))) next

        if(here[row_index, get(province_column)] %in% c("British Columbia", "BC"))
        {
            prospectives <- HR_info_mine[CSDUID2016 == UID, c("HR", "HRUID2018")]
        } else {
            prospectives <- HR_info_StatCan[CSDUID2016 == UID, c("HR", "HRUID2018")]
        }
        
        if(raw_numbers)
        {
            raw_HRUIDs <- c(raw_HRUIDs, prospectives$HRUID2018)
            next
        }

        hr_name <- standard_HR_name(prospectives[1]$HR)
        here[row_index][1]$HR <- hr_name
        # seemed to be the most convenient way to deal with the mismatched names between the StatCan correspondence file and the case data
        here[row_index][1]$HRUID2018 <- if(grepl("huron|perth", tolower(hr_name))) 3539 else prospectives[1]$HRUID
        if(nrow(prospectives) > 1){ for(index in 2:nrow(prospectives[-1]))
        {
            here <- rbind(
                here,
                cbind(here[row_index] %>% mutate(HR=standard_HR_name(prospectives[index]$HR), HRUID2018=prospectives[index]$HRUID2018))

            )
        }}
    }
    if(raw_numbers) return(raw_HRUIDs)
    return(here)
}

add_HRUIDs <- function(table, HR_column="HR", province_column="province")
{
    here <- data.table(table)
    here[, HRUID2018:=as.numeric(NA)]
    
    for(row_index in 1:nrow(here))
    {
        province <- here[row_index, get(province_column)]
        region <- here[row_index, get(HR_column)]

        if(province %in% c("British Columbia", "BC"))
        {
            relevant <- HR_info_mine_lookup[HR==region & Province==province, unique(HRUID2018)]
        } else if(grepl("huron|perth", tolower(region))) {
            relevant <- 3539
        } else {
            relevant <- HR_info_StatCan_lookup[HR==region & Province==province, unique(HRUID2018)]
        } 
        
        if(length(relevant) == 1){ here[row_index]$HRUID2018 <- relevant}
    }
    
    return(here)
}

value_if_number_else_NA <- function(theList)
{
    return(
        unlist(lapply(
            theList, 
            \(x) if(suppressWarnings(is.na(as.numeric(x)))) NaN else as.numeric(x)
        ))
    )
}

# add_wave_numbers <- function(input_table, case_col="cases", date_col="date", num_waves=4, days_apart=45) 
# {
#     weekly_moving_average <- function(x) stats::filter(x, rep(1,7), sides = 1)/7
# 
#     # get rid of all the extraneous information
#     tab_here <- input_table %>%
#         dplyr::rename(cases=case_col, date=date_col) %>%
#         dplyr::filter( !is.na(cases) & cases>0) %>%
#         dplyr::mutate(
#             cases = as.numeric(cases),
#             date = as.Date(date)
#         ) %>%
#         dplyr::group_by(date) %>%
#         dplyr::tally(cases) %>%
#         dplyr::rename(cases=n) %>%
#         dplyr::mutate(
#             index = as.numeric(date),
#             date = as.Date(date),
#             weekly_rolling_avg = weekly_moving_average(cases),
#             smooth_spline = predict(smooth.spline(index, cases, spar=0.35))$y
#         ) %>%
#         # we search for the date with the minimum if cases, so we don't want to trivially get a breakpoint within the first few days
#         dplyr::filter(date >= min(date) + 45) %>%
#         data.table()
#     
#     return(tab_here)
# 
#     # print(tab_here[date %in% as.Date(c("2020-05-01", "2020-07-01", "2021-01-01", "2021-02-22", "2021-04-15", "2021-06-30"))])
# 
#     # do a breakpoint analysis to get the valley between the two peaks
#     seg_reg <- segmented(
#         # lm(log(weekly_rolling_avg) ~ index, data=tab_here),
#         lm(log(smooth_spline) ~ index, data=tab_here),
#         seg.Z = ~ index,
#         npsi = 2*num_waves-1
#         # psi = tab_here[date %in% as.Date(c("2020-05-01", "2020-07-01", "2021-01-01", "2021-02-22", "2021-04-15", "2021-06-30")), index]
#     )
# 
#     break_dates <- tab_here[index %in% floor(data.frame(seg_reg$psi)$`Est.`), date]
#     wave_dates <- break_dates[ ! (1:length(break_dates) %% 2) ]
#     
#     # remember to actually add the wave numbers
#     
#     return(list(
#         dates.breaks = break_dates,
#         dates.waves = c(min(tab_here$date), break_dates[!(1:length(break_dates)%%2)]),
#         rgeression = seg_reg,
#         data = tab_here
#     ))
# }

CSD_score_normal <- function(x)
{
    if(grepl("cma", tolower(x))) return(1)
    if(grepl("ca", tolower(x))) return(2)
    if(grepl("strong", tolower(x))) return(3)
    if(grepl("moderate", tolower(x))) return(4)
    if(grepl("weak", tolower(x))) return(5)
    if(grepl("none", tolower(x))) return(6)
    return(NA)
}

# https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
# Tommy's answer to "Finding local maxima and minima "
localMaxima <- function(x) 
{
    # Use -Inf instead if x is numeric (non-integer)
    y <- diff(c(-.Machine$integer.max, x)) > 0L
    # rle(y)$lengths
    y <- cumsum(rle(y)$lengths)
    y <- y[seq.int(1L, length(y), 2L)]
    if (x[[1]] == x[[2]]) { y <- y[-1] }
    return(y)
}

find_wave_indices <- function(timeSeries)
{
    data_table <- data.table(cases = timeSeries) %>%
        dplyr::mutate(
            index = 1:nrow(.), 
            spline = predict(smooth.spline(index, cases, spar=0.7))$y
        )
    
    valley_indices <- localMaxima(-data_table$spline) %>% 
        .[! . %in% c(1:50, (nrow(data_table)-15):nrow(data_table))]
    
    return(valley_indices)
}

# doing the wave analysis for all of Canada

Canada_Data <- fread(file.path(PROJECT_FOLDER, "CaseDataTables/all_canada.csv")) %>%
    dplyr::select(-cumulative_cases) %>%
    dplyr::mutate(
        date = as.Date(date_report, format="%d-%m-%Y"),
        moving_avg_seven_days = weekly_moving_average(cases),
        moving_avg_seven_days = ifelse(is.na(moving_avg_seven_days), 0, moving_avg_seven_days),
        index = as.numeric(date),
        spline = predict(smooth.spline(index, moving_avg_seven_days, spar=0.7))$y
    ) %>%
    data.table

# getting the dates for the waves
Canada_Wave_Dates <- Canada_Data[find_wave_indices(Canada_Data$spline)] %>% 
    dplyr::pull(date) %>%
    as.Date %>% 
    c(min(Canada_Data$date), Sys.time()) %>% 
    sort

canada_waves <- list(
    `1` = list(
        dates = Canada_Wave_Dates[1:2],
        integers = Canada_Wave_Dates[1]:Canada_Wave_Dates[2]
    ),
    `2` = list(
        dates = Canada_Wave_Dates[2:3],
        integers = Canada_Wave_Dates[2]:Canada_Wave_Dates[3]
    ), 
    `3` = list(
        dates = Canada_Wave_Dates[3:4],
        integers = Canada_Wave_Dates[3]:Canada_Wave_Dates[4]
    ),
    `4` = list(
        dates = Canada_Wave_Dates[4:5],
        integers = Canada_Wave_Dates[4]:Canada_Wave_Dates[5]
    )
    
)

how_much_of_the_wave <- function(the_dates, which_wave = 1)
{
    # if there are no dates, return 0
    if(length(the_dates) == 0) return("None")
    # sort the integer dates
    theDates <- sort(unique(the_dates))
    # select a valid wave
    if(! which_wave %in% 1:4) return(NA)
    #select the dates of the wave requested
    theDates <- theDates %>% .[
        . >= min(canada_waves[[which_wave]]$integers) & 
        . < max(canada_waves[[which_wave]]$integers)
    ]
    # if there are no dates, return 0
    if(length(theDates) == 0) return("None")
    # print(theDates)
    # if non-zero length, but the intervention dates don't span the entire period
    if(abs(length(theDates) - length(canada_waves[[which_wave]]$integers))>=2) return("Partial")
    return("Entire")
}

is.Date <- function(theList)
{
   return( unlist(lapply(
        theList,
        \(x) as.character(x) %>% substr(start = 1, stop = 10) %>% as.Date(tz = 'UTC', format = '%Y-%m-%d') %>% is.na() %>% `!`
    )) )
}

get.Date <- function(theList)
{
    unlist(lapply(
        theList,
        \(x) tryCatch(
            { x %>% as.character() %>% substr(start = 1, stop = 10) },
            error = function(e) as.Date(NA)
        )
    )) %>% as.Date(., origin="1970-01-01")
    # re-casting because the list is always being cast to numeric and I don't know why
}

date_corrector <- function(theDateString)
{
    if(nchar(theDateString) == 0) return("")
    if(is.na(str_extract(theDateString, "\\d"))) return("-1")
    
    the_split <- strsplit(theDateString, '-')[[1]]
    len <- length(the_split);
    
    string <- theDateString
    if(len == 2) 
    {
        string <- paste0(theDateString, "-01")
    } else if(len == 1){
        string <- paste0(theDateString, "-01-01")
    }
    
    string <- string  %>% 
        gsub("-+", "-", .) %>%
        (\(x) if(nchar(the_split[1]) == 2) as.Date(x, format="%m-%d-%Y") else as.Date(x)) %>%
        as.numeric %>% 
        as.character
    
    return(string)
}

# calculate the confidence interval of some data
CI <- function(data, two_sided_inter) 
{
    meann <- mean(data)
    sdd<- sd(data)
    N <- length(data)
    if(N < 50)
    {
        T_or_N <- qt( (two_sided_inter+1)/2, df=N-1) 
    } else {
        T_or_N <- qnorm((two_sided_inter+1)/2)
    }
    
    error <- T_or_N*sdd/sqrt(N)
    
    return(list(low=meann-error, up=meann+error))
}



# add_wave_numbers <- function(input_table, case_col="cases", date_col="date", num_waves=4, days_apart=45) 
# {
#     weekly_moving_average <- function(x) stats::filter(x, rep(1,7), sides = 1)/7
#     
#     # get rid of all the extraneous information
#     tab_here <- input_table %>%
#         dplyr::rename(cases=case_col, date=date_col) %>%
#         dplyr::filter( !is.na(cases) & cases>0) %>%
#         dplyr::mutate(
#             cases = as.numeric(cases),
#             date = as.Date(date)
#         ) %>%
#         dplyr::group_by(date) %>%
#         dplyr::tally(cases) %>%
#         dplyr::rename(cases=n) %>%
#         dplyr::mutate(
#             index = as.numeric(date),
#             date = as.Date(date),
#             weekly_rolling_avg = weekly_moving_average(cases),
#             smooth_spline = predict(smooth.spline(index, cases, spar=0.6))$y
#         ) %>%
#         # we search for the date with the minimum if cases, so we don't want to trivially get a breakpoint within the first few days
#         dplyr::filter(date >= min(date) + 45) %>%
#         data.table()
#     
#     # discrete second derivative
#     num_maxima <- which(diff(sign(diff(case_data$smooth_spline)))==-2)+1
#     
#     number_of_waves <- min(num_waves, num_maxima-1)
#     
#     return(number_of_waves)
#     
#     # return(tab_here)
#     
#     # do a breakpoint analysis to get the valley between the two peaks
#     seg_reg <- segmented(
#         # lm(log(weekly_rolling_avg) ~ index, data=tab_here),
#         lm(log(smooth_spline) ~ index, data=tab_here),
#         seg.Z = ~ index,
#         npsi = 2*num_waves-1
#         # psi = tab_here[date %in% as.Date(c("2020-05-01", "2020-07-01", "2021-01-01", "2021-02-22", "2021-04-15", "2021-06-30")), index]
#     )
#     
#     return(seg_reg)
#     
#     break_dates <- tab_here[index %in% floor(data.frame(seg_reg$psi)$`Est.`), date]
#     wave_dates <- break_dates[ ! (1:length(break_dates) %% 2) ]
#     
#     # remember to actually add the wave numbers
#     
#     return(list(
#         dates.breaks = break_dates,
#         dates.waves = c(min(tab_here$date), break_dates[!(1:length(break_dates)%%2)]),
#         rgeression = seg_reg,
#         data = tab_here
#     ))
# }

