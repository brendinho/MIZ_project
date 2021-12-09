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

# PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)
PROJECT_FOLDER <- "/home/bren/Documents/GitHub/MIZ_project/"

# shorthand
meann <- function(...) mean(..., na.rm=TRUE)
summ  <- function(...) sum(...,  na.rm=TRUE)
sdd   <- function(...) sd(..., na.rm=TRUE)

# used for standardising the regression variables
z_transform <- function(x) (x-meann(x))/sdd(x)

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
    return("")
}
lookup_alphas <- function(theList) unlist(lapply(theList, lookup_alpha)) 

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

add_HRs <-  function(table, uid_column, province_column)
{
    here <- data.table(table)
    here[, c("HR", "HRUID2018"):=list("", -1)]
    
    for(row_index in 1:nrow(here))
    {
        UID <- here[row_index, get(uid_column)]

        if(here[row_index, get(province_column)] %in% c("British Columbia", "BC"))
        {
            prospectives <- HR_info_mine[CSDUID2016 == UID, c("HR", "HRUID2018")]
        } else {
            prospectives <- HR_info_StatCan[CSDUID2016 == UID, c("HR", "HRUID2018")]
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

# we've decided to add the breaks manually
# add_wave_numbers <- function(input_table, case_col="cases", date_col="date", num_waves=4, days_apart=45) # make it a variadic function for extimated breakpoints 
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
#             date=as.Date(date),
#             avg = weekly_moving_average(cases)
#         ) %>%
#         # we search for the date with the minimum if cases, so we don't want to trivially get a breakpoint within the first few days
#         dplyr::filter(date >= min(date) + 45) %>%
#         data.table()
#     
#     print(tab_here[date %in% as.Date(c("2020-05-01", "2020-07-01", "2021-01-01", "2021-02-22", "2021-04-15", "2021-06-30"))])
# 
#     # do a breakpoint analysis to get the valley between the two peaks
    # seg_reg <- segmented(
    #     lm(log(avg) ~ index, data=tab_here),
    #     seg.Z = ~ index,
    #     # npsi = num_waves-1
    #     psi = tab_here[date %in% as.Date(c("2020-05-01", "2020-07-01", "2021-01-01", "2021-02-22", "2021-04-15", "2021-06-30")), index]
    # )
#     
#     break_dates <- tab_here[index %in% ceiling(data.frame(seg_reg$psi)$`Est.`), date]
# 
#     return(break_dates)
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


