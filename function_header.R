library(dplyr)
library(data.table)
library(segmented)

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

age_range_min <- \(x) age_range(x)$lower
age_range_max <- \(x) age_range(x)$upper

select_year <- \(name, year) if(!grepl("20", name)) name else if(grepl(year, name)) name


replaceAccents <- function(string)
{
    accentedCharacters <- list("é"="e", "è"="e", "ô"="o", "Î"="I", "\xe9"="e", "\xe8"="e", "\x96"="-", "\x97"="-", "\xce"="I", "\xf4"="o")
    for(index in 1:length(accentedCharacters))
    {
        string <- gsub(names(accentedCharacters)[index], accentedCharacters[[index]], string)
    }
    return(string)
}
replace_accents <- \(strings) unlist(lapply(strings, replaceAccents))

trim_numbers <- \(strings) unlist(lapply(strings, function(x) trimws(tail(strsplit(x, " - ")[[1]], 1))))
underscore <- \(strings) unlist(lapply(strings, function(x) paste(strsplit(x, " ")[[1]], collapse="_")))

zone_scores <- \(vec)
{
    zones <- list("none"=0, "weak"=1, "moderate"=2, "strong"=3, "ca"=4, "cma"=5)
    unlist(lapply( tolower(vec), \(x) zones[[x]] ))
}

# extract the first 4 numbers in the geocode for matching CSD to health regions
extract_code <- \(x) as.numeric(paste0(head(strsplit(x, '')[[1]], 4), collapse=''))
extract_codes <- \(theList) unlist(lapply(as.character(theList), extract_code))

remove_CA_name <- function(x)
{
    if(grepl("cma", x)) return("cma")
    if(grepl("ca", x)) return("ca")
    return(x)
}
remove_CA_names <- \(theList) unlist(lapply(theList, remove_CA_name))

get_region_type <- \(x) gsub("\\(|\\)", "", str_extract(x, regex("\\(([^)]+)\\)")))
get_region_types <- \(theList) unlist(lapply(theList, get_region_type))

strip_region_type <- \(x) trimws(strsplit(as.character(x), '\\(')[[1]][1])
strip_region_types <- \(theList) unlist(lapply(theList, strip_region_type))

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
)) |> 
dplyr::rename(province=V1, abbreviations=V2, alpha=V3, SGC=V4, region=V5)

lookup_province <- function(x)
{
    province_number <- substr(as.character(x), 1, 2)
    if(province_number %in% province_LUT$SGC) return(province_LUT[SGC==province_number, province])
    return("")
}
lookup_provinces <- \(theList) unlist(lapply(theList, lookup_province))

HA_crosswalk <- fread("Classifications/health_region_crosswalk.csv") |>
    unique() |>
    dplyr::mutate(
        case_data_name = trimws(case_data_name),
        display_name = trimws(display_name)
    )

standard_HR_name <- \(x) if(trimws(x) %in% HA_crosswalk$case_data_name) HA_crosswalk[case_data_name==trimws(x), display_name] else trimws(x)
standard_HR_names <- \(theList) unlist(lapply(theList, standard_HR_name))

HR_info_mine <- fread("Classifications/HR_info_mine.csv")
HR_info_mine_lookup <- HR_info_mine |> dplyr::select(-CSDUID2016) |> unique()
lookup_HR_mine <- function(x)
{
    if(x %in% unique(HR_info_mine$CSDUID2016))
    {
        temp <- HR_info_mine |> dplyr::filter(CSDUID2016 == x) |> dplyr::select(HR, HRUID2018) |> as.list()
        if(length(temp) > 1) "duplicate" else temp
    } else { "missing" }
}

HR_info_StatCan <- fread("Classifications/HR_info_StatCan.csv")
HR_info_StatCan_lookup <- HR_info_StatCan |> dplyr::select(-CSDUID2016) |> unique()
lookup_HR_StatCan <- function(x)
{
    if(x %in% unique(HR_info_StatCan$CSDUID2016)) {
        temp <- HR_info_StatCan |> dplyr::filter(CSDUID2016 == 1001101) |> dplyr::select(HR, HRUID2018) |> as.list()
        if(length(temp) > 1) "duplicate" else temp
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
                cbind(here[row_index] |> mutate(HR=standard_HR_name(prospectives[index]$HR), HRUID2018=prospectives[index]$HRUID2018))

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

add_wave_numbers <- function(input_table, case_col="cases", date_col="date", days_apart=50)
{ 
    # this function assumes that 1) there are only two waves, 2) exponential growth and decline of case numbers during each phase
    # perhaps this could be better handled with simple breakpoint analysis and the `npsi` argument to segmented (also allowing the 
    #    user to speciufy the number of waves they're looking for), but that's a problem for future me
    
    # get rid of all the extraneous information
    tab_here <- input_table |>        
        dplyr::rename(cases=case_col, date=date_col) |>
        dplyr::filter( !is.na(cases) & cases>0) |>
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
    output_table <- input_table |>
        dplyr::rename(cases = case_col) |>
        dplyr::mutate(wave = if_else(date>start_of_second_wave, 2, 1)) |>
        dplyr::relocate(date, wave) |>
        dplyr::relocate(cases, .after=last_col())

    # return both the date of the second wave and the case table with the information added
    return(list(
        "date"=start_of_second_wave,
        "cases"= output_table
    ))
}

CSD_score_class <- function(x)
{
    if(grepl("cma", tolower(x))) return(1)
    if(grepl("ca", tolower(x))) return(1)
    if(grepl("strong", tolower(x))) return(2)
    if(grepl("moderate", tolower(x))) return(2)
    if(grepl("weak", tolower(x))) return(3)
    if(grepl("none", tolower(x))) return(3)
    return(NA)
}

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


