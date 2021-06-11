age_range <- function(string)
{
    if(is.na(string))
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
)) |> rename(province=V1, abbreviations=V2, alpha=V3, SGC=V4, region=V5)

lookup_province <- function(x)
{
    province_number <- substr(as.character(x), 1, 2)
    if(province_number %in% province_LUT$SGC) return(province_LUT[SGC==province_number, province])
    return("")
}
lookup_provinces <- \(theList) unlist(lapply(theList, lookup_province))


