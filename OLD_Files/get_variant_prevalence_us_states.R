##################################################
## Project: 
## Script purpose: find the prevalence of various COVID-19 strains in all USA states by date
##
## Script author: Brendon Phillips, Di Shan
## Date written: 2021-08-10
##
## Institution: York University
## Lab: ABM-Lab
## Principal Investigator: Seyed M. Moghadas
##################################################

## Edit: 2021-08-10, B Phillips - initial file
## Edit: 2021-08-11, B Phillips - added ECDC variant classification, with deference to the (American) CDC

rm(list = ls())

library(jsonlite)
library(data.table)
library(dplyr)

######################## !!!! CHECK THIS TABLE, DI !!!! ########################
#
# I'm not sure how to automate the retrieval of this list, but I'll work on it
# ECDC - https://www.ecdc.europa.eu/en/covid-19/variants-concern
# CDC - https://www.cdc.gov/coronavirus/2019-ncov/variants/variant-info.html
# in cases where the CDC and ECDC differ, I've commented the ECCD entries
#    (in some cases, their designation is based on prevalence in EUropean countries)

variant_classifications <- matrix(c(
        c("a.23.1",     NA,       "under monitoring"), # ECDC
        c("a.27",       NA,       "under monitoring"), # ECDC
        c("a.28",       NA,       "under monitoring"), # ECDC
        c("at.1",       NA,       "under monitoring"), # ECDC
        c("av.1",       NA,       "under monitoring"), # ECDC
        c("ay.1",       "delta",  "concern"), # CDC
        c("ay.2",       "delta",  "concern"), # CDC
        c("ay.3",       "delta",  "concern"), # CDC
        c("b.1.1.318",  NA,       "under monitoring"), # ECDC
        c("b.1.1.519",  NA,       "under monitoring"), # ECDC
        c("b.1.1.7",    "alpha",  "concern"), # CDC, ECDC
        c("b.1.214.4",  NA,       "under monitoring"), # ECDC
        c("b.1.351",    "beta",   "concern"), # CDC, ECDC
        c("b.1.351.2",  "beta",   "concern"), # CDC
        c("b.1.351.3",  "beta",   "concern"), # CDC
        c("b.1.427",    NA,       "interest"), # CDC
        # c("b.1.427",  "epsilon", "concern"), # https://science.sciencemag.org/content/373/6555/648, CDC gives no name
        c("b.1.429",    NA,       "interest"), # CDC
        # c("b.1.429",  "epsilon", "concern"), # https://science.sciencemag.org/content/373/6555/648, CDC gives no name
        c("b.1.525",    "eta",    "interest"), # CDC, ECDC
        c("b.1.526",    "iota",   "interest"), # CDC
        # c("b.1.526",  "iota",    "under monitoring") # ECDC
        c("b.1.526.1",  "iota",   "under monitoring"), # ECDC
        c("b.1.526.2",  "iota",   "under monitoring"), # ECDC
        c("b.1.617.1",  "kappa",  "interest"), # CDC, ECDC
        c("b.1.617.2",  "delta",  "concern"), # CDC, ECDC
        c("b.1.617.3",  NA,       "interest"), # CDC
        # c("b.1.617.3",  NA,      "under monitoring"), # ECDC
        c("b.1.620",    NA,       "interest"), # ECDC
        c("b.1.621",    NA,       "interest"), # ECDC
        c("b.1.671.2",  NA,       "under monitoring"), # ECDC
        c("c.1.2",      NA,       "under monitoring"), # ECDC
        c("c.16",       NA,       "under monitoring"), # ECDC
        c("c.36",       NA,       "under monitoring"), # ECDC
        c("c.37",       "lambda", "interest"), # ECDC
        c("p.1",        "gamma",  "concern"), # CDC, ECDC
        c("p.1.1",      "gamma",  "concern"), # CDC
        c("p.1.2",      "gamma",  "concern"), # CDC
        c("p.2",        "zeta",   "under monitoring"), # ECDC
        c("p.3",        "theta",  "interest") # ECDC
    ), ncol = 3, byrow=T) %>% 
    data.table() %>% 
    dplyr::rename(
        pango_lineage = V1, 
        who_label = V2, 
        status = V3
    )

##################################################################################

start_time <- Sys.time()

usa_variant_prevalence <- data.table()

# stats.abb and state.name are R base for US state names and abbreviations
for(us_state in state.abb)
{
    print(us_state)
    
    query <- sprintf("https://api.outbreak.info/genomics/prevalence-by-location-all-lineages?location_id=USA_US-%s", us_state)
    
    api_response <- tryCatch(
    {
       jsonlite::fromJSON(query) # execute API call
    },
    error = function(condition){ 
        list(success=FALSE, results=data.frame()) # fail quetly
    },
    warning = function(condition){
        list(success=FALSE, results=data.frame()) # fail quietly
    })
    
    if(api_response$success) # execute upon success
    {
        usa_variant_prevalence <- rbind(
            usa_variant_prevalence,
            api_response$results %>%
                # add geographic identifiers
                dplyr::mutate(
                    country = "USA",
                    state   = state.name[match(us_state, state.abb)]
                )
        )
    }
}

# left outer join to add the CDC/ECDC classifications to all rows in the original table of variants
usa_variant_prevalence <- merge(
        usa_variant_prevalence, 
        variant_classifications, 
        by.x="lineage", by.y="pango_lineage", 
        all.x=TRUE
    ) %>%
    # pretty order
    dplyr::relocate(date, lineage, country, state, date, lineage, who_label, status)

print(Sys.time() - start_time)

