library(data.table)

setwd('/home/bren/Documents/GitHub/MIZ_project/')

# source of the data tables giving the CMAs, CAs and MIZs
# https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/comprehensive.cfm

101 # Canada, provinces and territories
201 # census metropolitan areas and census agglomerates
301 # census subdivisions
601 # statistical area classification
701 # census divisions
1601 # census tracts
1404 # economic regions
1701 # aggregate disseminatiom areas
1901 # dissemination areas

datasetNumber <- 101
theTable <- fread(sprintf("T%iEN.CSV", 201))

# 101 - provincial populations
# 201 gives names and classifications
# 301 - CSD types
# 601 - MIZ and population numbers per province, no names
# 701 - Geographic type
# 1401 - has population density


theTable <- theTable[, .SD, .SDcols = ! names(theTable) %like% "french|Indian|V10"]

Oshawa_indices <- which("Oshawa (CMA)" == theTable[['Census metropolitan area / census agglomeration, english']])
