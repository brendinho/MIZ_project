[//]: # (Brendon Phillips, Postdoctoral Visitor, Computational Epidemiology, ABM-Lab, York University, Toronto ON)

## _Assessing the effects of region remoteness, demographic factors and public health interventions on COVID-19 pandemic waves in Canada_

# Code Files

### `function_header.R`
- contains helper functions for gathering and parsing data
- calculates the starts and ends of pandemic waves in Canada-wide case incidence

### `refresh_all_data.R`
- assembles 2016 census profiles for each province and calculates the age cohort sizes in each census subdivision (CSD)
- gathers a list of all the census metropolitan areas (CMAs), census agglomerations (CAs), strongly metropolitan-influenced zones (Strong), moderately metropolitan-influenced zones (Moderate), weakly metropolitan-influenced zones (Weak), Territories, and Zones with no metropolitan influence (None)
- reads the locations of long-term care homes (LTCHs) in Canada and tallies the number of LTCHs in each public health unit (PHU)
- scans timelines of public health interventions enacted in Canada, parses and cleans the data set, and finds dates for school closures and lockdowns (per province)
- calculates rates of vaccine administration (at least one dose) per province
- downloads COVID-19 data for each province (by PHU); BC, Quebec, Saskatchewan, Ontario, Alberta from provincial government, all other provinces and territories from the OpenCovid API [https://opencovid.ca/api/]
- combines all data (remoteness, age cohort tallies, LTCH tallies, shapefile information) into a table of PHU profiles

### `ridge_regression.R`
- implements OLS and Ridge regression models for each wave

### `graphs.R`
- outputs LaTeX-formatted tables for effect sizes, confidence intervals and p values, summary statistics describing the data sets, etc.
- outputs visualisations of LTCH locations, Canada-wide incidence and waves of infection, intervention timelines (per province)

### Running
The order of execution would be: `refresh_all_data.R` (to generate a `regression_data.R` file), `ridge_regression.R` (to rerun the L2 and OLS regressions), and `graphs.R` to output statistics tables and figures. However, these files won't redownload data if the data files are already present (this can be changed by commenting `if(!file.exists(...` lines).

# Folders
### CaseDataTables
contains the COVID-19 case incidence data files for each province (broken into PSUs)
### Census_Profile_zips
zip files containing Statistics Canada census profiles (2016) for each province 

### Classifications
files related to the Statistics Canada statistical area classifications (SACs) of each CSD, lookup tables mapping CSDs to PHUs, public health intervention timelines

### Graphs
folder for the graphs produces by the `graphs.R` file

### Interventions
public health intervention scan data from the Canadian Institute for Health Information

### ODHF_v1.1
LTCH locations (from the Open Database of Healthcare Facilities)

### Spatial_Features
shapefiles used for generating choropleths and sorting LTCHs, airports, etc by the PHU whose boundaries it falls in



