# MIZ_project

refresh_all_data.R gets all the incidence data and shapefiles from various sources. you don't need to run this, since the files it produces are in the CaseData folders, and up-to-date.

regression_prelims.R assembles the data sets for the regressions

lasso_and_linea_regression carries out the LASSO and lm regressions on the data set using given covariates and substitution of each of the six measures of remoteness we're testing

function_header.R defines a bunch of functions used later on in the code

choropleths.R plots the choropleth maps and wave diagram for the paper

