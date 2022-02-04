rm(list=ls())

library(data.table)
library(ggplot2)
library(vistime)
library(plotly)
library(stringr)
library(sf)

PROJECT_FOLDER <- dirname(rstudioapi::getSourceEditorContext()$path)

source(file.path(PROJECT_FOLDER, "function_header.R"))

dir.create(file.path(PROJECT_FOLDER, "Graphs"), showWarnings=FALSE)

##################### INTERVENTION TIMELINES


# # OUTDATED
# # verified
# # function to parse and collate the overlaps of multiple timelines, per required jurisdiction
# valmorphanize <- function(DATA, JURIS)
# {
#     DATA %>% dplyr::filter(jurisdiction == JURIS) %>%
#         split(1:nrow(.)) %>%
#         lapply(\(xx) seq(xx$date.implemented, xx$effective.until)) %>%
#         unlist %>% unname %>% unique %>% sort %>%
#         (\(xx) {c(min(xx), max(xx), xx[which(diff(xx)>1)], xx[which(diff(xx)>1)+1])}) %>%
#         sort %>% split(., ceiling(seq_along(.)/2)) %>%
#         lapply(\(xx) data.table(JURIS, t(xx))) %>% rbindlist %>%
#         setNames(c("jurisdiction", "start", "end")) %>%
#         dplyr::mutate(across(c("start", "end"), ~as.Date(.x, origin="1970-01-01")))
# }

TEI  <- fread(file.path(PROJECT_FOLDER, "Classifications/tightened_educational_restrictions.R"))
LDM <- fread(file.path(PROJECT_FOLDER, "Classifications/lockdown_measures.csv"))
VAX <- fread(file.path(PROJECT_FOLDER, "Classifications/vaxx_info_dates.csv")) %>%
    dplyr::mutate(date.implemented = as.numeric(week_end)-7, effective.until = week_end, jurisdiction = province)

if(!file.exists( file.path(PROJECT_FOLDER, "Classifications/timeline_data.csv") ))
{
    timelines <- rbind(
            lapply(unique(VAX$jurisdiction), \(xx) valmorphanize(VAX, xx)) %>% rbindlist %>%
                dplyr::mutate(measure = "Vaccination", colour = "lightgreen"),
            lapply(unique(LDM$jurisdiction), \(xx) valmorphanize(LDM, xx)) %>% rbindlist %>%
                dplyr::mutate(measure = "Lockdown", colour = "lightblue"),
            lapply(unique(TEI$jurisdiction), \(xx) valmorphanize(TEI, xx)) %>% rbindlist %>%
                dplyr::mutate(measure = "School Closure", colour = "red")
        ) %>%
        dplyr::mutate(alpha = lookup_alphas(jurisdiction)) %>%
        dplyr::rename(province = jurisdiction)

    fwrite(timelines, file.path(PROJECT_FOLDER, "Classifications/timeline_data.csv"))
}
timelines <- fread(file.path(PROJECT_FOLDER, "Classifications/timeline_data.csv"))

# make a dummy plot to get the y min and max for the vertical wave lines
y_min_max <- plotly_build(
    timelines %>%
        dplyr::mutate(
            start = as.POSIXct(as.Date(start, origin="1970-01-01")),
            end  = as.POSIXct(as.Date(end,  origin="1970-01-01"))
        ) %>%
        gg_vistime(
            col.event="measure",
            col.group="measure",
            col.start="start",
            col.end="end",
            # col.color="colour",
            show_labels=FALSE
        )
    )$x$layout$yaxis$range

# alter the library code to get all interventions of the same type on the same line
pl_timeline <- timelines %>%
    gg_vistime(col.event="measure", col.group="alpha", col.color="colour", show_labels=FALSE) +
    geom_vline(xintercept = as.POSIXct(Canada_Wave_Dates)) + # wave dates are defined in the function header
    scale_x_datetime(
        breaks = seq(
            min(as.POSIXct(timelines$start)),
            as.POSIXct(Sys.Date()),
            "months"
        ),
        date_labels = "%b %Y",
        expand = c(0,0)
    ) +
    theme(
        axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15)
    ) +
    labs(x="Month", y="Province")
    # ggsave(pl_timeline, file = file.path(PROJECT_FOLDER, "Graphs/interventionsplot.png"), width=10, height=6)
    
##################### AIRPORT LOCATIONS ON THE MAP

All_LTCHs <- fread(file.path(PROJECT_FOLDER, "Classifications/LTCH_locations.csv")) %>%
    dplyr::mutate(geometry = lapply(geometry, \(xx) st_point(as.numeric(strsplit(xx, "\\|")[[1]])))) %>%
    st_sf() %>%
    dplyr::mutate(geometry = st_sfc(geometry))
    st_crs(All_LTCHs$geometry) = 4326

All_Canada <- st_sf(readRDS(file.path(PROJECT_FOLDER, "Spatial_Features/All_Canada.rds")))

All_Provinces <- readRDS(file.path(PROJECT_FOLDER, "Spatial_Features/All_Provinces.rds")) %>%
    st_sf() %>%
    merge(
        province_LUT %>% dplyr::select(province, abbreviations, alpha),
        all=TRUE, by="province"
    )

Canada_map_with_HRs_and_LTCHs <- ggplot() +
    geom_sf(
        data = All_Canada, fill="lightgrey", colour="black",
        inherit.aes=FALSE, size=0.5
    ) +
    geom_sf(
        data = All_Provinces, fill=NA, colour="black",
        inherit.aes=FALSE, size=0.5
    ) +
    geom_sf(
        data = All_LTCHs$geometry, inherit.aes=FALSE, size=2,
        pch=8, colour='red'
    ) +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=13),
        legend.title = element_text(size=13),
        legend.key.height = unit(1, 'cm'),
        plot.background = element_rect(fill="white", colour="white"),
        legend.position="none"
    ) +
    coord_sf(datum=NA) +
    labs(x="", y="")

ggsave(
        Canada_map_with_HRs_and_LTCHs,
        file = sprintf("%s/Graphs/Canada_ltch.png", PROJECT_FOLDER),
        width=15, height=6
    )
    trim_image("Canada_ltch.png")

##################### WAVE ANLYSIS AND PLOTS
    
Total_Case_Data <- fread(sprintf(
        "%s/CaseDataTables/Total_Case_Data.csv",
        PROJECT_FOLDER
    ))

Regions <- data.table(readRDS(file.path(PROJECT_FOLDER, "Spatial_Features/Regions.rda")))

if(!file.exists(
    file.path(PROJECT_FOLDER, "CaseDataTables/all_canada.csv")
))
{
    Canada_case_data <- jsonlite::fromJSON(
        "https://api.opencovid.ca/timeseries?loc=canada")$cases %>%
        dplyr::mutate(date_report=as.Date(date_report, format="%d-%m-%Y"))
        fwrite(
            Canada_case_data,
            file.path(PROJECT_FOLDER, "CaseDataTables/all_canada.csv")
        )
}

Canada_case_data <- fread(file.path(PROJECT_FOLDER,
    "CaseDataTables/canada_total_cases.csv")) %>%
    dplyr::mutate(
        index = as.numeric(date_report),
        smooth_spline = pmax(predict(smooth.spline(index,cases,spar=0.75))$y,0),
        moving_average = weekly_moving_average(cases)
    )

shade_colour <- "yellow"
threshold_date <- as.Date("2021-12-01")

pl_canada_waves <- ggplot(
        Canada_case_data[date_report < threshold_date],
        aes(x=date_report)
    ) +
    annotate("rect",
        xmin = Canada_Wave_Dates[1], xmax = Canada_Wave_Dates[2],
        ymin = -Inf, ymax = Inf, fill = shade_colour, alpha = 0.1
    ) +
    annotate("rect",
        xmin = Canada_Wave_Dates[2], xmax = Canada_Wave_Dates[3],
        ymin = -Inf, ymax = Inf, fill = shade_colour, alpha = 0.25
    ) +
    annotate("rect",
         xmin = Canada_Wave_Dates[3], xmax = Canada_Wave_Dates[4],
         ymin = -Inf, ymax = Inf, fill = shade_colour, alpha = 0.4
    ) +
    annotate("rect",
        xmin = Canada_Wave_Dates[4], xmax = Canada_Wave_Dates[5],
        ymin = -Inf, ymax = Inf, fill = shade_colour, alpha = 0.5
    ) +
    annotate("rect",
        xmin = Canada_Wave_Dates[5], xmax = threshold_date,
        ymin = -Inf, ymax = Inf, fill = shade_colour, alpha = 0.6
    ) +
    geom_point(aes(y = cases), size = 2) +
    geom_line(aes(y = moving_average), size = 1.5, colour = "blue") +
    # geom_line(aes(y = smooth_spline), size = 1, colour = "red") +
    theme_bw() +
    theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle=45, hjust=1)
    ) +
    geom_vline(xintercept = Canada_Wave_Dates) +
    scale_x_date(expand=c(0,0), date_breaks="1 month", date_labels="%d %b %y")+
    scale_y_continuous(
        expand = c(0,0),
        breaks = Canada_case_data[, seq(min(cases), max(cases), by=2000)]
    ) +
    labs(x="Date", y="Cases")

ggsave(
        pl_canada_waves,
        file = file.path(PROJECT_FOLDER, "Graphs/canadawaves.png"),
        height=5, width=10
    )

################## GRAPH SAC STATISTICS

Regions <- data.table(readRDS(file.path(
        PROJECT_FOLDER,
        "CaseDataTables/Regions.rda"
    )))

province_metrics <- Regions[, .(
        num_HR = length(unique(HR)),
        num_csd = sum(num_csds, na.rm=TRUE),
        population = sum(population, na.rm=TRUE),
        area_sq_km = as.numeric(sum(st_area(geometry), na.rm=TRUE)/1000**2)
    ), by=.(province)] %>%
    dplyr::mutate(
        province = factor(province, levels=as.character(.[order(num_csd)]$province)),
    ) %>%
    merge(., province_LUT, by="province", all=TRUE)

scaling <- province_metrics[, max(num_HR)/max(num_csd)]

SAC_distribution <- ggplot(
        province_metrics,
        aes(x=str_wrap(alpha, 15), group=province)
    ) +
    geom_bar(aes(y=num_HR), position="dodge", stat="identity") +
    geom_line(aes(y=num_csd*scaling, group=1), colour="blue", alpha=0.75, size=2) +
    scale_y_continuous(
        sec.axis=sec_axis(trans=~./scaling, name="Census subdivisions contained"),
        expand=c(0,0)
    ) +
    labs(y="Public Health Units", x="Province") +
    theme_bw() +
    theme(
        axis.text.y = element_text(size=13),
        axis.title = element_text(size=15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=14),
        axis.text.x = element_text(size=14), #, angle=45, hjust=1), # angle=45, vjust=1, hjust=1
        legend.position = "top",
        legend.direction="horizontal",
        axis.line.y.right = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "blue"),
        axis.title.y.right = element_text(color = "blue"),
        axis.text.y.right = element_text(color = "blue"),
    ) +
    scale_fill_brewer(type="div", palette="BrBG") +
    guides(fill=guide_legend(title="Number of health regions", nrow=1))

ggsave(
        SAC_distribution,
        file=sprintf("%s/Graphs/SACdistribution.png", PROJECT_FOLDER),
        width=10, height=4
    )


###################### REGRESSION STATISTICS

Regression_Data <- readRDS(file.path(
    PROJECT_FOLDER,
    "Classifications/regression_results.rda"
))

############## INFO FOR LATEX TABLES

pretty_variable_names <- function(tab) return( tab %>% 
dplyr::mutate(pretty_regressor = factor(regressor, levels = c(
    "LTCHs", "previous_wave_incidence", "group_0_to_4", "group_5-19", 
    "group_20_to_49", "group_50_to_64", "group_65_plus",
    "interaction_vaccination_0_to_4", "interaction_vaccination_5_to_19", 
    "interaction_vaccination_20_to_49", "interaction_vaccination_50_to_64", 
    "interaction_vaccination_65_plus", "EIs",
    "interaction_children_school_closures", "is_cma", "is_ca", 
    "is_miz_strong", "is_miz_moderate", "is_miz_weak", "is_miz_none", 
    "PHU_pop_density", "interaction_popdensity_cma", 
    "interaction_popdensity_strong", "interaction_popdensity_moderate", 
    "interaction_popdensity_weak", "interaction_popdensity_none"
))) %>%
dplyr::mutate(pretty_regressor = dplyr::recode(regressor,
    "LTCHs" = "LTCH",
    "School Closures" = "$E$",
    "EIs" = "E",
    "group_0_to_4" = "$C_{0-4}$",
    "group_5_to_19" = "$C_{5-19}$",
    "group_20_to_49" = "$C_{20-49}$",
    "group_50_to_64" = "$C_{50-64}$",
    "group_65_plus" = "$C_{\\ge65}$",
    "PHU_pop_density" = "$D$",
    "interaction_children_school_closures" = "$E\\cdot C_{5-19}$",
    "is_cma" = "CMA",
    "is_ca" = "CA",
    "is_miz_strong" = "Strong",
    "is_miz_moderate" = "Moderate",
    "is_miz_weak" = "Weak",
    "is_miz_none" = "None",
    "previous_wave_incidence" = "$Y_{u-1}$",
    "interaction_popdensity_cma" = "$D\\cdot\\text{CMA}$",
    "interaction_popdensity_ca" = "$D\\cdot\\text{CA}$",
    "interaction_popdensity_moderate"="$D\\cdot\\text{MIZ}_{\\text{moderate}}$",
    "interaction_popdensity_strong" = "$D\\cdot\\text{MIZ}_{\\text{strong}}$",
    "interaction_popdensity_weak" = "$D\\cdot\\text{MIZ}_{\\text{weak}}$",
    "interaction_popdensity_none" = "$D\\cdot\\text{MIZ}_{\\text{none}}$",
    "interaction_vaccination_0_to_4" = "$v_{\\text{prov}}\\cdot C_{0-4}$",
    "interaction_vaccination_5_to_19" = "$v_{\\text{prov}}\\cdot C_{5-19}",
    "interaction_vaccination_20_to_49" = "$v_{\\text{prov}}\\cdot C_{20-49}$",
    "interaction_vaccination_50_to_64" = "$v_{\\text{prov}}\\cdot C_{50-64}$",
    "interaction_vaccination_50_to_64" = "$v_{\\text{prov}}\\cdot C_{50-64}$",
    "interaction_vaccination_65_plus" = "$v_{\\text{prov}}\\cdot C_{\\ge65}$",
    "interaction_LTCH_65_plus" = "\\text{LTCH}\\cdot C_{\\ge65}",
    "interaction_LTCHs_65_plus" = "\\text{LTCH}\\cdot C_{\\ge65}"
)))

latex_regression_coefficients <- Reduce(
    function(x, y) merge(x, y, by = c("wave", "regressor"), all = TRUE),
    list(
        Regression_Data$information %>% dplyr::select(regressor, wave) %>% 
            unique(),
        Regression_Data$information %>% filter(type == "Ridge.glmnet") %>%
            dplyr::select(wave, regressor, coefficient) %>%
            setNames(sprintf("Ridge.glmnet.%s", names(.))) %>%
            dplyr::rename(
                wave = Ridge.glmnet.wave,
                regressor = Ridge.glmnet.regressor
            ),
        Regression_Data$information %>% filter(type == "Ridge.hdi") %>%
            dplyr::select(-type) %>%
            setNames(sprintf("Ridge.hdi.%s", names(.))) %>%
            dplyr::rename(wave=Ridge.hdi.wave, regressor=Ridge.hdi.regressor),
        Regression_Data$information %>% filter(type == "OLS") %>% 
            dplyr::select(-type) %>%
            setNames(sprintf("OLS.%s", names(.))) %>%
            dplyr::rename(wave=OLS.wave, regressor=OLS.regressor)
    )) %>%
    dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
    dplyr::select(wave, regressor, matches("coefficient|CI|p.value")) %>%
    dplyr::mutate(
        Ridge.hdi = ifelse(
            Ridge.hdi.p.value < 0.001,
            sprintf(
                "$%.4f\\;(%.4f,\\;%.4f),\\;p < 0.001$",
                Ridge.hdi.coefficient, Ridge.hdi.CI025, Ridge.hdi.CI975
            ),
            sprintf("$%.4f\\;(%.4f,\\;%.4f),\\;p = %.4f$",
                    Ridge.hdi.coefficient, Ridge.hdi.CI025, 
                    Ridge.hdi.CI975, Ridge.hdi.p.value
            )
        ),
        Ridge.glmnet = sprintf("$%.3f$", Ridge.glmnet.coefficient),
        OLS = ifelse(
            OLS.p.value < 0.001,
            sprintf("$%.4f\\;(%.4f,\\;%.4f),\\;p < 0.001$", 
                    OLS.coefficient, OLS.CI025, OLS.CI975
            ),
            sprintf("$%.4f\\;(%.4f,\\;%.4f),\\;p = %.4f$", 
                    OLS.coefficient, OLS.CI025, OLS.CI975,
                    OLS.p.value
            )
        )
    ) %>%
    dplyr::select(wave, regressor, Ridge.hdi, Ridge.glmnet, OLS)

# latex_regression_coefficients %>% pretty_variable_names() %>% dplyr::select(-regressor) %>% dplyr::rename(regressor = pretty_regressor) %>% dplyr::relocate(regressor) %>% dplyr::filter(wave == 2) %>% dplyr::select(-wave, -Ridge.glmnet) %>% xtable %>% print.xtable(include.rownames=FALSE)

fwrite(
    latex_regression_coefficients %>% 
        pretty_variable_names() %>%
        dplyr::select(-regressor) %>%
        dplyr::rename(regressor = pretty_regressor) %>%
        dplyr::relocate(regressor) %>%
        dplyr::filter(wave == 2) %>%
        dplyr::select(-wave, -Ridge.glmnet), 
    file = file.path(
        PROJECT_FOLDER, 
        "Classifications/latex_regression_coefficients_wave_2.csv"
    )
)

fwrite(
    latex_regression_coefficients %>% 
        pretty_variable_names() %>% 
        dplyr::arrange(regressor), 
    file = file.path(
        PROJECT_FOLDER, 
        "Classifications/latex_regression_coefficients.csv"
    )
)

latex_regression_descriptives <- Regression_Data$data %>%
    dplyr::select(
        -province, -HRUID2018, -HR, -pruid, -wave, -incidence, -PROV_population,
        -matches("interaction|previous|vaxx"), -VIs, -EIs, -LIs
    ) %>%
    unique() %>%
    dplyr::mutate(
        PHU_population = PHU_population/1000,
        group_0_to_4 = group_0_to_4/100,
        group_5_to_19 = group_5_to_19/100,
        group_20_to_49 = group_20_to_49/100,
        group_50_to_64 = group_50_to_64/100,
        group_65_plus = group_65_plus/100
    ) %>%
    dplyr::summarise(across(
        names(.),
        \(x) sprintf(
            "%.3f (%.3f), range: (%.3f,\\;%.3f), IQR: %.3f",
            mean(x), sd(x), min(x), max(x), IQR(x)
        )
    )) %>%
    as.list %>%
    data.table(regressor = names(.), description = .) %>%
    dplyr::mutate(regressor = dplyr::recode(regressor,
                                            "PHU_population (x 1000)" = "Population",
                                            "PHU_pop_density" = "Population Density",
                                            "group_0_to_4 (x 100)" = "$C_{0-4}$",
                                            "group_5_to_19 (x 100)" = "$C_{5-19}$",
                                            "group_20_to_49 (x 100)" = "$C_{20-49}$",
                                            "group_50_to_64 (x 100)" = "$C_{50-64}$",
                                            "group_65_plus (x 100)" = "$C_{\\ge65}$",
                                            "is_cma" = "CMA",
                                            "is_ca" = "CA",
                                            "is_miz_strong" = "MIZ textsubscript{strong}",
                                            "is_miz_moderate" = "MIZ textsubscript{mod}",
                                            "is_miz_weak" = "MIZ textsubscript{weak}",
                                            "is_miz_weak" = "MIZ textsubscript{weak}",
                                            "is_miz_none" = "MIZ textsubscript{none}"
    ))
fwrite(
    latex_regression_descriptives, 
    file = file.path(
        PROJECT_FOLDER, 
        "Classifications/latex_regression_descriptives.csv"
    )
)

latex_vif_tables <- Regression_Data$VIF %>%
    dplyr::mutate(value = sprintf("%.3f", value)) %>%
    dplyr::relocate(wave, regressor, value) %>%
    pretty_variable_names() %>%
    dplyr::select(-regressor) %>%
    dplyr::relocate(wave, pretty_regressor)

fwrite(
    latex_vif_tables, 
    file.path(PROJECT_FOLDER, "Classifications/regression_vif_tables.csv")
)

############# GRAPH PLOTTING CODE

Residuals <- rbind(
    data.table(
        wave = Regression_Data$predictions$wave, 
        regression = "Ridge Regression", 
        incidence = Regression_Data$predictions$incidence,
        value = Regression_Data$predictions[, incidence-glmnet]
    ),
    data.table(
        wave=Regression_Data$predictions$wave, 
        regression="OLS Regression", 
        incidence = Regression_Data$predictions$incidence,
        value=Regression_Data$predictions[, incidence-lm]
    )
) %>%
    dplyr::mutate( 
        # wave = paste0("Wave ", wave),
        full_title = sprintf("Wave %i - %s", wave, regression)
    )

Residual_plot <- ggplot(Residuals, aes(x=incidence, y=value)) +
    geom_hline(yintercept=0) +
    geom_point(colour="blue", size=2) +
    facet_wrap(full_title~., scale="free", ncol=2) +
    theme_bw() +
    theme(
        axis.text = element_text(size=13),
        axis.title = element_text(size=15),
        strip.text=  element_text(size=13)
    ) +
    labs(x="Predicted Incidence", y="Residual")

ggsave(
    Residual_plot, 
    file = file.path(PROJECT_FOLDER, "Graphs/residuals_plot.png"), 
    width=9, height=8
)

pl_MSEs <- ggplot(
    Regression_Data$MSE %>% dplyr::mutate(wave = paste0("Wave ", wave)), 
    aes(x=log10(lambda), y=meann, ymin=lower, ymax=upper)
) +
    geom_ribbon(fill="red", alpha=0.1) +
    geom_vline(
        data = Regression_Data$optimal_lambdas %>% 
            dplyr::mutate(wave = paste0("Wave ", wave)), 
        aes(xintercept=log10(optimal.lambda)),
        colour="black"
    ) +
    geom_point(size=1, colour="red") +
    facet_grid(wave~., scale="free_y") +
    theme_bw() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 13)
    ) +
    labs(x="Log(lambda)", y="Ridge Regression MSE") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

ggsave(
    pl_MSEs, 
    file = file.path(PROJECT_FOLDER, "Graphs/MSE_plot.png"), 
    height=7, width=11
)

Coeff_vs_Lambda <- rbindlist(lapply (
    1:length(Regression_Data$fits),
    function(xx)
    {
        Regression_Data$fits[[xx]] %>% 
            coef %>% 
            as.matrix %>% 
            as.data.frame %>% 
            dplyr::mutate(regressor=rownames(.)) %>% 
            reshape::melt(id="regressor") %>%
            dplyr::mutate(
                variable = as.numeric(gsub('s', '', variable)),
                wave = paste0("Wave ", xx)
            ) %>%
            merge(Regression_Data$fits[[4]]$lambda %>% 
                      data.table(lambda=., variable=(1:length(.))-1)) %>%
            dplyr::filter(!grepl("intercept", tolower(regressor)))
    }
))

pl_coeff_lambda <- ggplot(
    Coeff_vs_Lambda, 
    aes(x=lambda, y=value, colour=regressor)
) +
    scale_x_log10() +
    geom_line() +
    labs(x="Lambda", y="Coefficients") +
    facet_grid(wave~., scale="free_y") +
    theme_bw() +
    theme(
        legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 13)
    )

ggsave(
    pl_coeff_lambda, 
    file = file.path(PROJECT_FOLDER, "Graphs/coeffs_vs_lambda.png"), 
    height = 7, width = 11
)

# # to find the culprits for the large VIFs, check the table of correlations
# # pcor(x_full)

Coefficients <- Regression_Data$information %>%
    dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
    # dplyr::filter(type == "OLS") %>%
    # dplyr::filter(type == "Ridge.glmnet") %>%
    dplyr::mutate(signif=ifelse(p.value<0.05, TRUE, FALSE)) %>%
    dplyr::mutate(graph_factor = unlist(lapply(
        regressor,
        \(xx)
        {
            if(grepl("ca|cma|miz|strong|moderate|weak|none|density", tolower(xx))) 
                return("Remoteness")
            if(grepl("closure|Is", xx)) return("Interventions")
            if(grepl("group", tolower(xx))) return("Cohort")
            if(grepl("lockdown|vaccination", tolower(xx))) 
                return("Vaccination")
            return("General")
        }
    ))) %>%
    dplyr::mutate(graph_factor = factor(graph_factor)) %>%
    pretty_variable_names()

# example report
# https://arxiv.org/pdf/2111.12272.pdf

pl_regression <- ggplot(
    Coefficients %>% 
        dplyr::filter(type %in% c("Ridge.hdi")) %>% 
        # dplyr::filter(graph_factor == "Interventions") %>%# , "OLS"
        dplyr::mutate(wave = paste0("Wave ", wave)) %>%
        dplyr::mutate(type = dplyr::recode(type, "Ridge.hdi" = "Ridge")),
    aes(
        x=str_wrap(regressor, 20), y=abs(coefficient), 
        group=type, colour=type, shape=type, fill=type
    )
) +
    geom_point(size=2) +
    # geom_errorbar(aes(ymin=CI025, ymax=CI975)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_grid(wave~graph_factor, scales="free", space="free_x") +# graph_factor
    theme_bw() +
    theme(
        axis.text = element_text(size=12),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=15),
        strip.text = element_text(size=13),
        # legend.position = "none"
        # panel.spacing.y = unit(1, "lines")
    ) +
    labs(x="Regressor", y="Coefficients") +
    guides(
        fill = guide_legend(title = "Regression\nType"),
        colour = guide_legend(title = "Regression\nType"),
        shape = guide_legend(title = "Regression\nType")
    )

ggsave(
    pl_regression,
    file = file.path(PROJECT_FOLDER, "Graphs/regression_coefficients_by_category.png"),
    width=10, height=7
)

pl_regression_by_wave <-  ggplot(
    Regression_Data$information %>% 
        dplyr::filter(type %in% c("Ridge.hdi")) %>% # "OLS", 
        dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
        pretty_variable_names() %>%
        dplyr::rename("Regression" = "type") %>%
        dplyr::mutate(
            Regression = dplyr::recode(Regression, "Ridge.hdi" = "Ridge")
        ),
    aes(
        x=wave, y=coefficient, ymin=CI025, ymax=CI975, 
        group=Regression, fill=Regression, colour=Regression
    )
) +
    geom_point() +
    geom_errorbar(aes(ymin=CI025, ymax=CI975)) +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_wrap(regressor~., scale="free", ncol=4) +
    theme_bw() +
    theme(
        
    ) +
    labs(x="Epidemiological Wave", y="Regression Coefficient")

ggsave(
    pl_regression_by_wave, 
    file = file.path(PROJECT_FOLDER, "Graphs/regression_by_wave.png"),
    height=15, width=10 
)

graph_parsing_sugar <- function(xx)
{return( xx %>%
             dplyr::mutate(pretty_regressor = dplyr::recode(regressor,
                                                            "is_cma" ="(R1)~CMA", 
                                                            "is_ca" = "(R2)~CA",
                                                            "is_miz_strong" = "(R3)~Strong",
                                                            "is_miz_moderate" = "(R4)~Moderate", 
                                                            "is_miz_weak" = "(R5)~Weak",
                                                            "is_miz_none" = "(R6)~None",
                                                            "LTCHs" = "(R7)~LTCH",
                                                            "group_0_to_4" = "(A1)~C['0-4']",
                                                            "group_5_to_19" = "(A2)~C['5-19']",
                                                            "group_20_to_49" = "(A3)~C['20-49']",
                                                            "group_50_to_64" = "(A4)~C['50-64']",
                                                            "group_65_plus" = "(A5)~C['\u2265 65']",
                                                            # "(D1)~D\U00B7" - I can;t find out how to get the centre dot
                                                            # without an error, so will fix this later on
                                                            "previous_wave_incidence" = "(P)~Y['u-1']",
                                                            "PHU_pop_density" = "(D0)~D",
                                                            "interaction_popdensity_cma" = "(D1)~D:CMA", 
                                                            "interaction_popdensity_ca" = "(D2)~D:CA",
                                                            "interaction_popdensity_strong" = "(D3)~D:MIZ['strong']",
                                                            "interaction_popdensity_moderate" = "(D4)~D:MIZ['mod']", 
                                                            "interaction_popdensity_weak" = "(D5)~D:MIZ['weak']",
                                                            "interaction_popdensity_none" = "(D6)~D:MIZ['none']", 
                                                            "EIs" = "(S0)~E",
                                                            "interaction_children_school_closures" = "(S1)~E:C['5-19']",
                                                            "interaction_vaccination_0_to_4" = "(V1)~v['prov']:C['0-4']",
                                                            "interaction_vaccination_5_to_19" = "(V2)~v['prov']:C['5-19']",
                                                            "interaction_vaccination_20_to_49" = "(V3)~v['prov']:C['20-49']",
                                                            "interaction_vaccination_50_to_64" = "(V4)~v['prov']:C['50-64']",
                                                            "interaction_vaccination_65_plus" = "(V5)~v['prov']:C['\u2265 65']"
             ))
)}

pl_regression_by_wave_hdi_only <- ggplot(
    Regression_Data$information %>% 
        dplyr::filter(type %in% c("Ridge.hdi")) %>%
        dplyr::filter(!grepl("intercept", tolower(regressor))) %>%
        graph_parsing_sugar() %>%
        dplyr::rename("Regression" = "type") %>%
        dplyr::mutate(
            wave = factor(wave, 1:4),
            Regression = dplyr::recode(Regression, "Ridge.hdi" = "Ridge")
        ),
    aes(x=wave, y=coefficient, ymin=CI025, ymax=CI975)
) +
    geom_point() +
    geom_errorbar(aes(ymin=CI025, ymax=CI975), colour="blue") +
    geom_hline(yintercept=0, linetype="dashed", colour="grey", size=1) +
    facet_wrap(pretty_regressor~., scale="free_y", ncol=4, labeller=label_parsed) +
    theme_bw() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        strip.text = element_text(size = 15)
    ) +
    labs(x="Epidemiological Wave", y="Regression Coefficient")

ggsave(
    pl_regression_by_wave_hdi_only, 
    file = file.path(PROJECT_FOLDER, "Graphs/regression_by_wave_hdi_only.png"),
    height=15, width=10 
)

Added_Variable_Data <- Regression_Data$added_var_data %>%
    rbindlist() %>%
    dplyr::group_by(wave, regressor) %>%
    dplyr::mutate(
        gradient = lm(y~x)$coefficients[["x"]],
        # # the intercept is always zero for these plots
        # intercept = lm(y~x)$coefficients[["(Intercept)"]]
    ) %>% 
    graph_parsing_sugar() %>%
    dplyr::mutate(pos_gradient = ifelse(gradient>0, "positive", "negative"))

latex_gradients <- Added_Variable_Data %>% 
    data.table %>% 
    dplyr::filter(wave==2) %>% 
    dplyr::select(-x, -y) %>% 
    unique %>% 
    dplyr::arrange(-abs(gradient)) %>%
    pretty_variable_names %>% 
    dplyr::select(pretty_regressor, gradient) %>% 
    # dplyr::mutate(gradient = round(gradient, 4)) %>%
    xtable(digits=4)

pl_added_variables <- ggplot(
    Added_Variable_Data %>% dplyr::filter(wave == 2),
    aes(x=x, y=y)
) +
    geom_point(aes(colour=pos_gradient)) + 
    # geom_smooth(method='lm', formula=y~x) +
    geom_abline(aes(intercept=intercept, slope=gradient)) +
    facet_wrap(pretty_regressor~., labeller=label_parsed, ncol=4) + # , scale="free_x"
    theme_bw() +
    scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
    labs(x=expression("e( X"['i']*" | X )"), y="e( Y | X )") +
    theme(
        # legend.key.size = unit(5, "line"),
        axis.text = element_text(size = 13),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(size=15),
        # legend.position = "bottom",
        legend.position = c(0.95,0.01),
        legend.justification = c(1,0),
        strip.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        
    ) +
    guides(colour=guide_legend(title="Gradient"))

ggsave(
    pl_added_variables,
    file = file.path(PROJECT_FOLDER, "Graphs/regression_added_variable_plots.png"),
    height=15, width=10
)
