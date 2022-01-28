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

# verified
# function to parse and collate the overlaps of multiple timelines, per required jurisdiction
valmorphanize <- function(DATA, JURIS)
{
    DATA %>% dplyr::filter(jurisdiction == JURIS) %>%
        split(1:nrow(.)) %>%
        lapply(\(xx) seq(xx$date.implemented, xx$effective.until)) %>%
        unlist %>% unname %>% unique %>% sort %>%
        (\(xx) {c(min(xx), max(xx), xx[which(diff(xx)>1)], xx[which(diff(xx)>1)+1])}) %>%
        sort %>% split(., ceiling(seq_along(.)/2)) %>%
        lapply(\(xx) data.table(JURIS, t(xx))) %>% rbindlist %>%
        setNames(c("jurisdiction", "start", "end")) %>%
        dplyr::mutate(across(c("start", "end"), ~as.Date(.x, origin="1970-01-01")))
}

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

# SAC_distribution <-

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
