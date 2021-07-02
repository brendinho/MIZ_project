rm(list=ls())

library(cancensus)
library(data.table)
library(dplyr)
library(readxl)
library(XLConnect)
library(stringr)
library(ggplot2)
library(ggpubr)

source("LUT_header.R")
source("prepare_tables.R")

setwd('/home/bren/Documents/GitHub/MIZ_project/')

# xl_data <- "Classifications/CONNECT1_Mixing matrices_20210512.xlsx"
# 
# # problem - StatCan definition: "Furthermore, it defines child, for census purposes, as "any unmarried individual, regardless of age, who lives with his or her parent(s) and has no children in the same household." (The subsequent discussion clarifies that unmarried includes never married and divorced.)"
# 
# # executive decision - child means 0-14, adult means >=80, in line with the matrices we have
# 
# child_indices <- c("X0.4","X5.9", "X10.14")
# adult_indices <- c("X15.19", "X20.24", "X25.29", "X30.34", "X35.39", "X40.44", "X45.49", 
#                    "X50.54", "X55.59", "X60.64", "X65.69", "X70.74", "X75.79", "X.80")
# 
# contacts_home_w_ch <- as.matrix(readWorksheetFromFile(
#     xl_data, 
#     sheet="Home with children", 
#     startRow = 2, 
#     endRow=19, 
#     endCol = 18
# ))
# rownames(contacts_home_w_ch) <- sprintf("X%s", gsub('-|≥', '.', contacts_home_w_ch[, "Age.contacts"]))
# class(contacts_home_w_ch) <- "numeric"
# contacts_home_w_ch <- contacts_home_w_ch[,-1:-1]
# 
# Homes_w_Ch <- matrix(nrow=2,ncol=2); rownames(Homes_w_Ch) <- c("C", "A"); colnames(Homes_w_Ch) <- c("C", "A")
# Homes_w_Ch["C", "C"] <- mean(contacts_home_w_ch[child_indices, child_indices])
# Homes_w_Ch["C", "A"] <- mean(contacts_home_w_ch[child_indices, adult_indices])
# Homes_w_Ch["A", "C"] <- mean(contacts_home_w_ch[adult_indices, child_indices])
# Homes_w_Ch["A", "A"] <- mean(contacts_home_w_ch[adult_indices, adult_indices])
# 
# contacts_home_wo_ch <- as.matrix(readWorksheetFromFile(
#     xl_data,
#     sheet="Home without children",
#     startRow = 2,
#     endRow=19,
#     endCol = 18
# ))
# rownames(contacts_home_wo_ch) <- sprintf("X%s", gsub('-|≥', '.', contacts_home_wo_ch[, "Age.contacts"]))
# class(contacts_home_wo_ch) <- "numeric"
# contacts_home_wo_ch <- contacts_home_wo_ch[,-1:-1]
# 
# Homes_wo_Ch <- matrix(nrow=2,ncol=2); rownames(Homes_wo_Ch) <- c("C", "A"); colnames(Homes_wo_Ch) <- c("C", "A")
# Homes_wo_Ch["C", "C"] <- 0 # mean(contacts_home_wo_children[child_indices, child_indices])
# Homes_wo_Ch["C", "A"] <- 0 # mean(contacts_home_wo_children[child_indices, adult_indices])
# Homes_wo_Ch["A", "C"] <- 0 # mean(contacts_home_wo_children[adult_indices, child_indices])
# Homes_wo_Ch["A", "A"] <- mean(contacts_home_wo_ch[adult_indices, adult_indices])
# 
# # until I figure out the reporting regions
# Total <- fread("Classifications/Classification_HR_Households_Commute.csv") |>
#     filter(! Province %in% c("Saskatchewan")) |>
#     # filter(! Province %in% c("Ontario", "British Columbia", "Saskatchewan")) |>
#     select(-MIZ_score, -Region, -Population_Density) |>
#     mutate(HR = unlist(lapply(HR, get_standard_name))) |>
#     mutate(Index_of_remoteness = as.numeric(Index_of_remoteness))
# 
# Total$Cpls_w_0_ch_interac <- Total$Cpls_w_0_ch * (2*Homes_wo_Ch['A','A'])
# 
# Total$Cpls_w_1_ch_interac <- Total$Cpls_w_1_ch * (2*Homes_w_Ch['A','A'] + 2*Homes_w_Ch['A','C'] + 2*Homes_w_Ch['C','A'])
# Total$Cpls_w_2_ch_interac <- Total$Cpls_w_2_ch * (2*Homes_w_Ch['A','A'] + 4*Homes_w_Ch['A','C'] + 4*Homes_w_Ch['C','A'] + 2*Homes_w_Ch['C','C'])
# Total$Cpls_w_3_or_more_ch_interac <- Total$Cpls_w_3_or_more_ch * (2*Homes_w_Ch['A','A'] + 6*Homes_w_Ch['A','C'] + 6*Homes_w_Ch['C','A'] + 2*Homes_w_Ch['C','C'])
# 
# Total$Sgls_w_1_ch_interac <- Total$Sgls_w_1_ch * (2*Homes_w_Ch['A','A'] + 2*Homes_w_Ch['A','C'] + 2*Homes_w_Ch['C','A'])
# Total$Sgls_w_2_ch_interac <- Total$Sgls_w_2_ch * (2*Homes_w_Ch['A','A'] + 4*Homes_w_Ch['A','C'] + 4*Homes_w_Ch['C','A'] + 2*Homes_w_Ch['C','C'])
# Total$Sgls_w_3_or_more_ch_interac <- Total$Sgls_w_3_or_more_ch * (2*Homes_w_Ch['A','A'] + 6*Homes_w_Ch['A','C'] + 6*Homes_w_Ch['C','A'] + 2*Homes_w_Ch['C','C'])
# 
# # Sask_indices <- which(CSD_info$Province=="Saskatchewan")
# # CSD_info[Sask_indices, HA:=lookup_SK_HAs(Region)]
# # 
# # summing_columns <- setdiff(names(CSD_info), c("GeoUID", "Region", "CD_UID", "PR_UID", "CMA_UID", "rguid", "HA", "Province"))
# # CSD_info <- CSD_info[, lapply(.SD, sum, na.rm=T), .SDcols=summing_columns, by=c("Province", "HA")]
# 
# Scores <- data.table(Province=character(), HR=character(), MIZ_score=numeric())
# 
# for(prov in unique(Total$Province))
# {
#     Classes_Here <- Total[Province==prov]
#     for(zone in unique(Classes_Here$HR))
#     {
#         temp_classes <- Classes_Here[HR==zone]
#         
#         CMAs <- temp_classes[Class == "CMA", Class]
#         CAs  <- temp_classes[Class == "CA", Class]
#         MIZs <- temp_classes[! Class %in% c("CMA", "CA"), Class]
# 
#         numerator <- length(CMAs)*zone_scores("cma") + length(CAs)*zone_scores("ca") + sum(zone_scores(MIZs))
#         denominator <- length(CMAs) + length(CAs) + length(MIZs)
# 
#         final_score <- numerator/denominator
# 
#         Scores <- rbind(Scores, list(prov, zone, final_score))
#     }
# }
# 
# ######################################################################
# 
# Case_Data <- rbind(
#         fread("CaseDataTables/BC_cases.csv"),
#         fread("CaseDataTables/QC_cases.csv"),
#         fread("CaseDataTables/MB_cases.csv"),
#         fread("CaseDataTables/SK_cases.csv"),
#         fread("CaseDataTables/Territories_cases.csv"),
#         fread("CaseDataTables/ON_cases.csv"),
#         fread("CaseDataTables/AB_cases.csv"),
#         fread("CaseDataTables/NB_cases.csv"),
#         fread("CaseDataTables/NS_cases.csv"),
#         fread("CaseDataTables/NL_cases.csv")
#     ) %>%
#     mutate(HR = unlist(lapply(HA, get_standard_name))) |>
#     filter(HR != "Not Reported") |>
#     rename(Province = province) |>
#     select(-HA)
# 
# Summary_Case_Data <- Case_Data[, .(Cumul_Cases=sum(cases, na.rm=T)), by=c("Province", "HR")]
# 
# Peak_Data <- Case_Data[Case_Data[,.I[which.max(cases)], by=.(Province, HR)]$V1] %>%
#     mutate(Days_In=as.IDate(date)-as.IDate('2020-01-23')) %>%
#     rename(Peak_Cases=cases, Peak_Date=date)
# 
# Total_HR_Info <- Reduce(
#         function(...) merge(..., all=TRUE, by=c("Province", "HR")),
#         list(
#             Scores,
#             Total[, lapply(.SD, sum, na.rm=TRUE), by=c("Province", "HR"), .SDcols=setdiff(names(Total), c("Province", "HR", "Class"))],
#             Summary_Case_Data,
#             Peak_Data
#         )
#     ) %>%
#     subset(HR != "") %>%
#     filter(!is.na(MIZ_score))
# 
# x_vars <- c("MIZ_score", "Index_of_remoteness", "Area_sq_km", "Population", "Dwellings", "Households",
#     "Cpls_w_0_ch", "Cpls_w_children", "Cpls_w_1_ch", "Cpls_w_2_ch", "Cpls_w_3_or_more_ch",
#     "Sgls_w_ch", "Sgls_w_1_ch", "Sgls_w_2_ch", "Sgls_w_3_or_more_ch", "Not_in_families",
#     "Commute_wi_CSD", "Commute_wi_CD_not_CSD", "Commute_wi_prov_not_CD", "Commute_extra_prov") # ,
# 
# # not including these ones, since they just multiply the number of couples by a constant factor
# # becomes important whe we start looking at the effects of adding an adult or child in the home
# #
# 
# ppl_x_vars <- c("Cpls_w_0_ch_interac", "Cpls_w_1_ch_interac", "Cpls_w_2_ch_interac", "Cpls_w_3_or_more_ch_interac", "Sgls_w_1_ch_interac", "Sgls_w_2_ch_interac", "Sgls_w_3_or_more_ch_interac")
# 
# y_vars <- c("Cumul_Cases", "Peak_Date", "Peak_Cases")
# 
# Label_Dictionary <- list(
#     "MIZ_score" = "Cumulative SAC score in each HR",
#     "Remoteness" = "Cumulative Remoteness Index in each HR",
#     "Area" = "Total Area (sq. km) of each HR",
#     "Population" = "Total population of CSDs in each HR",
#     "Dwellings" = "Total number of dwellings in each HR",
#     "Households" = "Total number of households in each HR",
#     "Cpls_w_0_ch" = "Total number of (2 adult, 0 child) households in each HR",
#     "Cpls_w_children" = "Total number of couples with children in each HR",
#     "Cpls_w_1_ch" = "Total number of (2 parent, 1 child) households in each HR",
#     "Cpls_w_2_ch" = "Total number of (2 parent, 2 child) households in each HR",
#     "Cpls_w_3_or_more_ch" = "Total number of (2 parent, ≥3 child) households in each HR",
#     "Sgls_w_ch" = "Total number of single parents in each HR",
#     "Sgls_w_1_ch" = "Total number of (1 parent, 1 child) households in each HR",
#     "Sgls_w_2_ch" = "Total number of (1 parent, 2 child) households each HR",
#     "Sgls_w_3_or_more_ch" = "Total number of (1 parent, ≥3 child) households each HR",
#     "Not_in_families" = "Total number of (1 adult, 0 child) househoilds in each HR",
#     "Commute_wi_CSD" = "Number commuting to work within their CSD", 
#     "Commute_wi_CD_not_CSD", "Number commuting outside their CSD but within their CD", 
#     "Commute_wi_prov_not_CD" = "Number commutiung outside their CD but within province", 
#     "Commute_extra_prov" = "Number commuting outside their province",
#     "Cpls_w_0_ch_interac" = "Total number of interactions in (2 adult, 0 child) households in each HR",
#     "Cpls_w_1_ch_interac" = "Total number of interactions in (2 parent, 1 child) households in each HR",
#     "Cpls_w_2_ch_interac" = "Total number of interactions in (2 parent, 2 child) households in each HR",
#     "Cpls_w_3_or_more_ch_interac" = "Total number of interactions in (2 parent, ≥3 child) households in each HR",
#     "Sgls_w_1_ch_interac" = "Total number of interactions in (1 adult, 1 child) households in each HR",
#     "Sgls_w_2_ch_interac" = "Total number of interactions in (2 parent, 2 child) households in each HR",
#     "Sgls_w_3_or_more_ch_interac" = "Total number of interactions in (2 parent, ≥3 child) households in each HR",
#     "Cumul_Cases" = "Cumulative number of reported cases in each HR",
#     "Peak_Date" = "Date in which daily case reporting peaked",
#     "Peak_Cases" = "Peak number of reported cases per day",
#     "Days_In" = "Number of days taken to reach peak in daily case reports (since 2020-01-23)"
# )
# 
# dir.create(file.path(getwd(), "Graphs/Canada"), showWarnings = FALSE)
# for(y_var in y_vars)
# {
#     for(x_var in x_vars)
#     {
#         the_plot <- ggscatter(Total_HR_Info,
#                               x=x_var, y=y_var,
#                               add="reg.line",
#                               conf.int = TRUE,
#                               add.params = list(color = "blue", fill = "lightgray")
#                               )+
#             stat_cor(method="pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
#             # stat_regline_equation() +
#             labs(x=Label_Dictionary[[x_var]], y=Label_Dictionary[[y_var]]) +
#             ggtitle("Canada") +
#             theme_bw()
# 
#         ggsave(
#             plot = the_plot,
#             file = sprintf("Graphs/Canada/%s_vs_%s.png", y_var, x_var),
#             width = 10, height = 5
#         )
#     }
# 
#     the_plot <- ggscatter(Total_HR_Info[, sum(.SD, na.rm=T), .SDcols=ppl_x_vars, by=c("Province", "HR", y_vars)],
#                           x="V1", y=y_var,
#                           add="reg.line",
#                           conf.int = TRUE,
#                           add.params = list(color = "blue", fill = "lightgray")
#         ) +
#         stat_cor(method="pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
#         # stat_regline_equation() +
#         labs(x="Total number of household interactions in each HR", y=Label_Dictionary[[y_var]]) +
#         ggtitle("Canada") +
#         theme_bw()
# 
#     ggsave(
#         plot = the_plot,
#         file = sprintf("Graphs/Canada/%s_vs_%s.png", y_var, "Total_Household_Contacts"),
#         width = 10, height = 5
#     )
# }
# 
# for(prov in setdiff(unique(Total_HR_Info$Province), c("Yukon", "Prince Edward Island", "Nunavut", "Northwest Territories")))
# {
#     prov_print <- paste(strsplit(prov, ' ')[[1]], collapse='_')
# 
#     dir.create(file.path(getwd(), sprintf("Graphs/%s", prov_print)), showWarnings = FALSE)
# 
#     for(y_var in y_vars)
#     {
#         for(x_var in x_vars)
#         {
# 
#             the_plot <- ggscatter(Total_HR_Info[Province == prov],
#                                   x=x_var, y=y_var,
#                                   add="reg.line",
#                                   conf.int = TRUE,
#                                   add.params = list(color = "blue", fill = "lightgray")
#             )+
#                 stat_cor(method="pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
#                 # stat_regline_equation() +
#                 labs(x=Label_Dictionary[[x_var]], y=Label_Dictionary[[y_var]]) +
#                 ggtitle(prov) +
#                 theme_bw()
# 
#             ggsave(
#                 plot = the_plot,
#                 file = sprintf("Graphs/%s/%s_vs_%s.png", prov_print, y_var, x_var),
#                 width = 10, height = 5
#             )
#         }
# 
#         the_plot <- ggscatter(Total_HR_Info[Province == prov, sum(.SD, na.rm=T), .SDcols=ppl_x_vars, by=c("Province", "HR", y_vars)],
#                               x="V1", y=y_var,
#                               add="reg.line",
#                               conf.int = TRUE,
#                               add.params = list(color = "blue", fill = "lightgray")
#         ) +
#             stat_cor(method="pearson", aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
#             # stat_regline_equation() +
#             labs(x="Total number of household interactions in each HR", y=Label_Dictionary[[y_var]]) +
#             ggtitle(prov) +
#             theme_bw()
# 
#         ggsave(
#             plot = the_plot,
#             file = sprintf("Graphs/%s/%s_vs_%s.png", prov_print, y_var, "Total_Household_Contacts"),
#             width = 10, height = 5
#         )
#     }
# }
# 
# Total_HR_Info$Numeric_Peak_Date = as.numeric(Total_HR_Info$Peak_Date)
# LM_Table <- data.table(Province=character(), X=character(), Y=character(), R2=numeric(), p=numeric())
# for(x_var in x_vars){ for(y_var in c("Cumul_Cases", "Peak_Cases", "Numeric_Peak_Date"))
# {
#     for(prov in setdiff(unique(Total_HR_Info$Province), c("Yukon", "Prince Edward Island", "Nunavut", "Northwest Territories")))
#     {
#         the_fit <- lm(
#             as.formula(sprintf("%s~%s", y_var, x_var)),
#             data = Total_HR_Info[Province==prov]
#         )
# 
#         LM_Table <- rbind(
#             LM_Table,
#             list(
#                 Province = prov,
#                 X = x_var,
#                 Y = y_var,
#                 R2 = summary(the_fit)$r.squared,
#                 p = summary(the_fit)$coefficients[x_var,"Pr(>|t|)"]
#             )
#         )
#     }
# 
#     canada_fit <- lm(
#         as.formula(sprintf("%s~%s", y_var, x_var)),
#         data = Total_HR_Info
#     )
# 
#     LM_Table <- rbind(
#         LM_Table,
#         list(
#             Province = "*** ALL CANADA ***",
#             X = x_var,
#             Y = y_var,
#             R2 = summary(canada_fit)$r.squared,
#             p = summary(canada_fit)$coefficients[x_var,"Pr(>|t|)"]
#         )
#     )
# }}
# 
# LM_Table$Acceptable <- as.factor(LM_Table$p <= 0.05)
# LM_Table$Y <- as.factor(LM_Table$Y)
# LM_Table$Province <- as.factor(LM_Table$Province)
# 
# dir.create(file.path(getwd(), "Graphs/Combined"), showWarnings = FALSE)
# for(depen in unique(LM_Table$Y))
# {
#     total_thing <- ggplot(LM_Table[Y==depen], aes(x=X, y=R2, fill=Acceptable)) +
#         geom_bar(position="dodge", stat="Identity") +
#         geom_hline(yintercept=0.7, linetype="dashed", color = "black") +
#         facet_wrap(~Province, ncol=2) +
#         labs(fill="p < 0.05") +
#         theme_bw() +
#         ggtitle(sprintf("%s ~ x", depen)) +
#         theme(
#             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
#         )
# 
#     ggsave(
#         plot = total_thing,
#         file = sprintf("Graphs/Combined/%s_X_R2_p.png", depen),
#         height=10, width=10
#     )
# }














