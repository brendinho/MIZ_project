
total_SAC_distribution_agg <- ggplot(Regions %>% group_by(miz_score_agg) %>% tally() |> dplyr::mutate(n=factor(n))) +
    geom_bar(aes(y=n, x=str_wrap(miz_score_agg, width=15)), fill="blue", position="dodge", stat="identity") +
    labs(x="MIZ score", y="SAC count") +
    theme_bw() +
    theme(
    )

MIZ_remoteness_correlations <- Regions[, 
        .(total_miz=sum(miz_score_agg, na.rm=T), 
         total_remoteness=sum(index_of_remoteness, na.rm=T)), 
        by=.(province, HR)
    ][, 
      .(corr_coeff=cor(total_miz, total_remoteness, "complete.obs")), 
      by=.(province)
    ]

# find a way to get ggplot from reordering the factor levels
total_SAC_distribution_raw <- ggplot(
        Regions[, .(population=sum(population), prevalence=.N), by=.(class)],
        aes(x=class)
    ) +
    geom_bar(aes(y=prevalence, fill=log(population)), position="dodge", stat="identity") +
    labs(x="Classification", y="Prevalence") +
    theme_bw() +
    scale_x_discrete(limits=sort(unique(Regions$class))) +
    scale_fill_viridis_b("log(Pop)") +
    theme(legend.position = "right")
ggsave(total_SAC_distribution_raw, file="Graphs/total_SAC_dist.png", width=10, height=4)

Canada_score_distribution <- ggplot(
        Regions[, sum(miz_score_agg), by=.(province, HR)],
        aes(y=V1, x=str_wrap(HR, width=20))
    ) +
    geom_bar(stat="identity", position="stack") + #,
    labs(x="Health Region", y="CSD count") +
    facet_grid(.~province) +
    scale_color_viridis(discrete = TRUE, option = "D")+
    scale_fill_viridis(discrete = TRUE) +
    # scale_x_discrete(limits=Regions[, sum(miz_score_agg), by=.(province, HR)]$HR) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5)
    )
print(ON_score_distribution)
ggsave(ON_score_distribution, file="Graphs/ontario_score_dist.jpg", width=10, height=4)



ON_score_distribution <- ggplot(Regions[province=="Ontario", .(total_miz=sum(miz_score_agg), total_remoteness=sum(index_of_remoteness)), by=.(province, HR)]) +
    geom_bar(aes(y=total, x=str_wrap(HR, width=20)), fill="blue", position="dodge", stat="identity") +
    labs(x="Ontario Health Region", y="aggregated MIZ score") +
    theme_bw() +
    scale_x_discrete(expand = c(0, 0)) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5)
    )
# print(ON_score_distribution)
ggsave(ON_score_distribution, file="Graphs/ontario_score_dist.jpg", width=10, height=4)


ON_remoteness_distribution <- ggplot(Regions[province=="Ontario", .(total=sum(index_of_remoteness)), by=.(province, HR)]) +
    geom_bar(aes(y=total, x=str_wrap(HR, width=20)), fill="blue", position="dodge", stat="identity") +
    labs(x="Ontario Health Region", y="aggregated MIZ score") +
    theme_bw() +
    scale_x_discrete(expand = c(0, 0)) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5)
    )
# print(ON_score_distribution)
ggsave(ON_remoteness_distribution, file="Graphs/ontario_remoteness_dist.jpg", width=10, height=4)


# 
# MB_score_distribution <- ggplot(Regions %>% filter(province=="Manitoba") %>% group_by(HR) %>% tally() %>% arrange(n)) +
#     geom_bar(aes(y=n, x=str_wrap(HR, width=20)), fill="blue", position="dodge", stat="identity") +
#     labs(x="Health Region", y="CSD count") +
#     theme_bw() +
#     theme(
#         axis.text.x = element_text(angle = 45, vjust = 0.5)
#     )
# print(MB_score_distribution)
# ggsave(MB_score_distribution, file="Graphs/manitoba_score_dist.jpg", width=10, height=4)

all_province_cases <- ggplot(Case_Data[cases>=0, sum(cases, na.rm=T), by=c("date", "province")]) +
    geom_line(aes(x=date, y=V1, colour=province)) +
    labs(x="Date", y="Cases Reported", legend="Province") +
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    guides(
        colour = guide_legend(
            override.aes = list(shape = 1),
            title = guide_legend(title = "Province")
        )
    )
# ggsave(all_province_cases, file="Graphs/all_province_cases.jpg", width=10, height=4)
# 
ontario_cases <- ggplot(Total_Case_Data[cases>=0 & province=="Ontario", sum(cases), by=c("date", "HR", "province")]) +
    geom_line(aes(x=date, y=V1, colour=str_wrap(HR, 20))) +
    labs(x="Date", y="Cases Reported", legend="Health Region") +
    scale_x_date(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(
    ) +
    guides(
        colour = guide_legend(
            override.aes = list(shape = 1),
            title = guide_legend(title = "Health Authority")
        )
    )
# ggsave(ontario_cases, file="Graphs/ontario_cases.jpg", width=10, height=4)

ON_all_cases <- ON_cases[, .(cases=sum(cases, na.rm=T)), by=.(date, wave)]
start_of_wave_2 <- ON_all_cases[which(diff(wave) == 1), date]
first_date <- min(ON_all_cases$date)
last_date <- max(ON_all_cases$date)

results_I <- ggplot(
  ON_all_cases |> dplyr::mutate(wave=as.character(wave)), 
    aes(x=date, y=cases)
  ) +
  geom_rect(
    inherit.aes = FALSE,
    data = data.frame(x = 0, y = 0),
    aes(
      xmin = first_date,
      xmax = last_date,
      ymin=-Inf, ymax=Inf
    ),
    fill="orange", alpha=0.1
  ) +
  geom_rect(
    inherit.aes = FALSE,
    data = data.frame(x = 0, y = 0),
    aes(
      xmin = as.Date(start_of_wave_2),
      xmax = as.Date(last_date),
      ymin=-Inf, ymax=Inf
    ),
    fill="red", alpha=0.1
  ) +
  geom_vline(xintercept=start_of_wave_2, linetype="dashed") +
  # geom_text(aes(x=start_of_wave_2, label="the_date", y=3000), colour="blue", angle=90) +
  geom_label(aes(x=start_of_wave_2, label=start_of_wave_2, y=3000), colour="blue") +
  geom_label(aes(x=as.Date("2020-05-01"), label="first wave", y=4500), colour="blue") +
  geom_label(aes(x=as.Date("2021-06-15"), label="second wave", y=4500), colour="blue") +
  geom_line(size = 0.75, colour="blue") +
  theme_bw() +
  theme(
    legend.position="right",
    axis.title = element_text(size=12),
    axis.text = element_text(size=12)
  ) +
  labs(
    x="Date",
    y="Case incidence" # ,
  ) +
  scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y", limits=c(first_date, last_date))
  ggsave(results_I, file=sprintf("Graphs/ON_I_plot_diff_waves.png"), height=3, width=10)


print(ggplot(
        Total_Data |> dplyr::mutate(x=miz_score_agg, y=cases),
        aes(x=x, y=y, colour=province)) +
    geom_point(size=3) +
    theme_bw() +
    labs(x="Score", y="cumulative COVID-19 cases since 2020-03-01") +
    theme(
    ) +
    scale_color_viridis(discrete = TRUE, option = "D")+
    scale_fill_viridis(discrete = TRUE) +
    # scale_x_discrete(expand = c(0, 0)) +
    # scale_y_continuous(expand = c(0, 0)) +
    guides(
        colour = guide_legend(
            # override.aes = list(shape = 2),
            title = guide_legend(title = "Province")
        )
    ))
# ggsave(all_provinces_points, file="Graphs/all_provinces_points.jpg", width=10, height=4)

Total_Case_Data <- fread( "CaseDataTables/Total_Case_Data.csv") Table <- data.table(province=as.character(), date=as.character())
for(province in Total_Case_Data[, unique(province)])
{
  wave_date <- add_wave_numbers(Total_Case_Data[province==province, .(cases=sum(cases, na.rm=T)), by=.(date)])$date
  Table <- rbind(Table, list(province, as.character(wave_date)))
}
print(xtable(Table, type="latex"), include.rownames=F)

# for(prov in Total_Data |> filter(! province %in% c("Northwest Territories", "Yukon", "Prince Edward Island", "Nunatut")) |> pull(province) |> unique())
# {
#     ggplot(Total_Data[province==prov], aes(x=index_of_remoteness, y=cumul_cases, colour=province)) +
#         geom_point(size=3) +
#         theme_bw() +
#         labs(x="Remoteness", y="cumulative case count") +
#         theme(
#         ) +
#         scale_x_discrete(expand = c(0, 0)) +
#         scale_y_continuous(expand = c(0, 0)) +
#         guides(
#             colour = guide_legend(
#                 # override.aes = list(shape = 2),
#                 title = guide_legend(title = "Province")
#             )
#         ) +
#         ggsave(
#             file=paste0("Graphs/", gsub(' ', '_', prov), "_remoteness_points.jpg"),
#             width=10, height=4
#         )
#     
#     ggplot(Total_Data[province==prov], aes(x=miz_score, y=cumul_cases, colour=province)) +
#         geom_point(size=3) +
#         theme_bw() +
#         labs(x="MIZ score", y="cumulative case count") +
#         theme(
#         ) +
#         scale_x_discrete(expand = c(0, 0)) +
#         scale_y_continuous(expand = c(0, 0)) +
#         guides(
#             colour = guide_legend(
#                 # override.aes = list(shape = 2),
#                 title = guide_legend(title = "Province")
#             )
#         ) +
#         ggsave(
#             file=paste0("Graphs/", gsub(' ', '_', prov), "_miz_score_points.jpg"),
#             width=10, height=4
#         )
#     
#     ggplot(Total_Data[province==prov], aes(x=households, y=cumul_cases, colour=province)) +
#         geom_point(size=3) +
#         theme_bw() +
#         labs(x="MIZ score", y="cumulative case count") +
#         theme(
#         ) +
#         scale_x_discrete(expand = c(0, 0)) +
#         scale_y_continuous(expand = c(0, 0)) +
#         guides(
#             colour = guide_legend(
#                 # override.aes = list(shape = 2),
#                 title = guide_legend(title = "Province")
#             )
#         ) +
#         ggsave(
#             file=paste0("Graphs/", gsub(' ', '_', prov), "_households_points.jpg"),
#             width=10, height=4
#         )
# }

# parse_string_as_number <- function(...)
# {
#     unlist(lapply(..., \(x){ is_percentage <- grepl("\\%", x); x <- gsub(',|%', '', x); return(as.numeric(x)/(if(is_percentage) 100 else 1)) } ))
# }








