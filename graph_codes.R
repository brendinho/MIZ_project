scaling_factor <- Regions[, .(num_HR=factor(length(unique(HR))), num_csd=sum(num_csds), population=sum(population)), by=.(province)][, max(num_csd)/max(population)]
SAC_distribution <- ggplot(
        Regions[, .(num_HR=factor(length(unique(HR))), num_csd=sum(num_csds), population=sum(population)), by=.(province)],
        aes(x=reorder(str_wrap(province, width=15), -population), group=province)
    ) +
    geom_bar(aes(y=num_csd, fill=num_HR), position="dodge", stat="identity") +
    geom_line(aes(y=population*scaling_factor), size=1, group=1) +
    labs(x="Province", y="Number of census subdivisions (CSDs)") +
    scale_y_continuous(sec.axis = sec_axis(~./scaling_factor, name="Provincial population")) +
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 0.5, size=12),
        legend.position = "top",
        legend.direction="horizontal"
    ) +
    scale_color_viridis()+
    scale_fill_viridis_d() +
    guides(fill=guide_legend(title="Health Region count:", nrow=1))
ggsave(SAC_distribution, file="Graphs/SAC_dist.png", width=10, height=7)

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
ontario_cases <- ggplot(Case_Data[cases>=0 & province=="Ontario", sum(cases), by=c("date", "HR", "province")]) +
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
# 
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








