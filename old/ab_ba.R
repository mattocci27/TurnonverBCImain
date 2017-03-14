

ab_data <- as.data.frame(sapply(D20m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)

ba_data <- as.data.frame(sapply(D20ba,function(x)apply(x,2,sum)))
ba_data$sp <- rownames(ba_data)


fig_dat <- data_frame(ab1982 = ab_data$census_1982,
    ab2010 = ab_data$census_2010,
    ba1982 = ba_data$census_1982,
        ba2010 = ba_data$census_2010,
        sp = ab_data$sp) %>%
    tidyr::gather("Time", "abund", c(1,2)) %>%
    tidyr::gather("Time2", "BA", c(1,2)) %>%
    filter((Time == "ab1982" & Time2 == "ba1982") |
      (Time == "ab2010" & Time2 == "ba2010")) %>%
      mutate(Time3 = rep(c(1982, 2010), each = nrow(ab_data)))

fig_seg <- data_frame(ab1982 = ab_data$census_1982,
    ab2010 = ab_data$census_2010,
    ba1982 = ba_data$census_1982,
        ba2010 = ba_data$census_2010,
        sp = ab_data$sp)


ggplot(fig_dat, aes(x = abund, y = BA, colour = Time)) +
  geom_point() +
  geom_segment(data = fig_seg,
    mapping = aes(x = ab1982, y = ba1982, xend = ab2010, yend = ba2010),
    colour = "black", arrow = arrow(length = unit(0.05, "inches")), size = 0.2) +
  scale_y_log10() +
  scale_x_log10()




  ggplot(fig_dat %>% filter(Time == "ab1982"), aes(x = abund, y = BA, colour = Time)) +
    geom_point() +
    scale_y_log10() +
    scale_x_log10()
