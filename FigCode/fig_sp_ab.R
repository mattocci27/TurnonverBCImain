##
## Fig 3. species-level respones
##

rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")

library(dplyr)
library(ggplot2)


# function to make subplot data
make_dat <- function(plot_size = "plot20m"){
  if(plot_size == "plot20m") {
    N <- 1250
    subplot <- D20m} else {
      N <- 50
      subplot <- D100m
    }
  # N <- 50
  # subplot <- D100m
  ab_t_data <- NULL
  for (i in 1:N){
    ab_data <- data_frame(
      sp = colnames(subplot[[1]]),
      ab1982 = subplot[[1]][i,],
      ab2010 = subplot[[7]][i,])

    trait_temp <- data_frame(
      sp = rownames(trait),
      moist = trait$Moist,
      slope = trait$sp.slope.mean,
      convex = - trait$sp.convex.mean, # Concavity
      WSG = trait$WSG)

    moge <- full_join(ab_data, trait_temp, by = "sp") %>%
      na.omit %>%
      mutate(site = paste("subplot", i, sep = "_"))
    ab_t_data <- bind_rows(moge, ab_t_data)
  }

  ab_t_data$site <- as.factor(ab_t_data$site)

  ab_t_data2 <- ab_t_data %>%
    filter(ab1982 != 0 | ab2010 != 0) %>%
    mutate(ab1982 = ab1982 + 1) %>%
    mutate(ab2010 = ab2010 + 1)
  ab_t_data2
}


ab_data <- as.data.frame(sapply(D20m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)

trait.temp <- data.frame(sp=rownames(trait),
  moist=trait$Moist,
  slope=trait$sp.slope.mean,
  slope.sd = trait$sp.slope.sd,
  convex= - trait$sp.convex.mean, # Concavity
  convex.sd=trait$sp.convex.sd,
  WSG=trait$WSG)

ab_t_data <- merge(ab_data,trait.temp,by="sp")

ab_t_data2 <- na.omit(ab_t_data)


ab_sp <- data_frame(
  sp = ab_t_data2$sp,
  ab1982 = ab_t_data2$census_1982 + 1,
  ab2010 = ab_t_data2$census_2010 + 1,
  WSG = ab_t_data2$WSG,
  moist = ab_t_data2$moist,
  slope = ab_t_data2$slope,
  convex = ab_t_data2$convex,
  site = "temp",
  size = "50ha",
  cat = "mean") %>%
  tidyr::gather(., "trait", "val", 4:7)


p100m <- make_dat("plot100m")
p20m <- make_dat("plot20m")


# > p100m %>% filter(sp == "APEIHY")
# A tibble: 0 × 8
# ... with 8 variables: sp <chr>, ab1982 <dbl>, ab2010 <dbl>, moist <dbl>, slope <dbl>, convex <dbl>, WSG <dbl>,
#   site <fctr>

p100m_2 <- p100m %>%
  mutate(size = "1ha") %>%
  mutate(cat = "each")

p100m_mean <- p100m %>%
  group_by(sp) %>%
  dplyr::summarise(ab1982 = mean(ab1982, na.rm = TRUE),
    ab2010 = mean(ab2010, na.rm = TRUE),
    WSG = mean(WSG, na.rm = TRUE),
    moist = mean(moist, na.rm = TRUE),
    slope = mean(slope, na.rm = TRUE),
    convex = mean(convex, na.rm = TRUE)) %>%
  mutate(site = "temp") %>%
  mutate(size = "1ha") %>%
  mutate(cat = "mean")


p100m_dat <- bind_rows(p100m_2, p100m_mean) %>%
  tidyr::gather(., "trait", "val", 4:7)


p20m_2 <- p20m %>%
  mutate(size = "0.04ha") %>%
  mutate(cat = "each")

p20m_mean <- p20m %>%
  group_by(sp) %>%
  dplyr::summarise(ab1982 = mean(ab1982),
    ab2010 = mean(ab2010),
    WSG = mean(WSG),
    moist = mean(moist),
    slope = mean(slope),
    convex = mean(convex)) %>%
  mutate(site = "temp") %>%
  mutate(size = "0.04ha") %>%
  mutate(cat = "mean")


p20m_dat <- bind_rows(p20m_2, p20m_mean) %>%
  tidyr::gather(., "trait", "val", 4:7)

p_dat <- bind_rows(p20m_dat, p100m_dat, ab_sp) %>%
  mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
  # filter(size != "0.04ha") %>%
  mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)")))

my_breaks <- function(...){
  c(0.05, 0.1, 0.5, 1, 5, 10)
}


moge <- data.frame(temp = letters[1:12],
    trait2 = rep(c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)"), 3),
    size = rep(c("50ha", "1ha", "0.04ha"), each = 4),
    x = rep(c(0.2, -2, -0.25, -3), 3),
    y = 11)

# temp <- ab_t_data2

p_dat2 <- ab_t_data2 %>%
  dplyr::select(sp, census_1982) %>%
  full_join(., p_dat, by = "sp") %>%
  as_data_frame

sp_vec <- c("PIPECO", "POULAR", "TET2PA", "SWARS1", "ALSEBL", "HYBAPR")

dat_mean <- p_dat2 %>%
  filter(cat == "mean") %>%
  mutate(sp2 = ifelse(sp == "PIPECO", "PIPECO",
    ifelse(sp == "POULAR", "POULAR", "Other species"))) %>%
  mutate(sp2 = factor(sp2, levels = c("PIPECO", "POULAR", "Other species"))) %>%
 mutate(sp3 = ifelse(sp %in% sp_vec, as.character(sp), "Other species")) %>%
  mutate(sp3 = factor(sp3, levels = c("ALSEBL", "HYBAPR", "PIPECO", "POULAR", "SWARS1", "TET2PA", "Other species")))

  # %>%
  # mutate(abund_cat = as.integer(log10(census_1982)) + 1)

# sp_vec <- c("PIPECO", "POULAR", "PIPECA", "PSYCLI", "HYBAPR", "FARAOC")

dat_each <- p_dat2 %>%
  filter(cat == "each") %>%
  mutate(sp2 = ifelse(sp == "PIPECO", "PIPECO",
    ifelse(sp == "POULAR", "POULAR", "Other species"))) %>%
  mutate(sp2 = factor(sp2, levels = c("PIPECO", "POULAR", "Other species"))) %>%
  mutate(sp3 = ifelse(sp %in% sp_vec, as.character(sp), "Other species")) %>%
  mutate(sp3 = factor(sp3, levels = c("ALSEBL", "HYBAPR", "PIPECO", "POULAR", "SWARS1", "TET2PA", "Other species")))


dat_mean %>% filter(ab2010/ab1982 == 0 & cat == "mean")

lab_dat <- data_frame(lab = paste("(", letters[1:12], ")", sep = ""),
    y = 20,
    x = rep(c(0.21, -1.9, -0.47, 0.5),  3),
    size = rep(c("50ha", "1ha", "0.04ha"), each = 4),
    trait = rep(c("WSG", "moist", "convex", "slope"), 3),
    sp2 = "Other species",
    sp3 = "Other species") %>%
    # filter(size != "0.04ha") %>%
    mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha"))) %>%
    mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
    mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)"))) %>%
    mutate(cat = "mean")



# fig. 3
pdf("~/Dropbox/MS/TurnoverBCI/TurnoverBCI_MS/fig/fig3_new.pdf", width = 6, height = 5, paper = "special")

pdf("~/Desktop/fig3_new.pdf", width = 6, height = 4.33, paper = "special")

  ggplot(p_dat2 %>% arrange(cat), aes(y = ab2010/ab1982, x = val, shape = cat, fill = cat, alpha = cat, colour = cat)) +
    geom_point() +
    facet_grid(size ~ trait2, scale = "free_x", labeller = labeller(trait2 = label_parsed, size = label_value)) +
    scale_shape_manual(values = c(4, 21), guide = F) +
    scale_fill_manual(values = c("gray", "black"), guide = F) +
    scale_colour_manual(values = c("gray", "black"), guide = F) +
    scale_alpha_manual(values = c(0.5, 0.8),
      labels = c(each = "Species-subplot combination", mean = "Species mean"),
      guide = guide_legend(override.aes =
        list(alpha = c(1, 0.8),
          fill = c("gray", "black"),
          colour = c("gray", "black"),
          shape = c(4, 21))))+
    scale_y_log10(breaks = my_breaks()) +
    theme_bw() +
    geom_hline(yintercept = 1, lty = 2) +
    # theme(legend.position = "none") +
    ylab("(No. of individual in 2010 + 1) / \n (No. of individual in 1982 + 1)") +
    xlab("Trait values") +
    geom_text(data = lab_dat, aes(label = lab, x = x, y = y),
     hjust = 0, show.legend = FALSE) +
    theme(legend.position = "bottom",
      legend.key.size = unit(0.2, "cm"),
      legend.title = element_blank()) +
    theme(
      strip.text = element_text(size = 7),
      axis.title = element_text(size = 7),
      axis.text.x = element_text(size = 6, angle = 45),
      axis.text.y = element_text(size = 6),
      legend.text = element_text(size = 7))

dev.off()


