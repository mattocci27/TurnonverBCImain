##
## Figure 1: kernel density
##

rm(list = ls()) # This clears everything from memory.
before0 <- proc.time()
setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")

library(ggplot2)
library(gridExtra)
library(dplyr)
theme_set(theme_bw())

# - convex = concave
kernel.20 <- data.frame(WSG=unlist(WSG20),
             convex= -unlist(convex20),
             slope=unlist(slope20),
             moist=unlist(Moist20),
             time=c(rep(1982,50),
                    rep(1985,50),
                    rep(1990,50),
                    rep(1995,50),
                    rep(2000,50),
                    rep(2005,50),
                    rep(2010,50)))

kernel.100 <- data.frame(WSG=unlist(WSG100),
             #convex=unlist(convex.100.rare[[10]),
             #slope=unlist(slope.100.rare[[10]),
             convex= - unlist(convex100),
             slope=unlist(slope100),
             moist=unlist(Moist100),
             time=c(rep(1982,50),
                    rep(1985,50),
                    rep(1990,50),
                    rep(1995,50),
                    rep(2000,50),
                    rep(2005,50),
                    rep(2010,50)))

temp <- sapply(WSG.ind,length)
k.ind.WSG <- data.frame(WSG=unlist(WSG.ind),
            time=c(rep(1982,temp[1]),
                  rep(1985,temp[2]),
                  rep(1990,temp[3]),
                  rep(1995,temp[4]),
                  rep(2000,temp[5]),
                  rep(2005,temp[6]),
                  rep(2010,temp[7])))

temp <- sapply(Moist.ind,length)
k.ind.moist <- data.frame(moist=unlist(Moist.ind),
            time=c(rep(1982,temp[1]),
                  rep(1985,temp[2]),
                  rep(1990,temp[3]),
                  rep(1995,temp[4]),
                  rep(2000,temp[5]),
                  rep(2005,temp[6]),
                  rep(2010,temp[7])))
# temp <- sapply(slope.ind.rare[[1]],length)
temp <- sapply(slope.ind,length)
k.ind.slope <- data.frame(slope=unlist(slope.ind),
            time=c(rep(1982,temp[1]),
                  rep(1985,temp[2]),
                  rep(1990,temp[3]),
                  rep(1995,temp[4]),
                  rep(2000,temp[5]),
                  rep(2005,temp[6]),
                  rep(2010,temp[7])))

# temp <- sapply(convex.ind.rare[[1]],length)
temp <- sapply(convex.ind, length)
k.ind.convex <- data.frame(convex= - unlist(convex.ind),
            time=c(rep(1982,temp[1]),
                  rep(1985,temp[2]),
                  rep(1990,temp[3]),
                  rep(1995,temp[4]),
                  rep(2000,temp[5]),
                  rep(2005,temp[6]),
                  rep(2010,temp[7])))



temp100 <- kernel.100 %>%
  mutate(size = "1ha")

temp20 <- kernel.20 %>%
  mutate(size = "0.04ha")

fig_dat <- bind_rows(temp100, temp20) %>%
  tidyr::gather(., "trait", "val", 1:4)

temp1 <- data_frame(time = k.ind.WSG$time,
    size = "All individuals (50ha)",
    trait = "WSG",
    val = k.ind.WSG[,1])

temp2 <- data_frame(time = k.ind.moist$time,
    size = "All individuals (50ha)",
    trait = "moist",
    val = k.ind.moist[,1])

temp3 <- data_frame(time = k.ind.convex$time,
    size = "All individuals (50ha)",
    trait = "convex",
    val = k.ind.convex[,1])

temp4 <- data_frame(time = k.ind.slope$time,
    size = "All individuals (50ha)",
    trait = "slope",
    val = k.ind.slope[,1])

fig_dat2 <- bind_rows(fig_dat, temp1, temp2, temp3, temp4) %>%
  mutate(size = factor(size, levels = c("All individuals (50ha)","1ha", "0.04ha"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
  mutate(size2 = factor(size, labels = c("All individuals (50ha)", "1ha", "0.04ha"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)")))


dummy1 <- data_frame(time = 1982,
    size = "1ha",
    trait = "WSG",
    val = c(0.55, 0.65))

dummy2 <- data_frame(time = 1982,
    size = "1ha",
    trait = "moist",
    val = c(0.3, 0.9))

dummy3 <- data_frame(time = 1982,
    size = "All individuals (50ha)",
    trait = "moist",
    val = c(-2, 4))

dummy4 <- data_frame(time = 1982,
    size = "1ha",
    trait = "slope",
    val = c(4, 6))

dummy5 <- data_frame(time = 1982,
    size = "0.04ha",
    trait = "slope",
    val = c(4, 6))


dummy0 <- fig_dat2 %>%
    filter(., size != "0.04ha" | trait != "convex" | (val < -0.035 & val > -0.085)) %>%
    filter(., size != "All individuals (50ha)" | trait != "convex" | (val < 0.15 & val > -0.25)) %>%
    filter(., size != "0.04ha" | trait != "slope" | (val > 4 & val < 6)) %>%
    filter(., size != "All individuals (50ha)" | trait != "slope" | (val > 2 & val < 8))
    # filter(size != "0.04ha") %>%# new

dummy <- bind_rows(dummy0, dummy1, dummy2, dummy3, dummy4, dummy5) %>%
  # filter(size != "0.04ha") %>%# new
  mutate(size = factor(size, levels = c("All individuals (50ha)", "1ha", "0.04ha"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
  mutate(size2 = factor(size, labels = c("All individuals (50ha)", "1ha", "0.04ha"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)")))


###
# small data for test
###
# dummy <- dummy %>% filter(size == "1ha")
#
# temp1 <- dummy %>% mutate(size = "0.04ha")
# temp2 <- dummy %>% mutate(size = "All individuals (50ha)")
#
# dummy <- bind_rows(dummy, temp1, temp2) %>%
#   mutate(size = factor(size, levels = c("All individuals (50ha)", "1ha", "0.04ha"))) %>%
#   mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
#   mutate(size2 = factor(size, labels = c("All individuals (50ha)", "1ha", "0.04ha"))) %>%
#   mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Concavity~(m)", "Slope~(degrees)")))
#
#
#
# dummy0 <- dummy0 %>% filter(size == "1ha")
#
# dummy0 <- bind_rows(dummy0, temp1, temp2) %>%
#   mutate(size = factor(size, levels = c("All individuals (50ha)", "1ha", "0.04ha"))) %>%
#   mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
#   mutate(size2 = factor(size, labels = c("All individuals (50ha)", "1ha", "0.04ha"))) %>%
#   mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Concavity~(m)", "Slope~(degrees)")))

## need to redcued to 6inches
before <- proc.time()
#postscript("~/Dropbox/MS/TurnoverBCI/TurnoverBCI_MS/fig/fig1_new.eps",
#  width = 9, height = 8, paper = "special")

postscript("~/Desktop/fig1_new.eps",
  width = 6, height = 5, paper = "special")

p <- ggplot(dummy0, aes(x = val)) +
  facet_wrap(~ size2 + trait2, nrow = 3, scale = "free",
  labeller = labeller(trait2 = label_parsed, sizes = label_value)) +
  geom_blank(data = dummy) +
  geom_density(data = dummy0 %>% filter(size != "All individuals (50ha)"), adjust = 1,
    aes(colour = as.factor(time))) +
  guides(colour = guide_legend(title = NULL), size = 21) +
  # guides(fill = guide_legend(override.aes = list(fill = as.factor(time)))) +
  theme(
   legend.position = c(0.99, 0.99), legend.justification = c(0.8,0.9),
    #legend.text = element_text(size = 9),
    legend.background = element_rect(fill=alpha('blue', 0)),
    legend.key.size = unit(0.3, "cm"),
    strip.text = element_text(size = 7, lineheight=0.5),
    axis.title = element_text(size = 7),
    axis.text.x = element_text(size = 5, angle = 45),
    axis.text.y = element_text(size = 5),
    legend.text = element_text(size = 7),
    plot.margin = unit(c(0.5, 0.2, 0.2 , 0.2), units = "cm")) +
  geom_density(data = filter(dummy0, size == "All individuals (50ha)"), adjust = 4, aes(colour = as.factor(time))) +
  ylab("Density") +
  xlab("Trait values")

g <- ggplotGrob(p)

g$heights
g
for (i in c(6, 11, 16)) g$heights[[i]] <- unit(0.35, "cm")
for (i in c(62:73)) g$grobs[[i]]$heights <- unit(c(1,1), "npc")

grid.draw(g)
dev.off()
after <- proc.time()
after - before
# after - before0
