rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")

####
#kernel density
#####
library(ggplot2)
library(gridExtra)
library(dplyr)
theme_set(theme_bw())


WSG100 <- lapply(D100ba, function(x)com.mean.ab(x, trait, "WSG"))
Moist100 <- lapply(D100ba, function(x)com.mean.ab(x, trait, "Moist"))
slope100 <- lapply(D100ba, function(x)com.mean.ab(x, trait, "sp.slope.mean"))
convex100 <- lapply(D100ba, function(x)com.mean.ab(x, trait, "sp.convex.mean"))


WSG20 <- lapply(D20ba, function(x)com.mean.ab(x, trait, "WSG"))
Moist20 <- lapply(D20ba, function(x)com.mean.ab(x, trait, "Moist"))
slope20 <- lapply(D20ba, function(x)com.mean.ab(x, trait, "sp.slope.mean"))
convex20 <- lapply(D20ba, function(x)com.mean.ab(x, trait, "sp.convex.mean"))

kernel.20 <- data.frame(WSG=unlist(WSG20),
             convex=unlist(convex20),
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
             convex=unlist(convex100),
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
k.ind.convex <- data.frame(convex=unlist(convex.ind),
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
    size = "individual",
    trait = "WSG",
    val = k.ind.WSG[,1])

temp2 <- data_frame(time = k.ind.moist$time,
    size = "individual",
    trait = "moist",
    val = k.ind.moist[,1])

temp3 <- data_frame(time = k.ind.convex$time,
    size = "individual",
    trait = "convex",
    val = k.ind.convex[,1])

temp4 <- data_frame(time = k.ind.slope$time,
    size = "individual",
    trait = "slope",
    val = k.ind.slope[,1])

fig_dat2 <- bind_rows(fig_dat, temp1, temp2, temp3, temp4) %>%
  mutate(size = factor(size, levels = c("1ha", "0.04ha", "individual"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
  mutate(size2 = factor(size, labels = c("1ha", "0.04ha", "Individual"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Convexity~(m)", "Slope~(degrees)")))


dummy1 <- data_frame(time = 1982,
    size = "1ha",
    trait = "WSG",
    val = c(0.55, 0.65))
dummy2 <- data_frame(time = 1982,
    size = "1ha",
    trait = "moist",
    val = c(-2, 4))

dummy3 <- data_frame(time = 1982,
    size = "individual",
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
    filter(., size != "0.04ha" | trait != "convex" | (val > 0.035 & val < 0.085)) %>%
    filter(., size != "individual" | trait != "convex" | (val > -0.15 & val < 0.25)) %>%
    filter(., size != "0.04ha" | trait != "slope" | (val > 4 & val < 6)) %>%
    filter(., size != "individual" | trait != "slope" | (val > 2 & val < 8)) %>%
    filter(size != "0.04ha") %>%# new
    mutate(size = factor(size, levels = c("1ha", "individual"))) %>%
    mutate(size2 = factor(size, labels = c("1ha", "Individual")))

dummy <- bind_rows(dummy0, dummy1, dummy2, dummy3, dummy4, dummy5) %>%
  filter(size != "0.04ha") %>%# new
  mutate(size = factor(size, levels = c("1ha","individual"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
  mutate(size2 = factor(size, labels = c("1ha", "Individual"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Convexity~(m)", "Slope~(degrees)")))



before <- proc.time()
pdf("~/Dropbox/MS/TurnoverBCI/fig/fig1_BA.pdf",
  width = 8, height = 5, paper = "special")

ggplot(dummy0, aes(x = val)) +
  facet_wrap(~ size2 + trait2, nrow = 2, scale = "free",
  labeller = labeller(trait2 = label_parsed, sizes = label_value)) +
  geom_density(data = filter(dummy0, size == "1ha"), adjust = 1,
    aes(colour = as.factor(time))) +
  guides(colour = guide_legend(title = NULL)) +
  # guides(fill = guide_legend(override.aes = list(fille = as.factor(time)))) +
  theme(panel.margin.x = unit(0.5, "lines"),
  legend.position = c(1, 0.4), legend.justification = c(1,1),
  legend.text = element_text(size = 7),
  legend.background = element_rect(fill=alpha('blue', 0)),
  legend.key.size = unit(0.4, "cm"),
  axis.text.x = element_text(angle = 45)) +

  geom_blank(data = dummy) +
  geom_density(data = filter(dummy0, size == "individual"), adjust = 4, aes(colour = as.factor(time))) +
  ylab("Density") +
  xlab("Trait values")
dev.off()
after <- proc.time()
after - before
