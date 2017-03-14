rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")
load("gam_fig.RData")

library(dplyr)
library(ggplot2)
library(grid)
library(mgcv)
# library(MASS)

data_frame(WSG100 = WSG100, WSG20 = WSG20, WSG_ind = WSG.ind)

str(WSG100)
str(WSG20)
str(WSG.ind)

time <- c(1982, 1985, 1990, 1995, 2000, 2005, 2010)
# rep_ind <- sapply(WSG.ind, length)

#Concavity = - Convexity
fig_dat <- data_frame(WSG = c(unlist(WSG100) - WSG100[[1]],
  unlist(WSG20) - WSG20[[1]]),
  Moist = c(unlist(Moist100) - Moist100[[1]],
    unlist(Moist20) - Moist20[[1]]),
  Convex = -c(unlist(convex100) - convex100[[1]],
    unlist(convex20) - convex20[[1]]),
  Slope = c(unlist(slope100) - slope100[[1]],
    unlist(slope20) - slope20[[1]]),
  Time = c(rep(time, each = 50),
    rep(time, each = 1250)),
  size = rep(c("1ha", "0.04ha"), c(50*7, 1250*7)))

temp <- sample(1:50, 50)
test <- temp[1:5]
train <- temp[6:50]

test_dat <- mapply(function(x){WSG100[[x]][test]}, 1:7)
train_dat <- mapply(function(x){WSG100[[x]][train]}, 1:7)

test_dat2 <- data_frame(WSG =apply(test_dat - test_dat[,1], 2, mean),
  Time = time)

train_dat2 <- data_frame(WSG =apply(train_dat - train_dat[,1], 2, mean),
  Time = time)

plot(WSG~time, train_dat2, ylim = c(0,0.015))
points(WSG~time, test_dat2, pch =16)

c(train_dat) - train_dat[,1]

fig_dat2 <- fig_dat %>%
  tidyr::gather(., "trait", "val", 1:4)

fig_dat3 <- fig_dat2 %>%
  group_by(Time, trait) %>%
  summarise(val = mean(val))

fig_dat3 <- data_frame(Time = fig_dat3$Time,
    size = "50ha",
    trait = fig_dat3$trait,
    val = fig_dat3$val)


fig_dat4 <- bind_rows(fig_dat2, fig_dat3) %>%
  mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Concavity~(m)", "Slope~(degrees)")))

temp <- fig_dat3 %>% filter(trait == "WSG")


ggplot(fig_dat4, aes(x=Time, y = val)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(trait ~ size, scale = "free", ncol = 3)



WSG100.rm <-list()
Moist100.rm <-list()
slope100.rm <-list()
convex100.rm <-list()


WSG20.rm <-list()
Moist20.rm <-list()
slope20.rm <-list()
convex20.rm <-list()
###use convex mean
for (i in 1:7){
  WSG100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))],trait,"WSG")
  Moist100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))],trait,"Moist")
  slope100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))],trait,"sp.slope.mean")
  # convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("ALSEBL","SWARS1"))],trait,"sp.convex.mean")
  convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))],trait,"sp.convex.mean")
}

for (i in 1:7){
  WSG20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("PIPECO","POULAR"))],trait,"WSG")
  Moist20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("PIPECO","POULAR"))],trait,"Moist")
  slope20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("PIPECO","POULAR"))],trait,"sp.slope.mean")
  # convex20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("ALSEBL","SWARS1"))],trait,"sp.convex.mean")
  convex20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("PIPECO","POULAR"))],trait,"sp.convex.mean")
}

# apply(D100m[[1]], 2, sum)
#
# moge <- sapply(D100m, function(x)apply(x,2,sum)) %>% t
# com.mean.ab(moge, trait, "WSG") - 0.5933383
#
# moge <- data.frame(a = com.mean.ab(moge, trait, "WSG"), b = sapply(WSG100, mean))



moge <- data.f
