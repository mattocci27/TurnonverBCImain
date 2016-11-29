rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")
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


# fig_dat <- data_frame(WSG = c(unlist(WSG100) / WSG100[[1]],
#   unlist(WSG20) / WSG20[[1]]),
#   Moist = c(unlist(Moist100) / Moist100[[1]],
#     unlist(Moist20) / Moist20[[1]]),
#   Convex = c(unlist(convex100) / convex100[[1]],
#     unlist(convex20) / convex20[[1]]),
#   Slope = c(unlist(slope100) / slope100[[1]],
#     unlist(slope20) / slope20[[1]]),
#   Time = c(rep(time, each = 50),
#     rep(time, each = 1250)),
#   size = rep(c("1ha", "0.04ha"), c(50*7, 1250*7)))


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



moge <- data.frame(WSG = unlist(WSG100) - WSG100[[1]],
                   Moist = unlist(Moist100) - Moist100[[1]],
                   slope = unlist(slope100) - slope100[[1]],
                   convex = -unlist(convex100) + convex100[[1]],
                   site = as.factor(rep(1:50,7)),
                   Time)


moge20 <- data.frame(WSG = unlist(WSG20) - WSG20[[1]],
                  Moist = unlist(Moist20) - Moist20[[1]],
                  slope = unlist(slope20) - slope20[[1]],
                  convex = -unlist(convex20) + convex20[[1]],
                  site = as.factor(rep(1:1250,7)),
                  Time)


moge100r <- data.frame(WSG = unlist(WSG100.rm) - WSG100.rm[[1]],
                   Moist = unlist(Moist100.rm) - Moist100.rm[[1]],
                   slope = unlist(slope100.rm) - slope100.rm[[1]],
                   convex = -unlist(convex100.rm) + convex100.rm[[1]],
                   site = rep(1:50,7),
                   Time)

moge50 <- data.frame(WSG = sapply(WSG100, mean) - mean(WSG100[[1]]),
                   Moist = sapply(Moist100, mean) - mean(Moist100[[1]]),
                   slope = sapply(slope100, mean) - mean(slope100[[1]]),
                   convex = -sapply(convex100, mean) + mean(convex100[[1]]),
                   site = as.factor(1:7),
                   Time = time)

moge50r <- data.frame(WSG = sapply(WSG100.rm, mean) - mean(WSG100.rm[[1]]),
                  Moist = sapply(Moist100.rm, mean) - mean(Moist100.rm[[1]]),
                  slope = sapply(slope100.rm, mean) - mean(slope100.rm[[1]]),
                  convex = -sapply(convex100.rm, mean) + mean(convex100.rm[[1]]),
                  site = as.factor(1:7),
                  Time = time)


# diffenet  ================
# moge <- data.frame(WSG = unlist(WSG100) / WSG100[[1]],
#                   Moist = unlist(Moist100) / Moist100[[1]],
#                   slope = unlist(slope100) / slope100[[1]],
#                   convex = unlist(convex100) / convex100[[1]],
#                   site = as.factor(rep(1:50,7)),
#                   Time)
#
#
#
# moge100r <- data.frame(WSG = unlist(WSG100.rm) / WSG100.rm[[1]],
#                   Moist = unlist(Moist100.rm) / Moist100.rm[[1]],
#                   slope = unlist(slope100.rm) / slope100.rm[[1]],
#                   convex = unlist(convex100.rm) / convex100.rm[[1]],
#                   site = rep(1:50,7),
#                   Time)
r1.r50<- gam(WSG ~  s(Time,k=4),data=moge50r,
  correlation = corAR1(form = ~Time))
r2.r50<- gam(Moist ~  s(Time,k=4),data=moge50r,
  correlation = corAR1(form = ~Time))
r3.r50<- gam(convex ~  s(Time,k=4),data=moge50r,
  correlation = corAR1(form = ~Time))
r4.r50 <- gam(slope ~  s(Time,k=4),data=moge50r,
  correlation = corAR1(form = ~Time))

r1_50 <- gam(WSG ~  s(Time,k=4),data=moge50,
  correlation = corAR1(form = ~Time))
r2_50 <- gam(Moist ~  s(Time,k=4),data=moge50,
  correlation = corAR1(form = ~Time))
r3_50 <- gam(convex ~  s(Time,k=4),data=moge50,
  correlation = corAR1(form = ~Time))
r4_50 <- gam(slope ~  s(Time,k=4),data=moge50,
  correlation = corAR1(form = ~Time))



r1_20 <- gam(WSG ~  s(Time,k=4),data=moge20,
  correlation = corAR1(form = ~Time))
r2_20 <- gam(Moist ~  s(Time,k=4),data=moge20,
  correlation = corAR1(form = ~Time))
r3_20 <- gam(convex ~  s(Time,k=4),data=moge20,
  correlation = corAR1(form = ~Time))
r4_20 <- gam(slope ~  s(Time,k=4),data=moge20,
  correlation = corAR1(form = ~Time))


r1.r100<- gam(WSG ~  s(Time,k=4),data=moge100r,
  correlation = corAR1(form = ~Time))
r2.r100<- gam(Moist ~  s(Time,k=4),data=moge100r,
  correlation = corAR1(form = ~Time))
r3.r100<- gam(convex ~  s(Time,k=4),data=moge100r,
  correlation = corAR1(form = ~Time))
r4.r100 <- gam(slope ~  s(Time,k=4),data=moge100r,
  correlation = corAR1(form = ~Time))

r1_100 <- gam(WSG ~  s(Time,k=4),data=moge,
  correlation = corAR1(form = ~Time))
r2_100 <- gam(Moist ~  s(Time,k=4),data=moge,
  correlation = corAR1(form = ~Time))
r3_100 <- gam(convex ~  s(Time,k=4),data=moge,
  correlation = corAR1(form = ~Time))
r4_100 <- gam(slope ~  s(Time,k=4),data=moge,
  correlation = corAR1(form = ~Time))

res1_100 <- predict(r1_100, se.fit=T)
res1r100 <- predict(r1.r100, se.fit=T)
res2_100 <- predict(r2_100, se.fit=T)
res2r100 <- predict(r2.r100, se.fit=T)
res3_100 <- predict(r3_100, se.fit=T)
res3r100 <- predict(r3.r100, se.fit=T)
res4_100 <- predict(r4_100, se.fit=T)
res4r100 <- predict(r4.r100, se.fit=T)

res1_50 <- predict(r1_50, se.fit=T)
res1r50 <- predict(r1.r50, se.fit=T)
res2_50 <- predict(r2_50, se.fit=T)
res2r50 <- predict(r2.r50, se.fit=T)
res3_50 <- predict(r3_50, se.fit=T)
res3r50 <- predict(r3.r50, se.fit=T)
res4_50 <- predict(r4_50, se.fit=T)
res4r50 <- predict(r4.r50, se.fit=T)


# fig_dat_n <- data_frame(WSG = c(unlist(WSG100.rm), unlist(WSG20.rm)),
#   Moist = c(unlist(Moist100.rm), unlist(Moist20.rm)),
#   Convex = c(unlist(convex100.rm), unlist(convex20.rm)),
#   Slope = c(unlist(slope100.rm), unlist(slope20.rm)),
#   Time = c(rep(time, each = 50),
#   rep(time, each = 1250)),
#   size = rep(c("1ha", "0.04ha"), c(50*7, 1250*7))) %>%
#   tidyr::gather(., "trait", "val", 1:4) %>%
#   mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
#   mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Concavity~(m)", "Slope~(degrees)")))

fig_dat_n <- data_frame(WSG = c(sapply(WSG100.rm, mean) - mean(WSG100.rm[[1]]),
    unlist(WSG100.rm) - WSG100.rm[[1]],
    unlist(WSG20.rm) - WSG20.rm[[1]]),
  Moist = c(sapply(Moist100.rm, mean) - mean(Moist100.rm[[1]]),
    unlist(Moist100.rm) - Moist100.rm[[1]],
    unlist(Moist20.rm) - Moist20.rm[[1]]),
  Convex = c(sapply(convex100.rm, mean) - mean(convex100.rm[[1]]),
    unlist(convex100.rm) - convex100.rm[[1]],
    unlist(convex20.rm) - convex20.rm[[1]]),
  Slope = c(sapply(slope100.rm, mean) - mean(slope100.rm[[1]]),
    unlist(slope100.rm) - slope100.rm[[1]],
    unlist(slope20.rm) - slope20.rm[[1]]),
  Time = c(rep(time, each = 1),
    rep(time, each = 50),
    rep(time, each = 1250)),
  size = rep(c("50ha", "1ha", "0.04ha"), c(7, 50*7, 1250*7)))  %>%
  tidyr::gather(., "trait", "val", 1:4) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Concavity~(m)", "Slope~(degrees)")))


fig_dat_n %>%
# filter(size != "50ha") %>%
ggplot(., aes(x = jitter(Time), y = val) ) +
  geom_point() +
  facet_wrap(size ~ trait2, scale = "free_y", nrow = 2, labeller = labeller(trait2 = label_parsed, size = label_value)) +
  geom_smooth(data = filter(fig_dat4, size == "1ha" & trait == "WSG"), fill = "blue", alpha = 0.2) +
  theme_bw() +
  xlab("Time") +
  ylab("Trait values")


temp1 <- fig_dat4 %>%
  mutate(obs = "ori")

temp2 <- fig_dat_n %>%
  mutate(obs = "tr")

temp3 <- bind_rows(temp1, temp2) %>%
filter(size != "50ha")


fig_dat5 <- fig_dat4 %>%
  mutate(obs = "ori") %>%
  bind_rows(., fig_dat_n) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Concavity~(m)", "Slope~(degrees)")))


  m1 <- function(x){min(x) * 0.94}
  m2 <- function(x){max(x) * 1.04}




##########



r1data100 <- data.frame(time = spline(r1_100$model$Time, res1_100$fit)$x,
          mean = spline(r1_100$model$Time, res1_100$fit)$y,
          upper = spline(r1_100$model$Time,
              res1_100$fit + 1.96*res1_100$se.fit)$y,
          lower = spline(r1_100$model$Time,
              res1_100$fit - 1.96*res1_100$se.fit)$y,
          obs = "obs")

r1data100_r <- data.frame(time = spline(r1.r100$model$Time, res1r100$fit)$x,
          mean = spline(r1.r100$model$Time, res1r100$fit)$y,
          upper = spline(r1.r100$model$Time,
              res1r100$fit + 1.96*res1r100$se.fit)$y,
          lower = spline(r1.r100$model$Time,
              res1r100$fit - 1.96*res1r100$se.fit)$y,
          obs = "tr")

moge1 <- bind_rows(r1data100, r1data100_r)


r2data100 <- data.frame(time = spline(r2_100$model$Time, res2_100$fit)$x,
          mean = spline(r2_100$model$Time, res2_100$fit)$y,
          upper = spline(r2_100$model$Time,
              res2_100$fit + 1.96*res2_100$se.fit)$y,
          lower = spline(r2_100$model$Time,
              res2_100$fit - 1.96*res2_100$se.fit)$y,
          obs = "obs")
r2data100_r <- data.frame(time = spline(r2.r100$model$Time, res2r100$fit)$x,
          mean = spline(r2.r100$model$Time, res2r100$fit)$y,
          upper = spline(r2.r100$model$Time,
              res2r100$fit + 1.96*res2r100$se.fit)$y,
          lower = spline(r2.r100$model$Time,
              res2r100$fit - 1.96*res2r100$se.fit)$y,
          obs = "tr")

moge2 <- bind_rows(r2data100, r2data100_r)


r3data100 <- data.frame(time = spline(r3_100$model$Time, res3_100$fit)$x,
          mean = spline(r3_100$model$Time, res3_100$fit)$y,
          upper = spline(r3_100$model$Time,
              res3_100$fit + 1.96*res3_100$se.fit)$y,
          lower = spline(r3_100$model$Time,
              res3_100$fit - 1.96*res3_100$se.fit)$y,
          obs = "obs")
r3data100_r <- data.frame(time = spline(r3.r100$model$Time, res3r100$fit)$x,
          mean = spline(r3.r100$model$Time, res3r100$fit)$y,
          upper = spline(r3.r100$model$Time,
              res3r100$fit + 1.96*res3r100$se.fit)$y,
          lower = spline(r3.r100$model$Time,
              res3r100$fit - 1.96*res3r100$se.fit)$y,
          obs = "tr")

moge3 <- bind_rows(r3data100, r3data100_r)



r4data100 <- data.frame(time = spline(r4_100$model$Time, res4_100$fit)$x,
          mean = spline(r4_100$model$Time, res4_100$fit)$y,
          upper = spline(r4_100$model$Time,
              res4_100$fit + 1.96*res4_100$se.fit)$y,
          lower = spline(r4_100$model$Time,
              res4_100$fit - 1.96*res4_100$se.fit)$y,
          obs = "obs")

r4data100_r <- data.frame(time = spline(r4.r100$model$Time, res4r100$fit)$x,
          mean = spline(r4.r100$model$Time, res4r100$fit)$y,
          upper = spline(r4.r100$model$Time,
              res4r100$fit + 1.96*res4r100$se.fit)$y,
          lower = spline(r4.r100$model$Time,
              res4r100$fit - 1.96*res4r100$se.fit)$y,
          obs = "tr")

moge4 <- bind_rows(r4data100, r4data100_r)

moge <- bind_rows(moge1, moge2, moge3, moge4) %>%
  mutate(Trait = rep(c("WSG", "Moist", "Convex", "Slope"), each = 42)) %>%
  mutate(size = "1ha") %>%
  mutate(Trait = as.factor(Trait))

r1data50 <- data.frame(time = spline(r1_50$model$Time, res1_50$fit)$x,
          mean = spline(r1_50$model$Time, res1_50$fit)$y,
          upper = spline(r1_50$model$Time,
              res1_50$fit + 1.96*res1_50$se.fit)$y,
          lower = spline(r1_50$model$Time,
              res1_50$fit - 1.96*res1_50$se.fit)$y,
          obs = "obs")

r1data50_r <- data.frame(time = spline(r1.r50$model$Time, res1r50$fit)$x,
          mean = spline(r1.r50$model$Time, res1r50$fit)$y,
          upper = spline(r1.r50$model$Time,
              res1r50$fit + 1.96*res1r50$se.fit)$y,
          lower = spline(r1.r50$model$Time,
              res1r50$fit - 1.96*res1r50$se.fit)$y,
          obs = "tr")

moge1 <- bind_rows(r1data50, r1data50_r)


r2data50 <- data.frame(time = spline(r2_50$model$Time, res2_50$fit)$x,
          mean = spline(r2_50$model$Time, res2_50$fit)$y,
          upper = spline(r2_50$model$Time,
              res2_50$fit + 1.96*res2_50$se.fit)$y,
          lower = spline(r2_50$model$Time,
              res2_50$fit - 1.96*res2_50$se.fit)$y,
          obs = "obs")
r2data50_r <- data.frame(time = spline(r2.r50$model$Time, res2r50$fit)$x,
          mean = spline(r2.r50$model$Time, res2r50$fit)$y,
          upper = spline(r2.r50$model$Time,
              res2r50$fit + 1.96*res2r50$se.fit)$y,
          lower = spline(r2.r50$model$Time,
              res2r50$fit - 1.96*res2r50$se.fit)$y,
          obs = "tr")

moge2 <- bind_rows(r2data50, r2data50_r)


r3data50 <- data.frame(time = spline(r3_50$model$Time, res3_50$fit)$x,
          mean = spline(r3_50$model$Time, res3_50$fit)$y,
          upper = spline(r3_50$model$Time,
              res3_50$fit + 1.96*res3_50$se.fit)$y,
          lower = spline(r3_50$model$Time,
              res3_50$fit - 1.96*res3_50$se.fit)$y,
          obs = "obs")
r3data50_r <- data.frame(time = spline(r3.r50$model$Time, res3r50$fit)$x,
          mean = spline(r3.r50$model$Time, res3r50$fit)$y,
          upper = spline(r3.r50$model$Time,
              res3r50$fit + 1.96*res3r50$se.fit)$y,
          lower = spline(r3.r50$model$Time,
              res3r50$fit - 1.96*res3r50$se.fit)$y,
          obs = "tr")

moge3 <- bind_rows(r3data50, r3data50_r)



r4data50 <- data.frame(time = spline(r4_50$model$Time, res4_50$fit)$x,
          mean = spline(r4_50$model$Time, res4_50$fit)$y,
          upper = spline(r4_50$model$Time,
              res4_50$fit + 1.96*res4_50$se.fit)$y,
          lower = spline(r4_50$model$Time,
              res4_50$fit - 1.96*res4_50$se.fit)$y,
          obs = "obs")

r4data50_r <- data.frame(time = spline(r4.r50$model$Time, res4r50$fit)$x,
          mean = spline(r4.r50$model$Time, res4r50$fit)$y,
          upper = spline(r4.r50$model$Time,
              res4r50$fit + 1.96*res4r50$se.fit)$y,
          lower = spline(r4.r50$model$Time,
              res4r50$fit - 1.96*res4r50$se.fit)$y,
          obs = "tr")

moge4 <- bind_rows(r4data50, r4data50_r)

moge50 <- bind_rows(moge1, moge2, moge3, moge4) %>%
  mutate(Trait = rep(c("WSG", "Moist", "Convex", "Slope"), each = 42)) %>%
  mutate(size = "50ha") %>%
  mutate(Trait = as.factor(Trait))
# names(r1data100)[2:3] <- c("obs", "rm")
# names(r2data100)[2:3] <- c("obs", "rm")
# names(r3data100)[2:3] <- c("obs", "rm")
# names(r4data100)[2:3] <- c("obs", "rm")
gam_dat <- bind_rows(r1data100, r2data100, r3data100, r4data100) %>%
  mutate(Trait = rep(c("WSG", "Moist", "Convex", "Slope"), each = 21)) %>%
  tidyr::gather(., "obs", "val", 2:3)

ggplot(gam_dat, aes(x = time, y = val, colour = obs)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Trait, scale = "free")

r1data100 %>% tidyr::gather(., "obs", "val", 2:3) %>%
  tidyr::gather(., "se", "val2", 2:4) %>% unique



fig_dat4 <- bind_rows(fig_dat2, fig_dat3) %>%
  mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Concavity~(m)", "Slope~(degrees)")))

fig_dat4 <- fig_dat4 %>%
  mutate(obs2 = "dat") %>%
  mutate(est = "dat") %>%
  mutate(est_mean = NA) %>%
  mutate(est_lo = NA) %>%
  mutate(est_up= NA)


moge5 <- moge %>%
  mutate(Time = time)
  #%>%
  # tidyr::gather(.,"est", "val", 2:4) %>%
  # mutate(est_mean = ifelse(est == "mean", "Y", "N")) %>%
  # mutate(est_lo = ifelse(est == "lower", "Y", "N")) %>%
  # mutate(est_up = ifelse(est == "upper", "Y", "N")) %>%
  # tidyr::spread(., est, val)


moge6 <- data_frame(Time = moge5$time,
    size = moge5$size,
    trait = moge5$Trait,
    val = NA,
    obs2 = moge5$obs,
    est = "est",
    trait2 = moge5$Trait,
    est_mean = moge5$mean,
    est_lo = moge5$lower,
    est_up = moge5$upper)


moge50_2 <- data_frame(Time = moge50$time,
    size = moge50$size,
    trait = moge50$Trait,
    val = NA,
    obs2 = moge50$obs,
    est = "est",
    trait2 = moge50$Trait,
    est_mean = moge50$mean,
    est_lo = moge50$lower,
    est_up = moge50$upper)

temp <- bind_rows(fig_dat4, moge6, moge50_2) %>%
  # filter(size != "50ha") %>%
  mutate(obs2 = as.factor(obs2)) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)"))) %>%
  mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha")))
  # mutate(obs2 = ifelse(obs2 == "dat", "obs", obs2))


fig_dat_n2 <- fig_dat_n %>%
  filter(size == "50ha") %>%
  mutate(obs2 = "tr") %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)"))) %>%
  mutate(est = NA) %>%
  mutate(est_mean = NA) %>%
  mutate(est_lo = NA) %>%
  mutate(est_up = NA)


temp2 <- bind_rows(temp, fig_dat_n2) %>%
  mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha")))


# 9 inche version
pdf("~/Dropbox/MS/TurnoverBCI/TurnoverBCI_MS/fig/fig2_new.pdf", width = 9, height = 7)
  ggplot(filter(temp2, is.na(est_mean) == TRUE)) +
    geom_point(data = temp2 %>% filter(size != "50ha" | obs2 != "tr"),
      aes(x = jitter(Time),
    y = val), size = 1.2, alpha = 0.4) +
    # geom_point(data = temp2 %>% filter(size == "50ha" & obs2 == "tr"), aes(x = jitter(Time), y = val), size = 0.8, alpha = 0.4, colour = "red") +
    facet_wrap(size ~ trait2, scale = "free",
    labeller = labeller(trait2 = label_parsed),
    switch = NULL, ncol = 4) + # geom_smooth() +
    theme_bw() +
    xlab("Time") +
    ylab("Deviation from initial trait values") +
    geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs" & est == "est"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.1) +
    geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs" & est == "est"), aes(x = Time, y = est_mean), colour = "blue") +

    geom_ribbon(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")),
    aes(ymin = est_lo, ymax = est_up, x = Time, fill = obs2), fill = "blue", alpha = 0.2) +
    geom_line(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")), aes(x = Time, y = est_mean, colour = obs2, lty = obs2), colour = "blue", lty = 1, lwd = 0.375) +

    geom_ribbon(data = filter(temp, size == "50ha" & obs2 == "obs"),
    aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.2) +
    geom_line(data = filter(temp, size == "50ha" & obs2 == "obs"),  aes(x = Time, y = est_mean, lty = obs2), colour = "blue", lty = 1, lwd = 0.375) +

    scale_fill_manual(values = c("gray", "gray", "gray")) +
    scale_colour_manual(values = c("black", "black", "black")) +
    scale_linetype_manual(values = c(2, 2, 2)) +
    guides(linetype = FALSE) +
    guides(colour = FALSE) +
    guides(fill = FALSE) +
    theme(
      plot.margin = unit(c(0.2, 0.2, 0.2 , 0.2), units = "lines"),
      strip.text = element_text(size = 10.5, lineheight=0.5),
      axis.title = element_text(size = 10.5),
      axis.text.x = element_text(size = 9, angle = 45),
      axis.text.y = element_text(size = 9),
      legend.text = element_text(size = 10.5))

dev.off()

# 6 inche width
#
# theme(
#   plot.margin = unit(c(0.2, 0.2, 0.2 , 0.2), units = "lines"),
#   strip.text = element_text(size = 7, lineheight=0.5),
#   axis.title = element_text(size = 7),
#   axis.text.x = element_text(size = 6, angle = 45),
#   axis.text.y = element_text(size = 6),
#   legend.text = element_text(size = 7))
