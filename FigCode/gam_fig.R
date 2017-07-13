###
### Fig 2. community-level responses (GAMM)
###

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
time2 <- 1:7
Time2 <- rep(time2, each = 50)
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
  size = rep(c("1ha", "0.04ha"), c(50*7, 1250*7)),
  site = c(paste("site",rep(1:50,7), sep = "_"),
           paste("site", rep(1:1250,7), sep = "_")) %>% as.factor
  )

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
                   Time,
                   Time2)


moge20 <- data.frame(WSG = unlist(WSG20) - WSG20[[1]],
                  Moist = unlist(Moist20) - Moist20[[1]],
                  slope = unlist(slope20) - slope20[[1]],
                  convex = -unlist(convex20) + convex20[[1]],
                  site = as.factor(rep(1:1250,7)),
                  Time,
                  Time2)


moge100r <- data.frame(WSG = unlist(WSG100.rm) - WSG100.rm[[1]],
                   Moist = unlist(Moist100.rm) - Moist100.rm[[1]],
                   slope = unlist(slope100.rm) - slope100.rm[[1]],
                   convex = -unlist(convex100.rm) + convex100.rm[[1]],
                   site = rep(1:50,7),
                   Time,
                   Time2)

moge50 <- data.frame(WSG = sapply(WSG100, mean) - mean(WSG100[[1]]),
                   Moist = sapply(Moist100, mean) - mean(Moist100[[1]]),
                   slope = sapply(slope100, mean) - mean(slope100[[1]]),
                   convex = -sapply(convex100, mean) + mean(convex100[[1]]),
                   site = as.factor(1:7),
                   Time = time,
                   Time2 = time2)

moge50r <- data.frame(WSG = sapply(WSG100.rm, mean) - mean(WSG100.rm[[1]]),
                  Moist = sapply(Moist100.rm, mean) - mean(Moist100.rm[[1]]),
                  slope = sapply(slope100.rm, mean) - mean(slope100.rm[[1]]),
                  convex = -sapply(convex100.rm, mean) + mean(convex100.rm[[1]]),
                  site = as.factor(1:7),
                  Time = time,
                  Time2 = time2)


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
r1.r50<- gamm(WSG ~  s(Time,k=4),data=moge50r,
  correlation = corAR1(form = ~ Time2))
r2.r50<- gamm(Moist ~  s(Time,k=4),data=moge50r,
  correlation = corAR1(form = ~ Time2))
r3.r50<- gamm(convex ~  s(Time,k=4),data=moge50r,
  correlation = corAR1(form = ~ Time2))
r4.r50 <- gamm(slope ~  s(Time,k=4),data=moge50r,
  correlation = corAR1(form = ~ Time2))

r1_50 <- gamm(WSG ~  s(Time,k=4),data=moge50,
  correlation = corAR1(form = ~ Time2))
r2_50 <- gamm(Moist ~  s(Time,k=4),data=moge50,
  correlation = corAR1(form = ~ Time2))
r3_50 <- gamm(convex ~  s(Time,k=4),data=moge50,
  correlation = corAR1(form = ~ Time2))
r4_50 <- gamm(slope ~  s(Time,k=4),data=moge50,
  correlation = corAR1(form = ~ Time2))

# heteroskedasticity
m1 <- gamm(Moist ~  s(Time,k=4), data = moge,
  random = list(site = ~ 1),
  correlation = corAR1(form = ~ Time2),
  weights = varFixed(~Time2),
  method = "ML")


m2 <- gamm(Moist ~  s(Time,k=4), data = moge,
  random = list(site = ~ 1),
  correlation = corAR1(form = ~ Time2))

par(mfrow=c(1,2))
plot(resid(m2$lme, type = "normalized") ~ moge$Time)
plot(resid(m1$lme, type = "normalized") ~ moge$Time)
par(mfrow=c(1,1))

library(bbmle)
AICctab(m1$lme, m2$lme)


r1_20 <- gamm(WSG ~  s(Time, k = 4), data=moge20,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r2_20 <- gamm(Moist ~  s(Time, k = 4), data=moge20,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r3_20 <- gamm(convex ~  s(Time, k = 4), data=moge20,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r4_20 <- gamm(slope ~  s(Time, k = 4), data=moge20,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))


r1.r100 <- gamm(WSG ~  s(Time, k = 4), data=moge100r,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r2.r100 <- gamm(Moist ~  s(Time, k = 4), data=moge100r,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r3.r100 <- gamm(convex ~  s(Time, k = 4), data=moge100r,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r4.r100 <- gamm(slope ~  s(Time, k = 4), data=moge100r,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))

r1_100 <- gamm(WSG ~  s(Time, k = 4), data=moge,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r2_100 <- gamm(Moist ~  s(Time, k = 4), data=moge,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r3_100 <- gamm(convex ~  s(Time, k = 4), data=moge,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))
r4_100 <- gamm(slope ~  s(Time, k = 4), data=moge,
              random = list(site = ~ 1),
              weights = varFixed(~ Time2),
              correlation = corAR1(form = ~ Time2))


res1_100 <- predict(r1_100$gam ,se.fit=T)
res1r100 <- predict(r1.r100$gam ,se.fit=T)
res2_100 <- predict(r2_100$gam ,se.fit=T)
res2r100 <- predict(r2.r100$gam ,se.fit=T)
res3_100 <- predict(r3_100$gam ,se.fit=T)
res3r100 <- predict(r3.r100$gam ,se.fit=T)
res4_100 <- predict(r4_100$gam ,se.fit=T)
res4r100 <- predict(r4.r100$gam ,se.fit=T)

res1_50 <- predict(r1_50$gam ,se.fit=T)
res1r50 <- predict(r1.r50$gam ,se.fit=T)
res2_50 <- predict(r2_50$gam ,se.fit=T)
res2r50 <- predict(r2.r50$gam ,se.fit=T)
res3_50 <- predict(r3_50$gam ,se.fit=T)
res3r50 <- predict(r3.r50$gam ,se.fit=T)
res4_50 <- predict(r4_50$gam ,se.fit=T)
res4r50 <- predict(r4.r50$gam ,se.fit=T)

res3_20 <- predict(r3_20$gam ,se.fit=T)
res4_20 <- predict(r4_20$gam ,se.fit=T)

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
r1data100 <- data.frame(time = spline(r1_100$gam$model$Time, res1_100$fit)$x,
          mean = spline(r1_100$gam$model$Time, res1_100$fit)$y,
          upper = spline(r1_100$gam$model$Time,
              res1_100$fit + 1.96*res1_100$se.fit)$y,
          lower = spline(r1_100$gam$model$Time,
              res1_100$fit - 1.96*res1_100$se.fit)$y,
          obs = "obs")

r1data100_r <- data.frame(time = spline(r1.r100$gam$model$Time, res1r100$fit)$x,
          mean = spline(r1.r100$gam$model$Time, res1r100$fit)$y,
          upper = spline(r1.r100$gam$model$Time,
              res1r100$fit + 1.96*res1r100$se.fit)$y,
          lower = spline(r1.r100$gam$model$Time,
              res1r100$fit - 1.96*res1r100$se.fit)$y,
          obs = "tr")

moge1 <- bind_rows(r1data100, r1data100_r)


r2data100 <- data.frame(time = spline(r2_100$gam$model$Time, res2_100$fit)$x,
          mean = spline(r2_100$gam$model$Time, res2_100$fit)$y,
          upper = spline(r2_100$gam$model$Time,
              res2_100$fit + 1.96*res2_100$se.fit)$y,
          lower = spline(r2_100$gam$model$Time,
              res2_100$fit - 1.96*res2_100$se.fit)$y,
          obs = "obs")
r2data100_r <- data.frame(time = spline(r2.r100$gam$model$Time, res2r100$fit)$x,
          mean = spline(r2.r100$gam$model$Time, res2r100$fit)$y,
          upper = spline(r2.r100$gam$model$Time,
              res2r100$fit + 1.96*res2r100$se.fit)$y,
          lower = spline(r2.r100$gam$model$Time,
              res2r100$fit - 1.96*res2r100$se.fit)$y,
          obs = "tr")

moge2 <- bind_rows(r2data100, r2data100_r)


r3data100 <- data.frame(time = spline(r3_100$gam$model$Time, res3_100$fit)$x,
          mean = spline(r3_100$gam$model$Time, res3_100$fit)$y,
          upper = spline(r3_100$gam$model$Time,
              res3_100$fit + 1.96*res3_100$se.fit)$y,
          lower = spline(r3_100$gam$model$Time,
              res3_100$fit - 1.96*res3_100$se.fit)$y,
          obs = "obs")
r3data100_r <- data.frame(time = spline(r3.r100$gam$model$Time, res3r100$fit)$x,
          mean = spline(r3.r100$gam$model$Time, res3r100$fit)$y,
          upper = spline(r3.r100$gam$model$Time,
              res3r100$fit + 1.96*res3r100$se.fit)$y,
          lower = spline(r3.r100$gam$model$Time,
              res3r100$fit - 1.96*res3r100$se.fit)$y,
          obs = "tr")

moge3 <- bind_rows(r3data100, r3data100_r)



r4data100 <- data.frame(time = spline(r4_100$gam$model$Time, res4_100$fit)$x,
          mean = spline(r4_100$gam$model$Time, res4_100$fit)$y,
          upper = spline(r4_100$gam$model$Time,
              res4_100$fit + 1.96*res4_100$se.fit)$y,
          lower = spline(r4_100$gam$model$Time,
              res4_100$fit - 1.96*res4_100$se.fit)$y,
          obs = "obs")

r4data100_r <- data.frame(time = spline(r4.r100$gam$model$Time, res4r100$fit)$x,
          mean = spline(r4.r100$gam$model$Time, res4r100$fit)$y,
          upper = spline(r4.r100$gam$model$Time,
              res4r100$fit + 1.96*res4r100$se.fit)$y,
          lower = spline(r4.r100$gam$model$Time,
              res4r100$fit - 1.96*res4r100$se.fit)$y,
          obs = "tr")

moge4 <- bind_rows(r4data100, r4data100_r)




r3data20 <- data.frame(time = spline(r3_20$gam$model$Time, res3_20$fit)$x,
          mean = spline(r3_20$gam$model$Time, res3_20$fit)$y,
          upper = spline(r3_20$gam$model$Time,
              res3_20$fit + 1.96*res3_20$se.fit)$y,
          lower = spline(r3_20$gam$model$Time,
              res3_20$fit - 1.96*res3_20$se.fit)$y,
          obs = "obs")

r4data20 <- data.frame(time = spline(r4_20$gam$model$Time, res4_20$fit)$x,
          mean = spline(r4_20$gam$model$Time, res4_20$fit)$y,
          upper = spline(r4_20$gam$model$Time,
              res4_20$fit + 1.96*res4_20$se.fit)$y,
          lower = spline(r4_20$gam$model$Time,
              res4_20$fit - 1.96*res4_20$se.fit)$y,
          obs = "obs")

moge <- bind_rows(moge1, moge2, moge3, moge4) %>%
  mutate(Trait = rep(c("WSG", "Moist", "Convex", "Slope"), each = 42)) %>%
  mutate(size = "1ha") %>%
  mutate(Trait = as.factor(Trait))

gam_dat <- bind_rows(r1data100, r2data100, r3data100, r4data100) %>%
  mutate(Trait = rep(c("WSG", "Moist", "Convex", "Slope"), each = 21)) %>%
  tidyr::gather(., "obs", "val", 2:3)

ggplot(gam_dat, aes(x = time, y = val, colour = obs)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Trait, scale = "free")


fig_dat4 <- bind_rows(fig_dat2, fig_dat3) %>%
  mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Concavity~(m)", "Slope~(degrees)")))
#filter(size != "50ha")

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

temp <- bind_rows(fig_dat4, moge6) %>%
  # filter(size != "50ha") %>%
  mutate(obs2 = as.factor(obs2)) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)"))) %>%
  mutate(size = factor(size, levels = c("50ha","1ha", "0.04ha")))
  # mutate(obs2 = ifelse(obs2 == "dat", "obs", obs2))


#fig_dat_n2 <- fig_dat_n %>%
#  filter(size == "50ha") %>%
#  mutate(obs2 = "tr") %>%
#  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
#  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Concavity~(m)", "Slope~(degrees)"))) %>%
#  mutate(est = NA) %>%
#  mutate(est_mean = NA) %>%
#  mutate(est_lo = NA) %>%
#  mutate(est_up = NA)
#
#
#temp2 <- bind_rows(temp, fig_dat_n2) %>%
#  mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha")))
save.image("fig2.rda")


load("fig2.rda")
# 9 inche version

pdf("~/Dropbox/MS/TurnoverBCI/TurnoverBCI_MS/ver2/fig2_new1.pdf", width = 9, height = 4.66)

#before <- proc.time()
ggplot(filter(temp, is.na(est_mean) == TRUE) %>% filter(size == "1ha")) +
    geom_point(data = temp %>% filter(obs2 != "tr"),
      aes(x = jitter(Time),
    y = val), size = 1.2, alpha = 0.4) +
    guides(fill = FALSE) +
    facet_wrap(size ~ trait2, scale = "free",
    labeller = labeller(trait2 = label_parsed),
    switch = NULL, ncol = 4) + # geom_smooth() +
    theme_bw() +
#after <- proc.time()
#after - before

    xlab("Time") +
    ylab("Deviation from initial trait values") +
    geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs" & est == "est"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.3) +
    geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs" & est == "est"), aes(x = Time, y = est_mean), colour = "blue", lwd = 0.5) +

    geom_ribbon(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")),
    aes(ymin = est_lo, ymax = est_up, x = Time, fill = obs2), fill = "blue", alpha = 0.3) +
    geom_line(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")),
              aes(x = Time, y = est_mean, colour = obs2, lty = obs2),
              colour = "blue", lty = 1, lwd = 0.5) +

    #scale_fill_manual(values = c("gray", "gray")) +
    scale_colour_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c(2, 2)) +
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


pdf("~/Dropbox/MS/TurnoverBCI/TurnoverBCI_MS/ver2/fig2_new2.pdf", width = 9, height = 4.66)

pdf("~/Desktop/fig2_new2.pdf", width = 9, height = 4.66)
#before <- proc.time()
ggplot(filter(temp, is.na(est_mean) == TRUE) %>% filter(size == "1ha")) +
    geom_point(data = temp %>% filter(obs2 != "tr"),
      aes(x = jitter(Time),
    y = val), size = 1.2, alpha = 0.4) +
    geom_line(data = temp, aes(x = Time, y = val, fill = site),
              alpha = 0.2, lwd = 0.3) +
    guides(fill = FALSE) +
    facet_wrap(size ~ trait2, scale = "free",
    labeller = labeller(trait2 = label_parsed),
    switch = NULL, ncol = 4) + # geom_smooth() +
    theme_bw() +
#after <- proc.time()
#after - before

    xlab("Time") +
    ylab("Deviation from initial trait values") +
    geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs" & est == "est"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.4) +
    geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs" & est == "est"), aes(x = Time, y = est_mean), colour = "blue", lwd = 0.5) +

    geom_ribbon(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")),
    aes(ymin = est_lo, ymax = est_up, x = Time, fill = obs2), fill = "blue", alpha = 0.4) +
    geom_line(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")),
              aes(x = Time, y = est_mean, colour = obs2, lty = obs2),
              colour = "blue", lty = 1, lwd = 0.5) +

    #scale_fill_manual(values = c("gray", "gray")) +
    scale_colour_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c(2, 2)) +
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


#pdf("~/Desktop/fig2.pdf", width = 6, height = 4.66)
pdf("~/Desktop/fig2.pdf", width = 6, height = 3.7 )

p <-ggplot(filter(temp, is.na(est_mean) == TRUE)) +
    geom_point(data = temp %>% filter(obs2 != "tr"),
      aes(x = jitter(Time),
    y = val), size = 1, alpha = 0.4) +
    geom_line(data = temp, aes(x = Time, y = val, fill = site),
              alpha = 0.2, lwd = 0.3) +
    guides(fill = FALSE) +
    facet_wrap( ~ size + trait2, scale = "free_y",
    labeller = labeller(trait2 = label_parsed),
    switch = NULL, ncol = 4) + # geom_smooth() +
    theme_bw() +

    xlab("Time") +
    ylab("Deviation from initial trait values") +
    geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs" & est == "est"),
                aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.4) +
    geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs" & est == "est"),
              aes(x = Time, y = est_mean), colour = "blue", lwd = 0.5) +
    geom_ribbon(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")) %>%
      filter(trait != "Moist"),
    aes(ymin = est_lo, ymax = est_up, x = Time, fill = obs2), fill = "blue", alpha = 0.4) +
    geom_line(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")) %>%
      filter(trait != "Moist"),
              aes(x = Time, y = est_mean, colour = obs2, lty = obs2),
              colour = "blue", lty = 1, lwd = 0.5) +

    #scale_fill_manual(values = c("gray", "gray")) +
    scale_colour_manual(values = c("black", "black")) +
    scale_linetype_manual(values = c(2, 2)) +
    guides(linetype = FALSE) +
    guides(colour = FALSE) +
    guides(fill = FALSE) +
    theme(
      plot.margin = unit(c(0.5, 0.2, 0.2 , 0.2), units = "cm"),
      strip.text = element_text(size = 7),
      #strip.background = element_rect(size = unit(5, units = "cm")),
      axis.title = element_text(size = 7),
      axis.text.x = element_text(size = 5, angle = 0),
      axis.text.y = element_text(size = 5),
      legend.text = element_text(size = 7))

g <- ggplotGrob(p)

g$heights
g
#g$grobs[[62]]$height

# strip height
for (i in c(6, 11, 16)) g$heights[[i]] <- unit(0.35, "cm")
for (i in c(5, 10, 15)) g$heights[[i]] <- unit(0, "cm")
#for (i in c(1, 2, 3,4)) g$heights[[i]] <- unit(0.1, "cm")

# marge between each subpanel
for (i in c(8, 13, 18)) g$heights[[i]] <- unit(0.3, "cm") 
for (i in c(62:73)) g$grobs[[i]]$heights <- unit(c(1,1), "npc")
#g$grobs[[72]]$height

#g$heights[[8]] <- unit(0.1, "cm")
grid.draw(g)

g$heights

dev.off()



## error
par(mfrow = c(2,4))
plot(r1_100$gam, residuals = T, main = "WSG 1ha")
plot(r2_100$gam, residuals = T, main =  )
plot(r3_100$gam, residuals = T)
plot(r4_100$gam, residuals = T)

plot(r1_20$gam, residuals = T)
plot(r2_20$gam, residuals = T)
plot(r3_20$gam, residuals = T)
plot(r4_20$gam, residuals = T)
