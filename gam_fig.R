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


fig_dat <- data_frame(WSG = c(unlist(WSG100), unlist(WSG20)),
  Moist = c(unlist(Moist100), unlist(Moist20)),
  Convex = c(unlist(convex100), unlist(convex20)),
  Slope = c(unlist(slope100), unlist(slope20)),
  Time = c(rep(time, each = 50),
    rep(time, each = 1250)),
  size = rep(c("1ha", "0.04ha"), c(50*7, 1250*7)))

fig_dat2 <- fig_dat %>%
  tidyr::gather(., "trait", "val", 1:4)

fig_dat3 <- fig_dat2 %>%
  group_by(Time, trait) %>%
  summarise(val = mean(val))

fig_dat3 <- data_frame(Time = fig_dat3$Time,
    size = "52ha",
    trait = fig_dat3$trait,
    val = fig_dat3$val)

fig_dat4 <- bind_rows(fig_dat2, fig_dat3) %>%
  mutate(size = factor(size, levels = c("52ha", "1ha", "0.04ha"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Convexity~(m)", "Slope~(degrees)")))

temp <- fig_dat3 %>% filter(trait == "WSG")



postscript("~/Dropbox/MS/TurnoverBCI/fig/moge.eps", width = 5.9, height = 5.8)

fig_dat4 %>% filter(size != "52ha") %>%
ggplot(., aes(x = jitter(Time), y = val) ) +
  geom_point() +
  facet_wrap(size ~ trait2, scale = "free_y", nrow = 2, labeller = labeller(trait2 = label_parsed, size = label_value)) +
  geom_smooth(data = filter(fig_dat4, size == "1ha" & trait == "WSG"), fill = "blue", alpha = 0.2) +
  theme_bw() +
  xlab("Time") +
  ylab("Trait values")

dev.off()



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



moge <- data.frame(WSG = unlist(WSG100),
                   Moist = unlist(Moist100),
                   slope = unlist(slope100),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time)

moge20 <- data.frame(WSG = unlist(WSG20),
                   Moist = unlist(Moist20),
                   slope = unlist(slope20),
                   convex = unlist(convex20),
                   site = as.factor(rep(1:1250,7)),
                   Time)

moge100r <- data.frame(WSG = unlist(WSG100.rm),
                   Moist = unlist(Moist100.rm),
                   slope = unlist(slope100.rm),
                   convex = unlist(convex100.rm),
                   site = rep(1:50,7),
                   Time)

moge20r <- data.frame(WSG = unlist(WSG20),
                   Moist = unlist(Moist20),
                   slope = unlist(slope20),
                   convex = unlist(convex20),
                   site = as.factor(rep(1:1250,7)),
                   Time)

#
#
# r1.r <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
# r2.r <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
# r3.r <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
r4.r <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
#
# r1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
# r2 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
# r3 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
r4 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())


# r1.r100<- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge100r, correlation = corAR1())
# r2.r100<- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge100r, correlation = corAR1())
# r3.r100<- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge100r, correlation = corAR1())
r4.r100 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge100r, correlation = corAR1())

# r1_100 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
# r2_100 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
# r3_100 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
r4_100 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())

# fig_dat <- data_frame(WSG = c(unlist(WSG100), unlist(WSG20)),
#   Moist = c(unlist(Moist100), unlist(Moist20)),
#   Convex = c(unlist(convex100), unlist(convex20)),
#   Slope = c(unlist(slope100), unlist(slope20)),
#   Time = c(rep(time, each = 50),
#     rep(time, each = 1250)),
#   size = rep(c("1ha", "0.04ha"), c(50*7, 1250*7)))

res4 <- predict(r4$gam, se.fit=T)
res4r <- predict(r4.r$gam, se.fit=T)
res4_100 <- predict(r4_100$gam, se.fit=T)
res4r100 <- predict(r4.r100$gam, se.fit=T)


fig_dat_n <- data_frame(WSG = c(unlist(WSG100.rm), unlist(WSG20.rm)),
  Moist = c(unlist(Moist100.rm), unlist(Moist20.rm)),
  Convex = c(unlist(convex100.rm), unlist(convex20.rm)),
  Slope = c(unlist(slope100.rm), unlist(slope20.rm)),
  Time = c(rep(time, each = 50),
  rep(time, each = 1250)),
  size = rep(c("1ha", "0.04ha"), c(50*7, 1250*7))) %>%
  tidyr::gather(., "trait", "val", 1:4) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Convexity~(m)", "Slope~(degrees)")))


fig_dat_n %>% filter(size != "52ha") %>%
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
filter(size != "52ha")

ggplot(temp3, aes(x = jitter(Time), y = val, shape = obs, alpha = 0.6, lty = obs)) +
  geom_point() +
  facet_wrap(size ~ trait2, scale = "free_y", nrow = 2, labeller = labeller(trait2 = label_parsed, size = label_value)) +
  geom_smooth(data = filter(temp3, size == "1ha" & trait == "WSG"), fill = "blue", alpha = 0.2) +
  geom_smooth(fill = "blue", alpha = 0.2) +
  theme_bw() +
  xlab("Time") +
  ylab("Trait values") +
  scale_colour_manual(values = c("black", "darkorange")) +
  scale_shape_manual(values = c(1, 16))


temp1 %>%
  filter(size != "52ha") %>%
ggplot(., aes(x = jitter(Time), y = val, alpha = 0.2)) +
  geom_point() +
  facet_wrap(size ~ trait2, scale = "free_y", nrow = 2, labeller = labeller(trait2 = label_parsed, size = label_value)) +
  geom_smooth(data = filter(temp3, size == "1ha" & trait == "WSG"), fill = "blue", alpha = 0.2) +
  theme_bw() +
  xlab("Time") +
  ylab("Trait values")






fig_dat5 <- fig_dat4 %>%
  mutate(obs = "ori") %>%
  bind_rows(., fig_dat_n) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Convexity~(m)", "Slope~(degrees)")))

fig_dat5 %>%
  filter(size != "52ha") %>%
  filter(trait != "Moist" | obs != "rm") %>%
ggplot(., aes(x = jitter(Time), y = val, colour = obs, alpha = 0.8) ) +
  geom_point() +
  facet_wrap(size ~ trait2, scale = "free_y", nrow = 2, labeller = labeller(trait2 = label_parsed, size = label_value)) +
  geom_smooth() +
  theme_bw() +
  # scale_colour_manual(values = c("blue", "red")) +
  xlab("Time") +
  ylab("Trait values")


  m1 <- function(x){min(x) * 0.94}
  m2 <- function(x){max(x) * 1.04}

p1 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=WSG),alpha=0.3)+ theme2 + labs(x="",title="0.04-ha subplots", y = "Wood density")
p1 <- p1 + geom_line(data = r1data,aes(x=time,y=WSG),col="black")
p1 <- p1 + geom_ribbon(data = r1data, aes(ymin=lower1,ymax=upper1,x=time),
  alpha=0.5, fill="gray") +
  ylim(moge20$WSG %>% m1, moge20$WSG %>% m2)

p1



##########

names(r1data100)[2:3] <- c("obs", "rm")
names(r2data100)[2:3] <- c("obs", "rm")
names(r3data100)[2:3] <- c("obs", "rm")
names(r4data100)[2:3] <- c("obs", "rm")

gam_dat <- bind_rows(r1data100, r2data100, r3data100, r4data100) %>%
  mutate(Trait = rep(c("WSG", "Moist", "Convex", "Slope"), each = 21)) %>%
  tidyr::gather(., "obs", "val", 2:3)

ggplot(gam_dat, aes(x = time, y = val, colour = obs)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Trait, scale = "free")

r1data100 %>% tidyr::gather(., "obs", "val", 2:3) %>%
  tidyr::gather(., "se", "val2", 2:4) %>% unique

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


moge <- bind_rows(moge1, moge2, moge3, moge4) %>%
  mutate(Trait = rep(c("WSG", "Moist", "Convex", "Slope"), each = 42)) %>%
  mutate(size = "1ha") %>%
  mutate(Trait = as.factor(Trait))

ggplot(moge, aes(x = time, y = mean, colour = obs, fill = obs)) +
# geom_point() +
facet_wrap(~Trait, scale = "free") +
geom_line() +
geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5)
#
# ggplot(fig_dat4, aes(x = jitter(Time), y = val) ) +
#   geom_point() +
#   facet_wrap(trait2 ~ size, scale = "free_y", nrow = 4, labeller = labeller(trait2 = label_parsed, size = label_value)) +
#   geom_smooth() +
#   theme_bw() +
#   xlab("Time") +
#   ylab("Trait values")

fig_dat4 <- bind_rows(fig_dat2, fig_dat3) %>%
  mutate(size = factor(size, levels = c("52ha", "1ha", "0.04ha"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Convexity~(m)", "Slope~(degrees)")))

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
    est = NA,
    trait2 = moge5$Trait,
    est_mean = moge5$mean,
    est_lo = moge5$lower,
    est_up = moge5$upper)

temp <- bind_rows(fig_dat4, moge6) %>%
  filter(size != "52ha") %>%
  mutate(obs2 = as.factor(obs2)) %>%
  mutate(trait = factor(trait, levels = c("WSG", "Moist", "Convex", "Slope"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Convexity~(m)", "Slope~(degrees)"))) %>%
  mutate(size = factor(size, levels = c("52ha", "1ha", "0.04ha")))

  # %>%
  # # mutate(lo_val = val) %>%
  # # mutate(up_val = val)
  # mutate(lo_val = ifelse(est_lo == "Y", val, 0)) %>%
  # mutate(up_val = ifelse(est_up == "Y", val, 0))
#
#
#
# ggplot(filter(temp, is.na(est_mean) == TRUE)) +
#   geom_point(aes(x = jitter(Time), y = val), alpha = 0.4) +
#   facet_wrap(size ~ trait2, scale = "free_y", nrow = 2, labeller = labeller(trait2 = label_parsed, size = label_value)) +
#   # geom_smooth() +
#   theme_bw() +
#   xlab("Time") +
#   ylab("Trait values") +
#   geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.5) +
#   geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "blue") +
#   geom_ribbon(data =  filter(temp, trait != "WSG" | size != "1ha" | obs2 != "obs"), aes(ymin = est_lo, ymax = est_up, x = Time,  fill = obs2), alpha = 0.5) +
#   geom_line(data = filter(temp, trait != "WSG" | size != "1ha" | obs2 != "obs"), aes(x = Time, y = est_mean, colour = obs2, lty = obs2)) +
#   scale_fill_manual(values = c("gray", "gray", "gray")) +
#   scale_colour_manual(values = c("black", "black", "black")) +
#   scale_linetype_manual(values = c(1, 2, 2))
#
# load("gam_fig201601006.RData")
# # library(Cairo)
# pdf("~/Dropbox/MS/TurnoverBCI/fig/moge.pdf", width = 10, height = 6)
# # ggsave("~/Dropbox/MS/TurnoverBCI/fig/moge.eps",
# #   width = 10, height = 6,
# #   units = "in",
# #   device = cairo_ps)
# #
# # Cairo("~/Dropbox/MS/TurnoverBCI/fig/moge.eps",
# #   width = 10, height = 6, bg = "transparent", type = "ps", dpi = 300)
# ggplot(filter(temp, is.na(est_mean) == TRUE)) + geom_point(aes(x = jitter(Time),
#     y = val), size = 0.8, alpha = 0.4) +
#     facet_wrap( ~ size + trait2, scale = "free_y", nrow = 2,
#     labeller = labeller(trait2 = label_parsed, size = label_value),
#     switch = NULL, dir = "h") + # geom_smooth() +
#     theme_bw() + xlab("Time") + ylab("Trait values") +
#     geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.5) +
#     geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "blue") +
#
#     geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "tr"), aes(ymin = est_lo,
#     ymax = est_up, x = Time), fill = "red", alpha = 0.5) +
#     geom_line(data = filter(temp,
#     trait == "WSG" & size == "1ha" & obs2 == "tr"), aes(x = Time, y = est_mean),
#     colour = "red", lty = 2) +
#
#     geom_ribbon(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")),
#     aes(ymin = est_lo, ymax = est_up, x = Time, fill = obs2), alpha = 0.5) +
#     geom_line(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")), aes(x = Time, y = est_mean, colour = obs2, lty = obs2)) +
#
#     scale_fill_manual(values = c("gray", "gray", "gray")) +
#     scale_colour_manual(values = c("black", "black", "black")) +
#     scale_linetype_manual(values = c(2, 2, 2)) +
#     guides(linetype = FALSE) +
#     guides(colour = FALSE) +
#     guides(fill = FALSE)
#     # +
#     # theme(strip.text = element_text(size = 7, lineheight = 3),
#     # axis.title = element_text(size = 8),
#     # axis.text.x = element_text(size = 7, angle = 45),
#     # axis.text.y = element_text(size = 7, angle = 45))
#
#
#
#     # g <- ggplotGrob(d)
#     # g$heights[[3]] = unit(3,"in")
#     #
#     # grid.newpage()
#     # grid.draw(g)
# dev.off()
#
# ## use cowplot
# library(cowplot)
#
# # pdf("~/Dropbox/MS/TurnoverBCI/fig/moge.pdf", width = 4.33, height = 6)
#
# pdf("~/Dropbox/MS/TurnoverBCI/fig/moge.pdf", width = 5, height = 10)
#
# before <- proc.time()
# # ylab(expression(paste("Wood density (g ",cm^-3,")"))) +
#
# theme <- theme(axis.text.x = element_text(size = 10.5, angle = 0),
#   axis.text.y = element_text(size = 10.5),
#   axis.title.y = element_text(size = 12, margin = margin(0, 0, 0, 0)),
#   axis.title.x = element_text(size = 12, margin = margin(-10, 0, 0, 0)),
#   plot.margin = unit(c(0.2, 0.2, 0, 0.05), units = "lines")
#   # panel.margin = unit(0, "lines")
#   )
#
# p1 <- temp %>%
#   filter(size == "1ha" & trait == "WSG" & obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.5) +
#   geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "blue") +
#
#   geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "tr"), aes(ymin = est_lo,
#       ymax = est_up, x = Time), fill = "red", alpha = 0.5) +
#   geom_line(data = filter(temp,
#       trait == "WSG" & size == "1ha" & obs2 == "tr"), aes(x = Time, y = est_mean),
#       colour = "red", lty = 2) +
#   theme_bw() +
#   xlab("") +
#   ylab("") +
#   theme +
#   theme(axis.text.x = element_blank()) +
#   theme(plot.margin = unit(c(2, 2, 0, 0), units = "lines"))
#
# p2 <- temp %>%
#   filter(size == "1ha", trait == "Moist", obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   geom_ribbon(data = filter(temp, trait == "Moist" & size == "1ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "gray", alpha = 0.5) +
#   geom_line(data = filter(temp, trait == "Moist" & size == "1ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "black") +
#   theme_bw() +
#   xlab("") +
#   # ylab("Moisture") +
#   ylab("") +
#   theme +
#   theme(axis.text.x = element_blank())
#
#
# p3 <- temp %>%
#   filter(size == "1ha", trait == "Convex", obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   geom_ribbon(data = filter(temp, trait == "Convex" & size == "1ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "gray", alpha = 0.5) +
#   geom_line(data = filter(temp, trait == "Convex" & size == "1ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "black") +
#   theme_bw() +
#   xlab("") +
#   # ylab("Convexity (m)") +
#   ylab("") +
#   theme +
#   theme(axis.text.x = element_blank())
#
# p4 <- temp %>%
#   filter(size == "1ha", trait == "Slope", obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   geom_ribbon(data = filter(temp, trait == "Slope" & size == "1ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "gray", alpha = 0.5) +
#   geom_line(data = filter(temp, trait == "Slope" & size == "1ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "black") +
#   theme_bw() +
#   # ylab("Slope (degrees)") +
#   ylab("") +
#   theme +
#   theme(axis.title.x = element_text(size = 12, margin = margin(5, 0, 0, 0)))
#
# p5 <- temp %>%
#   filter(size == "0.04ha" & trait == "WSG" & obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   geom_ribbon(data = filter(temp, trait == "WSG" & size == "0.04ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.5) +
#   geom_line(data = filter(temp, trait == "WSG" & size == "0.04ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "blue") +
#
#   geom_ribbon(data = filter(temp, trait == "WSG" & size == "0.04ha" & obs2 == "tr"), aes(ymin = est_lo,
#   ymax = est_up, x = Time), fill = "red", alpha = 0.5) +
#   geom_line(data = filter(temp,
#   trait == "WSG" & size == "0.04ha" & obs2 == "tr"), aes(x = Time, y = est_mean),
#   colour = "red", lty = 2) +
#   theme_bw() +
#   xlab("") +
#   ylab("") +
#   theme +
#   theme(axis.text.x = element_blank())
#
# p6 <- temp %>%
#   filter(size == "0.04ha", trait == "Moist", obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   theme_bw() +
#   theme(axis.text.x = element_blank()) +
#   xlab("") +
#   ylab("") +
#   theme +
#   theme(axis.text.x = element_blank())
#
#
# p7 <- temp %>%
#   filter(size == "0.04ha", trait == "Convex", obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   theme_bw() +
#   theme(axis.text.x = element_blank()) +
#   xlab("") +
#   ylab("") +
#   theme +
#   theme(axis.text.x = element_blank())
#
# p8 <- temp %>%
#   filter(size == "0.04ha", trait == "Slope", obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   theme_bw() +
#   ylab("") +
#   theme +
#   theme(axis.title.x = element_text(size = 12, margin = margin(5, 0, 0, 0)))
#
#
#
# xoff <- .27 # relative x position of label, within one plot
# yoff <- .93 # relative y position of label, within one plot
#
# plot_grid(p1, p5,
#       p2, p6,
#       p3, p7,
#       p4, p8,
#       # p1, p4, p7, p10,
#       # p1, p4, p7, p10,
#       ncol = 2,
#       align = "hv"
#     ) +
# draw_plot_label(label = paste("(", letters[1:8], ")", sep = ""),
#         x = rep((xoff + 0 +  0:1)/2, 4),
#         y = 1 - (1 - yoff + rep(0:3, each = 2))/4,
#         hjust = .5,
#         vjust = .5,
#         size = 12,
#         fontface = 1)
# after <- proc.time()
# dev.off()
# after - before
#
#
#
# ##
# pdf("~/Dropbox/MS/TurnoverBCI/fig/moge.pdf", width = 4.33, height = 6)
# small <- temp %>%
#   filter(size == "0.04ha" & obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   facet_grid(trait ~ ., scale = "free") +
#   theme(strip.text.y = element_blank())
#
# large <- temp %>%
#   filter(size == "1ha" & obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   facet_grid(trait ~ ., scale = "free") +
#   theme(strip.text.y = element_blank())
#
# plot_grid(small, large,
#   ncol = 2,
#   align = "hv")
# dev.off()
#
#
#
# xoff <- .27 # relative x position of label, within one plot
# yoff <- .93 # relative y position of label, within one plot
#
# pdf("~/Dropbox/MS/TurnoverBCI/fig/moge.pdf", width = 5, height = 10)
# temp %>%
#   filter(obs2 == "dat") %>%
#   ggplot(.) +
#   geom_point(aes(x = Time, y = val), alpha = 0.4, size = 0.8) +
#   facet_grid(trait ~ size, scale = "free", switch = "y")
# dev.off()


#last version
pdf("~/Dropbox/MS/TurnoverBCI/fig/moge.pdf", width = 4, height = 8)
ggplot(filter(temp, is.na(est_mean) == TRUE)) +
    geom_point(aes(x = jitter(Time),
    y = val), size = 0.8, alpha = 0.4) +
    facet_grid(trait2 ~ size, scale = "free",
    labeller = labeller(trait2 = label_parsed, size = label_value),
    switch = "y") + # geom_smooth() +
    theme_bw() +
    xlab("Time") +
    ylab("") +
    geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.5) +
    geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "blue") +

    geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "tr"), aes(ymin = est_lo,
    ymax = est_up, x = Time), fill = "red", alpha = 0.5) +
    geom_line(data = filter(temp,
    trait == "WSG" & size == "1ha" & obs2 == "tr"), aes(x = Time, y = est_mean),
    colour = "red", lty = 2) +

    geom_ribbon(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")),
    aes(ymin = est_lo, ymax = est_up, x = Time, fill = obs2), alpha = 0.5) +
    geom_line(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")), aes(x = Time, y = est_mean, colour = obs2, lty = obs2)) +

    scale_fill_manual(values = c("gray", "gray", "gray")) +
    scale_colour_manual(values = c("black", "black", "black")) +
    scale_linetype_manual(values = c(2, 2, 2)) +
    guides(linetype = FALSE) +
    guides(colour = FALSE) +
    guides(fill = FALSE) +
    theme(axis.text.x = element_text(angle = 45),
      plot.margin = unit(c(0.2, 0.2, 0.2 , -1), units = "lines")
      )

dev.off()




pdf("~/Dropbox/MS/TurnoverBCI/fig/moge2.pdf", width = 6, height = 12)
ggplot(filter(temp, is.na(est_mean) == TRUE)) +
    geom_point(aes(x = jitter(Time),
    y = val), size = 0.8, alpha = 0.4) +
    facet_wrap( ~ trait2 + size, scale = "free",
    labeller = labeller(trait2 = label_parsed, size = label_value),
    switch = NULL, ncol = 2) + # geom_smooth() +
    theme_bw() +
    xlab("Time") +
    ylab("Trait values") +
    geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(ymin = est_lo, ymax = est_up, x = Time), fill = "blue", alpha = 0.5) +
    geom_line(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "obs"), aes(x = Time, y = est_mean), colour = "blue") +

    geom_ribbon(data = filter(temp, trait == "WSG" & size == "1ha" & obs2 == "tr"), aes(ymin = est_lo,
    ymax = est_up, x = Time), fill = "red", alpha = 0.5) +
    geom_line(data = filter(temp,
    trait == "WSG" & size == "1ha" & obs2 == "tr"), aes(x = Time, y = est_mean),
    colour = "red", lty = 2) +

    geom_ribbon(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")),
    aes(ymin = est_lo, ymax = est_up, x = Time, fill = obs2), alpha = 0.5) +
    geom_line(data = filter(temp, trait != "WSG" & (size == "1ha" & obs2 == "obs")), aes(x = Time, y = est_mean, colour = obs2, lty = obs2)) +

    scale_fill_manual(values = c("gray", "gray", "gray")) +
    scale_colour_manual(values = c("black", "black", "black")) +
    scale_linetype_manual(values = c(2, 2, 2)) +
    guides(linetype = FALSE) +
    guides(colour = FALSE) +
    guides(fill = FALSE) +
    theme(axis.text.x = element_text(angle = 45),
      plot.margin = unit(c(0.2, 0.2, 0.2 , 0), units = "lines")
      )

dev.off()
