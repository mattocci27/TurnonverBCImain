# if only two species change, what would happen?

#gam20151102.r
rm(list = ls()) # This clears everything from memory.
setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")


library(mgcv)
library(ggplot2)
library(gridExtra)
library(grid)
library(spikeSlabGAM)
library(dplyr)

moge2 <- D20m[[1]][, c("PIPECO","POULAR")]
moge3 <- moge2 %>% as.data.frame %>% filter(PIPECO > 0)

D100m2 <- D100m
D20m2 <- D20m

WSG100m2 <- list()
WSG20m2 <- list()


# if the two species change do not change at 100m, what would happen?
# if the two species occurs all of the 20m quadrat at the first census, what would happen?

D20m2[[1]][, c("PIPECO","POULAR")] <- D20m[[1]][, c("PIPECO","POULAR")] + 4
for (i in 1:7){
  D100m2[[i]][, c("PIPECO","POULAR")] <- D100m[[1]][, c("PIPECO","POULAR")]
  WSG100m2[[i]] <- com.mean.ab(D100m2[[i]], trait, "WSG")
  WSG20m2[[i]] <- com.mean.ab(D20m2[[i]], trait, "WSG")
}


moge <- data.frame(Time = rep(1:7, each = 50), WSG = unlist(WSG100m2))
moge2 <- data.frame(Time = rep(1:7, each = 1250), WSG = unlist(WSG20m2),
  site = rep(1:1250, 7))

par(mfrow = c(1,2))
plot(WSG ~ Time, moge)
plot(WSG ~ Time, moge2)
par(mfrow=c(1,1))

res <- gamm(WSG ~  s(Time, k=4), random = list(site=~1), data = moge2, correlation = corAR1())

#
# summary(res$gam)
#
# Family: gaussian
# Link function: identity
#
# Formula:
# WSG ~ s(Time, k = 4)
#
# Parametric coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) 0.5983184  0.0005318    1125   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Approximate significance of smooth terms:
#           edf Ref.df    F p-value
# s(Time) 2.995  2.995 1363  <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# R-sq.(adj) =   0.11
#   Scale est. = 0.00013794  n = 8750
#




#ind
n_sample <- unlist(WSG.ind) %>% length

moge_ind <- data.frame(WSG = unlist(WSG.ind),
         Time = c(rep(1, length(WSG.ind[[1]])),
              rep(2, length(WSG.ind[[2]])),
              rep(3, length(WSG.ind[[3]])),
              rep(4, length(WSG.ind[[4]])),
              rep(5, length(WSG.ind[[5]])),
              rep(6, length(WSG.ind[[6]])),
              rep(7, length(WSG.ind[[7]]))
              ))

res <- gam(WSG ~  s(Time, k=4),  data = moge_ind, correlation = corAR1())

res <- gamm(WSG ~  s(Time, k=4), random = list(site=~1), data = moge2, correlation = corAR1())


summary(res)
#
# Family: gaussian
# Link function: identity
#
# Formula:
# WSG ~ s(Time, k = 4)
#
# Parametric coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) 5.997e-01  7.737e-05    7752   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Approximate significance of smooth terms:
#           edf Ref.df     F p-value
# s(Time) 2.997      3 853.1  <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# R-sq.(adj) =  0.00159   Deviance explained = 0.16%
# GCV = 0.0095875  Scale est. = 0.0095875  n = 1601745

par(mfrow = c(1, 3))
plot(WSG ~ Time, moge)
plot(WSG ~ Time, moge2)
# plot(WSG ~ Time, moge_ind)
par(mfrow=c(1,1))



moge_ind <- moge_ind %>%
  mutate(sp = as.character(WSG))


moge_ind2 <- moge_ind %>% count(sp, Time) %>%
  mutate(val = as.numeric(sp))

plot(val ~ Time, moge_ind2, cex = log(n) * 0.2)
