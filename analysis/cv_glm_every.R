# CV sp
# every censuss
rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")
library(dplyr)
library(cvTools)
library(MASS)
ab_data <- as.data.frame(sapply(D100m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)

trait.temp <- data.frame(sp=rownames(trait),
  moist=trait$Moist,
  slope=trait$sp.slope.mean,
  slope.sd = trait$sp.slope.sd,
  convex=trait$sp.convex.mean,
  convex.sd=trait$sp.convex.sd,
  WSG=trait$WSG)


ab_t_data <- merge(ab_data,trait.temp,by="sp") %>%
  mutate(rate = log(census_2010/census_1982)/(2010-1982))
ab_t_data2 <- ab_t_data %>%
  filter(is.finite(rate) == TRUE) %>%
  mutate(WSG = as.numeric(scale(WSG))) %>%
  mutate(slope = as.numeric(scale(slope))) %>%
  mutate(convex = as.numeric(scale(convex))) %>%
  mutate(moist = as.numeric(scale(moist))) %>%
  na.omit()


######
# GLM
######

cv_glm <- function(data, glmfit, last = "census_2010", ini = "census_1982", K = 10){
  res_cv <- NULL
  SS <- NULL
  PREDS <- NULL
  temp <- cvFolds(nrow(data), K = K, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)

  Call <- glmfit$call

  for (i in 1:K){
    Call$data <- filter(temp, gr != i)
    d_glm <- eval.parent(Call)

    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(d_glm, test_dat, type = "response")
    fitted2 <- fitted / test_dat[,ini]
    yy <- test_dat[,last] / test_dat[,ini]

    SS[i] <- (yy - mean(yy, na.rm = T))^2 %>% mean
    PREDS[i] <- (yy - fitted2)^2 %>% mean(na.rm = T)
  }
  # 1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)
  mean_ <- mean(1 - PREDS/SS, na.rm = T)
  se_ <- sd(1 - PREDS/SS, na.rm = T) / sqrt(K)
  upper <- mean_ + se_ * 1.96
  lower <- mean_ - se_ * 1.96
  data.frame(mean = mean_, lower = lower, upper = upper)

}

time <- colnames(ab_t_data)[2:8]

temp1 <- data.frame(ini = rep(time[1], 7), last = time)

temp2 <- data.frame(ini = time[-7], last = time[-1])

time_dat <- bind_rows(temp1, temp2) %>% unique %>%
  filter(ini != last)

res3 <- NULL
for (i in 1: nrow(time_dat)){

  form <- paste(time_dat$last[i], "~WSG + moist + convex + slope + WSG:moist + WSG:convex + WSG:slope + convex:slope + moist:slope + convex:moist + offset(log(",
              time_dat$ini[i],"))", sep = "") %>% noquote

  res_all <- glm.nb(form, data = ab_t_data2)
  #
  # res_all <- glm.nb(census_2005 ~WSG
  #             + moist
  #             + convex
  #             + slope
  #             + WSG:moist
  #             + WSG:convex
  #             + WSG:slope
  #             + convex:slope
  #             + moist:slope
  #             + convex:moist
  #             + offset(log(census_1982)),
  #             data = ab_t_data2)

  res2 <- stepAIC(res_all)

  res <- cv_glm(ab_t_data2, res2, last = time_dat$last[i], ini = time_dat$ini[i], K = 10)
  res3 <- rbind(res3, res)
}

res3

res3
          mean       lower      upper
1  -0.07818864 -0.17556276 0.01918548
2  -0.02128102 -0.19846008 0.15589804
3  -0.13883451 -0.33080684 0.05313782
4   0.02886835 -0.10894672 0.16668343
5  -0.05803463 -0.19019174 0.07412248
6  -0.12920770 -0.33241059 0.07399519
7  -0.09285158 -0.37265943 0.18695627
8  -0.05769736 -0.23509704 0.11970232
9  -0.06701823 -0.17695666 0.04292020
10 -0.03377823 -0.10239693 0.03484046
11 -0.02858482 -0.08570646 0.02853681


res4 <- bind_cols(time_dat, res3)

write.csv(res4, "~/Dropbox/MS/TurnoverBCI/TurnoverBCI_MS/fig/sp_res.csv")
