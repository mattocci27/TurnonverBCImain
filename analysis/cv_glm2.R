#CV sp
rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")
library(dplyr)
library(cvTools)
ab_data <- as.data.frame(sapply(D100m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)

trait.temp <- data.frame(sp=rownames(trait),
  moist=trait$Moist,
  slope=trait$sp.slope.mean,
  slope.sd = trait$sp.slope.sd,
  convex=trait$sp.convex.mean,
  convex.sd=trait$sp.convex.sd,
  WSG=trait$WSG)

# Rick style
ab_t_data <- merge(ab_data,trait.temp,by="sp") %>%
  mutate(rate = log(census_2010/census_1982)/(2010-1982))

par(mfrow=c(2,2))
plot(rate ~ WSG, ab_t_data2)
plot(rate ~ moist, ab_t_data2)
plot(rate ~ convex, ab_t_data2)
plot(rate ~ slope, ab_t_data2)
par(mfrow=c(1,1))

ab_t_data2 <- ab_t_data %>%
  filter(is.finite(rate) == TRUE) %>%
  mutate(WSG = as.numeric(scale(WSG))) %>%
  mutate(slope = as.numeric(scale(slope))) %>%
  mutate(convex = as.numeric(scale(convex))) %>%
  mutate(moist = as.numeric(scale(moist))) %>%
  na.omit()

res_ori <- lm(rate ~ WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + convex:slope
            + moist:slope
            + convex:moist, ab_t_data2)

stepAIC(res_ori)

res_aic <- lm(rate ~ WSG
            + convex
            + slope
            + WSG:slope, ab_t_data2)

data <- ab_t_data  %>% filter(is.finite(rate) == TRUE)
data <- ab_t_data2

# moge <-cvFit(res_aic, data = ab_t_data2, y = ab_t_data2$rate, K = 10, R = 10)
#####
#rate
####
res_cv <- NULL
SS <- NULL
PREDS <- NULL
K <- 10
r2_lm0 <- NULL
before <- proc.time()
for (j in 1:100){
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)
  for (i in 1:K){
    res_temp <- lm(rate ~ WSG
              + convex
              + slope
              + WSG:slope, temp %>% filter(gr != i))
    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(res_temp, test_dat)

    SS[i] <- (test_dat$rate - mean(test_dat$rate, na.rm = T))^2 %>% mean
    PREDS[i] <- (test_dat$rate - fitted )^2 %>% mean(na.rm = T)
  }

  r2_lm0[j] <- 1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)

}
after <- proc.time()
after - before

quantile(r2_lm0, 0.025)
quantile(r2_lm0, 0.975)
mean(r2_lm0)

#bootstrap
r2 <- NULL
for (i in 1:1000){
    temp2 <- temp %>% sample_n(10, replace = T)
    r2[i] <- 1 - mean(temp2$PREDS) / mean(temp2$SS)
}
quantile(r2, 0.025)


######
# GLM
######
res_all <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + convex:slope
            + moist:slope
            + convex:moist
            + offset(log(census_1982)),
            data = ab_t_data2)

stepAIC(res_all)

res_all2 <- glm.nb(census_2010 ~WSG
            + convex
            + slope
            + WSG:slope
            + offset(log(census_1982)),
            data = ab_t_data2)

# fitted <- predict(res_all2, temp)

res_cv <- NULL
SS <- NULL
PREDS <- NULL
K <- 10
r2_glm0 <- NULL
for (j in 1:100){
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)
  for (i in 1:K){
    res_temp <- glm.nb(census_2010 ~ WSG
            + convex
            + slope
            + WSG:slope
            + offset(log(census_1982)),
            temp %>% filter(gr != i))
    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(res_temp, test_dat) %>% exp # link
    fitted2 <- fitted / test_dat$census_1982
    yy <- test_dat$census_2010 / test_dat$census_1982

    SS[i] <- (yy - mean(yy, na.rm = T))^2 %>% mean
    PREDS[i] <- (yy - fitted2)^2 %>% mean(na.rm = T)
  }

  r2_glm0[j] <- 1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)

}

quantile(r2_glm0, 0.025)
quantile(r2_glm0, 0.975)
mean(r2_glm0)

#######
# + 1
######

# Rick style
ab_t_data <- merge(ab_data,trait.temp,by="sp") %>%
  mutate(rate = log((census_2010 + 1)/(census_1982+1))/(2010-1982))

ab_t_data2 <- ab_t_data %>%
  filter(is.finite(rate) == TRUE) %>%
  mutate(WSG = as.numeric(scale(WSG))) %>%
  mutate(slope = as.numeric(scale(slope))) %>%
  mutate(convex = as.numeric(scale(convex))) %>%
  mutate(moist = as.numeric(scale(moist))) %>%
  mutate(census_2010 = census_2010 + 1) %>%
  mutate(census_1982 = census_1982 + 1) %>%
  na.omit()



#####
#rate
####
res_ori <- lm(rate ~ WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + convex:slope
            + moist:slope
            + convex:moist, ab_t_data2)

stepAIC(res_ori)

res_cv <- NULL
SS <- NULL
PREDS <- NULL
K <- 10
r2_lm1 <- NULL
for (j in 1:100){
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)
  for (i in 1:K){
    res_temp <- lm(rate ~ WSG
              + convex
              + slope
              + WSG:slope, temp %>% filter(gr != i))
    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(res_temp, test_dat)

    SS[i] <- (test_dat$rate - mean(test_dat$rate, na.rm = T))^2 %>% mean
    PREDS[i] <- (test_dat$rate - fitted )^2 %>% mean(na.rm = T)
  }

  r2_lm1[j] <- 1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)

}

quantile(r2_lm1, 0.025)
quantile(r2_lm1, 0.975)
mean(r2_lm1)

# #bootstrap
# r2 <- NULL
# for (i in 1:1000){
#     temp2 <- temp %>% sample_n(10, replace = T)
#     r2[i] <- 1 - mean(temp2$PREDS) / mean(temp2$SS)
# }
# quantile(r2, 0.025)


######
# GLM
######
res_all <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + convex:slope
            + moist:slope
            + convex:moist
            + offset(log(census_1982)),
            data = ab_t_data2)

stepAIC(res_all)

res_all2 <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:slope
            + moist:convex
            + offset(log(census_1982)),
            data = ab_t_data2)

# fitted <- predict(res_all2, temp)

res_cv <- NULL
SS <- NULL
PREDS <- NULL
K <- 10
r2_glm1 <- NULL
for (j in 1:100){
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)
  for (i in 1:K){
    res_temp <- glm.nb(census_2010 ~ WSG
            + moist
            + convex
            + slope
            + WSG:slope
            + moist:convex
            + offset(log(census_1982)),
            temp %>% filter(gr != i))
    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(res_temp, test_dat) %>% exp
    fitted2 <- fitted / test_dat$census_1982
    yy <- test_dat$census_2010 / test_dat$census_1982

    SS[i] <- (yy - mean(yy, na.rm = T))^2 %>% mean
    PREDS[i] <- (yy - fitted2)^2 %>% mean(na.rm = T)
  }

  r2_glm1[j] <- 1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)

}

quantile(r2_glm1, 0.025)
quantile(r2_glm1, 0.975)
mean(r2_glm1)

save.image("glm1ha.RData")
