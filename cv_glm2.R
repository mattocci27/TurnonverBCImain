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

ab_t_data <- merge(ab_data,trait.temp,by="sp")

ab_t_data2 <- na.omit(ab_t_data)

# temp.data<-D[D$status1=="A",]

ab_t_data2$inc <- as.factor(ifelse(ab_t_data2$census_2010>ab_t_data2$census_1982,"UP","Down"))

# plot(convex~inc,data=ab.t.data2)
library(MASS)


ab_t_data2 <- ab_t_data %>%
  na.omit() %>%
  filter(census_1982 != 0 | census_2010 != 0) %>%
  mutate(census_1982 = census_1982 + 1) %>%
  mutate(census_2010 = census_2010 + 1) %>%
  # filter(census_1982 != 0) %>%
  mutate(WSG_moist = as.numeric(scale(WSG*moist))) %>%
  mutate(WSG_slope = as.numeric(scale(WSG*slope))) %>%
  mutate(WSG_convex = as.numeric(scale(WSG*convex))) %>%
  mutate(slope_moist = as.numeric(scale(slope*moist))) %>%
  mutate(convex_moist = as.numeric(scale(convex*moist))) %>%
  mutate(slope_convex = as.numeric(scale(slope*convex))) %>%
  mutate(WSG = as.numeric(scale(WSG))) %>%
  mutate(slope = as.numeric(scale(slope))) %>%
  mutate(convex = as.numeric(scale(convex))) %>%
  mutate(moist = as.numeric(scale(moist)))

r2.func = function(res, data){
  y = data$census_2010/data$census_1982
  residuals = y - res$fitted.values/data$census_1982
  1 - sum(residuals^2) / sum((y - mean(y))^2)
}

res_all <- glm.nb(census_2010 ~ WSG
            * moist
            * convex
            * slope
            + offset(log(census_1982)),
            data = ab_t_data2)

res_all2 <- glm.nb(census_2010 ~WSG
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
stepAIC(res_all2)

res_tr <- stepAIC(res_all2)

r2.func(res_all, ab_t_data2)


moge <- cvFit(res_tr, data = ab_t_data2, y = ab_t_data2$census_1982, K = 10)

(res_tr$fitted - mean(res_tr$fitted))^2 %>% sqrt %>% mean

CV <- mean(res.cv,na.rm=T)
R2 <- 1 - (moge$cv)^2 / mean((res_tr$y - mean(res_tr$y))^2)



xx <- rnorm(100)
yy <- rnorm(100, 1.2*xx - 0.5, sd = 100)
moge <- data.frame(yy, xx) %>%
  mutate(gr = rep(1:10, each = 10))

res <- lm(yy ~ xx, moge)
res_cv <- cvFit(res, data = moge, y = moge$yy, K = 10, foldType = "consecutive")

aa <- cvFit(res, data = moge, y = moge$yy, K = 10, foldType = "consecutive")

1 - sum(res$residuals^2) / sum((yy - mean(yy))^2)


res_cv <- NULL
SS_cv <- NULL
PREDS_cv <- NULL
for (i in 1:10){
  res_temp <- lm(yy ~ xx, filter(moge, gr != i))
  fitted <- res_temp$coefficients[[1]] + filter(moge, gr == i)$xx * res_temp$coefficients[[2]]
  SS <- (filter(moge, gr == i)$yy - mean(filter(moge, gr == i)$yy))^2 %>% mean
  PREDS <- (fitted - filter(moge, gr == i)$yy)^2 %>% mean
  res_cv[i] <- 1 - PREDS/SS
  SS_cv[i] <- SS
  PREDS_cv[i] <- PREDS

  data.frame(res_cv, SS_cv, PREDS_cv)
}


# this one is equal to "cv"
# n = 10
mean(PREDS_cv /10) %>% sqrt


data <- temp


## without intercept
K <- 10
my_cv <- function(data, K){
  res_cv <- NULL
  SS_cv <- NULL
  PREDS_cv <- NULL
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)
  for (i in 1:K){
    res_temp <- glm.nb(census_2010 ~ WSG
            + moist
            + convex
            + slope
            + WSG:convex
            + moist:convex
            + offset(log(census_1982)), filter(temp, gr != i))

    fitted <- res_temp$coefficients[[1]] +
      filter(temp, gr == i)$WSG * res_temp$coefficients[[2]] +
      filter(temp, gr == i)$moist * res_temp$coefficients[[3]] +
      filter(temp, gr == i)$convex * res_temp$coefficients[[4]] +
      filter(temp, gr == i)$slope * res_temp$coefficients[[5]] +
      filter(temp, gr == i)$WSG * filter(temp, gr == i)$convex * res_temp$coefficients[[6]] +
      filter(temp, gr == i)$moist * filter(temp, gr == i)$convex * res_temp$coefficients[[6]]

    yy <- filter(temp, gr == i)$census_2010 / filter(temp, gr == i)$census_1982

    SS <- (yy - mean(yy))^2 %>% mean

    PREDS <- (exp(fitted) - yy)^2 %>% mean
    res_cv[i] <- 1 - PREDS/SS
    SS_cv[i] <- SS
    PREDS_cv[i] <- PREDS

  }
  data.frame(res_cv, SS_cv, PREDS_cv)
}


my_cv(ab_t_data2, K = 10)
#
#
# before <- proc.time()
# moge2 <- NULL
# for(i in 1:100){
#   moge <- my_cv(ab_t_data2, K = 10)
#   moge2[i] <- mean(moge$res_cv)
# }
# after <- proc.time()
#
# after - before


res_all2 <- glm.nb(census_2010 ~WSG
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



# res_all2 <- lm(log(census_2010/census_1982) ~WSG
#             + moist
#             + convex
#             + slope
#             + WSG:moist
#             + WSG:convex
#             + WSG:slope
#             + convex:slope
#             + moist:slope
#             + convex:moist,
#             data = ab_t_data2)


# res_all2 <- glm(census_2010 ~WSG
#             + moist
#             + convex
#             + slope,
#             family = poisson,
#             data = ab_t_data2)
glmfit <- stepAIC(res_all2)

plot(census_2010 ~ WSG, head(ab_t_data2, 30), log = "y")

my_cv2 <- function(data, glmfit, K){
  res_cv <- NULL
  SS_cv <- NULL
  PREDS_cv <- NULL
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)

  Call <- glmfit$call

  for (i in 1:K){
    Call$data <- filter(temp, gr != i)
    d_glm <- eval.parent(Call)

    temp_fit <- predict(d_glm, filter(temp, gr == i), type = "response")
    y_fit <- temp_fit / filter(temp, gr == i)$census_1982
    y_dat <- filter(temp, gr == i)$census_2010 / filter(temp, gr == i)$census_1982

    SS <- (y_dat - mean(y_dat))^2 %>% mean

    PREDS <- (y_fit - y_dat)^2 %>% mean
    res_cv[i] <- 1 - PREDS/SS
    SS_cv[i] <- SS
    PREDS_cv[i] <- PREDS

  }
  data.frame(res_cv, SS_cv, PREDS_cv)
}

moge <- my_cv2(ab_t_data2, glmfit, K = 10)


ab_t_data3 <- ab_t_data2
ab_t_data3$census_1982 <- ab_t_data3$census_1982  + 1
ab_t_data3$census_2010 <- ab_t_data3$census_2010  + 1

res_add <- glm.nb(census_2010 ~WSG
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
            data = ab_t_data3)

temp <- stepAIC(res_add)

moge2 <-  my_cv2(ab_t_data3, temp, K = 10)

# > summary(moge2)
#      res_cv             SS_cv            PREDS_cv
#  Min.   :-0.96010   Min.   :0.09217   Min.   :0.1807
#  1st Qu.:-0.11754   1st Qu.:0.23952   1st Qu.:0.2457
#  Median : 0.02325   Median :0.37017   Median :0.3362
#  Mean   :-0.08535   Mean   :0.42575   Mean   :0.4372
#  3rd Qu.: 0.09662   3rd Qu.:0.54335   3rd Qu.:0.6119
#  Max.   : 0.16023   Max.   :0.87887   Max.   :0.9702
# > summary(moge)
#      res_cv              SS_cv           PREDS_cv
#  Min.   :-0.555486   Min.   :0.1026   Min.   :0.1038
#  1st Qu.:-0.157496   1st Qu.:0.2438   1st Qu.:0.2426
#  Median :-0.084384   Median :0.3704   Median :0.4304
#  Mean   :-0.097015   Mean   :0.4449   Mean   :0.4712
#  3rd Qu.: 0.007051   3rd Qu.:0.5927   3rd Qu.:0.6791
#  Max.   : 0.152308   Max.   :0.9657   Max.   :0.9893

p20 <- read.csv("~/Dropbox/MS/TurnoverBCI/plot20m.csv")
p100 <- read.csv("~/Dropbox/MS/TurnoverBCI/plot100m.csv")



xx <- rnorm(100)
yy <- rnorm(100, 2 * xx - 1)
zz <- rnorm(100, 2 * xx + 3 * xx^2 -2*xx^3 - 1) + rnorm(100, 0, 3)

moge <- data.frame(yy, xx, zz)

plot(zz ~ xx)
glmfit <- lm(zz~xx+I(xx^2) +I(xx^3) + I(xx^4) + I(xx^5) + I(xx^6) + I(xx^7) + I(xx^8) + I(xx^9), moge)

glmfit <- lm(zz~xx+I(xx^2) +I(xx^3), moge)

my_cv3 <- function(data, glmfit, K){
  res_cv <- NULL
  SS_cv <- NULL
  PREDS_cv <- NULL
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)

  Call <- glmfit$call

  for (i in 1:K){
    Call$data <- filter(temp, gr != i)
    d_glm <- eval.parent(Call)

    temp_fit <- predict(d_glm, filter(temp, gr == i), type = "response")
    y_fit <- temp_fit
    y_dat <- filter(temp, gr == i)$zz

    SS <- (y_dat - mean(y_dat))^2 %>% mean

    PREDS <- (y_fit - y_dat)^2 %>% mean
    res_cv[i] <- 1 - PREDS/SS
    SS_cv[i] <- SS
    PREDS_cv[i] <- PREDS

  }
  data.frame(res_cv, SS_cv, PREDS_cv)
}

summary(glmfit)
my_cv3(moge, glmfit, K = 10) %>% apply(., 2, mean)
