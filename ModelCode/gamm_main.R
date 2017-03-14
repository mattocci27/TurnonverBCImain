###
### analyses for community-level changes (GAMM)
###

rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("~/Dropbox/MS/TurnoverBCI/temp.RData")

load("BCI_turnover20141213.RData")


library(mgcv)
library(lme4)
library(dplyr)

Time <- rep(c(1982,1985,1990,1995,2000,2005,2010), each = 50)
##
## Feeley's suggetions: community means of 1982 are standraized to 0.
##

# data for 1ha scale
moge <- data.frame(WSG = unlist(WSG100),
                   Moist = unlist(Moist100),
                   slope = unlist(slope100),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time,
                   Time2 = rep(1:7, each = 50)) %>%
  mutate(WSG = WSG - WSG100[[1]]) %>% # for stadrazing the initial values
  mutate(Moist = Moist - Moist100[[1]]) %>%
  mutate(convex = convex - convex100[[1]]) %>%
  mutate(slope = slope - slope100[[1]]) %>%
  mutate(concav = -convex)

# data for 0.04ha scale
moge20 <- data.frame(WSG = unlist(WSG20),
                   Moist = unlist(Moist20),
                   slope = unlist(slope20),
                   convex = unlist(convex20),
                   site = as.factor(rep(1:1250,7)),
                   Time,
                   Time2 = rep(1:7, each = 50)) %>%
  mutate(WSG = WSG - WSG20[[1]]) %>%
  mutate(Moist = Moist - Moist20[[1]]) %>%
  mutate(convex = convex - convex20[[1]]) %>%
  mutate(slope = slope - slope20[[1]]) %>%
  mutate(concav = -convex)



##
## check for temporal autocorrelation and heteroscedasicity
##

varf <- moge %>%
  group_by(Time) %>%
  summarize(varf = var(WSG))

moge2 <- moge %>%
  mutate(varf = rep(varf$varf, each = 50)) %>%
  mutate(varf = ifelse(varf == 0, 0, varf))


m1 <- gamm(WSG ~ s(Time, k=4), random = list(site=~1),
           data = moge2 %>% filter(varf >0),
           weights = varFixed(~ varf),
           correlation = corCAR1(form=~Time2))


m2 <- gamm(WSG ~ s(Time, k=4), random = list(site=~1),
           data = moge2 %>% filter(varf >0),
           correlation = corCAR1(form=~Time2))

m3 <- gamm(WSG ~ s(Time, k=4), random = list(site=~1),
           data = moge,
           weights = varFixed(~ Time2),
           correlation = corCAR1(form=~Time2))

m4 <- gamm(WSG ~ s(Time, k=4), random = list(site=~1),
           data = moge,
           correlation = corCAR1(form=~Time2))

#par(mfrow=c(2,2))
plot(m1$lme)
plot(m2$lme)
plot(m3$lme)
plot(m4$lme)
#par(mfrow=c(1,1))

#summary(m1$gam)
par(mfrow=c(2,2))
plot(m1$gam, residuals = T)
plot(m2$gam, residuals = T)
plot(m3$gam, residuals = T)
plot(m4$gam, residuals = T)
par(mfrow=c(1,1))

# m3 is the most apporpiate model

####


# function for cross_validation(1ha) ============================
gamm_cv <- function(var_name, data = moge, k = 10){
  res.cv <- NULL
  SS <- NULL
  PREDS <- NULL

  cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
   arrange(temp)
  form <- paste(var_name, "~ s(Time, k=4)") %>% noquote %>% as.formula

  i <- 1
  temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
  r2.1 <- gamm(form, random = list(site=~1),
               data = temp_train1,
               weights = varFixed(~ Time2),
               correlation = corCAR1(form=~Time2))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time = r2.1$gam$model$Time)

  temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])

  res2 <- unique(res[order(res$Time),])
  res3 <- merge(temp_pre, res2, by = "Time")

  SS[i] <- sum((res3[,var_name] - mean(res3[,var_name]))^2)
  PREDS[i] <- sum((res3$fitted - res3[,var_name])^2)


  for (i in 2:(k-1)){
    temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
    temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

    temp.data <- bind_rows(temp_train1, temp_train2)
    r2.1 <- gamm(form, random = list(site=~1),
                 weights = varFixed(~ Time2),
                 data = temp.data, correlation = corCAR1(form = ~Time2))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])

    temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3[,var_name] - mean(res3[,var_name]))^2)
    PREDS[i] <- sum((res3$fitted - res3[,var_name])^2)
  }


  i <- 10
  temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
  temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])

  r2.1 <- gamm(form, random = list(site=~1),
               weights = varFixed(~ Time2),
               data=temp_train1, correlation = corCAR1(form=~Time2))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])
  res3 <- merge(temp_pre, res2, by = "Time")
  SS[i] <- sum((res3[,var_name] - mean(res3[,var_name]))^2)
  PREDS[i] <- sum((res3$fitted - res3[,var_name])^2)

  1 - mean(PREDS, na.rm=T)/mean(SS,na.rm=T)
}


# result
set.seed(3)
gamm_cv("WSG", data = moge)
gamm_cv("Moist", data = moge)
gamm_cv("slope", data = moge)
gamm_cv("concav", data = moge)


# cross_validaiton (0.04ha) =================================================================
gamm_cv20 <- function(var_name, data = moge20, k = 10){
  res.cv <- NULL
  SS <- NULL
  PREDS <- NULL

  cross_site <- data.frame(site = 1:1250, temp = rnorm(1250)) %>%
   arrange(temp)
  form <- paste(var_name, "~ s(Time, k=4)") %>% noquote %>% as.formula

  i <- 1
  temp_train1 <- data %>% filter(site %in% cross_site[126:1250, "site"])
  r2.1 <- gamm(form, random = list(site=~1),
               data = temp_train1,
               weights = varFixed(~ Time2),
               correlation = corCAR1(form=~Time2))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time = r2.1$gam$model$Time)

  temp_pre <- data %>% filter(site %in% cross_site[1:125, "site"])

  res2 <- unique(res[order(res$Time),])
  res3 <- merge(temp_pre, res2, by = "Time")

  SS[i] <- sum((res3[,var_name] - mean(res3[,var_name]))^2)
  PREDS[i] <- sum((res3$fitted - res3[,var_name])^2)


  for (i in 2:(k-1)){
    temp_train1 <- data %>% filter(site %in% cross_site[1:(125*(i-1)), "site"])
    temp_train2 <- data %>% filter(site %in% cross_site[(125*i +1):1250, "site"])

    temp.data <- bind_rows(temp_train1, temp_train2)
    r2.1 <- gamm(form, random = list(site=~1),
                 weights = varFixed(~ Time2),
                 data = temp.data, correlation = corCAR1(form = ~Time2))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])

    temp_pre <- data %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3[,var_name] - mean(res3[,var_name]))^2)
    PREDS[i] <- sum((res3$fitted - res3[,var_name])^2)
  }


  i <- 10
  temp_train1 <- data %>% filter(site %in% cross_site[1:1125, "site"])
  temp_pre <- data %>% filter(site %in% cross_site[1126:1250, "site"])

  r2.1 <- gamm(form, random = list(site=~1),
               weights = varFixed(~ Time2),
               data=temp_train1, correlation = corCAR1(form=~Time2))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])
  res3 <- merge(temp_pre, res2, by = "Time")
  SS[i] <- sum((res3[,var_name] - mean(res3[,var_name]))^2)
  PREDS[i] <- sum((res3$fitted - res3[,var_name])^2)

  1 - mean(PREDS, na.rm=T)/mean(SS,na.rm=T)
}



m1 <- gamm(WSG ~ s(Time, k=4), random = list(site=~1),
               weights = varFixed(~ Time2),
               data=moge20, correlation = corCAR1(form=~Time2))

m2 <- gamm(Moist ~ s(Time, k=4), random = list(site=~1),
               weights = varFixed(~ Time2),
               data=moge20, correlation = corCAR1(form=~Time2))

m3 <- gamm(concav ~ s(Time, k=4), random = list(site=~1),
               weights = varFixed(~ Time2),
               data=moge20, correlation = corCAR1(form=~Time2))

m4 <- gamm(slope ~ s(Time, k=4), random = list(site=~1),
               weights = varFixed(~ Time2),
               data=moge20, correlation = corCAR1(form=~Time2))

summary(m1$gam)
summary(m2$gam)
summary(m3$gam) # p < 0.05
summary(m4$gam) # p < 0.05


# this takes time (2~3mins)
gamm_cv20("slope", data = moge20)

gamm_cv20("convex", data = moge20)


