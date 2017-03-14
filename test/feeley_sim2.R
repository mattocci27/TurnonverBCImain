##############################################################
#Feeley's R2
##############################################################
#use Feely RData
#wood density = 0.4
#slope = 0.02
#convex = 0.09
#moist =
#WSG_rm = 0.4399393
# moge %>% filter(site %in% cross_site[1:5, "site"])
# 1:5 vs 6:50
# 6:10 vs 1:5, 11:50
rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("~/Dropbox/MS/TurnoverBCI/temp.RData")

load("Ken.RData")

library(dplyr)
library(mgcv)
library(lme4)
library(ggplot2)

before <- proc.time()
k <- 10
# set.seed(615)
set.seed(1228)
# set.seed(5)
# set.seed(128)
# set.seed(129)
# set.seed(130)
# set.seed(131)
for (l in 1:10){
  # set.seed(l)
  cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
    arrange(temp)

  par(mfrow = c(2, 5))
  # res.cv <- numeric(k)
    res.cv <- NULL
    SS <- NULL
    PREDS <- NULL
    i <- 1
    temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
    r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data = temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])

    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")

    SS[i] <- sum((res3$WSG - mean(res3$WSG))^2)
    PREDS[i] <- sum((res3$fitted - res3$WSG)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)

    # r2.1$lme$residuals$site
    site_vec <- res3$site %>% unique
    plot(WSG ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(WSG ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)

  for (i in 2:(k-1)){
    temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
    temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

    temp.data <- bind_rows(temp_train1, temp_train2)
    r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data = temp.data, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])

    temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$WSG - mean(res3$WSG))^2)
    PREDS[i] <- sum((res3$fitted - res3$WSG)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
    site_vec <- res3$site %>% unique
    plot(WSG ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(WSG ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)
  }


  i <- 10
  temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
  temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])

    r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$WSG - mean(res3$WSG))^2)
    PREDS[i] <- sum((res3$fitted - res3$WSG)^2)
    # res.cv[i] <- 1 - mean(PREDS)/(SS)
    site_vec <- res3$site %>% unique
    plot(WSG ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(WSG ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)


    r2_ci2 <- NULL
    for (i in 1:1000){
      temp <- data_frame(PREDS, SS) %>%
        sample_n(10, replace = T)

      r2_ci2[i] <- 1 - mean(temp$PREDS) / mean(temp$SS)
      # r2_ci2[i] <-   1  - mean(sample(PREDS/SS, 10, replace = T))
    }


  my_ci <- function(r) {
     c(mean = mean(r), lower = quantile(r, 0.05), upper = quantile(r, 0.95)) }

  WSG_CI <- my_ci(r2_ci2)


  par(mfrow = c(2, 5))
  # res.cv <- numeric(k)
    res.cv <- NULL
    SS <- NULL
    PREDS <- NULL
    i <- 1
    temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
    r2.1 <- gamm(WSG_rm ~  s(Time,k=4), random = list(site=~1), data = temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])

    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")

    SS[i] <- sum((res3$WSG_rm - mean(res3$WSG_rm))^2)
    PREDS[i] <- sum((res3$fitted - res3$WSG_rm)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)

    # r2.1$lme$residuals$site
    site_vec <- res3$site %>% unique
    plot(WSG_rm ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(WSG_rm ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)

  for (i in 2:(k-1)){
    temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
    temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

    temp.data <- bind_rows(temp_train1, temp_train2)
    r2.1 <- gamm(WSG_rm ~  s(Time,k=4), random = list(site=~1), data = temp.data, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])

    temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$WSG_rm - mean(res3$WSG_rm))^2)
    PREDS[i] <- sum((res3$fitted - res3$WSG_rm)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
    site_vec <- res3$site %>% unique
    plot(WSG_rm ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(WSG_rm ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)
  }


  i <- 10
  temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
  temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])

    r2.1 <- gamm(WSG_rm ~  s(Time,k=4), random = list(site=~1), data=temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$WSG_rm - mean(res3$WSG_rm))^2)
    PREDS[i] <- sum((res3$fitted - res3$WSG_rm)^2)
    # res.cv[i] <- 1 - mean(PREDS)/(SS)
    site_vec <- res3$site %>% unique
    plot(WSG_rm ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(WSG_rm ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)


    r2_ci2 <- NULL
    for (i in 1:1000){
      temp <- data_frame(PREDS, SS) %>%
        sample_n(10, replace = T)

      r2_ci2[i] <- 1 - mean(temp$PREDS) / mean(temp$SS)
      # r2_ci2[i] <-   1  - mean(sample(PREDS/SS, 10, replace = T))
    }
  my_ci <- function(r) {
     c(mean = mean(r), lower = quantile(r, 0.05), upper = quantile(r, 0.95)) }

  WSG_rm_CI <- my_ci(r2_ci2)


  par(mfrow = c(2, 5))
  # res.cv <- numeric(k)
    res.cv <- NULL
    SS <- NULL
    PREDS <- NULL
    i <- 1
    temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
    r2.1 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data = temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])

    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")

    SS[i] <- sum((res3$Moist - mean(res3$Moist))^2)
    PREDS[i] <- sum((res3$fitted - res3$Moist)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)

    # r2.1$lme$residuals$site
    site_vec <- res3$site %>% unique
    plot(Moist ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(Moist ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)

  for (i in 2:(k-1)){
    temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
    temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

    temp.data <- bind_rows(temp_train1, temp_train2)
    r2.1 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data = temp.data, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])

    temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$Moist - mean(res3$Moist))^2)
    PREDS[i] <- sum((res3$fitted - res3$Moist)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
    site_vec <- res3$site %>% unique
    plot(Moist ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(Moist ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)
  }


  i <- 10
  temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
  temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])

    r2.1 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$Moist - mean(res3$Moist))^2)
    PREDS[i] <- sum((res3$fitted - res3$Moist)^2)
    # res.cv[i] <- 1 - mean(PREDS)/(SS)
    site_vec <- res3$site %>% unique
    plot(Moist ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(Moist ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)


    r2_ci2 <- NULL
    for (i in 1:1000){
      temp <- data_frame(PREDS, SS) %>%
        sample_n(10, replace = T)

      r2_ci2[i] <- 1 - mean(temp$PREDS) / mean(temp$SS)
      # r2_ci2[i] <-   1  - mean(sample(PREDS/SS, 10, replace = T))
    }
  my_ci <- function(r) {
     c(mean = mean(r), lower = quantile(r, 0.05), upper = quantile(r, 0.95)) }

  Moist_CI <- my_ci(r2_ci2)


  par(mfrow = c(2, 5))
  # res.cv <- numeric(k)
    res.cv <- NULL
    SS <- NULL
    PREDS <- NULL
    i <- 1
    temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
    r2.1 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data = temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])

    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")

    SS[i] <- sum((res3$slope - mean(res3$slope))^2)
    PREDS[i] <- sum((res3$fitted - res3$slope)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)

    # r2.1$lme$residuals$site
    site_vec <- res3$site %>% unique
    plot(slope ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(slope ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)

  for (i in 2:(k-1)){
    temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
    temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

    temp.data <- bind_rows(temp_train1, temp_train2)
    r2.1 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data = temp.data, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])

    temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$slope - mean(res3$slope))^2)
    PREDS[i] <- sum((res3$fitted - res3$slope)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
    site_vec <- res3$site %>% unique
    plot(slope ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(slope ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)
  }


  i <- 10
  temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
  temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])

    r2.1 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$slope - mean(res3$slope))^2)
    PREDS[i] <- sum((res3$fitted - res3$slope)^2)
    # res.cv[i] <- 1 - mean(PREDS)/(SS)
    site_vec <- res3$site %>% unique
    plot(slope ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(slope ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)


    r2_ci2 <- NULL
    for (i in 1:1000){
      temp <- data_frame(PREDS, SS) %>%
        sample_n(10, replace = T)

      r2_ci2[i] <- 1 - mean(temp$PREDS) / mean(temp$SS)
      # r2_ci2[i] <-   1  - mean(sample(PREDS/SS, 10, replace = T))
    }

  my_ci <- function(r) {
     c(mean = mean(r), lower = quantile(r, 0.05), upper = quantile(r, 0.95)) }

  slope_CI <- my_ci(r2_ci2)



  par(mfrow = c(2, 5))
  # res.cv <- numeric(k)
    res.cv <- NULL
    SS <- NULL
    PREDS <- NULL
    i <- 1
    temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
    r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])

    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")

    SS[i] <- sum((res3$convex - mean(res3$convex))^2)
    PREDS[i] <- sum((res3$fitted - res3$convex)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)

    # r2.1$lme$residuals$site
    site_vec <- res3$site %>% unique
    plot(convex ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(convex ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)

  for (i in 2:(k-1)){
    temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
    temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

    temp.data <- bind_rows(temp_train1, temp_train2)
    r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = temp.data, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])

    temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$convex - mean(res3$convex))^2)
    PREDS[i] <- sum((res3$fitted - res3$convex)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
    site_vec <- res3$site %>% unique
    plot(convex ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(convex ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)
  }


  i <- 10
  temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
  temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])

    r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$convex - mean(res3$convex))^2)
    PREDS[i] <- sum((res3$fitted - res3$convex)^2)
    # res.cv[i] <- 1 - mean(PREDS)/(SS)
    site_vec <- res3$site %>% unique
    plot(convex ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(convex ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)


    r2_ci2 <- NULL
    for (i in 1:1000){
      temp <- data_frame(PREDS, SS) %>%
        sample_n(10, replace = T)

      r2_ci2[i] <- 1 - mean(temp$PREDS) / mean(temp$SS)
      # r2_ci2[i] <-   1  - mean(sample(PREDS/SS, 10, replace = T))
    }


  my_ci <- function(r) {
     c(mean = mean(r), lower = quantile(r, 0.05), upper = quantile(r, 0.95)) }

  convex_CI <- my_ci(r2_ci2)

  after <- proc.time()
  after - before
  print(l)
  print(WSG_CI)
  print(WSG_rm_CI)
  print(Moist_CI)
  print(convex_CI)
  print(slope_CI)
}


WSG_CI
WSG_rm_CI
Moist_CI
convex_CI
slope_CI

#
# Moist_CI
#          mean      lower.5%     upper.95%
# -1.123202e-01 -2.406394e-01  4.278172e-06
# > convex_CI
#        mean    lower.5%   upper.95%
# -0.02704076 -0.22901255  0.13693806
# > slope_CI
#        mean    lower.5%   upper.95%
#  0.04197939 -0.05826612  0.14817259
# >
#
# ###
# PREDS2 <- NULL
# SS2 <- NULL
# before <- proc.time()
# for (l in 1:10){
#   cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
#     arrange(temp)
#
#   # res.cv <- numeric(k)
#     res.cv <- NULL
#     SS <- NULL
#     PREDS <- NULL
#     i <- 1
#     temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
#     r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = temp_train1, correlation = corCAR1(form=~Time))
#     res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
#
#     temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])
#
#     res2 <- unique(res[order(res$Time),])
#     res3 <- merge(temp_pre, res2, by = "Time")
#
#     SS[i] <- sum((res3$convex - mean(res3$convex))^2)
#     PREDS[i] <- sum((res3$fitted - res3$convex)^2)
#     # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
#
#     # r2.1$lme$residuals$site
#     site_vec <- res3$site %>% unique
#
#   for (i in 2:(k-1)){
#     temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
#     temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])
#
#     temp.data <- bind_rows(temp_train1, temp_train2)
#     r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = temp.data, correlation = corCAR1(form=~Time))
#     res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
#     res2 <- unique(res[order(res$Time),])
#
#     temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])
#
#     res3 <- merge(temp_pre, res2, by = "Time")
#     SS[i] <- sum((res3$convex - mean(res3$convex))^2)
#     PREDS[i] <- sum((res3$fitted - res3$convex)^2)
#     # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
#     site_vec <- res3$site %>% unique}
#
#
#   i <- 10
#   temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
#   temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])
#
#     r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=temp_train1, correlation = corCAR1(form=~Time))
#     res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
#     res2 <- unique(res[order(res$Time),])
#     res3 <- merge(temp_pre, res2, by = "Time")
#     SS[i] <- sum((res3$convex - mean(res3$convex))^2)
#     PREDS[i] <- sum((res3$fitted - res3$convex)^2)
#     # res.cv[i] <- 1 - mean(PREDS)/(SS)
#     site_vec <- res3$site %>% unique
#
#     SS2 <- c(SS2, SS)
#     PREDS2 <- c(PREDS2, PREDS)
# }
#
# after <- proc.time()
# after - before
#
#
#
# r2_ci2 <- NULL
# for (i in 1:1000){
#   r2_ci2[i] <-   1  - mean(sample(PREDS2/SS2, 10, replace = T))
# }
#
# my_ci <- function(r) {
#    c(mean = mean(r), lower = quantile(r, 0.05), upper = quantile(r, 0.95)) }
#
# convex_CI <- my_ci(r2_ci2)




moge <- moge %>% filter(Time != 2010)
cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
  arrange(temp)
par(mfrow = c(2, 5))
# res.cv <- numeric(k)
  res.cv <- NULL
  SS <- NULL
  PREDS <- NULL
  i <- 1
  temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
  r2.1 <- gamm(WSG_rm ~  s(Time,k=4), random = list(site=~1), data = temp_train1, correlation = corCAR1(form=~Time))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

  temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])

  res2 <- unique(res[order(res$Time),])
  res3 <- merge(temp_pre, res2, by = "Time")

  SS[i] <- sum((res3$WSG_rm - mean(res3$WSG_rm))^2)
  PREDS[i] <- sum((res3$fitted - res3$WSG_rm)^2)
  # res.cv[i] <- 1 - mean(PREDS)/mean(SS)

  # r2.1$lme$residuals$site
  site_vec <- res3$site %>% unique
  plot(WSG_rm ~ Time, res3, main = paste("subset", i), type = "n")
  for (j in 1:7) points(WSG_rm ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
  points(fitted ~ Time, res3, pch = 16)

for (i in 2:(k-1)){
  temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
  temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

  temp.data <- bind_rows(temp_train1, temp_train2)
  r2.1 <- gamm(WSG_rm ~  s(Time,k=4), random = list(site=~1), data = temp.data, correlation = corCAR1(form=~Time))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])

  temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

  res3 <- merge(temp_pre, res2, by = "Time")
  SS[i] <- sum((res3$WSG_rm - mean(res3$WSG_rm))^2)
  PREDS[i] <- sum((res3$fitted - res3$WSG_rm)^2)
  # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
  site_vec <- res3$site %>% unique
  plot(WSG_rm ~ Time, res3, main = paste("subset", i), type = "n")
  for (j in 1:7) points(WSG_rm ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
  points(fitted ~ Time, res3, pch = 16)
}



  i <- 10
  temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
  temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])

    r2.1 <- gamm(WSG_rm ~  s(Time,k=4), random = list(site=~1), data=temp_train1, correlation = corCAR1(form=~Time))
    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
    res2 <- unique(res[order(res$Time),])
    res3 <- merge(temp_pre, res2, by = "Time")
    SS[i] <- sum((res3$WSG_rm - mean(res3$WSG_rm))^2)
    PREDS[i] <- sum((res3$fitted - res3$WSG_rm)^2)
    # res.cv[i] <- 1 - mean(PREDS)/(SS)
    site_vec <- res3$site %>% unique
    plot(WSG_rm ~ Time, res3, main = paste("subset", i), type = "n")
    for (j in 1:7) points(WSG_rm ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
    points(fitted ~ Time, res3, pch = 16)


    r2_ci2 <- NULL
    for (i in 1:1000){
      temp <- data_frame(PREDS, SS) %>%
        sample_n(10, replace = T)

      r2_ci2[i] <- 1 - mean(temp$PREDS) / mean(temp$SS)
      # r2_ci2[i] <-   1  - mean(sample(PREDS/SS, 10, replace = T))
    }
  my_ci <- function(r) {
     c(mean = mean(r), lower = quantile(r, 0.05), upper = quantile(r, 0.95)) }

  WSG_rm_CI <- my_ci(r2_ci2)
WSG_rm_CI
