rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("~/Dropbox/MS/TurnoverBCI/temp.RData")

load("BCI_turnover20141213.RData")


library(mgcv)
library(lme4)
library(dplyr)
unlist(WSG100)
unlist(Moist100)
unlist(slope.100.rare[10])
unlist(convex.100.rare[10])

time <- c(1982,1985,1990,1995,2000,2005,2010)
  Time=c(rep(1982,50),
         rep(1985,50),
         rep(1990,50),
         rep(1995,50),
         rep(2000,50),
         rep(2005,50),
         rep(2010,50))
#

WSG100.rm <-list()
Moist100.rm <-list()
slope100.rm <-list()
convex100.rm <-list()

# D100m[[1]][1:5,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))]


###use convex mean
for (i in 1:7){
  WSG100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))],trait,"WSG")
  Moist100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO"))],trait,"Moist")
  slope100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("FARAOC","POULAR"))],trait,"sp.slope.mean")
  # convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("ALSEBL","SWARS1"))],trait,"sp.convex.mean")
  convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("SWARS1", "ALSEBL", "FARAOC"))],trait,"sp.convex.mean")
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

##############################################################
#Feeley's R2
##############################################################
moge <- data.frame(WSG = unlist(WSG100),
                   Moist = unlist(Moist100),
                   slope = unlist(slope100),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time,
                   Time2 = rep(1:7, each = 50))
moge <- moge %>%
  mutate(WSG = WSG - WSG100[[1]]) %>%
  mutate(Moist = Moist - Moist100[[1]]) %>%
  mutate(convex = convex - convex100[[1]]) %>%
  mutate(slope = slope - slope100[[1]]) %>%
  mutate(concav = -convex)


moge20 <- moge20 %>%
  mutate(WSG = WSG - WSG20[[1]]) %>%
  mutate(Moist = Moist - Moist20[[1]]) %>%
  mutate(convex = convex - convex20[[1]]) %>%
  mutate(slope = slope - slope20[[1]]) %>%
  mutate(concav = -convex)

#m1 <- gamm(WSG ~ s(Time, k=4), random = list(site=~1),
#           data = moge20,
#          # weights = varFixed(~ Time),
#           correlation = corCAR1(form=~Time2))


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

par(mfrow=c(1,2))
plot(m1$lme)
plot(m2$lme)
par(mfrow=c(1,1))

m3 <- gamm(WSG ~ s(Time, k=4), random = list(site=~1),
           data = moge,
           weights = varFixed(~ Time),
           correlation = corCAR1(form=~Time2))

m4 <- gamm(WSG ~ s(Time, k=4), random = list(site=~1),
           data = moge,
           correlation = corCAR1(form=~Time2))


#summary(m1$gam)
par(mfrow=c(2,2))
plot(m1$gam, residuals = T)
plot(m2$gam, residuals = T)
plot(m3$gam, residuals = T)
plot(m4$gam, residuals = T)
par(mfrow=c(1,1))

moge3 <- moge2 %>% filter(varf >0)

plot(residuals(m1$lme) ~ moge3$Time)



tapply(moge2$WSG, moge2$Time, mean)


plot(temp ~ Time, moge2)


# function
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
               weights = varFixed(~ Time),
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
                 weights = varFixed(~ Time),
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
               weights = varFixed(~ Time),
               data=temp_train1, correlation = corCAR1(form=~Time2))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])
  res3 <- merge(temp_pre, res2, by = "Time")
  SS[i] <- sum((res3[,var_name] - mean(res3[,var_name]))^2)
  PREDS[i] <- sum((res3$fitted - res3[,var_name])^2)

  1 - mean(PREDS, na.rm=T)/mean(SS,na.rm=T)
}


set.seed(3)
gamm_cv("WSG", data = moge)
gamm_cv("Moist", data = moge)
gamm_cv("slope", data = moge)
gamm_cv("concav", data = moge)

moge2 <- moge
moge2$concav <- abs(moge$concav)

for (i in 1:10) gamm_cv("concav", data = moge2) %>% print

m1 <- gamm(concav ~ s(Time, k=4), random = list(site=~1),
               weights = varFixed(~ Time),
               data=moge2, correlation = corCAR1(form=~Time2))


#wood density = 0.4
#slope = 0.02
#convex = 0.09
#moist =
#WSG_rm = 0.4399393
# moge %>% filter(site %in% cross_site[1:5, "site"])
# 1:5 vs 6:50
# 6:10 vs 1:5, 11:50

set.seed(5)
k <- 10

par(mfrow = c(2, 5))
# res.cv <- numeric(k)
  res.cv <- NULL
  SS <- NULL
  PREDS <- NULL
  i <- 1
  temp_train1 <- moge %>% filter(site %in% cross_site[6:50, "site"])
  r2.1 <- gamm(concav ~  s(Time,k=4), random = list(site=~1),
               data = temp_train1,
               weights = varFixed(~ Time),
               correlation = corCAR1(form=~Time2))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time = r2.1$gam$model$Time)

  temp_pre <- moge %>% filter(site %in% cross_site[1:5, "site"])

  res2 <- unique(res[order(res$Time),])
  res3 <- merge(temp_pre, res2, by = "Time")

  SS[i] <- sum((res3$concav - mean(res3$concav))^2)
  PREDS[i] <- sum((res3$fitted - res3$concav)^2)
  # res.cv[i] <- 1 - mean(PREDS)/mean(SS)

  # r2.1$lme$residuals$site
  site_vec <- res3$site %>% unique
  #plot(concav ~ Time, res3, main = paste("subset", i), ylab = "concav", type = "n")
  #for (j in 1:7) points(concav ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
  #points(fitted ~ Time, res3, pch = 16)

for (i in 2:(k-1)){
  temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
  temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

  temp.data <- bind_rows(temp_train1, temp_train2)
  r2.1 <- gamm(concav ~  s(Time,k=4), random = list(site=~1),
               weights = varFixed(~ Time),
               data = temp.data, correlation = corCAR1(form = ~Time2))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])

  temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])

  res3 <- merge(temp_pre, res2, by = "Time")
  SS[i] <- sum((res3$concav - mean(res3$concav))^2)
  PREDS[i] <- sum((res3$fitted - res3$concav)^2)
  # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
  site_vec <- res3$site %>% unique
  #plot(concav ~ Time, res3, main = paste("subset", i), ylab = "concav", type = "n")
  #for (j in 1:7) points(concav ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
  #points(fitted ~ Time, res3, pch = 16)
}


i <- 10
temp_train1 <- moge %>% filter(site %in% cross_site[1:45, "site"])
temp_pre <- moge %>% filter(site %in% cross_site[46:50, "site"])

  r2.1 <- gamm(concav ~  s(Time,k=4), random = list(site=~1),
               weights = varFixed(~ Time),
               data=temp_train1, correlation = corCAR1(form=~Time2))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])
  res3 <- merge(temp_pre, res2, by = "Time")
  SS[i] <- sum((res3$concav - mean(res3$concav))^2)
  PREDS[i] <- sum((res3$fitted - res3$concav)^2)
  # res.cv[i] <- 1 - mean(PREDS)/(SS)
  site_vec <- res3$site %>% unique
  #plot(concav ~ Time, res3, main = paste("subset", i), ylab = "slope", type = "n")

  #for (j in 1:7) points(slope ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
  #points(fitted ~ Time, res3, pch = 16)


1 - mean(PREDS, na.rm=T)/mean(SS,na.rm=T)
########################################

m1 <- gamm(slope ~  s(Time,k=4), random = list(site=~1),
               data=moge, correlation = corCAR1(form=~Time2))





#check HYBAPR
trait %>% filter(SP=="HYBAPR") %>% select(sp.slope.mean, sp.convex.mean)
trait %>% filter(SP=="FARAOC") %>% select(sp.slope.mean, sp.convex.mean)
c(4.7177, trait$sp.slope.mean) %>% rank
c(4.027, trait$sp.slope.mean) %>% rank


c(0.00418, -trait$sp.convex.mean) %>% rank
c(0.059, -trait$sp.convex.mean) %>% rank



##############################################################
#original R2
##############################################################
moge$temp <- rnorm(nrow(moge))
moge100r <- moge100r %>% mutate(temp = rnorm(nrow(.)))

moge.cross <- moge100r %>% arrange(temp)

rownames(moge.cross) <- NULL
k <- 10
par(mfrow = c(2,5))
# res.cv <- numeric(k)
  res.cv <- NULL
  SS <- NULL
  PREDS <- NULL
  i <- 1
  r2.1 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge.cross[(35*i+1):350,], correlation = corCAR1(form=~Time))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

  # res <- data.frame(fitted = r2.1$gam$fitted.values,
                    # res = r2.1$gam$residuals,
                    # res2 = r2.1$gam$y - r2.1$gam$fitted.values -r2.1$gam$residuals,
                    # Time= r2.1$gam$model$Time)


  res2 <- unique(res[order(res$Time),])
  res3 <- merge(moge.cross[1:35,],res2,by="Time")

  SS[i] <- sum((moge.cross[1:35,"Moist"] - mean(moge.cross[1:35,"WSG"]))^2)
  PREDS[i] <- sum((res3$fitted - res3$WSG)^2)
  # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
  plot(WSG ~ Time, res3, main = paste("subset", i), ylab = "Wood density")
  points(fitted ~ Time, res3, pch = 16)
  # r2.1$lme$residuals$site

for (i in 2:(k-1)){
  temp.data <- rbind(moge.cross[1:(35*(i-1)),],
                     moge.cross[(35*i+1):350,])
  r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=temp.data, correlation = corCAR1(form=~Time))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])

  K1 <- 35*(i-1) +1
  K2 <- 35*i
  res3 <- merge(moge.cross[K1:K2,],res2,by="Time")
  SS[i] <- sum((moge.cross[K1:K2,"WSG"] - mean(moge.cross[K1:K2,"WSG"]))^2)
  PREDS[i] <- sum((res3$fitted - res3$WSG)^2)
  plot(WSG ~ Time, res3, main = paste("subset", i), ylab = "Wood density")
  points(fitted ~ Time, res3, pch = 16)
  # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
}

i <- 10
  r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge.cross[1:35*k,], correlation = corCAR1(form=~Time))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])
  res3 <- merge(moge.cross[316:350,],res2,by="Time")
  SS[i] <- sum((moge.cross[316:350,"WSG"] - mean(moge.cross[316:350,"WSG"]))^2)
  PREDS[i] <- sum((res3$fitted - res3$WSG)^2)
  # res.cv[i] <- 1 - mean(PREDS)/(SS)
  plot(WSG ~ Time, res3, main = paste("subset", i), ylab = "Wood density")
  points(fitted ~ Time, res3, pch = 16)
1 - mean(PREDS,na.rm=T)/mean(SS,na.rm=T)
########################################
#convex
# [1] -0.0003082712

#Moist
#[1] -0.01544178

#slope
#[1] -0.02532901

#WSG

set.seed(0)
dat <- gamSim(1,n=200,scale=2)

f

CV <- mean(res.cv,na.rm=T)
R2 <- 1 - mean(res.cv,na.rm=T)/mean((r2.1$gam$y - mean(r2.1$gam$y))^2)

data.frame(CV,R2)

res <- gamm(WSG ~  s(Time, k=4), random = list(site=~1), data=moge, correlation = corCAR1())
res2 <- res$gam$residuals
fit <- res$gam$fitted.values
sp_ef <- rep(ranef(res$lme)$site %>% unlist, 7)

data_frame(res = res2, fit = fit) %>%
  bind_cols(., moge) %>%
  mutate(res2 = WSG - fit) %>%
  mutate(fit2 = WSG - res) %>%
  mutate(WSG2 = fit + res) %>%
  # filter(site == 1)  %>%
  mutate(sp_ef = sp_ef) %>%
  mutate(fit3 = fit + sp_ef)

gamm(Moist ~  s(Time, k=4), random = list(site=~1), data=moge, correlation = corCAR1()) %>% .$gam %>% .$fitted.values %>% unique

gam(Moist ~  s(Time, k=4), data=moge, correlation = corCAR1()) %>% .$fitted.values %>% unique

  group_by(site) %>%
  summarize(res_mean = mean(res)) %>% .$res_mean %>% hist


plot(moge$Moist, res$gam$residuals)

gam(WSG ~  s(Time, k=4), data = moge %>% filter(site == 3), correlation = corCAR1()) %>% .$fitted.values %>% plot
#
# par(mfrow=c(4,5))
# for (i in 201:220) plot(Moist ~ Time, data = moge20 %>%  filter(site == i) %>% arrange(Time), type = "b")

summary(res$lme)
summary(res$gam)
plot(res$lme)

res$lme$residuals
res2 <- res$lme$fitted

res2 <- res$gam$residuals
res3 <- res$lme$residuals

data_frame(res2, res3[,3])

res2 <- res$gam$residuals
fit <- res$gam$fitted.values




res3 <- cbind(res2, moge)
par(mfrow=c(1,1))

plot(res2 ~ WSG, res3)

par(mfrow=c(1,3))
hist(res$lme$residuals[,1])
hist(res$lme$residuals[,2])
hist(res$lme$residuals[,3])

res2 <- gam(slope ~  s(Time,k=4), data=moge, correlation = corCAR1())

par(mfrow = c(1,1))
plot(WSG ~ Time, moge)
for (i in 1:50){
  points(WSG ~ Time, moge %>% filter(site == i), type = "l")
}

dat <- dat %>%
  arrange(x2)
plot(y ~x2,dat)
for(i in 1:4) points(y~x2, dat%>%filter(fac ==i), type = "l")
res <- gamm(y ~ s(x2), data=dat, random = list(fac=~1), family=poisson)

res$lme$fitted
res$gam$fitted.value


gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge.cross[(35*i+1):350,], correlation = corCAR1())

gamm(noquote("slope ~ s(Time, k= 4)"), random = list(site=~1), data=moge.cross[(35*i+1):350,], correlation = corCAR1())

sp_ef <- rep(ranef(res$lme)$site %>% unlist, 7)

data_frame(res = res2, fit = fit) %>%
  bind_cols(., moge) %>%
  mutate(res2 = WSG - fit) %>%
  mutate(fit2 = WSG - res) %>%
  mutate(WSG2 = fit + res) %>%
  # filter(site == 1)  %>%
  mutate(sp_ef = sp_ef) %>%
  mutate(fit3 = fit + sp_ef)

##############################################
## cross validation new
###########################################

  moge$temp <- rnorm(nrow(moge))
  moge100r$temp <- rnorm(nrow(moge100r))

  moge.cross <- moge[order(moge$temp),] %>%
    mutate(site = as.factor(site))
  rownames(moge.cross) <- NULL
  k <- 10
  # res.cv <- numeric(k)
  res.cv <- NULL
  SS <- NULL
  PREDS <- NULL
  i <- 1
  r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge.cross[(35*i+1):350,], correlation = corCAR1(form=~Time))
  site_ef <- ranef(r2.1$lme)$site %>% unlist
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)
  res <- data_frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time,
    site = moge.cross[(35*i+1):350, "site"])

    # %>%
    # merge(., site_ef_dat, by = "site") %>%
    # mutate(fitted_site = fitted + site_ef)

  # res <- data.frame(fitted = r2.1$gam$fitted.values,
                    # res = r2.1$gam$residuals,
                    # res2 = r2.1$gam$y - r2.1$gam$fitted.values -r2.1$gam$residuals,
                    # Time= r2.1$gam$model$Time)
  res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

  res3 <- full_join(moge.cross[1:35,], res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

  res3 <- res3 %>%
    mutate(obs2 = WSG - site_ef)

# plot(obs2 ~ Time, res3)
# points(fitted ~ Time, res3, pch = 16)

  # SS[i] <- sum((moge.cross[1:35,"WSG"] - mean(moge.cross[1:35,"WSG"]))^2)
  SS[i] <- sum((res3$obs2 - mean(res3$obs2))^2)
  PREDS[i] <- sum((res3$fitted - res3$obs2)^2)
  # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
  # r2.1$lme$residuals$site


  for (i in 2:(k-1)){
    temp.data <- rbind(moge.cross[1:(35*(i-1)),],
                       moge.cross[(35*i+1):350,])
    r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=temp.data, correlation = corCAR1(form=~Time))

    site_ef <- ranef(r2.1$lme)$site %>% unlist
    site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)

    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    # Time= r2.1$gam$model$Time)
    res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

    K1 <- 35*(i-1) +1
    K2 <- 35*i

    res3 <- left_join(moge.cross[K1:K2,], res2, by = "Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

    res3 <- res3 %>%
    mutate(obs2 = WSG - site_ef)

    SS[i] <- sum((res3$obs2 - mean(res3$obs2))^2)
    PREDS[i] <- sum((res3$fitted - res3$obs2)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
  }

  i <- 10
  r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge.cross[1:350,], correlation = corCAR1(form=~Time))

  site_ef <- ranef(r2.1$lme)$site %>% unlist
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)

  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    # Time= r2.1$gam$model$Time)
    res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

    res3 <- full_join(moge.cross[316:350,], res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

    res3 <- res3 %>%
    mutate(obs2 = WSG - site_ef)

  SS[i] <- sum((res3$obs2 - mean(res3$obs2))^2)
  PREDS[i] <- sum((res3$fitted - res3$obs2)^2)
  # res.cv[i] <- 1 - mean(PREDS)/(SS)

  1 - mean(PREDS,na.rm=T)/mean(SS,na.rm=T)


########################===============================================



##############################################
## cross validation  fixed + rand
###########################################

  moge$temp <- rnorm(nrow(moge))
  moge100r$temp <- rnorm(nrow(moge100r))

  moge.cross <- moge100r[order(moge100r$temp),] %>%
    mutate(site = as.factor(site))
  rownames(moge.cross) <- NULL
  k <- 10
  # res.cv <- numeric(k)
  res.cv <- NULL
  SS <- NULL
  PREDS <- NULL
  i <- 1
  r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge.cross[(35*i+1):350,], correlation = corCAR1(form=~Time))
  site_ef <- ranef(r2.1$lme)$site %>% unlist
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)
  res <- data_frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time,
    site = moge.cross[(35*i+1):350, "site"])

    # %>%
    # merge(., site_ef_dat, by = "site") %>%
    # mutate(fitted_site = fitted + site_ef)

  # res <- data.frame(fitted = r2.1$gam$fitted.values,
                    # res = r2.1$gam$residuals,
                    # res2 = r2.1$gam$y - r2.1$gam$fitted.values -r2.1$gam$residuals,
                    # Time= r2.1$gam$model$Time)
  res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

  res3 <- full_join(moge.cross[1:35,], res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

  res3 <- res3 %>%
    mutate(obs2 = WSG - site_ef)


  SS[i] <- sum((res3$WSG - mean(res3$WSG))^2)
  PREDS[i] <- sum((res3$fitted_site - res3$WSG)^2)
  # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
  # r2.1$lme$residuals$site


  for (i in 2:(k-1)){
    temp.data <- rbind(moge.cross[1:(35*(i-1)),],
                       moge.cross[(35*i+1):350,])
    r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=temp.data, correlation = corCAR1(form=~Time))

    site_ef <- ranef(r2.1$lme)$site %>% unlist
    site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)

    res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    # Time= r2.1$gam$model$Time)
    res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

    K1 <- 35*(i-1) +1
    K2 <- 35*i

    res3 <- left_join(moge.cross[K1:K2,], res2, by = "Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

    res3 <- res3 %>%
    mutate(obs2 = WSG - site_ef)

    SS[i] <- sum((res3$WSG - mean(res3$WSG))^2)
    PREDS[i] <- sum((res3$fitted_site - res3$WSG)^2)
    # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
  }

  i <- 10
  r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge.cross[1:350,], correlation = corCAR1(form=~Time))

  site_ef <- ranef(r2.1$lme)$site %>% unlist
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)

  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

    # Time= r2.1$gam$model$Time)
    res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

    res3 <- full_join(moge.cross[316:350,], res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

    res3 <- res3 %>%
    mutate(obs2 = WSG - site_ef)
    SS[i] <- sum((res3$WSG - mean(res3$WSG))^2)
    PREDS[i] <- sum((res3$fitted_site - res3$WSG)^2)
  # res.cv[i] <- 1 - mean(PREDS)/(SS)

  1 - mean(PREDS,na.rm=T)/mean(SS,na.rm=T)


########################===============================================



plot(obs2 - fitted ~fitted, data = res3)


set.seed(2)
sp <- rnorm(10, 0 , 10)

x <- rnorm(20)

mu <- 0.5 * rep(x, 10) - 3 + rep(sp, each = 20)
mu2 <- -0.5 * rep(x, 10) - 3 + rep(sp, each = 20)

# mu <- c(mu[1:100], mu2[101:200])

dat <- data_frame(y = y, mu = mu, x = rep(x, 10), sp = rep(sp, each = 20)) %>%
  mutate(sp2 = rep(1:10, each = 20)) %>%
  arrange(x)

plot(y ~ x, dat)
for(i  in 1:10) points(y ~ x, data = dat %>% filter(sp2 == i), type = "l")


lm(y ~ x + as.factor(sp2), dat) %>% summary

res1 <- lm(y ~ x, dat)
res2 <- lmer(y ~ x + (1|sp2), dat)

plot(y ~ x,dat)
abline(res1, lty =2)
abline(res2)

dat %>%
  mutate(sp_ef = rep(ranef(res2) %>% unlist, 20)) %>%
  mutate(y2 = y - sp_ef) %>%
  plot(y2 ~ x,.)

Fixed <- fixef(res2)[1] + fixef(res2)[2] * dat$x
VarF <- var(Fixed)
VarF / (VarF + VarCorr(res2)$sp2[1] + attr(VarCorr(res2), "sc")^2)

(VarF + VarCorr(res2)$sp2[1] )/ (VarF + VarCorr(res2)$sp2[1] + attr(VarCorr(res2), "sc")^2)

##ALL ---------------------------------------------------------------------
moge <- data.frame(WSG = scale(unlist(WSG100)),
                   Moist = scale(unlist(Moist100)),
                   slope = scale(unlist(slope100)),
                   convex = scale(unlist(convex100)),
                   site = as.factor(rep(1:50,7)),
                   Time)


moge100r <- data.frame(WSG = scale(unlist(WSG100.rm)),
                  Moist = scale(unlist(Moist100.rm)),
                  slope = scale(unlist(slope100.rm)),
                  convex = scale(unlist(convex100.rm)),
                  site = rep(1:50,7),
                  Time)

r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = moge100r %>% arrange(WSG), correlation = corCAR1())

# plot(r2.1$gam)

# r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = moge100r %>% arrange(convex))

# r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = moge100r %>% arrange(convex), correlaion = corARMA(form = ~1|site, p =1))

  (site_ef <- ranef(r2.1$lme)$site %>% unlist)
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)
  res <- data_frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time,
    site = as.factor(moge100r[,"site"]))


  res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

  res3 <- full_join(moge100r %>% mutate(site =as.factor(site)), res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

  res3 <- res3 %>%
    mutate(obs2 = convex - site_ef)

par(mfrow=c(1,2))
plot(obs2 ~ Time, res3, col = "gray")
for(i in 1:50) points(obs2 ~ Time, res3 %>% filter(site == i),
  col = "gray", type = "l")
points(fitted ~ Time, res3, pch = 16)

res3 %>% group_by(Time) %>% summarize(convex = mean(convex)) %>%
points(convex ~ Time, ., pch = 2)

plot(convex ~ Time, res3)
for (i in 1:50) points(convex ~ Time, res3 %>% filter(site==i),
  col = "grey", type = "l")
par(mfrow=c(1,1))


####
par(mfrow=c(3,4))
for (i in 1:12) plot(convex ~ Time, temp.data %>% filter(site ==i))
par(mfrow=c(1,1))


Fixed <- fixef(mF)[2] * mF@X[, 2] + fixef(mF)[3] * mF@X[, 3] + fixef(mF)[4] * mF@X[, 4]

# Calculation of the variance in fitted values
VarF <- var(Fixed)

# R2GLMM(m) - marginal R2GLMM
# Equ. 26, 29 and 30
# VarCorr() extracts variance components
# attr(VarCorr(lmer.model),'sc')^2 extracts the residual variance
VarF/(VarF + VarCorr(mF)$Container[1] + VarCorr(mF)$Population[1] + attr(VarCorr(mF), "sc")^2)


##
temp1 <- moge100r %>% filter(Time==1982)
temp2 <- moge100r %>% filter(Time==2010)

temp2$Moist - temp1$Moist

data_frame(site = D100m[[1]] %>% rownames,





temp <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge %>% filter(site==11), correlation = corCAR1())
plot(convex ~ Time, moge %>% filter(site ==11))
summary(temp$gam)



###

#wood density = 0.4
#slope = 0.02
#convex = 0.09
#moist =
#WSG_rm = 0.4399393
# moge %>% filter(site %in% cross_site[1:5, "site"])
# 1:5 vs 6:50
# 6:10 vs 1:5, 11:50

moge <- data.frame(WSG = unlist(WSG100.rm),
                   Moist = unlist(Moist100),
                   slope = unlist(slope100),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time)
moge <- moge %>%
  mutate(WSG = WSG - WSG100.rm[[1]]) %>%
  mutate(Moist = Moist - Moist100[[1]]) %>%
  mutate(convex = convex - convex100[[1]]) %>%
  mutate(slope = slope - slope100[[1]])

cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
  arrange(temp)
k <- 10

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
  plot(WSG ~ Time, temp_pre, main = paste("subset", i), ylab = "WSG", type = "n")
  for (j in 1:7) points(WSG ~ Time, temp_pre %>% filter(site == site_vec[j]), type = "b")
  # points(fitted ~ Time, temp_pre, pch = 16)

for (i in 2:(k-1)){
  temp_train1 <- moge %>% filter(site %in% cross_site[1:(5*(i-1)), "site"])
  temp_train2 <- moge %>% filter(site %in% cross_site[(5*i +1):50, "site"])

  temp.data <- bind_rows(temp_train1, temp_train2)
  # r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data = temp.data, correlation = corCAR1(form=~Time))
  # res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  # res2 <- unique(res[order(res$Time),])
  #
  # temp_pre <- moge %>% filter(site %in% cross_site[(5*(i-1) + 1):(5*i), "site"])
  #
  # res3 <- merge(temp_pre, res2, by = "Time")
  # SS[i] <- sum((res3$WSG - mean(res3$WSG))^2)
  # PREDS[i] <- sum((res3$fitted - res3$WSG)^2)
  # # res.cv[i] <- 1 - mean(PREDS)/mean(SS)
  # site_vec <- res3$site %>% unique
  plot(WSG ~ Time, temp_pre, main = paste("subset", i), ylab = "WSG", type = "n")
  for (j in 1:7) points(WSG ~ Time, temp_pre %>% filter(site == site_vec[j]), type = "b")
  # points(fitted ~ Time, temp_pre, pch = 16)
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
  plot(WSG ~ Time, temp_pre, main = paste("subset", i), ylab = "WSG", type = "n")
  for (j in 1:7) points(WSG ~ Time, temp_pre %>% filter(site == site_vec[j]), type = "b")
  # points(fitted ~ Time, res3, pch = 16)

1 - mean(PREDS, na.rm=T)/mean(SS,na.rm=T)
########################################
