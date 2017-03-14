##############################################################
#Feeley's R2
##############################################################

for (i in 1:7){
  WSG100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR", "SWARS1"))],trait,"WSG")
  Moist100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO"))],trait,"Moist")
  slope100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("FARAOC","POULAR","OCOTWH"))],trait,"sp.slope.mean")
  # convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("ALSEBL","SWARS1"))],trait,"sp.convex.mean")
  convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("SWARS1","ALSEBL", "GAR2IN"))],trait,"sp.convex.mean")
}


moge <- data.frame(WSG = unlist(WSG100),
                  WSG_rm= unlist(WSG100.rm),
                   Moist = unlist(Moist100),
                   slope = unlist(slope100),
                   slope_rm = unlist(slope100.rm),
                   convex = unlist(convex100),
                   convex_rm = unlist(convex100.rm),
                   site = as.factor(rep(1:50,7)),
                   Time)

moge <- moge %>%
  mutate(WSG = WSG - WSG100[[1]]) %>%
  mutate(WSG_rm = WSG_rm - WSG100.rm[[1]]) %>%
  mutate(Moist = Moist - Moist100[[1]]) %>%
  mutate(convex = convex - convex100[[1]]) %>%
  mutate(convex_rm = convex_rm - convex100.rm[[1]]) %>%
  mutate(slope = slope - slope100[[1]]) %>%
  mutate(slope_rm = slope_rm - slope100.rm[[1]])

moge <- moge %>%
  mutate(WSG = WSG / WSG100[[1]]) %>%
  mutate(WSG_rm = WSG_rm / WSG100.rm[[1]]) %>%
  mutate(Moist = Moist / Moist100[[1]]) %>%
  mutate(convex = convex / convex100[[1]]) %>%
  mutate(slope = slope / slope100[[1]])



moge20 <- data.frame(WSG = unlist(WSG20),
                   Moist = unlist(Moist20),
                   slope = unlist(slope20),
                   convex = unlist(convex20),
                   site = as.factor(rep(1:1250,7)),
                   Time) %>%
  mutate(WSG = WSG - WSG20[[1]]) %>%
  # mutate(WSG_rm = WSG_rm - WSG20.rm[[1]]) %>%
  mutate(Moist = Moist - Moist20[[1]]) %>%
  mutate(convex = convex - convex20[[1]]) %>%
  mutate(slope = slope - slope20[[1]])


r20_1 <- gam(WSG ~  s(Time,k=4), data = moge20,
  correlation = corCAR1(form=~Time))
r20_2 <- gam(Moist ~  s(Time,k=4), data = moge20,
  correlation = corCAR1(form=~Time))
r20_3 <- gam(convex ~  s(Time,k=4), data = moge20,
  correlation = corCAR1(form=~Time))
r20_4 <- gam(slope ~  s(Time,k=4), data = moge20,
  correlation = corCAR1(form=~Time))

r100_1 <- gam(WSG ~  s(Time,k=4), data = moge,
  correlation = corCAR1(form=~Time))
r100_2 <- gam(Moist ~  s(Time,k=4), data = moge,
  correlation = corCAR1(form=~Time))
r100_3 <- gam(convex ~  s(Time,k=4), data = moge,
  correlation = corCAR1(form=~Time))
r100_4 <- gam(slope ~  s(Time,k=4), data = moge,
  correlation = corCAR1(form=~Time))

# moge <- data.frame(WSG = unlist(WSG100.rm),
#                    Moist = unlist(Moist100),
#                    slope = unlist(slope100),
#                    convex = unlist(convex100),
#                    site = as.factor(rep(1:50,7)),
#                    Time)
# moge <- moge %>%
#   mutate(WSG = WSG - WSG100.rm[[1]]) %>%
#   mutate(Moist = Moist - Moist100[[1]]) %>%
#   mutate(convex = convex - convex100[[1]]) %>%
#   mutate(slope = slope - slope100[[1]])

before <- proc.time()

cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
  arrange(temp)


#wood density = 0.4
#slope = 0.02
#convex = 0.09
#moist =
#WSG_rm = 0.4399393
# moge %>% filter(site %in% cross_site[1:5, "site"])
# 1:5 vs 6:50
# 6:10 vs 1:5, 11:50
k <- 10

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


  r2_ci <- NULL
  for (i in 1:100){
    r2_ci[i] <- 1 - mean(sample(PREDS, 10,  replace = T)) / mean(sample(SS, 10,  replace = T) )
  }

  r2_ci2 <- NULL
  for (i in 1:100){
    r2_ci2[i] <-   1  - mean(sample(PREDS/SS, 10, replace = T))
  }


  # quantile(r2_ci, 0.05)
  # quantile(r2_ci, 0.95)

  quantile(r2_ci2, 0.05)
  quantile(r2_ci2, 0.95)

# save.image("Ken.RData")

after <- proc.time()
after - before
quantile(r2_ci, 0.05)
quantile(r2_ci, 0.95)
# WSG_rm
# >   quantile(r2_ci2, 0.05)
#         5%
# 0.06390781
# >   quantile(r2_ci2, 0.95)
#       95%
# 0.2242184

# #WSG
# quantile(r2_ci2, 0.05)
#        5%
# 0.3177681
# >   quantile(r2_ci2, 0.95)
#       95%
# 0.4723954
#
# Moist
# >   quantile(r2_ci2, 0.05)
#         5%
# -0.1807875
# >   quantile(r2_ci2, 0.95)
#          95%
# -0.001249136

#convex
# >   quantile(r2_ci2, 0.05)
#         5%
# -0.1436609
# >   quantile(r2_ci2, 0.95)
#       95%
# 0.1887277

#slope>
# quantile(r2_ci2, 0.05)
#         5%
# -0.2412032
# >   quantile(r2_ci2, 0.95)
#       95%
# 0.1858206


plot(WSG_rm ~ Time, res3, main = paste("subset", i), type = "n")
for (j in 1:7) points(convex ~ Time, temp_pre %>% filter(site == site_vec[j]), type = "b")
points(fitted ~ Time, res3, pch = 16)




#####


cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
  arrange(temp)


#wood density = 0.4
#slope = 0.02
#convex = 0.09
#moist =
#WSG_rm = 0.4399393
# moge %>% filter(site %in% cross_site[1:5, "site"])
# 1:5 vs 6:50
# 6:10 vs 1:5, 11:50
k <- 10

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

1  - mean(PREDS, na.rm=T)/mean(SS,na.rm=T)

r2_ci <- NULL
for (i in 1:100){
  r2_ci[i] <- 1 - mean(sample(PREDS, 10,  replace = T)) / mean(sample(SS, 10,  replace = T) )
}

r2_ci

##############################################################
moge <- data.frame(WSG = unlist(WSG100),
                  WSG_rm= unlist(WSG100.rm),
                   Moist = unlist(Moist100),
                   slope = unlist(slope100),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time)

                   WSG <- c(rep(0, 50),
                           abs(rnorm(50, 0.5)),
                           abs(rnorm(50, 1)),
                           abs(rnorm(50, 2)),
                           abs(rnorm(50, 3, 2)),
                           abs(rnorm(50, 4, 4)),
                           abs(rnorm(50, 5, 5)))

WSG <- c(rep(0, 50),
        sort(abs(rnorm(50, 0.5))),
        sort(abs(rnorm(50, 1, 2))),
        sort(abs(rnorm(50, 2, 2))),
        sort(abs(rnorm(50, 3, 2))),
        sort(abs(rnorm(50, 4, 4))),
        sort(abs(rnorm(50, 5, 5))))


# WSG <- c(rep(0, 50),
#       sort(rnorm(50, 0.5)),
#       sort(rnorm(50, 1, 2)),
#       sort(rnorm(50, 2, 2)),
#       sort(rnorm(50, 3, 2)),
#       sort(rnorm(50, 4, 4)),
#       sort(rnorm(50, 5, 5)))

moge$WSG <- WSG

par(mfrow = c(1,1))
plot(WSG ~ Time, moge, ylab = "simulated")
abline(h = 0, lty = 2)

r2 <- gam(WSG ~  s(Time,k=4), data = temp_train1, correlation = corCAR1(form=~Time))

before <- proc.time()

cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
 arrange(temp)


#wood density = 0.4
#slope = 0.02
#convex = 0.09
#moist =
#WSG_rm = 0.4399393
# moge %>% filter(site %in% cross_site[1:5, "site"])
# 1:5 vs 6:50
# 6:10 vs 1:5, 11:50
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
 plot(WSG ~ Time, res3, main = paste("subset", i), type = "n", ylab = "simulate")
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
 plot(WSG ~ Time, res3, main = paste("subset", i), type = "n", ylab ="simulated")
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
 plot(WSG ~ Time, res3, main = paste("subset", i), type = "n", ylab ="simulated")
 for (j in 1:7) points(WSG ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
 points(fitted ~ Time, res3, pch = 16)


 r2_ci <- NULL
 for (i in 1:100){
   r2_ci[i] <- 1 - mean(sample(PREDS, 10,  replace = T)) / mean(sample(SS, 10,  replace = T) )
 }

 r2_ci2 <- NULL
 for (i in 1:100){
   r2_ci2[i] <-   1  - mean(sample(PREDS/SS, 10, replace = T))
 }


 # quantile(r2_ci, 0.05)
 # quantile(r2_ci, 0.95)

 quantile(r2_ci2, 0.05)
 quantile(r2_ci2, 0.95)
