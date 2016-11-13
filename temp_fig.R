r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data = moge %>% arrange(Moist), correlation = corCAR1(form=~Time))

# plot(r2.1$gam)

# r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data = moge %>% arrange(WSG))

# r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data = moge %>% arrange(WSG), correlaion = corARMA(form = ~1, p =1))

  (site_ef <- ranef(r2.1$lme)$site %>% unlist)
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)
  res <- data_frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time,
    site = as.factor(moge[,"site"]))


  res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

  res3 <- full_join(moge %>% mutate(site =as.factor(site)), res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

  res3 <- res3 %>%
    mutate(obs2 = WSG - site_ef)

par(mfrow=c(4,2), mar=c(4,4,2, 2))
plot(obs2 ~ Time, res3, col = "gray",ylab = "scale(wood_density - site_effect)", main
= "Estimated values")
for(i in 1:50) points(obs2 ~ Time, res3 %>% filter(site == i),
  col = "gray", type = "l")
points(fitted ~ Time, res3, pch = 16)

plot(WSG ~ Time, res3, ylab = "scale(wood_density)", main = "Observed values")
for (i in 1:50) points(WSG ~ Time, res3 %>% filter(site==i),
  col = "grey", type = "l")

r2.1 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data = moge %>% arrange(WSG), correlation = corCAR1(form=~Time))


  (site_ef <- ranef(r2.1$lme)$site %>% unlist)
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)
  res <- data_frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time,
    site = as.factor(moge[,"site"]))


  res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

  res3 <- full_join(moge %>% mutate(site =as.factor(site)), res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

  res3 <- res3 %>%
    mutate(obs2 = Moist - site_ef)

plot(obs2 ~ Time, res3, col = "gray",ylab = "scale(Moisture - site_effect)")
for(i in 1:50) points(obs2 ~ Time, res3 %>% filter(site == i),
  col = "gray", type = "l")
points(fitted ~ Time, res3, pch = 16)

plot(Moist ~ Time, res3, ylab = "scale(Moisture)")
for (i in 1:50) points(Moist ~ Time, res3 %>% filter(site==i),
  col = "grey", type = "l")




r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = moge %>% arrange(WSG), correlation = corCAR1(form=~Time))


  (site_ef <- ranef(r2.1$lme)$site %>% unlist)
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)
  res <- data_frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time,
    site = as.factor(moge[,"site"]))


  res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

  res3 <- full_join(moge %>% mutate(site =as.factor(site)), res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

  res3 <- res3 %>%
    mutate(obs2 = convex - site_ef)

plot(obs2 ~ Time, res3, col = "gray",ylab = "scale(convexity - site_effect)")
for(i in 1:50) points(obs2 ~ Time, res3 %>% filter(site == i),
  col = "gray", type = "l")
points(fitted ~ Time, res3, pch = 16)

plot(convex ~ Time, res3, ylab = "scale(convexity)")
for (i in 1:50) points(convex ~ Time, res3 %>% filter(site==i),
  col = "grey", type = "l")





r2.1 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data = moge %>% arrange(WSG), correlation = corCAR1(form=~Time))


  (site_ef <- ranef(r2.1$lme)$site %>% unlist)
  site_ef_dat <- data_frame(site = as.factor(1:50), site_ef = site_ef)
  res <- data_frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time,
    site = as.factor(moge[,"site"]))


  res2 <- res %>%
    dplyr::select(fitted, Time) %>%
    unique %>%
    arrange(Time)

  res3 <- full_join(moge %>% mutate(site =as.factor(site)), res2, by="Time") %>%
    left_join(., site_ef_dat, by = "site") %>%
    mutate(fitted_site = fitted + site_ef)

  res3 <- res3 %>%
    mutate(obs2 = slope - site_ef)

plot(obs2 ~ Time, res3, col = "gray",ylab = "scale(slope - site_effect)")
for(i in 1:50) points(obs2 ~ Time, res3 %>% filter(site == i),
  col = "gray", type = "l")
points(fitted ~ Time, res3, pch = 16)


plot(slope ~ Time, res3, ylab = "scale(slope)")
for (i in 1:50) points(slope ~ Time, res3 %>% filter(site==i),
  col = "grey", type = "l")


#####
set.seed(3)
site_ef <- rnorm(50, sd = 1.5)
xx <- rep(1:7, 50)
mu <- xx * 0.2 + 3 + rep(site_ef, each = 7)
yy <- rnorm(350, mu, sd = 0.5)

data <- data_frame(yy = yy, mu = mu, xx = xx, site = rep(1:50, each = 7) %>% as.factor)

res <- gamm(yy ~ s(xx, k=4), random = list(site=~1), data = data, correlation = corCAR1(form=~xx))

summary(res$gam)

plot(yy ~ xx, data)
for (i in 1:50) points(yy ~ xx, data %>% filter(site==i),
  col = "grey", type = "l")

##CV
##############################################################
#original R2
##############################################################

moge <- data %>%
  mutate(Time = xx) %>%
  mutate(WSG = yy) %>%
  mutate(temp = rnorm(nrow(data))) %>% as.data.frame


moge100r <- moge100r %>% mutate(temp = rnorm(nrow(.)))

moge.cross <- moge %>% arrange(temp)

rownames(moge.cross) <- NULL
k <- 10
par(mfrow = c(2,5))
# res.cv <- numeric(k)
  res.cv <- NULL
  SS <- NULL
  PREDS <- NULL
  i <- 1
  r2.1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge.cross[(35*i+1):350,], correlation = corCAR1(form=~Time))
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

  # res <- data.frame(fitted = r2.1$gam$fitted.values,
                    # res = r2.1$gam$residuals,
                    # res2 = r2.1$gam$y - r2.1$gam$fitted.values -r2.1$gam$residuals,
                    # Time= r2.1$gam$model$Time)


  res2 <- unique(res[order(res$Time),])
  res3 <- merge(moge.cross[1:35,],res2,by="Time")

  SS[i] <- sum((moge.cross[1:35,"WSG"] - mean(moge.cross[1:35,"WSG"]))^2)
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

#=====================================
##############################################################
#Feeley's R2
##############################################################
cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
  arrange(temp)

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
  plot(WSG ~ Time, res3, main = paste("subset", i), ylab = "Wood density", type = "n")
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
  plot(WSG ~ Time, res3, main = paste("subset", i), ylab = "Wood density", type = "n")
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
  plot(WSG ~ Time, res3, main = paste("subset", i), ylab = "Wood density", type = "n")
  for (j in 1:7) points(WSG ~ Time, res3 %>% filter(site == site_vec[j]), type = "b")
  points(fitted ~ Time, res3, pch = 16)

1 - mean(PREDS, na.rm=T)/mean(SS,na.rm=T)
########################################



par(mfrow=c(3,4))
  for (i in 1:12) plot(yy ~ xx, data %>% filter(site==i),
    col = "grey", type = "l")
par(mfrow=c(1,1))

data <- data_frame(x = sample(1:7, 100, replace = T), y = rnorm(100))



gam(y ~ s(x, k=4), random = list(site=~1), data = data, correlation = corCAR1(form=~Time)) %>% summary




####
r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = moge %>% arrange(Moist), correlation = corCAR1(form=~Time))

summary(r2.1$gam)
