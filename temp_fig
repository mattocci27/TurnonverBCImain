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
xx <- 1:7
mu <- rnorm(350, sd = 0.2) + rep(site_ef, each = 7)
yy <- rnorm(mu)

data <- data_frame(yy = yy, mu = mu, xx = rep(xx, 50), site = rep(1:50, each = 7) %>% as.factor)

res <- gamm(yy ~ s(xx, k=4), random = list(site=~1), data = data, correlation = corCAR1(form=~Time))

summary(res$gam)

plot(yy ~ xx, data)
for (i in 1:50) points(yy ~ xx, data %>% filter(site==i),
  col = "grey", type = "l")


data <- data_frame(x = sample(1:7, 100, replace = T), y = rnorm(100))



gam(y ~ s(x, k=4), random = list(site=~1), data = data, correlation = corCAR1(form=~Time)) %>% summary




####
r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data = moge %>% arrange(Moist), correlation = corCAR1(form=~Time))

summary(r2.1$gam)
