#
# repeted

###

before <- proc.time()
for (l in 1:10){
  cross_site <- data.frame(site = 1:50, temp = rnorm(50)) %>%
    arrange(temp)

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
    site_vec <- res3$site %>% unique}


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

    r2_ci[l] <- 1 - mean(PREDS)/mean(SS)
}

my_ci <- function(r) {
   c(mean = mean(r), lower = quantile(r, 0.05), upper = quantile(r, 0.95)) }

convex_CI <- my_ci(r2_ci2)

after <- proc.time()
after - before
