rm(list = ls()) # This clears everything from memory.

# setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")

library(dplyr)
library(MASS)
# library(snowfall)
library(cvTools)


argv <- commandArgs(trailingOnly = TRUE)
plot_size <- argv[1]

# sfInit(parallel = T, cpu = 8, type = "SOCK")
# sfLibrary(dplyr)
# sfLibrary(MASS)
# library(glmmADMB)

# str(D20m)
# str(D100m)


# n_model <- "model2"
plot_size <- "100m"
save_name <- paste(plot_size, ".RData", sep = "")

# print(n_model)
print(plot_size)
print(save_name)

#####
moge <- NULL
ab_t_data <- NULL

if(plot_size == "plot20m") {
  N <- 1250
  subplot <- D20m} else {
    N <- 50
    subplot <- D100m
  }

# N <- 50
# subplot <- D100m

for (i in 1:N){
  ab_data <- data_frame(
    sp = colnames(subplot[[1]]),
    ab1982 = subplot[[1]][i,],
    ab2010 = subplot[[7]][i,])

  trait_temp <- data_frame(
    sp = rownames(trait),
    moist=trait$Moist,
    slope=trait$sp.slope.mean,
    convex=trait$sp.convex.mean,
    WSG=trait$WSG)

  moge <- full_join(ab_data, trait_temp, by = "sp") %>%
    na.omit %>%
    mutate(site = paste("subplot", i, sep = "_"))
  ab_t_data <- bind_rows(moge, ab_t_data)
}

ab_t_data$site <- as.factor(ab_t_data$site)

ab_t_data2 <- ab_t_data %>%
  filter(ab1982 != 0 | ab2010 != 0) %>%
  mutate(ab1982 = ab1982 + 1) %>%
  mutate(ab2010 = ab2010 + 1) %>%
  mutate(rate = log(ab2010/ab1982)/(2010-1982))


r2.func = function(res, data){
  y = data$ab2010/data$ab1982
  residuals = y - res$fitted/data$ab1982
  1 - sum(residuals^2)/sum((y-mean(y))^2)
}

# system.time(res_all <- glmmadmb(ab2010 ~WSG
#             + moist
#             + convex
#             + slope
#             + WSG:moist
#             + WSG:convex
#             + WSG:slope
#             + convex:slope
#             + moist:slope
#             + moist:convex
#             + offset(log(ab1982))
#             + (1 | site),
#             family = "nbinom",
#             data = ab_t_data2))
#
# res_all$formula %>% print
# res_all %>% AIC %>% print
#
# system.time(res_all <- glmmadmb(ab2010 ~WSG
#             + moist
#             + convex
#             + slope
#             + offset(log(ab1982))
#             + (1 | site),
#             family = "nbinom",
#             data = ab_t_data2))
#
# system.time(res_all2 <- glmer.nb(ab2010 ~WSG
#             + moist
#             + convex
#             + slope
#             + offset(log(ab1982))
#             + (1 | site),
#             data = ab_t_data2))


#
# system.time(res_all2 <- glm.nb(ab2010 ~WSG
#             + moist
#             + convex
#             + slope
#             + WSG:moist
#             + WSG:convex
#             + WSG:slope
#             + convex:slope
#             + moist:slope
#             + moist:convex
#             + offset(log(ab1982)),
#             data = filter(ab_t_data2, site == "subplot_1")))
#
# r2.func(res_all2, filter(ab_t_data2, site == "subplot_1"))

site_lev <- ab_t_data2$site %>% levels
rand_r2 <- NULL
r2_obs <- NULL
#
# rand_func <- function(lhs){
#   temp1 <- temp_dat %>%
#   dplyr::select(sp, ab1982, ab2010)
#   temp2 <- temp_dat %>%
#   dplyr::select(sp, WSG, moist, convex, slope)
#
#   temp1$sp <- sample(temp1$sp) %>% as.character
#
#   rand_dat <- full_join(temp1, temp2, by = "sp")
#   res_rand <- glm.nb(lhs, data = rand_dat)
#   return(r2.func(res_rand, rand_dat))
# }

data <- temp_dat
glmfit <-res_obs2
cv_glm <- function(data, glmfit, K){
  res_cv <- NULL
  SS <- NULL
  PREDS <- NULL
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)

  Call <- glmfit$call

  for (i in 1:K){
    Call$data <- filter(temp, gr != i)
    d_glm <- eval.parent(Call)

    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(d_glm, test_dat, type = "response")
    fitted2 <- fitted / test_dat$ab1982
    yy <- test_dat$ab2010 / test_dat$ab1982

    SS[i] <- (yy - mean(yy, na.rm = T))^2 %>% mean
    PREDS[i] <- (yy - fitted2)^2 %>% mean(na.rm = T)
  }
  1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)
}

cv_lm <- function(data, glmfit, K){
  res_cv <- NULL
  SS <- NULL
  PREDS <- NULL
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)

  Call <- glmfit$call

  for (i in 1:K){
    Call$data <- filter(temp, gr != i)
    d_glm <- eval.parent(Call)

    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(d_glm, test_dat, type = "response")
    fitted2 <- fitted
    yy <- test_dat$rate

    SS[i] <- (yy - mean(yy, na.rm = T))^2 %>% mean
    PREDS[i] <- (yy - fitted2)^2 %>% mean(na.rm = T)
  }
  1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)
}

# sfExport("rand_func", "r2.func")

before <- proc.time()
res_cv <- NULL
res_cv <- matrix(0, nrow = 10, ncol = 3)
# for (i in 1:length(site_lev)){
for (i in 1:10){
  paste("######## runs i = ", i, " #############", sep = "") %>% print
  temp_dat <- filter(ab_t_data2, site == site_lev[i])
  system.time(res_obs <- glm.nb(ab2010 ~WSG
              + moist
              + convex
              + slope
              + WSG:moist
              + WSG:convex
              + WSG:slope
              + convex:moist
              + slope:moist
              + convex:slope
              # + WSG:convex:moist
              # + WSG:slope:moist
              # + WSG:convex:slope
              # + moist:convex:slope
              + offset(log(ab1982)),
              data = temp_dat))
  # system.time(res_obs <- lm(rate ~WSG
  #           + moist
  #           + convex
  #           + slope
  #           + WSG:moist
  #           + WSG:convex
  #           + WSG:slope
  #           + convex:moist
  #           + slope:moist
  #           + convex:slope,
  #           data = temp_dat))
  res_obs2 <- stepAIC(res_obs)

  for (j in 1:3) {
    res_cv[i, j] <- cv_glm(temp_dat, res_obs2, K = 10)
  }
}

# system.time(sapply(1:99, function(x)rand_func()))

after <- proc.time()

after - before


write.csv(res_cv, paste(plot_size, ".csv", sep = ""))
save.image(save_name)
