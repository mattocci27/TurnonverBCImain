rm(list = ls()) # This clears everything from memory.

# setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")

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
# plot_size <- "100m"
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
  mutate(ab2010 = ab2010 + 1)



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
    y_fit <- temp_fit / filter(temp, gr == i)$ab1982
    y_dat <- filter(temp, gr == i)$ab2010 / filter(temp, gr == i)$ab1982

    SS <- (y_dat - mean(y_dat))^2 %>% mean

    PREDS <- (y_fit - y_dat)^2 %>% mean
    res_cv[i] <- 1 - PREDS/SS
    SS_cv[i] <- SS
    PREDS_cv[i] <- PREDS

  }
  data.frame(res_cv, SS_cv, PREDS_cv)
}

# sfExport("rand_func", "r2.func")

before <- proc.time()
res_cv <- NULL
res_cv_re <- NULL
for (i in 1:length(site_lev)){
# for (i in 1:10){
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

  res_obs2 <- stepAIC(res_obs)

  cv_dat <- my_cv2(temp_dat, res_obs2, K = 10)
  # r2_obs[i] <- r2.func(res_obs2, temp_dat)
  # r2_temp <- NULL
  # temp_lhs <- res_obs2$terms %>% .[[3]] %>% paste %>% .[2]
  # lhs <- paste("ab2010 ~ ", temp_lhs, " + offset(log(ab1982))", sep = "") %>% noquote
  res_cv[i] <- mean(cv_dat$res_cv)
  res_cv_re[i] <- 1 - mean(cv_dat$PREDS_cv) / mean(cv_dat$SS_cv)

  # sfExport("lhs", "temp_dat")
  # for (j in 1:99){
  #   # rand_dat <- temp_dat
  #   # rand_dat$WSG <- sample(rand_dat$WSG)
  #   # rand_dat$slope <- sample(rand_dat$slope)
  #   # rand_dat$convex <- sample(rand_dat$convex)
  #   # rand_dat$moist <- sample(rand_dat$moist)
  #   temp1 <- temp_dat %>%
  #     dplyr::select(sp, ab1982, ab2010)
  #   temp2 <- temp_dat %>%
  #     dplyr::select(sp, WSG, moist, convex, slope)
  #
  #   temp1$sp <- sample(temp1$sp) %>% as.character
  #
  #   rand_dat <- full_join(temp1, temp2, by = "sp")
  #   system.time(res_rand <- glm.nb(lhs, data = rand_dat))
  #   r2_temp[j] <- r2.func(res_rand, rand_dat)
  # }
  # system.time(r2_temp <- sapply(1:99, function(x)rand_func(lhs)))
  # r2_temp <- sfSapply(1:999, function(x)rand_func(lhs))
  # rand_r2 <- rbind(rand_r2, r2_temp)
}

# system.time(sapply(1:99, function(x)rand_func()))

after <- proc.time()

after - before
write.csv(res_cv, paste(plot_size, ".csv", sep = ""))
write.csv(res_cv_re, paste(plot_size, "_re.csv", sep = ""))
save.image(save_name)

# cv_apply <- function(i){
#   temp_dat <- filter(ab_t_data2, site == site_lev[i])
#   system.time(res_obs <- glm.nb(ab2010 ~WSG
#               + moist
#               + convex
#               + slope
#               + WSG:moist
#               + WSG:convex
#               + WSG:slope
#               + convex:moist
#               + slope:moist
#               + convex:slope
#               # + WSG:convex:moist
#               # + WSG:slope:moist
#               # + WSG:convex:slope
#               # + moist:convex:slope
#               + offset(log(ab1982)),
#               data = temp_dat))
#
#   res_obs2 <- stepAIC(res_obs)
#
#   cv_dat <- my_cv2(temp_dat, res_obs2, K = 10)
#   # r2_obs[i] <- r2.func(res_obs2, temp_dat)
#   # r2_temp <- NULL
#   # temp_lhs <- res_obs2$terms %>% .[[3]] %>% paste %>% .[2]
#   # lhs <- paste("ab2010 ~ ", temp_lhs, " + offset(log(ab1982))", sep = "") %>% noquote
#   mean(cv_dat$res_cv)
# }
#
# system.time(moge <- sapply(1:10, cv_apply))
# sfExport("lhs", "temp_dat")


# (r2_obs - apply(rand_r2, 1, mean)) / apply(rand_r2, 1, sd)
