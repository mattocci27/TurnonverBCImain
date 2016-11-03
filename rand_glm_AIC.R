rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")

library(dplyr)
library(glmmADMB)

str(D20m)
str(D100m)

argv <- commandArgs(trailingOnly = TRUE)
plot_size <- argv[1]
# n_model <- "model2"
# plot_size <- "100m"
save_name <- paste(n_model, plot_size, sep = "_")

print(n_model)
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

N <- 50
subplot <- D100m

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
before <- proc.time()
for (i in 1:3){
  temp_dat <- filter(ab_t_data2, site == site_lev[i])
  system.time(res_obs <- glm.nb(ab2010 ~WSG
              + moist
              + convex
              + slope
              + WSG:moist
              + WSG:convex
              + WSG:slope
              + convex:slope
              + moist:slope
              + moist:convex
              + offset(log(ab1982)),
              data = temp_dat))

  r2_obs[i] <- r2.func(res_obs, temp_dat)
  r2_temp <- NULL
  for (j in 1:9){
    # rand_dat <- temp_dat
    # rand_dat$WSG <- sample(rand_dat$WSG)
    # rand_dat$slope <- sample(rand_dat$slope)
    # rand_dat$convex <- sample(rand_dat$convex)
    # rand_dat$moist <- sample(rand_dat$moist)
    temp1 <- temp_dat %>%
      dplyr::select(sp, ab1982, ab2010)
    temp2 <- temp_dat %>%
      dplyr::select(sp, WSG, moist, convex, slope)

    temp1$sp <- sample(temp1$sp) %>% as.character

    rand_dat <- full_join(temp1, temp2, by = "sp")
    system.time(res_rand <- glm.nb(ab2010 ~WSG
                + moist
                + convex
                + slope
                + WSG:moist
                + WSG:convex
                + WSG:slope
                + convex:slope
                + moist:slope
                + moist:convex
                + offset(log(ab1982)),
                data = rand_dat))
    r2_temp[j] <- r2.func(res_rand, rand_dat)
  }
  rand_r2 <- rbind(rand_r2, r2_temp)
}

after <- proc.time()

after - before


(r2_obs - apply(rand_r2, 1, mean)) / apply(rand_r2, 1, sd)

save.image(save_name)
