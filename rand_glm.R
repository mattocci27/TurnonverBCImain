rm(list = ls()) # This clears everything from memory.

# setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")

library(dplyr)
library(glmmADMB)

str(D20m)
str(D100m)

argv <- commandArgs(trailingOnly = TRUE)
n_model <- argv[1]
plot_size <- argv[2]
n_model <- "model2"
plot_size <- "100m"
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



r2.func = function(res, n){
  data = ab_t_data2
  y = data$ab2010/data$ab1982
  residuals = y - res$fitted/data$ab1982
  1 - sum(residuals^2)/sum((y-mean(y))^2)
}

system.time(res_all <- glmmadmb(ab2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + convex:slope
            + moist:slope
            + moist:convex
            + offset(log(ab1982))
            + (1 | site),
            family = "nbinom",
            data = ab_t_data2))

r2.func(res_all, n = 0)

moge2 <- NULL
for (i in 1:100){
  moge <- ab_t_data2
  moge$WSG <- sample(moge$WSG)
  moge$slope <- sample(moge$slope)
  moge$convex <- sample(moge$convex)
  moge$moist <- sample(moge$moist)

  res_all <- glmmadmb(ab2010 ~WSG
              + moist
              + convex
              + slope
              + WSG:moist
              + WSG:convex
              + WSG:slope
              + convex:slope
              + moist:slope
              + moist:convex
              + offset(log(ab1982))
              + (1 | site),
              family = "nbinom",
              data = moge)
  moge2[i] <- r2.func(res, n = 0)
}
