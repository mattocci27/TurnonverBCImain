rm(list = ls()) # This clears everything from memory.

# setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")

library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 3)

str(D20m)
str(D100m)

argv <- commandArgs(trailingOnly = TRUE)
n_model <- argv[1]
plot_size <- argv[2]
# n_model <- "model5"
# plot_size <- "100m"
save_name <- paste(n_model, plot_size,"simple", sep = "_")


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

model1 <- '
  data{
    int<lower=0> n_sample;
    int<lower=0> n_para;
    int<lower=0> ab2010[n_sample];
    int<lower=0> n_site;
    int site[n_sample];
    vector[n_sample] ab1982;
    row_vector[n_para] TR[n_sample];
  }
  parameters{
    vector[n_para] beta;
    real<lower=0> phi;
    vector[n_site] r;
    real<lower=0> sigma;
  }
  transformed parameters{
    vector<lower=0>[n_sample] mu;
    for (i in 1:n_sample) mu[i] = exp(dot_product(beta, TR[i, ]) + r[site[i]] + log(ab1982[i]));
  }
  model{
    ab2010 ~ neg_binomial_2(mu, phi);
    phi ~ cauchy(0, 2.5);
    beta ~ normal(0, 10);
    r ~ normal(0, sigma);
    sigma ~ cauchy(0, 2.5);
  }
  generated quantities{
    vector[n_sample] log_lik;
    for (i in 1:n_sample)
      log_lik[i] = neg_binomial_2_lpmf(ab2010[i]| mu[i], phi);
  }
'


ab_t_data2 <- ab_t_data %>%
  filter(ab1982 != 0 | ab2010 != 0) %>%
  mutate(ab1982 = ab1982 + 1) %>%
  mutate(ab2010 = ab2010 + 1)

list_data <- list(n_sample = nrow(ab_t_data2),
  n_site = length(unique(as.character(ab_t_data2$site))),
  site = as.numeric(ab_t_data2$site),
  WSG = as.numeric(scale(ab_t_data2$WSG)),
  moist = as.numeric(scale(ab_t_data2$moist)),
  slope = as.numeric(scale(ab_t_data2$slope)),
  convex = as.numeric(scale(ab_t_data2$convex)),
  ab1982 = ab_t_data2$ab1982,
  ab2010 = ab_t_data2$ab2010
  )

list_data$WSG_moist <- list_data$WSG * list_data$moist
list_data$WSG_convex <- list_data$WSG * list_data$convex
list_data$WSG_slope <- list_data$WSG * list_data$slope

list_data$slope_moist <- list_data$slope * list_data$moist
list_data$slope_convex <- list_data$slope * list_data$convex
list_data$moist_convex <- list_data$moist * list_data$convex

list_data$int_WSG <- list_data$moist * list_data$convex * list_data$slope
list_data$int_slope <- list_data$moist * list_data$convex * list_data$WSG
list_data$int_convex <- list_data$moist * list_data$WSG * list_data$slope
list_data$int_moist <- list_data$WSG * list_data$convex * list_data$slope
list_data$int_all <- list_data$int_moist * list_data$moist

list_data$TR <- cbind(rep(1, nrow(ab_t_data2)),
  list_data$WSG,
  list_data$moist,
  list_data$slope,
  list_data$convex)

list_data$TR2 <- cbind(list_data$TR,
  list_data$WSG * list_data$moist,
  list_data$WSG * list_data$convex,
  list_data$WSG * list_data$slope,
  list_data$slope * list_data$moist,
  list_data$slope * list_data$convex,
  list_data$moist * list_data$convex)

list_data$TR3 <- cbind(list_data$TR2,
  list_data$moist * list_data$convex * list_data$slope,
  list_data$moist * list_data$convex * list_data$WSG,
  list_data$moist * list_data$WSG * list_data$slope,
  list_data$WSG * list_data$convex * list_data$slope)

list_data$TR4 <- cbind(list_data$TR3,
  list_data$int_moist * list_data$moist)

list_data$n_para <- 5

if(n_model == "model2"){
  list_data$TR <- list_data$TR2
  list_data$n_para <- 11
} else if (n_model == "model3") {
  list_data$TR <- list_data$TR3
  list_data$n_para <- 15
} else if (n_model == "model4") {
  list_data$TR <- list_data$TR4
  list_data$n_para <- 16
}


str(list_data) %>% print

system.time(fit <- stan(model_code = model1,
            data = list_data,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(res <- stan(fit = fit,
            data = list_data,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 3,
            control = list(stepsize = 0.01,
                  adapt_delta = 0.9,
                  max_treedepth = 10)))


fit.summary <- data.frame(summary(res)$summary)

save.image(paste(save_name, ".RData", sep = ""))
