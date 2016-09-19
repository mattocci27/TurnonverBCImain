rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")

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
n_model <- "model5"
plot_size <- "100m"
save_name <- paste(n_model, plot_size, sep = "_")


print(n_model)
print(plot_size)
print(save_name)

n_model <- noquote(n_model)
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
    int<lower=0> ab2010[n_sample];
    int<lower=0> n_site;
    int site[n_sample];
    vector[n_sample] moist;
    vector[n_sample] ab1982;
    vector[n_sample] slope;
    vector[n_sample] convex;
    vector[n_sample] WSG;
  }
  parameters{
    vector[5] beta;
    real<lower=0> phi;
    vector[5] r[n_site];
    real<lower=0> sigma;
  }
  transformed parameters{
    vector<lower=0>[n_sample] mu;
    for (i in 1:n_sample) mu[i] = exp((beta[1] + r[site[i]][1])
      + (beta[2] + r[site[i]][2]) * WSG[i]
      + (beta[3] + r[site[i]][3]) * moist[i]
      + (beta[4] + r[site[i]][4]) * slope[i]
      + (beta[5] + r[site[i]][5]) * convex[i]
      + log(ab1982[i]));
  }
  model{
    ab2010 ~ neg_binomial_2(mu, phi);
    phi ~ cauchy(0, 2.5);
    beta ~ normal(0, 10);
    for (i in 1:5) r[i] ~ normal(0, sigma);
    sigma ~ cauchy(0, 2.5);

  }
  generated quantities{
    vector[n_sample] log_lik;
    for (i in 1:n_sample)
      log_lik[i] = neg_binomial_2_lpmf(ab2010[i]| mu[i], phi);
  }
'

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
    vector[5] beta;
    real<lower=0> phi;
    vector[5] r[n_site];
    real<lower=0> sigma;
  }
  transformed parameters{
    vector<lower=0>[n_sample] mu;
    for (i in 1:n_sample) mu[i] = exp(dot_product(beta + r[site[i]], TR[i, ]) + log(ab1982[i]));
  }
  model{
    ab2010 ~ neg_binomial_2(mu, phi);
    phi ~ cauchy(0, 2.5);
    beta ~ normal(0, 10);
    for (i in 1:5) r[i] ~ normal(0, sigma);
    sigma ~ cauchy(0, 2.5);
  }
  generated quantities{
    vector[n_sample] log_lik;
    for (i in 1:n_sample)
      log_lik[i] = neg_binomial_2_lpmf(ab2010[i]| mu[i], phi);
  }
'


model2 <- '
  data{
    int<lower=0> n_sample;
    //int<lower=0> ab1982[n_sample];
    int<lower=0> ab2010[n_sample];
    int<lower=0> n_site;
    int site[n_sample];
    vector[n_sample] moist;
    vector[n_sample] ab1982;
    vector[n_sample] slope;
    vector[n_sample] convex;
    vector[n_sample] WSG;
    vector[n_sample] WSG_moist;
    vector[n_sample] WSG_slope;
    vector[n_sample] WSG_convex;
    vector[n_sample] slope_moist;
    vector[n_sample] slope_convex;
    vector[n_sample] moist_convex;
  }
  parameters{
    vector[11] beta;
    real<lower=0> phi;
    vector[n_site] r;
    real<lower=0> sigma;
  }
  transformed parameters{
    vector<lower=0>[n_sample] mu;
    mu = exp(beta[1] + r[site[i]][1]
      + (beta[2] + r[site[i]][2]) * WSG
      + (beta[3] + r[site[i]][3]) * moist
      + (beta[4] + r[site[i]][4]) * slope
      + (beta[5] + r[site[i]][5]) * convex
      + (beta[6] + r[site[i]][6]) * WSG_moist
      + (beta[7] + r[site[i]][7]) * WSG_slope
      + (beta[8] + r[site[i]][8]) * WSG_convex
      + (beta[9] + r[site[i]][9]) * slope_moist
      + (beta[10] + r[site[i]][10]) * slope_convex
      + (beta[11] + r[site[i]][11]) * moist_convex
      + log(ab1982)
      + r[site]);
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

model3 <- '
  data{
    int<lower=0> n_sample;
    //int<lower=0> ab1982[n_sample];
    int<lower=0> ab2010[n_sample];
    int<lower=0> n_site;
    int site[n_sample];
    vector[n_sample] moist;
    vector[n_sample] ab1982;
    vector[n_sample] slope;
    vector[n_sample] convex;
    vector[n_sample] WSG;
    vector[n_sample] WSG_moist;
    vector[n_sample] WSG_slope;
    vector[n_sample] WSG_convex;
    vector[n_sample] slope_moist;
    vector[n_sample] slope_convex;
    vector[n_sample] moist_convex;
    vector[n_sample] int_WSG;
    vector[n_sample] int_convex;
    vector[n_sample] int_slope;
    vector[n_sample] int_moist;
  }
  parameters{
    vector[15] beta;
    real<lower=0> phi;
    vector[n_site] r;
    real<lower=0> sigma;
  }
  transformed parameters{
    vector<lower=0>[n_sample] mu;
    mu = exp(beta[1]
      + beta[2] * WSG
      + beta[3] * moist
      + beta[4] * slope
      + beta[5] * convex
      + beta[6] * WSG_moist
      + beta[7] * WSG_slope
      + beta[8] * WSG_convex
      + beta[9] * slope_moist
      + beta[10] * slope_convex
      + beta[11] * moist_convex
      + beta[12] * int_WSG
      + beta[13] * int_moist
      + beta[14] * int_slope
      + beta[15] * int_convex
      + log(ab1982)
      + r[site]);
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

model4 <- '
  data{
    int<lower=0> n_sample;
    //int<lower=0> ab1982[n_sample];
    int<lower=0> ab2010[n_sample];
    int<lower=0> n_site;
    int site[n_sample];
    vector[n_sample] moist;
    vector[n_sample] ab1982;
    vector[n_sample] slope;
    vector[n_sample] convex;
    vector[n_sample] WSG;
    vector[n_sample] WSG_moist;
    vector[n_sample] WSG_slope;
    vector[n_sample] WSG_convex;
    vector[n_sample] slope_moist;
    vector[n_sample] slope_convex;
    vector[n_sample] moist_convex;
    vector[n_sample] int_WSG;
    vector[n_sample] int_convex;
    vector[n_sample] int_slope;
    vector[n_sample] int_moist;
    vector[n_sample] int_all;
  }
  parameters{
    vector[16] beta;
    real<lower=0> phi;
    vector[n_site] r;
    real<lower=0> sigma;
  }
  transformed parameters{
    vector<lower=0>[n_sample] mu;
    mu = exp(beta[1]
      + beta[2] * WSG
      + beta[3] * moist
      + beta[4] * slope
      + beta[5] * convex
      + beta[6] * WSG_moist
      + beta[7] * WSG_slope
      + beta[8] * WSG_convex
      + beta[9] * slope_moist
      + beta[10] * slope_convex
      + beta[11] * moist_convex
      + beta[12] * int_WSG
      + beta[13] * int_moist
      + beta[14] * int_slope
      + beta[15] * int_convex
      + beta[16] * int_all
      + log(ab1982)
      + r[site]);
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

model5 <- '
  data{
    int<lower=0> n_sample;
    int<lower=0> n_site;
    int<lower=0> n_para;
    int<lower=0> ab2010[n_sample];
    row_vector[n_para] TR[n_sample];
    vector[n_sample] ab1982;
    int site[n_sample];
  }
  parameters{
    vector[n_para] beta[n_site];
    vector[n_para] gamma;
    vector<lower=0>[n_para] L_sigma;
    real<lower=0> phi;
    cholesky_factor_corr[n_para] L_Omega;
  }
  transformed parameters{
    vector[n_sample] mu;
    vector[n_para] mu_beta[n_site];
    for (i in 1:n_sample){
      mu[i] = exp(dot_product(beta[site[i]], TR[i, ]) + log(ab1982[i]));
    }
    for (i in 1:n_para){
     for (j in 1:n_site){
       mu_beta[j, i] <- dot_product(gamma[i, ], PRE[j, ]);
     }
   }
  }
  model{
    ab2010 ~ neg_binomial_2(mu, phi);
    beta ~ multi_normal_cholesky(mu_beta, diag_pre_multiply(L_sigma, L_Omega));
    gamma ~ normal(0, 10); //vector
    L_Omega ~ lkj_corr_cholesky(1); //uniform of L_Omega * L_Omega`
    L_sigma ~ cauchy(0, 2.5);
    phi ~ cauchy(0, 10);
  }
'

ab_t_data2 <- ab_t_data %>% filter(ab1982 > 0)

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

list_data$n_para <- 5

str(list_data) %>% print

system.time(fit <- stan(model_code = get(n_model),
            data = list_data,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(res <- stan(fit = fit,
            data = list_data,
            iter = 20,
            warmup = 10,
            thin = 1,
            chains = 3))

fit.summary <- data.frame(summary(res)$summary)

save.image(paste(save_name, ".RData", sep = ""))
