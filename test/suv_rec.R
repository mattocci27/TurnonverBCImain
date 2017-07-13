

rm(list = ls()) # This clears everything from memory.

library(dplyr)
library(ggplot2)
library(cowplot)

library(rstan)
options(mc.cores = 3)

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")

temp <- D %>% group_by(sp) %>%
  summarize(N1 = sum(a1),
            N2 = sum(a2),
            D2 = sum(d2),
            R2 = sum(r2)) %>%
  mutate(A2 = N2 - R2) %>%
  mutate(mort = D2/N1) %>%
	mutate(rec = R2/N1) 


trait_temp <- data_frame(
    sp = rownames(trait),
    moist=trait$Moist,
    slope=trait$sp.slope.mean,
    convex=trait$sp.convex.mean,
    WSG=trait$WSG)


dat <- full_join(temp, trait_temp, by = "sp")

model <- "
  data{
    int<lower=0> n_sample;
    int<lower=0> N1[n_sample];
    int<lower=0> A2[n_sample];
    int<lower=0> N2[n_sample];
    int<lower=0> D2[n_sample];
    int<lower=0> R2[n_sample];
    vector[n_sample] moist;
    vector[n_sample] slope;
    vector[n_sample] convex;
    vector[n_sample] wsg;
  }
  parameters{
	  vector[5] beta;
	  vector[5] gamma;
  }
  transformed parameters{
    vector[n_sample] alpha;
    vector[n_sample] log_lambda;
    
    for (i in 1:n_sample){
    alpha[i] = beta[1] 
        + beta[2] * moist[i]
        + beta[3] * slope[i]
        + beta[4] * convex[i]
        + beta[5] * wsg[i];
    
    log_lambda[i] = gamma[1] 
        + gamma[2] * moist[i]
        + gamma[3] * slope[i]
        + gamma[4] * convex[i]
        + gamma[5] * wsg[i];
    }
  }
  model{
    vector[n_sample] logLik1;
    vector[n_sample] logLik2;
    for (i in 1:n_sample){
      logLik1[i] = binomial_logit_lpmf(A2[i] | N1[i], alpha[i]);
      logLik2[i] = poisson_log_lpmf(R2[i] | log(N1[i]) + log_lambda[i]);
			target += logLik1[i] + logLik2[i];
    }
    beta ~ normal(0, 10);
    gamma ~ normal(0, 10);
  }
"


#model <- "
#  data{
#    int<lower=0> n_sample;
#    int<lower=0> N1[n_sample];
#    int<lower=0> A2[n_sample];
#    int<lower=0> D2[n_sample];
#    int<lower=0> R2[n_sample];
#  }
#  parameters{
#		real alpha;
#    real lambda;
#  }
#  model{
#    vector[n_sample] logLik1;
#    vector[n_sample] logLik2;
#    for (i in 1:n_sample){
#      logLik1[i] = binomial_logit_lpmf(A2[i] | N1[i], alpha);
#      logLik2[i] = poisson_log_lpmf(R2[i] | log(N1[i]) + lambda);
#		
#			target += logLik1[i] + logLik2[i];
#    }
#  }
#"

#model <- "
#  data{
#    int<lower=0> n_sample;
#    int<lower=0> N1[n_sample];
#    int<lower=0> A2[n_sample];
#  }
#  parameters{
#		real alpha;
#  }
#  model{
#    vector[n_sample] logLik1;
#    for (i in 1:n_sample){
#      logLik1[i] = binomial_logit_lpmf(A2[i] | N1[i], alpha);
#		
#			target += logLik1[i];
#    }
#  }
#"
dat <- na.omit(dat) %>% as_data_frame

list_data <- list(n_sample = nrow(dat),
                  N1 = dat$N1,
                  N2 = dat$N2,
                  D2 = dat$D2,
                  R2 = dat$R2,
                  A2 = dat$A2,
                  moist = dat$moist,
                  slope = dat$slope,
                  convex = dat$convex,
                  wsg = dat$WSG)

system.time(fit <- stan(model_code = model,
                        data = list_data,
                        iter = 1,
                        warmup = 0,
                        thin = 1,
                        chains = 1))

n_iter <- 200
n_warm <- 100
n_thin <- 1
n_chains <- 3

system.time(res <- stan(fit = fit,
          data = list_data,
          iter = n_iter,
          warmup = n_warm,
          thin = n_thin,
          chains = n_chains,
          control = list(adapt_delta = 0.8, max_treedepth = 10)))


fit_summary <- summary(res)$summary

fit_summary


alpha_vec <- paste("alpha[", 1:nrow(dat), "]" ,sep = "")
lambda_vec <- paste("log_lambda[", 1:nrow(dat), "]" ,sep = "")

dat2 <- dat %>%
  mutate(alpha = fit_summary[alpha_vec, "mean"]) %>%
  mutate(log_lambda = fit_summary[lambda_vec, "mean"]) %>%
  mutate(suv  = 1/(1 + exp(-alpha))) %>%
  mutate(obs_suv = 1 - (D2/N1)) %>%
  mutate(rec = exp(log_lambda)) %>%
  mutate(obs_rec = R2/N1)

plot(obs_suv ~ WSG, dat2)
points(suv ~ WSG, dat2, pch = 16)

plot(obs_suv ~ suv, dat2 %>% filter(suv > 0.85), log = "xy")
plot(obs_suv ~ suv, dat2, log = "xy")

plot(obs_rec ~ rec, dat2, log = "xy")

plot(obs_rec ~ WSG, dat2)
points(rec ~ WSG, dat2, pch = 16)


plot(suv ~ WSG, dat2, pch = 16)

model_n <- "
  data{
    int<lower=0> n_sample;
    int<lower=0> N1[n_sample];
    int<lower=0> A2[n_sample];
    int<lower=0> D2[n_sample];
    int<lower=0> R2[n_sample];
  }
  parameters{
		real<lower=0, upper=1> beta;
  }
  model{
    vector[n_sample] logLik1;
    vector[n_sample] logLik2;
    for (i in 1:n_sample){
      logLik1[i] = binomial_lpmf(A2[i] | N1[i], 1 - beta);
      logLik2[i] = poisson_lpmf(R2[i] | N1[i] * beta);
		
			target += logLik1[i] + logLik2[i];
    }
  }
"
system.time(fit_n <- stan(model_code = model_n,
                        data = list_data,
                        iter = 1,
                        warmup = 0,
                        thin = 1,
                        chains = 1))

system.time(res_n <- stan(fit = fit_n,
          data = list_data,
          iter = n_iter,
          warmup = n_warm,
          thin = n_thin,
          chains = n_chains,
          control = list(adapt_delta = 0.8, max_treedepth = 10)))

n_sm <- summary(res_n)$summary


