rm(list = ls()) # This clears everything from memory.

library(loo)
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 3)
load("model1_plot100m.RData")
waic(extract_log_lik(res,"log_lik"))$waic

load("model2_plot100m.RData")
waic(extract_log_lik(res,"log_lik"))$waic

load("model3_plot100m.RData")
waic(extract_log_lik(res,"log_lik"))$waic

load("model4_plot100m.RData")
waic(extract_log_lik(res,"log_lik"))$waic


load("model1_plot100m_simple.RData")
waic(extract_log_lik(res,"log_lik"))$waic

load("model2_plot100m_simple.RData")
waic(extract_log_lik(res,"log_lik"))$waic

load("model3_plot100m_simple.RData")
waic(extract_log_lik(res,"log_lik"))$waic

load("model4_plot100m_simple.RData")
waic(extract_log_lik(res,"log_lik"))$waic

# setwd("~/Dropbox/BCI_Turnover")
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
# n_model <- "model5"
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
    vector[n_para] r[n_site];
    vector<lower=0>[n_para] sigma;
  }
  transformed parameters{
    vector<lower=0>[n_sample] mu;
    for (i in 1:n_sample) mu[i] = exp(dot_product(beta + r[site[i]], TR[i, ]) + log(ab1982[i]));
  }
  model{
    ab2010 ~ neg_binomial_2(mu, phi);
    phi ~ cauchy(0, 2.5);
    beta ~ normal(0, 10);
    for (i in 1:n_para) r[i] ~ normal(0, sigma[i]);
    sigma ~ cauchy(0, 2.5);
  }
  generated quantities{
    vector[n_sample] log_lik;
    for (i in 1:n_sample)
      log_lik[i] = neg_binomial_2_lpmf(ab2010[i]| mu[i], phi);
  }
'


ab_t_data2 <- ab_t_data %>% filter(ab1982 > 0)
ab_t_data3 <- ab_t_data %>% filter(ab1982 != 0 | ab2010 != 0) %>%
  mutate(ab1982_2 = ab1982 + 1) %>%
  mutate(ab2010_2 = ab2010 + 1)

par(mfrow=c(1,2))
plot(ab2010/ab1982 ~ WSG, ab_t_data3, log = "y")
abline(h=1)
plot(ab2010_2/ab1982_2 ~ WSG, ab_t_data3, log = "y")
abline(h=1)


moge <- head(ab_t_data3, 50) %>% as.data.frame

ab_t_data %>% filter(ab1982 == 0 & ab2010 != 0) %>% summary




rnbinom(50, mu = 9, size = 100)
