rm(list = ls()) # This clears everything from memory.
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores())

options(mc.cores = 3)
# setwd("~/Dropbox/BCI_Turnover")

# load("/scratch/lfs/mattocci27/BCI_Turnover/BCI_turnover20141213.RData")
load("~/Dropbox/BCI_Turnover/BCI_turnover20141213.RData")

ab.data <- as.data.frame(sapply(D20m,function(x)apply(x,2,sum)))
ab.data$sp <- rownames(ab.data)


trait.temp <- data.frame(sp=rownames(trait),
           moist=trait$Moist,
           slope=trait$sp.slope.mean,
           slope.sd = trait$sp.slope.sd,
           convex=trait$sp.convex.mean,
           convex.sd=trait$sp.convex.sd,
           WSG=trait$WSG,
           slope10=trait$slope_size_10,
           slope20=trait$slope_size_20,
           slope30=trait$slope_size_30,
           slope40=trait$slope_size_40,
           slope50=trait$slope_size_50,
           slope60=trait$slope_size_60,
           slope70=trait$slope_size_70,
           slope80=trait$slope_size_80,
           slope90=trait$slope_size_90,
           slope100=trait$slope_size_100,
           convex10=trait$convex_size_10,
           convex20=trait$convex_size_20,
           convex30=trait$convex_size_30,
           convex40=trait$convex_size_40,
           convex50=trait$convex_size_50,
           convex60=trait$convex_size_60,
           convex70=trait$convex_size_70,
           convex80=trait$convex_size_80,
           convex90=trait$convex_size_90,
           convex100=trait$convex_size_100)

ab.t.data <- merge(ab.data,trait.temp,by="sp")

ab.t.data2 <- na.omit(ab.t.data)

# temp.data<-D[D$status1=="A",]

ab.t.data2$inc <- as.factor(ifelse(ab.t.data2$census_2010>ab.t.data2$census_1982,"UP","Down"))



model <-"
     data{
          int<lower=0> n_sample;
          vector[n_sample] WSG;
          vector[n_sample] moist;
          vector[n_sample] slope;
          vector[n_sample] convex;
          vector[n_sample] moist_int;
          vector[n_sample] slope_int;
          vector[n_sample] convex_int;
          vector[n_sample] relative_ab;
          int y[n_sample];
          # int census_1982[n_sample];
          vector[n_sample] census_1982;

     }

     parameters{
       vector[8] beta;
       real<lower=0> phi;
     }

     transformed parameters {
          vector<lower=0>[n_sample] mu;
                mu<- exp(beta[1]
                  + beta[2] * WSG
                  + beta[3] * moist
                  + beta[4] * slope
                  + beta[5] * convex
                  + beta[6] * moist_int
                  + beta[7] * slope_int
                  + beta[8] * convex_int
                  + log(census_1982));

     }
     model{
        real temp1;
        real temp2;
        real temp3;
        real temp_like;

        for (i in 1:n_sample){
             temp1 <- lgamma(y[i] + 1/phi) - lgamma(1/phi) - lgamma(y[i]+1);
             temp2 <- -(1/phi)*log(1+mu[i]*phi);
             temp3 <- y[i]*log(mu[i]/(1/phi + mu[i]));
             //temp_like <- (temp1+temp2+temp3)*(1-relative_ab[i]);
             temp_like <- (temp1+temp2+temp3)*relative_ab[i]*100;
             increment_log_prob(temp_like);
        }

        phi ~ cauchy(0, 5);
        beta ~ normal(0, 10);
     }
"

model2 <-"
     data{
      int<lower=0> n_sample;
      vector[n_sample] WSG;
      vector[n_sample] moist;
      vector[n_sample] slope;
      vector[n_sample] convex;
      vector[n_sample] moist_int;
      vector[n_sample] slope_int;
      vector[n_sample] convex_int;
      vector[n_sample] relative_ab;
      int y[n_sample];
      # int census_1982[n_sample];
      vector[n_sample] census_1982;

     }

     parameters{
       vector[8] beta;
       real<lower=0> phi;
     }

     transformed parameters {
        vector<lower=0>[n_sample] mu;
              mu<- exp(beta[1]
                + beta[2] * WSG
                + beta[3] * moist
                + beta[4] * slope
                + beta[5] * convex
                + beta[6] * moist_int
                + beta[7] * slope_int
                + beta[8] * convex_int
                + log(census_1982));

     }
     model{
       y ~ NegBinomial2(mu, phi);
       phi ~ cauchy(0, 5);
     }
"


list.data_log <- list(n_sample=nrow(ab.t.data2),
                  WSG=as.numeric(scale(ab.t.data2$WSG)),
                  moist=as.numeric(scale(ab.t.data2$moist)),
                  slope=as.numeric(scale(ab.t.data2$slope)),
                  convex=as.numeric(scale(ab.t.data2$convex)),
                  moist_int = as.numeric(scale(ab.t.data2$moist * ab.t.data2$WSG)),
                  slope_int = as.numeric(scale(ab.t.data2$slope * ab.t.data2$WSG)),
                  convex_int = as.numeric(scale(ab.t.data2$convex * ab.t.data2$WSG)),
                  y=ab.t.data2$census_2010,
                  census_1982=ab.t.data2$census_1982,
                  relative_ab = log(ab.t.data2$census_1982)/sum(log(ab.t.data2$census_1982)))

list.data <- list(n_sample=nrow(ab.t.data2),
                  WSG=as.numeric(scale(ab.t.data2$WSG)),
                  moist=as.numeric(scale(ab.t.data2$moist)),
                  slope=as.numeric(scale(ab.t.data2$slope)),
                  convex=as.numeric(scale(ab.t.data2$convex)),
                  moist_int = as.numeric(scale(ab.t.data2$moist * ab.t.data2$WSG)),
                  slope_int = as.numeric(scale(ab.t.data2$slope * ab.t.data2$WSG)),
                  convex_int = as.numeric(scale(ab.t.data2$convex * ab.t.data2$WSG)),
                  y=ab.t.data2$census_2010,
                  census_1982=ab.t.data2$census_1982,
                  relative_ab = ab.t.data2$census_1982/sum(ab.t.data2$census_1982))


ab.t.data3 = ab.t.data2[ab.t.data2$census_1982>100,]

list.data_ori <- list(n_sample=nrow(ab.t.data2),
                  WSG=as.numeric(scale(ab.t.data2$WSG)),
                  moist=as.numeric(scale(ab.t.data2$moist)),
                  slope=as.numeric(scale(ab.t.data2$slope)),
                  convex=as.numeric(scale(ab.t.data2$convex)),
                  moist_int = as.numeric(scale(ab.t.data2$moist * ab.t.data2$WSG)),
                  slope_int = as.numeric(scale(ab.t.data2$slope * ab.t.data2$WSG)),
                  convex_int = as.numeric(scale(ab.t.data2$convex * ab.t.data2$WSG)),
                  y=ab.t.data2$census_2010,
                  census_1982=ab.t.data2$census_1982,
                  relative_ab =rep(1/nrow(ab.t.data2),nrow(ab.t.data2)))



system.time(fit_ori <- stan(model_code = model,
            data=list.data_ori,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit_nb <- stan(model_code = model,
            data=list.data_ori,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit <- stan(model_code = model,
            data=list.data,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))


system.time(fit_log<- stan(model_code = model,
            data=list.data_log,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit2_ori<- stan(fit = fit_ori,
            data = list.data_ori,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 3))

system.time(fit2_nb<- stan(fit = fit_nb,
            data = list.data_ori,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 3))


system.time(fit2<- stan(fit = fit,
            data = list.data,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 3))


system.time(fit2_log<- stan(fit = fit_log,
            data = list.data_log,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 3))

fit.summary <- data.frame(summary(fit2)$summary)
fit.summary.log <- data.frame(summary(fit2_log)$summary)
fit.summary.ori <- data.frame(summary(fit2_ori)$summary)
fit.summary.nb <- data.frame(summary(fit2_nb)$summary)

#
# 1 - sum((list.data$y - fit.summary.ori[10:211,"mean"])^2) /sum( (list.data$y - mean(list.data$census_1982)) ^2)
#
# plot(list.data$y , fit.summary.ori[10:211,"mean"],log="xy")

# plot(WSG_slope2~WSG_slope,ab.t.data2)

# save.image("/scratch/lfs/mattocci27/BCI_Turnover/NB_res20160629_replace.RData")
save.image("~/Dropbox/BCI_Turnover/NB_res20160629_replace.RData")
