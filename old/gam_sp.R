setwd("~/Dropbox/BCI_Turnover")

load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")

library(mgcv)
library(MASS)
# library(snowfall)
library(cvTools)

ab_data <- as.data.frame(sapply(D100m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)

trait.temp <- data.frame(sp=rownames(trait),
  moist=trait$Moist,
  slope=trait$sp.slope.mean,
  slope.sd = trait$sp.slope.sd,
  concav= -trait$sp.convex.mean,
  WSG=trait$WSG)

ab_t_data2 <- merge(ab_data,trait.temp,by="sp") %>% na.omit()


res_cv <- NULL
SS <- NULL
PREDS <- NULL
K <- 10
r2_glm0 <- NULL

data <- ab_t_data2

set.seed(615)
for (j in 1:1){ # one ten folds
  temp <- cvFolds(nrow(data), K = 10, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(data, temp)
  for (i in 1:K){
    #res_temp <- glm.nb(census_2010 ~ WSG
    #        + concav
    #        + slope
    #        + WSG:slope
    #        + offset(log(census_1982)),
    #        temp %>% filter(gr != i))

    res_temp <- gam(census_2010 ~ s(WSG, k=4)
            + s(concav, k = 4)
            + s(slope, k=4)
            + s(moist, k=4)
            + offset(log(census_1982)),
            data = temp %>% filter(gr != i))

    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(res_temp, test_dat) #%>% exp # link
    fitted2 <- fitted / test_dat$census_1982
    yy <- test_dat$census_2010 / test_dat$census_1982

    SS[i] <- (yy - mean(yy, na.rm = T))^2 %>% mean
    PREDS[i] <- (yy - fitted2)^2 %>% mean(na.rm = T)
  }

  r2_glm0[j] <- 1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)

}

mean_ <-mean(1 - PREDS/SS)
se_ <- sd(1 - PREDS/SS) / sqrt(10)
mean_
mean_ - se_ * 1.96
mean_ + se_ * 1.96



    res_temp <- gam(census_2010 ~ s(WSG, k=4)
            + s(concav, k = 4)
            + s(slope, k=4)
            + s(WSG:slope, k=4)
            + offset(log(census_1982)),
            data = temp)
