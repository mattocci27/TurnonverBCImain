rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")

library(dplyr)
library(ggplot2)
library(nnet)

ab_data <- as.data.frame(sapply(D20m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)

trait.temp <- data.frame(sp=rownames(trait),
  moist=trait$Moist,
  slope=trait$sp.slope.mean,
  slope.sd = trait$sp.slope.sd,
  convex=trait$sp.convex.mean,
  convex.sd=trait$sp.convex.sd,
  WSG=trait$WSG)

ab_t_data <- merge(ab_data,trait.temp,by="sp")

ab_t_data2 <- na.omit(ab_t_data)


ab_t_data2
temp1 <- ab_t_data[1:150, -1] %>% na.omit
temp2 <- ab_t_data[151:312, -1] %>% na.omit

temp_nnet <- nnet(census_2010 ~ census_1982 + moist + slope + convex + WSG, size = 5, data = temp1, lineout = TRUE)

temp_pred <- predict(temp_nnet, temp2)


predict(temp_nnet)


xx <- rnorm(100)
xx1 <- rnorm(100)
yy <- rnorm(100, 2 * xx - xx1 - 1)



dat <- data.frame(xx,xx1, yy)
temp1 <- dat[1:50,]
temp1 <- rbind(temp1, temp1)
temp2 <- dat[51:100,]

temp_nnet <- nnet(yy ~ xx + xx1, size = 2,
    data = temp1, lineout = T, skip = T,
    decay=1e-3, Hess = T)

predict(temp_nnet)

temp_pred <- predict(temp_nnet, temp2)

plot(temp2[,"yy"], temp_pred[,1])


rock.nn <- nnet(log(perm) ~ area + peri + shape, rock1,
                size=3, decay=1e-3, linout=T, skip=T, maxit=1000, Hess=T)


predict(rock.nn)
