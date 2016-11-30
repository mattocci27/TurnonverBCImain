# CV sp
# every censuss
rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")
library(dplyr)
library(cvTools)
library(MASS)
ab_data <- as.data.frame(sapply(D100m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)

temp <- data.frame(sp=rownames(trait),
  moist=trait$Moist,
  slope=trait$sp.slope.mean,
  slope.sd = trait$sp.slope.sd,
  concave= - trait$sp.convex.mean, # concave
  convex.sd=trait$sp.convex.sd,
  WSG=trait$WSG)

ab_t_data <- merge(ab_data, temp,by="sp")


cor.test(ab_t_data$WSG, ab_t_data$moist, method = "spearman")
cor.test(ab_t_data$WSG, ab_t_data$concave, method = "spearman")
cor.test(ab_t_data$WSG, ab_t_data$slope, method = "spearman")

cor.test(ab_t_data$moist, ab_t_data$concave, method = "spearman")
cor.test(ab_t_data$moist, ab_t_data$slope, method = "spearman")

cor.test(ab_t_data$concave, ab_t_data$slope, method = "spearman")
