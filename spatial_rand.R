rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")

library(dplyr)
library(MASS)
library(glmmADMB)

str(D20m)
str(D100m)

r2_func <- function(res, data){
  # data <- ab_t_data2[ab_t_data2$ab1982 >= n, ]
  data <- ab_t_data2
  y <- data$ab2010/data$ab1982
  residuals <- y - res$fitted/data$ab1982
  # residuals = y - exp(res$linear.predictors)/data$census_1982
  1 - sum(residuals^2) / sum((y - mean(y))^2)
}

as_data_frame(sapply(D100m, function(x)apply(x, 2, sum)))

moge2 <- NULL
for (j in 1:10){
  moge <- NULL
  ab_t_data <- NULL
  for (i in 1:50){
    ab_data <- data_frame(
      sp = colnames(D100m[[1]]),
      ab1982 = D100m[[1]][i,],
      ab2010 = D100m[[7]][i,])

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

  ab_t_data2n <- ab_t_data %>%
    filter(ab1982 != 0 | ab2010 != 0) %>%
    mutate(ab1982 = ab1982 + 1) %>%
    mutate(ab2010 = ab2010 + 1)


  system.time(res_all <- glmmadmb(ab2010 ~WSG
              + moist
              + convex
              + slope
              + WSG:moist
              + WSG:convex
              + WSG:slope
              + offset(log(ab1982))
              + (1 | site),
              family = "nbinom",
              data = ab_t_data2n))
    moge2[j] <- r2_func(res_all, ab_t_data2n)

}

obs_rank <- apply(rbind(r2_obs, moge3), 2, rank)[1,]
(1 - (obs_rank /100)) * 2 # one tail



system.time(res_all3 <- glmmadmb(ab2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + convex:moist
            + slope:convex
            + moist:slope
            + WSG:moist:convex
            + WSG:convex:slope
            + WSG:slope:moist
            + moist:convex:slope
            + offset(log(ab1982))
            + (1 | site),
            family = "nbinom",
            data = ab_t_data2,
            save.dir = "~/Desktop/momo3"))


res_all2 <- glmmadmb(ab2010 ~WSG
            + moist
            + convex
            + slope
            + offset(log(ab1982))
            + (1 | site),
            family = "nbinom",
            data = ab_t_data2,
            save.dir = "~/Desktop/momo2")

library(scales)
pdf("~/Dropbox/MS/TurnoverBCI/fig/glm_fig100.pdf", width = 6, height = 2)
  par(mfrow=c(1,4))
  par(mar=c(0,0,0,0))
  par(oma=c(3,4,2,2))

  plot(ab2010/ab1982 ~ WSG, ab_t_data, log = "y", axes = F, col=alpha("black", 0.2), pch = 16)
  abline(h=1,lty=2)
  box()
  axis(2,tick=FALSE,line=-0.8)
  axis(2,tcl=0.2, labels = FALSE)
  axis(1,tick=FALSE,line=-0.8)
  axis(1,tcl=0.2, labels = FALSE)
  mtext(expression(paste("Wood density (g ",cm^-3,")")),side=1, line=1.5,cex=0.8)
  mtext("No. of individuals in 2010 / \n No. of individuals in 1982",side=2, line=1.5,cex=0.8)

  plot(ab2010/ab1982 ~ moist, ab_t_data, log = "y", axes = F, col=alpha("black", 0.2), pch = 16)
  abline(h=1,lty=2)
  box()
  axis(1,tick=FALSE,line=-0.8)
  axis(1,tcl=0.2, labels = FALSE)
  mtext("Moisture",side=1, line=1.5,cex=0.8)

  plot(ab2010/ab1982 ~ convex, ab_t_data, log = "y", axes = F, col=alpha("black", 0.2), pch = 16)
  box()
  abline(h=1,lty=2)
  axis(1,tick=FALSE,line=-0.8)
  axis(1,tcl=0.2, labels = FALSE)
  mtext("Convexity (m)",side=1, line=1.5,cex=0.8)

  plot(ab2010/ab1982 ~ slope, ab_t_data, log = "y", axes = F, col=alpha("black", 0.2), pch = 16)
  abline(h=1,lty=2)
  box()
  axis(1,tick=FALSE,line=-0.8)
  axis(1,tcl=0.2, labels = FALSE)
  mtext("Slope (degrees)",side=1, line=1.5,cex=0.8)
  par(mfrow=c(1,1))
dev.off()




ab_t_data20 <- NULL
for (i in 1:1250){
  ab_data20 <- data_frame(
    sp = colnames(D20m[[1]]),
    ab1982 = D20m[[1]][i,],
    ab2010 = D20m[[7]][i,])

  trait_temp <- data_frame(
    sp = rownames(trait),
    moist=trait$Moist,
    slope=trait$sp.slope.mean,
    convex=trait$sp.convex.mean,
    WSG=trait$WSG)

  moge <- full_join(ab_data20, trait_temp, by = "sp") %>%
    na.omit %>%
    mutate(site = paste("subplot", i, sep = "_"))
  ab_t_data20 <- bind_rows(moge, ab_t_data20)
}

ab_t_data20$site <- as.factor(ab_t_data20$site)
plot(ab2010/ab1982 ~ WSG, ab_t_data20, log = "y")


pdf("~/Dropbox/MS/TurnoverBCI/fig/glm_fig20.pdf", width = 6, height = 2)
  par(mfrow=c(1,4))
  par(mar=c(0,0,0,0))
  par(oma=c(3,4,2,2))

  plot(ab2010/ab1982 ~ WSG, ab_t_data20, log = "y", axes = F, col=alpha("black", 0.2), pch = 16)
  abline(h=1,lty=2)
  box()
  axis(2,tick=FALSE,line=-0.8)
  axis(2,tcl=0.2, labels = FALSE)
  axis(1,tick=FALSE,line=-0.8)
  axis(1,tcl=0.2, labels = FALSE)
  mtext(expression(paste("Wood density (g ",cm^-3,")")),side=1, line=1.5,cex=0.8)
  mtext("No. of individuals in 2010 / \n No. of individuals in 1982",side=2, line=1.5,cex=0.8)

  plot(ab2010/ab1982 ~ moist, ab_t_data20, log = "y", axes = F, col=alpha("black", 0.2), pch = 16)
  abline(h=1,lty=2)
  box()
  axis(1,tick=FALSE,line=-0.8)
  axis(1,tcl=0.2, labels = FALSE)
  mtext("Moisture",side=1, line=1.5,cex=0.8)

  plot(ab2010/ab1982 ~ convex, ab_t_data20, log = "y", axes = F, col=alpha("black", 0.2), pch = 16)
  box()
  abline(h=1,lty=2)
  axis(1,tick=FALSE,line=-0.8)
  axis(1,tcl=0.2, labels = FALSE)
  mtext("Convexity (m)",side=1, line=1.5,cex=0.8)

  plot(ab2010/ab1982 ~ slope, ab_t_data20, log = "y", axes = F, col=alpha("black", 0.2), pch = 16)
  abline(h=1,lty=2)
  box()
  axis(1,tick=FALSE,line=-0.8)
  axis(1,tcl=0.2, labels = FALSE)
  mtext("Slope (degrees)",side=1, line=1.5,cex=0.8)
  par(mfrow=c(1,1))
dev.off()


ab_t_data2 %>%
  filter(site == "subplot_600") %>%
  ggplot(., aes(x = WSG, y = ab2010/ab1982)) +
  geom_point() +
  scale_y_log10()
