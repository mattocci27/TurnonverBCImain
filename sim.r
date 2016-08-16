# if only two species change, what would happen?

#gam20151102.r
rm(list = ls()) # This clears everything from memory.
setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")


library(mgcv)
library(ggplot2)
library(gridExtra)
library(grid)
library(spikeSlabGAM)


moge2 <- D20m[[1]][, c("PIPECO","POULAR")]
moge3 <- moge2 %>% as.data.frame %>% filter(PIPECO > 0)

D100m2 <- D100m
D20m2 <- D20m

WSG100m2 <- list()
WSG20m2 <- list()


# if the two species change do not change at 100m, what would happen?
# if the two species occurs all of the 20m quadrat at the first census, what would happen?

D20m2[[1]][, c("PIPECO","POULAR")] <- D20m[[1]][, c("PIPECO","POULAR")] + 4
for (i in 1:7){
  D100m2[[i]][, c("PIPECO","POULAR")] <- D100m[[1]][, c("PIPECO","POULAR")]
  WSG100m2[[i]] <- com.mean.ab(D100m2[[i]], trait, "WSG")
  WSG20m2[[i]] <- com.mean.ab(D20m2[[i]], trait, "WSG")
}


moge <- data.frame(Time = rep(1:7, each = 50), WSG = unlist(WSG100m2))
moge2 <- data.frame(Time = rep(1:7, each = 1250), WSG = unlist(WSG20m2))

par(mfrow = c(1,2))
plot(WSG ~ Time, moge)
plot(WSG ~ Time, moge2)
par(mfrow=c(1,1))
