##NMDS analysis

rm(list = ls()) # This clears everything from memory.
library(vegan)
library(dplyr)
# setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")

trait1 <- trait

tr <- data.frame(sp=trait1$SP,
         WSG = trait1$WSG,
         moist = trait1$Moist,
         slope = trait1$sp.slope.mean,
         convex = trait1$sp.convex.mean)

rownames(tr)<-tr$name

D100m.all <- rbind(D100m[[1]],
    D100m[[2]],
    D100m[[3]],
    D100m[[4]],
    D100m[[5]],
    D100m[[6]],
    D100m[[7]])


###PIPECO and POULAR do not change their abundance
D100m.all.r <- D100m.all
D100m.all.r[,"PIPECO"] <- rep(D100m[[1]][,"PIPECO"],7)
D100m.all.r[,"POULAR"] <- rep(D100m[[1]][,"POULAR"],7)

system.time(com.nmds.all <- metaMDS(D100m.all, engine = "monoMDS",
  k = 3, trymax = 50))

system.time(com.nmds.all2 <- metaMDS(D100m.all, engine = "monoMDS",
  k = 3, trymax = 50, parallel = 8))


system.time(com.nmds.all2 <- metaMDS(D100m.all, engine = "monoMDS",
  k = 3, trymax = 50, parallel = 26))

# com.nmds.south.r <- metaMDS(D100m.south.r,engine="monoMDS",k=3, trymax=50)
# com.nmds.north.r <- metaMDS(D100m.north.r,engine="monoMDS",k=3, trymax=50)
# com.nmds.all.r <- metaMDS(D100m.all.r,engine="monoMDS",k=3, trymax=50)
#

arrow.col <- gray.colors(12)

save.image("nmds_100.RData")
