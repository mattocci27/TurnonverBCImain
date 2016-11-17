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

D20m.all <- rbind(D20m[[1]],
    D20m[[2]],
    D20m[[3]],
    D20m[[4]],
    D20m[[5]],
    D20m[[6]],
    D20m[[7]])


###PIPECO and POULAR do not change their abundance
D20m.all.r <- D20m.all
D20m.all.r[,"PIPECO"] <- rep(D20m[[1]][,"PIPECO"],7)
D20m.all.r[,"POULAR"] <- rep(D20m[[1]][,"POULAR"],7)

com.nmds.all <- metaMDS(D20m.all, engine = "monoMDS",
  k = 3, trymax = 50, parallel = 10)

# com.nmds.south.r <- metaMDS(D20m.south.r,engine="monoMDS",k=3, trymax=50)
# com.nmds.north.r <- metaMDS(D20m.north.r,engine="monoMDS",k=3, trymax=50)
# com.nmds.all.r <- metaMDS(D20m.all.r,engine="monoMDS",k=3, trymax=50)
#

arrow.col <- gray.colors(12)

save.image("nmds_20.RData")
