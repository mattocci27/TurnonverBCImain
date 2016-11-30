rm(list = ls()) # This clears everything from memory.

#######
#analysis from here
######
library(FD)
library(picante)
library(gdata)
setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
load("~/Dropbox/temporal/BCI_temporal.rdata")
source("~/Dropbox/temporal/tempo_source2.r")

system.time(nate.res2 <- ses.temp.dpw(D100m[[1]], D100m[[2]], t.dis2, runs = 99))

system.time(nate.res3 <- ses.temp.dpw(D100m[[1]], D100m[[3]], t.dis2, runs = 99))
system.time(nate.res4 <- ses.temp.dpw(D100m[[1]], D100m[[4]], t.dis2, runs = 99))
system.time(nate.res5 <- ses.temp.dpw(D100m[[1]], D100m[[5]], t.dis2, runs = 99))
system.time(nate.res6 <- ses.temp.dpw(D100m[[1]], D100m[[6]], t.dis2, runs = 99))
system.time(nate.res7 <- ses.temp.dpw(D100m[[1]], D100m[[7]], t.dis2, runs = 99))

save.image("~/Dropbox/temporal/BCI_temporal_res20161122.rdata")


###shift
h_sp <- trait %>% arrange(desc(WSG)) %>% head(50) %>% .$SP
l_sp <- trait %>% arrange((WSG)) %>% head(50) %>% .$SP

sim_com <- D100m[[2]]
sim_com[,colnames(sim_com) %in% h_sp] <- sim_com[,colnames(sim_com) %in% h_sp] * 10

sim_com[,colnames(sim_com) %in% l_sp] <- sim_com[,colnames(sim_com) %in% l_sp] * 0.1 %>% as.integer

# D100ba[[i]][,!(colnames(D100ba[[1]]) %in% c("PIPECO","POULAR"))]
#
# sim_com[, "PIPECO"] <- 0
# sim_com[, "POULAR"] <- 0
# sim_com[, "DIPTPA"] <- sim_com[, "DIPTPA"] * 200
# sim_com[, "MYROFR"] <- sim_com[, "MYROFR"] * 200

system.time(temp_res <- ses.temp.dpw(D100m[[1]], sim_com, t.dis2, runs = 99))

hist(temp_res$dpw.obs.z)


load("~/Dropbox/temporal/BCI_temporal_res20161122.rdata")

par(mfrow=c(2,3))
hist(nate.res2$dpw.obs.z, xlab = "", main = "1982-1985")
hist(nate.res3$dpw.obs.z, xlab = "", main = "1982-1990")
hist(nate.res4$dpw.obs.z, xlab = "", main = "1982-1995")
hist(nate.res5$dpw.obs.z, xlab = "", main = "1982-2000")
hist(nate.res6$dpw.obs.z, xlab = "", main = "1982-2005")
hist(nate.res7$dpw.obs.z, xlab = "", main = "1982-2010")
par(mfrow=c(1,1))


par(mfrow=c(2,3))
hist(nate.res2$dpw.obs.rank, xlim=c(0,100),xlab = "Quantile", main = "1982-1985")
hist(nate.res3$dpw.obs.rank, xlim=c(0,100),xlab = "Quantile", main = "1982-1990")
hist(nate.res4$dpw.obs.rank, xlim=c(0,100),xlab = "Quantile", main = "1982-1995")
hist(nate.res5$dpw.obs.rank, xlim=c(0,100),xlab = "Quantile", main = "1982-2000")
hist(nate.res6$dpw.obs.rank, xlim=c(0,100),xlab = "Quantile", main = "1982-2005")
hist(nate.res7$dpw.obs.rank, xlim=c(0,100),xlab = "Quantile", main = "1982-2010")
par(mfrow=c(1,1))
