#######
#analysis from here
######
library(FD)
library(picante)
library(gdata)
load("~/Dropbox/temporal/BCI_temporal.rdata")
source("~/Dropbox/temporal/tempo_source2.r")

# undebug(ses.temp.dpw)
system.time(nate.res2<-ses.temp.dpw(D1_m,D2_m,t.dis2,runs=99))
system.time(nate.res3<-ses.temp.dpw(D1_m,D3_m,t.dis2,runs=99))
system.time(nate.res4<-ses.temp.dpw(D1_m,D4_m,t.dis2,runs=99))
system.time(nate.res5<-ses.temp.dpw(D1_m,D5_m,t.dis2,runs=99))
system.time(nate.res6<-ses.temp.dpw(D1_m,D6_m,t.dis2,runs=99))
system.time(nate.res7<-ses.temp.dpw(D1_m,D7_m,t.dis2,runs=99))

system.time(joe.res2<-ses.temp.dpw2(D1_m,D2_m,D2m,rec2_m,t.dis2,runs=99))
system.time(joe.res3<-ses.temp.dpw2(D1_m,D3_m,D3m,R3m,t.dis2,runs=99))
system.time(joe.res4<-ses.temp.dpw2(D1_m,D4_m,D4m,R4m,t.dis2,runs=99))
system.time(joe.res5<-ses.temp.dpw2(D1_m,D5_m,D5m,R5m,t.dis2,runs=99))
system.time(joe.res6<-ses.temp.dpw2(D1_m,D6_m,D6m,R6m,t.dis2,runs=99))
system.time(joe.res7<-ses.temp.dpw2(D1_m,D7_m,D7m,R7m,t.dis2,runs=99))

# save.image("~/Dropbox/temporal/BCI_temporal_res20140401.rdata")

###


system.time(nate.res2 <- ses.temp.dpw(D1_m, D2_m, t.dis2, runs = 3))

system.time(nate.res2 <- ses.temp.dpw(D100m[[1]], D100m[[2]], t.dis2, runs = 99))

system.time(nate.res3 <- ses.temp.dpw(D100m[[1]], D100m[[3]], t.dis2, runs = 99))
system.time(nate.res4 <- ses.temp.dpw(D100m[[1]], D100m[[4]], t.dis2, runs = 99))
system.time(nate.res5 <- ses.temp.dpw(D100m[[1]], D100m[[5]], t.dis2, runs = 99))
system.time(nate.res6 <- ses.temp.dpw(D100m[[1]], D100m[[6]], t.dis2, runs = 99))
system.time(nate.res7 <- ses.temp.dpw(D100m[[1]], D100m[[7]], t.dis2, runs = 99))

# system.time(joe.res2 <- ses.temp.dpw2(D100m[[1]], D100m[[2]], t.dis2, runs = 3))
# system.time(joe.res7 <- ses.temp.dpw2(D100m[[1]], D100m[[7]], t.dis2, runs = 99))

par(mfrow=c(2,3))
hist(nate.res2$dpw.obs.z)
hist(nate.res3$dpw.obs.z)
hist(nate.res4$dpw.obs.z)
hist(nate.res5$dpw.obs.z)
hist(nate.res6$dpw.obs.z)
hist(nate.res7$dpw.obs.z)
par(mfrow=c(1,1))


par(mfrow=c(2,3))
hist(joe.res2$dpw.obs.z)
hist(joe.res3$dpw.obs.z)
hist(joe.res4$dpw.obs.z)
hist(joe.res5$dpw.obs.z)
hist(joe.res6$dpw.obs.z)
hist(joe.res7$dpw.obs.z)
par(mfrow=c(1,1))



boxplot(nate.res2$dpw.obs.z,
        nate.res3$dpw.obs.z,
        nate.res4$dpw.obs.z,
        nate.res5$dpw.obs.z,
        nate.res6$dpw.obs.z,
        nate.res7$dpw.obs.z,
        joe.res2$dpw.obs.z,
        joe.res3$dpw.obs.z,
        joe.res4$dpw.obs.z,
        joe.res5$dpw.obs.z,
        joe.res6$dpw.obs.z,
        joe.res7$dpw.obs.z)

boxplot(nate.res2$dpw.obs.rank,
        nate.res3$dpw.obs.rank,
        nate.res4$dpw.obs.rank,
        nate.res5$dpw.obs.rank,
        nate.res6$dpw.obs.rank,
        nate.res7$dpw.obs.rank,
        joe.res2$dpw.obs.rank,
        joe.res3$dpw.obs.rank,
        joe.res4$dpw.obs.rank,
        joe.res5$dpw.obs.rank,
        joe.res6$dpw.obs.rank,
        joe.res7$dpw.obs.rank)

par(mfrow=c(2,3))
hist(joe.res2$dpw.obs.rank,breaks=20)
hist(joe.res3$dpw.obs.rank,breaks=20)
hist(joe.res4$dpw.obs.rank,breaks=20)
hist(joe.res5$dpw.obs.rank,breaks=20)
hist(joe.res6$dpw.obs.rank,breaks=20)
hist(joe.res7$dpw.obs.rank,breaks=20)
par(mfrow=c(1,1))

par(mfrow=c(2,3))
hist(nate.res2$dpw.obs.rank,breaks=20)
hist(nate.res3$dpw.obs.rank,breaks=20)
hist(nate.res4$dpw.obs.rank,breaks=20)
hist(nate.res5$dpw.obs.rank,breaks=20)
hist(nate.res6$dpw.obs.rank,breaks=20)
hist(nate.res7$dpw.obs.rank,breaks=20)
par(mfrow=c(1,1))
