##NMDS analysis
#nohup Rscript nmds_20.R > nmds_20.log &

rm(list = ls()) # This clears everything from memory.
library(vegan)
# library(dplyr)
# library(parallel)
setwd("~/Dropbox/BCI_Turnover")
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

# D20m.all <- rbind(D20m[[1]])
# cl <- makeCluster(4)
# clusterEvalQ(cl, library(vegan))
# clusterExport(cl, varlist = "D20m.all")
system.time(com.nmds.all <- metaMDS(D20m.all,engine="monoMDS",k=3, trymax=52, parallel = 26))

# com.nmds.south.r <- metaMDS(D20m.south.r,engine="monoMDS",k=3, trymax=50)
# com.nmds.north.r <- metaMDS(D20m.north.r,engine="monoMDS",k=3, trymax=50)
# com.nmds.all.r <- metaMDS(D20m.all.r,engine="monoMDS",k=3, trymax=50)
#

arrow.col <- gray.colors(12)

save.image("nmds_20_GCE.RData")

# samp <- D100m.all
# range <- "north"
# engine <- "monoMDS"
# k <- 3
# tyrmax <- 50
# n.census <- 7
# axis <-"1-2"

plot.nmds <- function(samp, range=c("north","south","all"), axis = c("1-2","1-3","2-3"), engine, k = 3, trymax =50, n.census = 7){

  com.nmds <- metaMDS(samp,engine=engine,k=k, trymax=trymax)
  if (axis == "1-2") {
      axis1 <- "MDS1"
      axis2 <- "MDS2"
      lab1 <- "NMDS axis1"
      lab2 <- "NMDS axis2"
    } else if (axis == "1-3") {
      axis1 <- "MDS1"
      axis2 <- "MDS3"
      lab1 <- "NMDS axis1"
      lab2 <- "NMDS axis3"
    } else if (axis == "2-3") {
      axis1 <- "MDS2"
      axis2 <- "MDS3"
      lab1 <- "NMDS axis2"
      lab2 <- "NMDS axis3"
    }

  if (range=="north"){
      range2 <- grep("_4$|_5$",rownames(samp))
    } else if(range=="south"){
      range2 <- grep("_1$|_2$",rownames(samp))
    } else if(range=="all") {range2 <- 1:nrow(samp)}


  plot(com.nmds$points[range2,] ~ com.nmds$points[range2,],xlim=c(-0.4,0.4),ylim=c(-0.4,0.4),type="n",xlab=paste(lab1), ylab=paste(lab2), main = paste(range))

    n.samp <- length(range2)/n.census

    for (i in 1:(n.census-1)) {
    N1 <- n.samp * (i-1) + 1
    N1.2 <- n.samp * i

    N2 <- n.samp * i + 1
    N2.2 <- n.samp * (i+1)

    x1 <- com.nmds$points[range2,paste(axis1)][N1:N1.2]
    y1 <- com.nmds$points[range2,paste(axis2)][N1:N1.2]
    x2 <- com.nmds$points[range2,paste(axis1)][N2:N2.2]
    y2 <- com.nmds$points[range2,paste(axis2)][N2:N2.2]

    arrows(x1,y1,x2,y2,length=0.05,col=arrow.col[i])
    }

}


###figure
pdf("~/Desktop/NMDS1-2.pdf",height=10,width=3)
par(mfrow=c(3,1))
plot.nmds(D20m.all,range="north",axis="1-2",n.census=7, engine="monoMDS",k=3,trymax=50)
plot.nmds(D20m.all,range="south",axis="1-2",n.census=7, engine="monoMDS",k=3,trymax=50)
plot.nmds(D20m.all,range="all",axis="1-2",n.census=7, engine="monoMDS",k=3,trymax=50)
par(mfrow=c(3,1))
dev.off()





###figure
# pdf("~/Desktop/NMDS1-3.pdf",height=10,width=6)
# par(mfrow=c(3,2))
# plot.nmds(D100m.north,D100m[[1]],range="north",axis="1-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# plot.nmds(D100m.south,D100m[[1]],range="south",axis="1-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# plot.nmds(D100m.all,D100m[[1]],range="all",axis="1-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# par(mfrow=c(3,1))
# dev.off()

# pdf("~/Desktop/NMDS2-3.pdf",height=10,width=6)
# par(mfrow=c(3,2))
# plot.nmds(D100m.north,D100m[[1]],range="north",axis="2-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# plot.nmds(D100m.south,D100m[[1]],range="south",axis="2-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# plot.nmds(D100m.all,D100m[[1]],range="all",axis="2-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# par(mfrow=c(3,1))
# dev.off()

#
pdf("~/Desktop/NMDS1-2_rm.pdf",height=10,width=3)
par(mfrow=c(3,1))
plot.nmds(D100m.north.r,range="north",axis="1-2",n.census=7, engine="monoMDS",k=3,trymax=50)
plot.nmds(D100m.south.r,range="south",axis="1-2",n.census=7, engine="monoMDS",k=3,trymax=50)
plot.nmds(D100m.all.r,range="all",axis="1-2",n.census=7, engine="monoMDS",k=3,trymax=50)
par(mfrow=c(1,1))
dev.off()


# pdf("~/Desktop/NMDS1-3_rm.pdf",height=10,width=6)
# par(mfrow=c(3,2))
# plot.nmds(D100m.north.r,D100m.all.r[1:50,],range="north",axis="1-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# plot.nmds(D100m.south.r,D100m.all.r[1:50,],range="south",axis="1-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# plot.nmds(D100m.all.r,D100m.all.r[1:50,],range="all",axis="1-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# par(mfrow=c(3,1))
# dev.off()


# pdf("~/Desktop/NMDS2-3_rm.pdf",height=10,width=6)
# par(mfrow=c(3,2))
# plot.nmds(D100m.north.r,D100m.all.r[1:50,],range="north",axis="2-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# plot.nmds(D100m.south.r,D100m.all.r[1:50,],range="south",axis="2-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# plot.nmds(D100m.all.r,D100m.all.r[1:50,],range="all",axis="2-3",n.census=7, engine="monoMDS",k=3,trymax=50)
# par(mfrow=c(3,1))
# dev.off()





###table

nmds.boot <- function(nmds.res, n.census, n.rep){
NMDS1.diff <- NULL
NMDS2.diff <- NULL
NMDS3.diff <- NULL
nmds1.boot <- matrix(numeric(0),nrow=(n.census-1),ncol=n.rep)
nmds2.boot <- matrix(numeric(0),nrow=(n.census-1),ncol=n.rep)
nmds3.boot <- matrix(numeric(0),nrow=(n.census-1),ncol=n.rep)


 n.samp <- nrow(nmds.res$points)/n.census


for (i in 1:(n.census-1)) {
  N1 <- n.samp * (i-1) + 1
  N1.2 <- n.samp * i

  N2 <- n.samp * i + 1
  N2.2 <- n.samp * (i+1)

  before1 <- nmds.res$points[,"MDS1"][N1:N1.2]
  before2 <- nmds.res$points[,"MDS2"][N1:N1.2]
  before3 <- nmds.res$points[,"MDS3"][N1:N1.2]
  after1 <- nmds.res$points[,"MDS1"][N2:N2.2]
  after2 <- nmds.res$points[,"MDS2"][N2:N2.2]
  after3 <- nmds.res$points[,"MDS3"][N2:N2.2]

  temp1 <- before1 - after1
  temp1[temp1>0] <- 1
  temp1[temp1<0] <- 0

  temp2 <- before2 - after2
  temp2[temp2>0] <- 1
  temp2[temp2<0] <- 0

  temp3 <- before3 - after3
  temp3[temp3>0] <- 1
  temp3[temp3<0] <- 0

  NMDS1.diff[i] <- n.samp-sum(temp1)
  NMDS2.diff[i] <- n.samp-sum(temp2)
  NMDS3.diff[i] <- n.samp-sum(temp3)

  nmds1.boot[i,]<-sample(before1-after1,n.rep,replace=T)
  nmds2.boot[i,]<-sample(before2-after2,n.rep,replace=T)
  nmds3.boot[i,]<-sample(before3-after3,n.rep,replace=T)
}

nmds1.mean <- apply(nmds1.boot,1,mean)
nmds2.mean <- apply(nmds2.boot,1,mean)
nmds3.mean <- apply(nmds3.boot,1,mean)



nmds1.lower <- apply(nmds1.boot,1,function(x)quantile(x,0.025))
nmds2.lower <- apply(nmds2.boot,1,function(x)quantile(x,0.025))
nmds3.lower <- apply(nmds3.boot,1,function(x)quantile(x,0.025))

nmds1.upper <- apply(nmds1.boot,1,function(x)quantile(x,0.975))
nmds2.upper <- apply(nmds2.boot,1,function(x)quantile(x,0.975))
nmds3.upper <- apply(nmds3.boot,1,function(x)quantile(x,0.975))


year.temp <- c("1981-1985",
               "1985-1990",
               "1990-1995",
               "1995-2000",
               "2000-2005",
               "2005-2010")


year <- year.temp[1:(n.census-1)]

nmds1.p<-nmds2.p<-nmds3.p <-NULL
for (i in 1:(n.census-1)) nmds1.p[i] <- binom.test(NMDS1.diff[i],n.samp)$p.value
for (i in 1:(n.census-1)) nmds2.p[i] <- binom.test(NMDS2.diff[i],n.samp)$p.value
for (i in 1:(n.census-1)) nmds3.p[i] <- binom.test(NMDS3.diff[i],n.samp)$p.value



data.frame(Census.interval= c("NMDS.axis.1",year,"NMDS.axis.2",year,"NMDS.axis.3",year),
           Number.of.subplots.with.negative.change=c(NA,NMDS1.diff,NA,NMDS2.diff,NA,NMDS3.diff),
           Binomial.P.value=c(NA,nmds1.p,NA,nmds2.p,NA,nmds3.p),
           Mean.rate.of.change=c(NA,nmds1.mean,NA,nmds2.mean,NA,nmds3.mean),
           Lower.95per.CI=c(NA,nmds1.lower,NA,nmds2.lower,NA,nmds3.lower),
           Upper.95per.CI=c(NA,nmds1.upper,NA,nmds2.upper,NA,nmds3.upper))

}

nmds.south <- nmds.boot(com.nmds.south, n.census=7, n.rep=5000)

nmds.north <- nmds.boot(com.nmds.north, n.census=7, n.rep=5000)

nmds.all <- nmds.boot(com.nmds.all, n.census=7, n.rep=5000)


write.csv(nmds.south,"s.csv")
write.csv(nmds.north,"n.csv")
write.csv(nmds.all,"all.csv")



nmds.south.r <- nmds.boot(com.nmds.south.r, n.census=7, n.rep=5000)

nmds.north.r <- nmds.boot(com.nmds.north.r, n.census=7, n.rep=5000)

nmds.all.r <- nmds.boot(com.nmds.all.r, n.census=7, n.rep=5000)
write.csv(nmds.south.r,"s_r.csv")
write.csv(nmds.north.r,"n_r.csv")
write.csv(nmds.all.r,"all_r.csv")
