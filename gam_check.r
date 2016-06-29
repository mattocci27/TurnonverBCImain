rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("~/Dropbox/MS/TurnoverBCI/temp.RData")

load("BCI_turnover20141213.RData")


library(mgcv)

unlist(WSG100)
unlist(Moist100)
unlist(slope.100.rare[10])
unlist(convex.100.rare[10])

time <- c(1982,1985,1990,1995,2000,2005,2010)
Time=c(rep(1982,50),
                           rep(1985,50),
                           rep(1990,50),
                           rep(1995,50),
                           rep(2000,50),
                           rep(2005,50),
                           rep(2010,50))


temp.ab <- data.frame(SP = colnames(D100m[[1]]),
                      ab10_3 = D100m[[1]][3,],
                      ab10_4 = D100m[[1]][3,])
rownames(temp.ab) <- NULL


temp <- merge(temp.ab,trait, by="SP")

temp$moge1 <- (temp$ab10_3 * temp$convex_size_100)/temp$ab10_3
temp$moge2 <- (temp$ab10_4 * temp$convex_size_100)/temp$ab10_4

moge <- data.frame(temp$SP,temp$moge1)

moge3 <- as.character(na.omit(moge[moge[,2] < -0.2,][,1]))




ab.t.data[(rownames(ab.t.data) %in% moge3),"census_1982"]
ab.t.data[(rownames(ab.t.data) %in% moge3),"census_2010"]

unlist(convex.100.rare[10])[1:50]

WSG100.rm <-list()
Moist100.rm <-list()
slope100.rm <-list()
convex100.rm <-list()

# D100m[[1]][1:5,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))]


###use convex mean
for (i in 1:7){
  WSG100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))],trait,"WSG")
  Moist100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR","PSYCHO"))],trait,"Moist")
  slope100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR","PSYCHO"))],trait,"slope_size_100")
  # convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("ALSEBL","SWARS1"))],trait,"sp.convex.mean")
  convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PSYCHO"))],trait,"sp.convex.mean")
}




moge <- data.frame(WSG = unlist(WSG100),
                   Moist = unlist(Moist100),
                   slope = unlist(slope.100.rare[10]),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time)

moge20 <- data.frame(WSG = unlist(WSG20),
                   Moist = unlist(Moist20),
                   slope = unlist(slope.20.rare[10]),
                   convex = unlist(convex20),
                   site = as.factor(rep(1:1250,7)),
                   Time)

moge100r <- data.frame(WSG = unlist(WSG100.rm),
                   Moist = unlist(Moist100.rm),
                   slope = unlist(slope100.rm),
                   convex = unlist(convex100.rm),
                   site = rep(1:50,7),
                   Time)

moge20r <- data.frame(WSG = unlist(WSG20),
                   Moist = unlist(Moist20),
                   slope = unlist(slope.20.rare[10]),
                   convex = unlist(convex20),
                   site = as.factor(rep(1:1250,7)),
                   Time)


r1.r <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
r2.r <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
r3.r <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
r4.r <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())

r1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
r2 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
r3 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())
r4 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())


r1.r100<- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge100r, correlation = corAR1())
r2.r100<- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge100r, correlation = corAR1())
r3.r100<- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge100r, correlation = corAR1())
r4.r100<- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge100r, correlation = corAR1())

r1_100 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
r2_100 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
r3_100 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
r4_100 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())


moge$temp <- rnorm(nrow(moge))

moge.cross <- moge[order(moge$temp),]
rownames(moge.cross) <- NULL
k <- 10
# res.cv <- numeric(k)
  res.cv <- NULL
  i <- 1
  r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge.cross[(35*i+1):350,], correlation = corCAR1())
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)

  # res <- data.frame(fitted = r2.1$gam$fitted.values,
                    # res = r2.1$gam$residuals,
                    # res2 = r2.1$gam$y - r2.1$gam$fitted.values -r2.1$gam$residuals,
                    # Time= r2.1$gam$model$Time)


  res2 <- unique(res[order(res$Time),])
  res3 <- merge(moge.cross[1:35,],res2,by="Time")

  SS <- sum((moge.cross[1:35,"convex"] - mean(moge.cross[1:35,"convex"]))^2)
  PREDS <- sum((res3$fitted - res3$convex)^2)
  res.cv[i] <- 1 - PREDS/SS

  # r2.1$lme$residuals$site


for (i in 2:(k-1)){
  temp.data <- rbind(moge.cross[1:(35*(i-1)),],
                     moge.cross[(35*i+1):350,])
  r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=temp.data, correlation = corCAR1())
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])

  K1 <- 35*(i-1) +1
  K2 <- 35*i
  res3 <- merge(moge.cross[K1:K2,],res2,by="Time")
  SS <- sum((moge.cross[K1:K2,"convex"] - mean(moge.cross[K1:K2,"convex"]))^2)
  PREDS <- sum((res3$fitted - res3$convex)^2)
  res.cv[i] <- 1 - PREDS/SS
}

i <- 10
  r2.1 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge.cross[1:35*k,], correlation = corCAR1())
  res <- data.frame(fitted = r2.1$gam$fitted.values, Time= r2.1$gam$model$Time)
  res2 <- unique(res[order(res$Time),])
  res3 <- merge(moge.cross[316:350,],res2,by="Time")
  SS <- sum((moge.cross[316:350,"convex"] - mean(moge.cross[316:350,"convex"]))^2)
  PREDS <- sum((res3$fitted - res3$convex)^2)
  res.cv[i] <- 1 - PREDS/SS


CV <- mean(res.cv,na.rm=T)
R2 <- 1 - mean(res.cv,na.rm=T)/mean((r2.1$gam$y - mean(r2.1$gam$y))^2)

data.frame(CV,R2)

save.image("~/Dropbox/MS/TurnoverBCI/temp.RData")





# r1 <- gam(Moist ~  s(Time,k=7), data=moge, correlation = corCAR1())
# r2 <- gamm(Moist ~  s(site,k=50), data=moge, correlation = corCAR1())
# r3 <- gamm(Moist ~  s(site,k=50)+s(Time,k=7), data=moge, correlation = corCAR1())
