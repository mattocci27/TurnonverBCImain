rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
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

r1_100 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge100, correlation = corAR1())
r2_100 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge100, correlation = corAR1())
r3_100 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge100, correlation = corAR1())
r4_100 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge100, correlation = corAR1())


# temp <- rnorm(50)
# temp2 <- as.vector(t(mapply(rnorm,rep(7,50),temp,0.1)))

# temp2 <- rep(1:7,50)

# moge <- data.frame(WSG = temp2,
#                    site = rep(1:50,7),
#                    Time = Time)




r1 <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
r2 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
r3 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())
r4 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())


r1.2 <- gam(WSG ~  s(Time,k=4), data=moge, correlation = corAR1())
plot(r1$gam$residuals ~r1.2$residuals)


temp2 <- data.frame(res=r1$gam$residuals,
                   fit=r1$gam$fitted.values,
                   y=moge$WSG,
                   site=moge$site,
                   Time=moge$Time)

temp2$res_all <- temp2$y -temp2$fit

temp2$res2 <- temp2$y - temp2$fit - temp2$res


r2 <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())

par(mfrow=c(2,5))
for (i in 31:40){
r2.2 <- gam(Moist ~  s(Time,k=5), data=moge[moge$site==i,], correlation = corAR1())
plot(r2.2)
print(summary(r2.2)$s.pv )
}
par(mfrow=c(1,1))

r2 <- gamm4(Moist ~  Time, random = ~(1|site), data=moge, correlation = corAR1())


plot(r2$gam$residuals ~r2.2$residuals)


temp <- data.frame(res=r2$gam$residuals,
                   fit=r2$gam$fitted.values,
                   y=moge$Moist,
                   site=moge$site,
                   Time=moge$Time)

temp$res_all <- temp$y -temp$fit

temp$res2 <- temp$y - temp$fit - temp$res

temp$y2 <- temp$y - temp$res2

head(temp)

moge$Time2 <- scale(moge$Time)

temp <- lmer(WSG~Time2 + Time2^2 + Time2^3 + Time2^4 + (1|site),data=moge)


sum((temp$res)^2)
sum((temp$res2)^2)
sum((temp$res_all)^2)


r3 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())



plot(convex ~ Time, data= moge, type="n")
for (i in 26:50) points(convex ~ Time, data= moge[moge$site==i,], type="l")


r4 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())


# plot(r1$gam)


# xx <- rnorm(50,100,100)
# yy <- rnorm(50, .05*xx -0.2,10)

# temp <- gam(yy~s(xx))
# par(mfrow=c(1,2))
# plot(yy~xx)

# mo <- lm(yy~xx)

# plot(xx, fitted(mo)-mean(fitted(mo)))
# par(mfrow=c(1,1))






# r1.2 <- gamm(WSG ~  s(Time,k=6), random = list(site=~1), data=moge)

# r4 <-gamm4(WSG ~ s(Time,k=6),random = ~(1|site),data=moge2)
# r3 <-gamm(WSG ~ s(Time,k=3), random = list(site=~1),data=moge2)
# r4 <-gamm(WSG ~ s(Time,k=4), random = list(site=~1),data=moge2)
# r5 <-gamm(WSG ~ s(Time,k=5), random = list(site=~1),data=moge2)
# r6 <-gamm(WSG ~ s(Time,k=6), random = list(site=~1),data=moge2)
# r7 <-gamm(WSG ~ s(Time,k=7), random = list(site=~1),data=moge2)

anova(r3$lme,r4$lme,r5$lme,r6$lme,r7$lme)

gam.check(r4$gam)
gam.check(r5$gam)
gam.check(r6$gam)
gam.check(r7$gam)

# r4$gam$gcv.ubre
# r5$gam$gcv.ubre
# r6$gam$gcv.ubre
# r7$gam$gcv.ubre
# plot(r1.2$gam)

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



library(disdat)
data(NSWtrain)
# a single species
srsp1 <- subset(NSWtrain, spid=='srsp1')
kfold(srsp1, k = 5)





r2 <- gamm(Moist ~  s(Time,k=7), random = list(site=~1), data=moge, correlation = corCAR1())

mean((r2$gam$residuals)^2)

summary(r2$gam)






# r1 <- gam(Moist ~  s(Time,k=7), data=moge, correlation = corCAR1())
# r2 <- gamm(Moist ~  s(site,k=50), data=moge, correlation = corCAR1())
# r3 <- gamm(Moist ~  s(site,k=50)+s(Time,k=7), data=moge, correlation = corCAR1())

anova(r2$gam,r3$gam)




r2 <- gam(Moist ~  s(Time,k=4),data=moge, correlation = corAR1())
gam.check(r2)


summary(r2$gam)

# library(gamm4)
r2.2 <- lme(Moist ~ 1, data=moge,  random = list(site=~1),correlation = corAR1())

anova(r2$lme,r2.2,test="F")

summary(r2$gam)
summary(r2.2$gam)


r2.2 <- lme(Moist ~  1, random = list(site=~1), data=moge)

RSS2 <- sum(residuals(r2$gam)^2)
RSS1 <- sum(residuals(r2.2)^2)

(RSS1 - RSS2 )/RSS2  * (350 - 2)/ (2-1)


summary(r1$gam, freq=T)

r3 <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge[moge$site!=10,], correlation = corAR1())
r4 <- gamm(slope ~  s(Time,k=4), random = list(site=~1), data=moge, correlation = corAR1())




r3n <- gamm(convex ~  s, random = list(site=~1), data=moge, correlation = corAR1())

n <- 500000
p <- 2

(n-1) /(n-p-1)

summary(r1$gam)
summary(r2$gam)
summary(r3$gam)


par(mfrow=c(2,2))
plot(r1$gam, se=T, shade=T, main="WSG")
plot(r2$gam, se=T, shade=T, main = "Moist")
plot(r3$gam, se=T, shade=T, main = "Convexity")
plot(r4$gam, se=T, shade=T, main = "Slope")
par(mfrow=c(1,1))

res1 <- predict(r1$gam, se.fit=T)
res1r <- predict(r1.r$gam, se.fit=T)

res2 <- predict(r2$gam, se.fit=T)
res2r <- predict(r2.r$gam, se.fit=T)


res3 <- predict(r3$gam, se.fit=T)
res3r <- predict(r3.r$gam, se.fit=T)

res4 <- predict(r4$gam, se.fit=T)
res4r <- predict(r4.r$gam, se.fit=T)


#1.96 is the 2-side 5% point of the standard normal distribution
#or qnorm(0.975, 0,1)
plot(0,type="n",xlim=c(1982,2010),ylim=c(0.58,0.61))
lines(spline(r1$gam$model$Time, res1$fit))
lines(spline(r1$gam$model$Time, res1$fit + 1.96*res1$se),lty=2)
lines(spline(r1$gam$model$Time, res1$fit - 1.96*res1$se),lty=2)

lines(spline(r1.r$gam$model$Time, res1r$fit))
lines(spline(r1.r$gam$model$Time, res1r$fit + 1.96*res1r$se),lty=2)
lines(spline(r1.r$gam$model$Time, res1r$fit - 1.96*res1r$se),lty=2)


library(ggplot2)

r1data <- data.frame(time = spline(r1$gam$model$Time, res1$fit)$x,
                    WSG = spline(r1$gam$model$Time, res1$fit)$y,
                    WSGr = spline(r1.r$gam$model$Time, res1r$fit)$y,
                    upper1 = spline(r1$gam$model$Time, res1$fit + 1.96*res1r$se.fit)$y,
                    lower1 = spline(r1$gam$model$Time, res1$fit - 1.96*res1r$se.fit)$y,
                    upper2 = spline(r1.r$gam$model$Time, res1r$fit + 1.96*res1r$se.fit)$y,
                    lower2 = spline(r1.r$gam$model$Time, res1r$fit - 1.96*res1r$se.fit)$y)
r2data <- data.frame(time = spline(r2$gam$model$Time, res2$fit)$x,
                    Moist = spline(r2$gam$model$Time, res2$fit)$y,
                    Moistr = spline(r2.r$gam$model$Time, res2r$fit)$y,
                    upper1 = spline(r2$gam$model$Time, res2$fit + 1.96*res2r$se.fit)$y,
                    lower1 = spline(r2$gam$model$Time, res2$fit - 1.96*res2r$se.fit)$y,
                    upper2 = spline(r2.r$gam$model$Time, res2r$fit + 1.96*res2r$se.fit)$y,
                    lower2 = spline(r2.r$gam$model$Time, res2r$fit - 1.96*res2r$se.fit)$y)

r3data <- data.frame(time = spline(r3$gam$model$Time, res3$fit)$x,
                    Convexity = spline(r3$gam$model$Time, res3$fit)$y,
                    Convexityr = spline(r3.r$gam$model$Time, res3r$fit)$y,
                    upper1 = spline(r3$gam$model$Time, res3$fit + 1.96*res3r$se.fit)$y,
                    lower1 = spline(r3$gam$model$Time, res3$fit - 1.96*res3r$se.fit)$y,
                    upper2 = spline(r3.r$gam$model$Time, res3r$fit + 1.96*res3r$se.fit)$y,
                    lower2 = spline(r3.r$gam$model$Time, res3r$fit - 1.96*res3r$se.fit)$y)

r4data <- data.frame(time = spline(r4$gam$model$Time, res4$fit)$x,
                    Slope = spline(r4$gam$model$Time, res4$fit)$y,
                    Sloper = spline(r4.r$gam$model$Time, res4r$fit)$y,
                    upper1 = spline(r4$gam$model$Time, res4$fit + 1.96*res4r$se.fit)$y,
                    lower1 = spline(r4$gam$model$Time, res4$fit - 1.96*res4r$se.fit)$y,
                    upper2 = spline(r4.r$gam$model$Time, res4r$fit + 1.96*res4r$se.fit)$y,
                    lower2 = spline(r4.r$gam$model$Time, res4r$fit - 1.96*res4r$se.fit)$y)

library(grid)
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)
}

theme_set(theme_bw())
p1 <- ggplot(r1data, aes(x=time,y=WSG)) +  geom_line(col="gray")
p1 <- p1 + geom_ribbon(aes(ymin=lower1,ymax=upper1),alpha=0.3,fill="gray")
p1 <- p1 + geom_line(aes(x=time,y=WSGr),col="red")
p1 + geom_ribbon(aes(ymin=lower2,ymax=upper2),alpha=0.3,fill="red")

p2 <- ggplot(r2data, aes(x=time,y=Moist)) +  geom_line(col="gray")
p2 <- p2 + geom_ribbon(aes(ymin=lower1,ymax=upper1),alpha=0.3,fill="gray")
# p2 <- p2 + geom_line(aes(x=time,y=Moistr),col="red")
# p2 + geom_ribbon(aes(ymin=lower2,ymax=upper2),alpha=0.3,fill="red")

p3 <- ggplot(r3data, aes(x=time,y=Convexity)) +  geom_line(col="gray")
p3 <- p3 + geom_ribbon(aes(ymin=lower1,ymax=upper1),alpha=0.3,fill="gray")
p3 <- p3 + geom_line(aes(x=time,y=Convexityr),col="red")
p3 + geom_ribbon(aes(ymin=lower2,ymax=upper2),alpha=0.3,fill="red")


p4 <- ggplot(r4data, aes(x=time,y=Slope)) +  geom_line(col="gray")
p4 <- p4 + geom_ribbon(aes(ymin=lower1,ymax=upper1),alpha=0.3,fill="gray")
p4 <- p4 + geom_line(aes(x=time,y=Sloper),col="red")
p4 + geom_ribbon(aes(ymin=lower2,ymax=upper2),alpha=0.3,fill="red")

########
p1 <- ggplot(data=moge) + geom_point(aes(x=Time,y=WSG),alpha=0.3)
p1 <- p1 + geom_line(data=r1data,aes(x=time,y=WSG),col="black")
p1 <- p1 + geom_ribbon(data=r1data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
p1 <- p1 + geom_line(data=r1data,aes(x=time,y=WSGr),col="red")
p1 <- p1 + geom_ribbon(data=r1data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")

p2 <- ggplot(data=moge) + geom_point(aes(x=Time,y=Moist),alpha=0.3)
p2 <- p2 + geom_line(data=r2data,aes(x=time,y=Moist),col="black")
p2 <- p2 + geom_ribbon(data=r2data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
# p2 <- p2 + geom_line(data=r2data,aes(x=time,y=WSGr),col="red")
# p2 <- p2 + geom_ribbon(data=r2data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")

p3 <- ggplot(data=moge) + geom_point(aes(x=Time,y=convex),alpha=0.3)
p3 <- p3 + geom_line(data=r3data,aes(x=time,y=Convexity),col="black")
p3 <- p3 + geom_ribbon(data=r3data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
p3 <- p3 + geom_line(data=r3data,aes(x=time,y=Convexityr),col="red")
p3 <- p3 + geom_ribbon(data=r3data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")


p4 <- ggplot(data=moge) + geom_point(aes(x=Time,y=slope),alpha=0.3)
p4 <- p4 + geom_line(data=r4data,aes(x=time,y=Slope),col="black")
p4 <- p4 + geom_ribbon(data=r4data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
# p4 <- p4 + geom_line(data=r4data,aes(x=time,y=WSGr),col="red")
# p4 <- p4 + geom_ribbon(data=r4data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")
pushViewport(viewport(layout=grid.layout(2, 2)))

print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(1,2))
print(p3,vp=vplayout(2,1))
print(p4,vp=vplayout(2,2))





p1 <- ggplot(moge) + geom_smooth(aes(x=Time,y=site))
p1

plot(WSG~Time,moge,pch=16,col=adjustcolor("black",alpha.f=0.4))
lines(WSG ~ time, r1data, col="black")
polygon(c(r1data$time,rev(r1data$time)), c(r1data$upper1,rev(r1data$lower1)),col=adjustcolor("gray",alpha.f=0.5),border=NA)

lines(WSGr ~ time, r1data,col="red")
polygon(c(r1data$time,rev(r1data$time)), c(r1data$upper2,rev(r1data$lower2)),col=adjustcolor("red",alpha.f=0.4),border=NA)


polygon(r1data$time,r1data$lower1)
polygon(r1data$time,r1data$upper1)

lines(WSGr ~ time, r1data, col="blue")
pl



temp <- c(r1$gam$fitted.values[1],
r1$gam$fitted.values[51],
r1$gam$fitted.values[101],
r1$gam$fitted.values[151],
r1$gam$fitted.values[201],
r1$gam$fitted.values[251],
r1$gam$fitted.values[301])

time1 <- c(1982,1985,1990,1995,2000,2005,2010)

plot(temp~time1,type="l")





summary(r1.r$gam)
summary(r2.r$gam)
summary(r3.r$gam)
summary(r4.r$gam)

par(mfrow=c(2,2))
plot(r1.r$gam, se=T, shade=T, main="WSG")
plot(r2.r$gam, se=T, shade=T, main = "Moist")
plot(r3.r$gam, se=T, shade=T, main = "Convexity")
plot(r4.r$gam, se=T, shade=T, main = "Slope")
par(mfrow=c(1,1))








r2 <- gamm(WSG ~   s(Time,k=6), data=moge, correlation = corAR1())
# plot(r1,se=T,shade=T)


# r1 <- gamm(WSG ~  s(Time,k=6), data=moge, correlation = corAR1())
# r2 <- gamm(Moist ~ s(site,k=49) + s(Time,k=6), data=moge, correlation = corAR1())
# r3 <- gamm(slope ~ s(site,k=49) + s(Time,k=6), data=moge, correlation = corAR1())
# r4 <- gamm(convex ~ s(site,k=49) + s(Time,k=6), data=moge, correlation = corAR1())
# r2 <- gamm(WSG ~ s(site,k=49) + s(Time,k=6), data=moge, correlation = corCAR1())
anova(r4$lme,r4$gam, test="F")



# res1.rm <- gamm(WSG ~  s(Time,k=6), data=moge, correlation = corCAR1())

summary(r1$gam)
par(mfrow=c(1,2))
plot(r1$gam, se=T, shade=T)
plot(r2$gam, se=T, shade=T)
plot(r3$gam, se=T, shade=T)
plot(r4$gam, se=T, shade=T)
par(mfrow=c(1,1))



r1 <- gamm(unlist(WSG100.rm)~s(Time,k=6),correlation = corCAR1())
r2 <- gamm(unlist(WSG100.rm)~s(Time,k=6),correlation = corAR1())
# r3 <- gamm(unlist(WSG100.rm)~s(Time,k=6),correlation = corARMA(form = ~ Time, p=2))



res1.rm <- gamm(unlist(WSG100.rm)~s(Time,k=6),correlation = corCAR1())
res2.rm <- gamm(unlist(Moist100.rm)~s(Time,k=6),correlation = corAR1())
res3.rm <- gamm(unlist(slope100.rm)~s(Time,k=6),correlation = corAR1())
res4.rm <- gamm(unlist(convex100.rm)~s(Time,k=6),correlation = corAR1())
res4.rm <- gamm(unlist(convex100.rm)~s(Time,k=6),correlation = corCAR1())
summary(res1.rm$gam)
summary(res4.rm$gam)


res1.rm <- gam(unlist(WSG100.rm)~s(Time,k=7))


gam.data <- data.frame(WSG=unlist(WSG100),
                       moist=unlist(Moist100),
                       slope=unlist(slope.100.rare[10]),
                       convex=unlist(convex.100.rare[10]),
                       WSG.rm=unlist(WSG100.rm),
                       # moist.rm=unlist(Moist100.rm),
                       # slope.rm=unlist(slope100.rm),
                       convex.rm=unlist(convex100.rm),
                       time=Time)




library(ggplot2)
library(grid)
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)
}

pdf("~/Dropbox/MS/TurnoverBCI/fig0/GAM3.pdf",height=16,width=8)
pushViewport(viewport(layout=grid.layout(4, 2)))


p1 <- ggplot(gam.data,aes(x=time,y=WSG))
p1 <- p1 +geom_point(alpha=0.3)+ geom_smooth(method="gam",formula=y~s(x,k=6),col="red") + coord_cartesian(ylim=c(0.56,0.632)) + labs(y="WSG", title="All species")

p2 <- ggplot(gam.data,aes(x=time,y=moist))
p2 <- p2 +geom_point(alpha=0.3)+ geom_smooth(method="gam",formula=y~s(x,k=6)) + coord_cartesian(ylim=c(0.36,0.82)) + labs(y="Moist", title="All species")

p3 <- ggplot(gam.data,aes(x=time,y=slope))
p3 <- p3 +geom_point(alpha=0.3)+ geom_smooth(method="gam",formula=y~s(x,k=6)) + coord_cartesian(ylim=c(6.9,9.32)) + labs(y=" Slope", title="All species")

p4 <- ggplot(gam.data,aes(x=time,y=convex))
p4 <- p4 +geom_point(alpha=0.3)+ geom_smooth(method="gam",formula=y~s(x,k=6),col="red") + coord_cartesian(ylim=c(0.025,0.077)) + labs(y="Convexity", title="All species")
# p4

print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(2,1))
print(p3,vp=vplayout(3,1))
print(p4,vp=vplayout(4,1))


p1 <- ggplot(gam.data,aes(x=time,y=WSG.rm))
p1 <- p1 +geom_point(alpha=0.3)+ geom_smooth(method="gam",formula=y~s(x,k=6)) + coord_cartesian(ylim=c(0.56,0.632)) + labs(y="WSG", title="Without PIPECO and POULAR")

p2 <- ggplot(gam.data,aes(x=time,y=Moist.rm))
p2 <- p2 +geom_point(alpha=0.3)+ geom_smooth(method="gam",formula=y~s(x,k=6)) + coord_cartesian(ylim=c(0.36,0.82)) + labs(y="Moist", title="Without PIPECO and POULAR")

p3 <- ggplot(gam.data,aes(x=time,y=slope.rm))
p3 <- p3 +geom_point(alpha=0.3)+ geom_smooth(method="gam",formula=y~s(x,k=6)) + coord_cartesian(ylim=c(6.9,9.32)) + labs(y=" Slope", title="Without PIPECO and POULAR")

p4 <- ggplot(gam.data,aes(x=time,y=convex.rm))
p4 <- p4 +geom_point(alpha=0.3)+ geom_smooth(method="gam",formula=y~s(x,k=6),col="blue") + coord_cartesian(ylim=c(0.025,0.077)) + labs(y="Convexity", title="Without PSYCHO and POULAR")


print(p1,vp=vplayout(1,2))
print(p2,vp=vplayout(2,2))
print(p3,vp=vplayout(3,2))
print(p4,vp=vplayout(4,2))

dev.off()






for (i in 1:7){
	slope100.rm[[i]] <- com.mean.ab(D100m[[i]][,-c(212,134)],trait,"slope_size_100")
}
res4.rm <- gamm(unlist(convex100.rm)~s(Time,k=6),correlation = corAR1())


res1.rm <- gamm(unlist(WSG100.rm)~s(Time,k=6),correlation = corAR1())
res2.rm <- gamm(unlist(Moist100.rm)~s(Time,k=6),correlation = corAR1())
res3.rm <- gamm(unlist(slope100.rm)~s(Time,k=6),correlation = corAR1())
res4.rm <- gamm(unlist(convex100.rm)~s(Time,k=6),correlation = corAR1())




moge1<-data.frame(sp=colnames(D100m[[1]]),
ab1982=apply(D100m[[1]],2,sum),
ab2010=apply(D100m[[7]],2,sum))


moge2 <- data.frame(sp=rownames(trait),
                    convex=trait$convex_size_100)


moge3 <- merge(moge1,moge2,by="sp")


write.csv(moge3,"convex_abund.csv",row.names=F)




xx <- rnorm(100,100)

yy <- rnorm(100, 1.05*xx-2,5)
plot(yy~xx)
summary(lm(yy~xx))



set.seed(0)
dat <- gamSim(1,n=200,scale=2)
b <- gamm(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
# b <- gamm(y~s(x3),data=dat)

plot(b$gam,pages=1)
summary(b$lme) # details of underlying lme fit
summary(b$gam)





set.seed(0)
dat <- gamSim(1,n=200,scale=2)
b <- gamm(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
plot(b$gam,pages=1)
summary(b$lme) # details of underlying lme fit
summary(b$gam) # gam style summary of fitted model
anova(b$gam)
gam.check(b$gam) # simple checking plots

b$gam$linear.predictors

b$gam$fitted.values

b$gam$scale.estimated



   set.seed(0)
     dat <- gamSim(1,n=400,scale=2) ## simulate 4 term additive truth
     ## Now add 20 level random effect `fac'...
     dat$fac <- fac <- as.factor(sample(1:20,400,replace=TRUE))
     dat$y <- dat$y + model.matrix(~fac-1)%*%rnorm(20)*.5

     br <- gamm4(y~s(x0)+x1+s(x2),data=dat,random=~(1|fac))


br$gam$gcv.ubre


par(mfrow=c(1,3),mar=c(4,4,4,4))
plot(Moist~WSG,temp,xlab="WSG")
plot(Moist~sp.convex.mean,temp,xlab="Convexity")
plot(Moist~slope_size_100,temp,xlab="Slope (rarefied)")
par(mfrow=c(1,1))



plot(sp.convex.mean~slope_size_100,temp)
