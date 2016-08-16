#gam20151102.r
rm(list = ls()) # This clears everything from memory.


setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")


library(mgcv)
library(ggplot2)
library(gridExtra)
library(grid)
library(spikeSlabGAM)
unlist(WSG100)
unlist(Moist100)
unlist(slope.100.rare[10])
unlist(convex.100.rare[10])

time <- c(1982,1985,1990,1995,2000,2005,2010)
Time <- c(rep(1982,50),
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

WSG20.rm <-list()
Moist20.rm <-list()
slope20.rm <-list()
convex20.rm <-list()

# D100m[[1]][1:5,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))]


###use convex mean
for (i in 1:7){
  WSG100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR"))],trait,"WSG")
  Moist100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR","PSYCHO"))],trait,"Moist")
  slope100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PIPECO","POULAR","PSYCHO"))],trait,"slope_size_100")
  # convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("ALSEBL","SWARS1"))],trait,"sp.convex.mean")
  convex100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]) %in% c("PSYCHO"))],trait,"sp.convex.mean")
}


for (i in 1:7){
  WSG20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("PIPECO","POULAR"))],trait,"WSG")
  Moist20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("PIPECO","POULAR","PSYCHO"))],trait,"Moist")
  slope20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("PIPECO","POULAR","PSYCHO"))],trait,"slope_size_20")
  # convex20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("ALSEBL","SWARS1"))],trait,"sp.convex.mean")
  convex20.rm[[i]] <- com.mean.ab(D20m[[i]][,!(colnames(D20m[[1]]) %in% c("PSYCHO"))],trait,"sp.convex.mean")
}



moge100 <- data.frame(WSG = unlist(WSG100),
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

moge20r <- data.frame(WSG = unlist(WSG20.rm),
                   Moist = unlist(Moist20.rm),
                   slope = unlist(slope20.rm),
                   convex = unlist(convex20.rm),
                   site = as.factor(rep(1:1250,7)),
                   Time)

r1.r <- gamm(WSG ~  s(Time,k=4),
  random = list(site=~1), data=moge20, correlation = corAR1())
r2.r <- gamm(Moist ~  s(Time,k=4),
  random = list(site=~1), data=moge20, correlation = corAR1())
r3.r <- gamm(convex ~  s(Time,k=4),
  random = list(site=~1), data=moge20, correlation = corAR1())
r4.r <- gamm(slope ~  s(Time,k=4),
  random = list(site=~1), data=moge20, correlation = corAR1())

r1 <- gamm(WSG ~  s(Time,k=4),
  random = list(site=~1), data=moge20, correlation = corAR1())
r2 <- gamm(Moist ~  s(Time,k=4),
  random = list(site=~1), data=moge20, correlation = corAR1())
r3 <- gamm(convex ~  s(Time,k=4),
  random = list(site=~1), data=moge20, correlation = corAR1())
r4 <- gamm(slope ~  s(Time,k=4),
  random = list(site=~1), data=moge20, correlation = corAR1())


par(mfrow=c(5,10),mar=c(0,0,0,0))
for(i in 1:50){
  plot(slope~Time, moge100[moge100$site==i,])
}
temp <- gam(convex ~   site, data=moge100, correlation = corAR1())
summary(temp)

r1.r100 <- gamm(WSG ~  s(Time,k=4),
  random = list(site=~1), data=moge100r, correlation = corAR1())

r2.r100<- gamm(Moist ~  s(Time,k=4),
  random = list(site=~1), data=moge100r, correlation = corAR1())
r3.r100<- gamm(convex ~  s(Time,k=4),
  random = list(site=~1), data=moge100r, correlation = corAR1())
r4.r100<- gamm(slope ~  s(Time,k=4),
  random = list(site=~1), data=moge100r, correlation = corAR1())

r1_100 <- gamm(WSG ~  s(Time,k=4),
  random = list(site=~1), data=moge100, correlation = corAR1())
# summary(r1_100$gam)
# plot(r1_100$gam)

r2_100 <- gamm(Moist ~  s(Time,k=4),
  random = list(site=~1), data=moge100, correlation = corAR1())
r3_100 <- gamm(convex ~  s(Time,k=4),
  random = list(site=~1), data=moge100, correlation = corAR1())

library(lme4)
temp = lmer(convex ~ Time + (1|site),moge100)

r4_100 <- gamm(slope ~  s(Time,k=4),
  random = list(site=~1), data=moge100, correlation = corAR1())

r1_100_2 <- gam(WSG ~  s(Time,k=4), data=moge100, correlation = corAR1())
r1.r100_2 <- gam(WSG ~  s(Time,k=4), data=moge100r, correlation = corAR1())

###100m
SS <- sum((moge[,"WSG"] - mean(moge[,"WSG"]))^2)
PREDS <- sum((r1_100$gam$fitted.values - r1_100$gam$y)^2)
rr1 = 1 - PREDS/SS

SS <- sum((moge[,"Moist"] - mean(moge[,"Moist"]))^2)
PREDS <- sum((r2_100$gam$fitted.values - r2_100$gam$y)^2)
rr2 = 1 - PREDS/SS

SS <- sum((moge[,"convex"] - mean(moge[,"convex"]))^2)
PREDS <- sum((r3_100$gam$fitted.values - r3_100$gam$y)^2)
rr3 = 1 - PREDS/SS

SS <- sum((moge[,"slope"] - mean(moge[,"slope"]))^2)
PREDS <- sum((r4_100$gam$fitted.values - r4_100$gam$y)^2)
rr4 = 1 - PREDS/SS
rcv100 = c(rr1,rr2,rr3,rr4)


###20m
SS <- sum((moge[,"WSG"] - mean(moge[,"WSG"]))^2)
PREDS <- sum((r1$gam$fitted.values - r1$gam$y)^2)
rr1 = 1 - PREDS/SS

SS <- sum((moge[,"Moist"] - mean(moge[,"Moist"]))^2)
PREDS <- sum((r2$gam$fitted.values - r2$gam$y)^2)
rr2 = 1 - PREDS/SS

SS <- sum((moge[,"convex"] - mean(moge[,"convex"]))^2)
PREDS <- sum((r3$gam$fitted.values - r3$gam$y)^2)
rr3 = 1 - PREDS/SS

SS <- sum((moge[,"slope"] - mean(moge[,"slope"]))^2)
PREDS <- sum((r4$gam$fitted.values - r4$gam$y)^2)
rr4 = 1 - PREDS/SS
rcv20 = c(rr1,rr2,rr3,rr4)
print(rcv20)


summary(r1_100_2)
summary(r1.r100_2)


summary(r1_100)
#
# temp <- gamm(convex ~  s(Time,k=4), random = list(site=~1), data=moge100[moge100$site!=10&moge100$site!=28,], correlation = corAR1())
# summary(temp$gam)
#
#
#
#
# aa <- gamm(Moist ~  s(Time,k=4), random = list(site=~1), data=moge100, correlation = corAR1())
# aa2 <- gamm(Moist ~  1, random = list(site=~1), data=moge100, correlation = corAR1())
#
# aa2 <- gamm(Moist ~  s(Time,k=4), data=moge100, correlation = corAR1())
#
res1 <- predict(r1$gam, se.fit=T)


res1 <- predict(r1$gam, se.fit=T)
res1r <- predict(r1.r$gam, se.fit=T)

res2 <- predict(r2$gam, se.fit=T)
res2r <- predict(r2.r$gam, se.fit=T)


res3 <- predict(r3$gam, se.fit=T)
res3r <- predict(r3.r$gam, se.fit=T)

res4 <- predict(r4$gam, se.fit=T)
res4r <- predict(r4.r$gam, se.fit=T)


res1_100 <- predict(r1_100$gam, se.fit=T)
res1r100 <- predict(r1.r100$gam, se.fit=T)

res2_100 <- predict(r2_100$gam, se.fit=T)
res2r100 <- predict(r2.r100$gam, se.fit=T)


res3_100 <- predict(r3_100$gam, se.fit=T)
res3r100 <- predict(r3.r100$gam, se.fit=T)

res4_100 <- predict(r4_100$gam, se.fit=T)
res4r100 <- predict(r4.r100$gam, se.fit=T)

#100m
r1data100 <- data.frame(time = spline(r1_100$gam$model$Time, res1_100$fit)$x,
                    WSG = spline(r1_100$gam$model$Time, res1_100$fit)$y,
                    WSGr = spline(r1.r100$gam$model$Time, res1r100$fit)$y,
                    upper1 = spline(r1_100$gam$model$Time,
                        res1_100$fit + 1.96*res1r100$se.fit)$y,
                    lower1 = spline(r1_100$gam$model$Time,
                        res1_100$fit - 1.96*res1r100$se.fit)$y,
                    upper2 = spline(r1.r100$gam$model$Time,
                        res1r100$fit + 1.96*res1r100$se.fit)$y,
                    lower2 = spline(r1.r100$gam$model$Time,
                        res1r100$fit - 1.96*res1r100$se.fit)$y)

r2data100 <- data.frame(time = spline(r2_100$gam$model$Time, res2_100$fit)$x,
                    Moist = spline(r2_100$gam$model$Time, res2_100$fit)$y,
                    Moistr = spline(r2.r100$gam$model$Time, res2r100$fit)$y,
                    upper1 = spline(r2_100$gam$model$Time,
                        res2_100$fit + 1.96*res2r100$se.fit)$y,
                    lower1 = spline(r2_100$gam$model$Time,
                        res2_100$fit - 1.96*res2r100$se.fit)$y,
                    upper2 = spline(r2.r100$gam$model$Time,
                        res2r100$fit + 1.96*res2r100$se.fit)$y,
                    lower2 = spline(r2.r100$gam$model$Time,
                        res2r100$fit - 1.96*res2r100$se.fit)$y)


r3data100 <- data.frame(time = spline(r3_100$gam$model$Time, res3_100$fit)$x,
                    Convexity = spline(r3_100$gam$model$Time, res3_100$fit)$y,
                    Convexityr = spline(r3.r100$gam$model$Time, res3r100$fit)$y,
                    upper1 = spline(r3_100$gam$model$Time,
                        res3_100$fit + 1.96*res3r100$se.fit)$y,
                    lower1 = spline(r3_100$gam$model$Time,
                        res3_100$fit - 1.96*res3r100$se.fit)$y,
                    upper2 = spline(r3.r100$gam$model$Time,
                        res3r100$fit + 1.96*res3r100$se.fit)$y,
                    lower2 = spline(r3.r100$gam$model$Time,
                        res3r100$fit - 1.96*res3r100$se.fit)$y)


r4data100 <- data.frame(time = spline(r4_100$gam$model$Time, res4_100$fit)$x,
                    Slope = spline(r4_100$gam$model$Time, res4_100$fit)$y,
                    Sloper = spline(r4.r100$gam$model$Time, res4r100$fit)$y,
                    upper1 = spline(r4_100$gam$model$Time,
                        res4_100$fit + 1.96*res4r100$se.fit)$y,
                    lower1 = spline(r4_100$gam$model$Time,
                        res4_100$fit - 1.96*res4r100$se.fit)$y,
                    upper2 = spline(r4.r100$gam$model$Time,
                        res4r100$fit + 1.96*res4r100$se.fit)$y,
                    lower2 = spline(r4.r100$gam$model$Time,
                        res4r100$fit - 1.96*res4r100$se.fit)$y)



###20m
r1data<- data.frame(time = spline(r1$gam$model$Time, res1$fit)$x,
                    WSG = spline(r1$gam$model$Time, res1$fit)$y,
                    WSGr = spline(r1.r$gam$model$Time, res1r$fit)$y,
                    upper1 = spline(r1$gam$model$Time,
                        res1$fit + 1.96*res1r$se.fit)$y,
                    lower1 = spline(r1$gam$model$Time,
                        res1$fit - 1.96*res1r$se.fit)$y,
                    upper2 = spline(r1.r$gam$model$Time,
                        res1r$fit + 1.96*res1r$se.fit)$y,
                    lower2 = spline(r1.r$gam$model$Time,
                        res1r$fit - 1.96*res1r$se.fit)$y)

r2data<- data.frame(time = spline(r2$gam$model$Time, res2$fit)$x,
                    Moist = spline(r2$gam$model$Time, res2$fit)$y,
                    Moistr = spline(r2.r$gam$model$Time, res2r$fit)$y,
                    upper1 = spline(r2$gam$model$Time,
                        res2$fit + 1.96*res2r$se.fit)$y,
                    lower1 = spline(r2$gam$model$Time,
                        res2$fit - 1.96*res2r$se.fit)$y,
                    upper2 = spline(r2.r$gam$model$Time,
                        res2r$fit + 1.96*res2r$se.fit)$y,
                    lower2 = spline(r2.r$gam$model$Time,
                        res2r$fit - 1.96*res2r$se.fit)$y)

r3data<- data.frame(time = spline(r3$gam$model$Time, res3$fit)$x,
                    Convexity = spline(r3$gam$model$Time, res3$fit)$y,
                    Convexityr = spline(r3.r$gam$model$Time, res3r$fit)$y,
                    upper1 = spline(r3$gam$model$Time,
                        res3$fit + 1.96*res3r$se.fit)$y,
                    lower1 = spline(r3$gam$model$Time,
                        res3$fit - 1.96*res3r$se.fit)$y,
                    upper2 = spline(r3.r$gam$model$Time,
                        res3r$fit + 1.96*res3r$se.fit)$y,
                    lower2 = spline(r3.r$gam$model$Time,
                        res3r$fit - 1.96*res3r$se.fit)$y)

r4data<- data.frame(time = spline(r4$gam$model$Time, res4$fit)$x,
                    Slope = spline(r4$gam$model$Time, res4$fit)$y,
                    Sloper = spline(r4.r$gam$model$Time, res4r$fit)$y,
                    upper1 = spline(r4$gam$model$Time,
                        res4$fit + 1.96*res4r$se.fit)$y,
                    lower1 = spline(r4$gam$model$Time,
                        res4$fit - 1.96*res4r$se.fit)$y,
                    upper2 = spline(r4.r$gam$model$Time,
                        res4r$fit + 1.96*res4r$se.fit)$y,
                    lower2 = spline(r4.r$gam$model$Time,
                        res4r$fit - 1.96*res4r$se.fit)$y)



library(grid)
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)
}

theme_set(theme_bw())


save.image("gam_fig.RData")

pdf("~/Dropbox/MS/TurnoverBCI/gamm.pdf", width = 4.5, height = 9)

theme = theme(axis.text.x = element_text(size=rel(0.8)),
    axis.text.y = element_text(size=rel(0.8)),
    axis.title.y = element_text(size=rel(0.8)),
    axis.title.x = element_text(size=rel(0.8)),
    # plot.background=element_rect(fill="red"),
    plot.margin= unit(c(1,0.5,0,0), units = "lines"))

theme2 = theme(axis.text.x = element_blank(),
    axis.text.y = element_text(size=rel(0.8)),
    axis.title.y = element_text(size=rel(0.8)),
    axis.title.x = element_text(size=rel(0.8)),
    # plot.background=element_rect(fill="red"),
    plot.margin= unit(c(1,0.5,-1,0), units = "lines"))


m1 <- function(x){min(x) * 0.94}
m2 <- function(x){max(x) * 1.04}


p1 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=WSG),alpha=0.3)+ theme2 + labs(x="",title="0.04-ha subplots", y = "Wood density")
p1 <- p1 + geom_line(data=r1data,aes(x=time,y=WSG),col="black")
p1 <- p1 + geom_ribbon(data = r1data, aes(ymin=lower1,ymax=upper1,x=time),
  alpha=0.5, fill="gray") +
  ylim(moge20$WSG %>% m1, moge20$WSG %>% m2)


p2 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=Moist),alpha=0.3)+ theme2+ labs(x="",y="Moisture")
p2 <- p2 + geom_line(data=r2data,aes(x=time,y=Moist),col="black")
p2 <- p2 + geom_ribbon(data=r2data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") +
ylim(moge20$Moist %>% m1, moge20$Moist %>% m2)
# p2 <- p2 + geom_line(data=r2data,aes(x=time,y=WSGr),col="red")+ labs(x="")
# p2 <- p2 + geom_ribbon(data=r2data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")

p3 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=convex),alpha=0.3)+ theme2+ labs(x="",y="Convexity")
p3 <- p3 + geom_line(data=r3data,aes(x=time,y=Convexity),col="black")
p3 <- p3 + geom_ribbon(data=r3data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") +
  ylim(moge20$convex %>% m1, moge20$convex %>% m2)
# p3 <- p3 + geom_line(data=r3data,aes(x=time,y=Convexityr),col="red")
# p3 <- p3 + geom_ribbon(data=r3data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red") + theme2+ labs(x="")


p4 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=slope),alpha=0.3)+ theme+ labs(x="Time",y="Slope")
p4 <- p4 + geom_line(data=r4data,aes(x=time,y=Slope),col="black")
p4 <- p4 + geom_ribbon(data=r4data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") +
  ylim(moge20$slope %>% m1, moge20$slope %>% m2)
# p4 <- p4 + geom_line(data=r4data,aes(x=time,y=WSGr),col="red")
# p4 <- p4 + geom_ribbon(data=r4data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")

p5 <- ggplot(data=moge100) + geom_point(aes(x=Time,y=WSG),alpha=0.3)
p5 <- p5 + geom_line(data=r1data100,aes(x=time,y=WSG),col="black")
p5 <- p5 + geom_ribbon(data=r1data100,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
p5 <- p5 + geom_line(data=r1data100,aes(x=time,y=WSGr),col="blue")
p5 <- p5 + geom_ribbon(data=r1data100,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="blue") + theme2 + labs(y="",x="",title="1-ha subplots") +
  ylim(moge100$WSG %>% m1, moge100$WSG %>% m2)



p6 <- ggplot(data=moge100) + geom_point(aes(x=Time,y=Moist),alpha=0.3)
p6 <- p6 + geom_line(data=r2data100,aes(x=time,y=Moist),col="black")
p6 <- p6 + geom_ribbon(data=r2data100,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") + theme2 + labs(y="",x="") +
  ylim(moge100$Moist %>% m1, moge100$Moist %>% m2)
# p6 <- p6 + geom_line(data=r2data100,aes(x=time,y=Moistr),col="red")
# p6 <- p6 + geom_ribbon(data=r2data100,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")

p7 <- ggplot(data=moge100[moge100$site!=28,]) + geom_point(aes(x=Time,y=convex),alpha=0.3)
p7 <- p7 + geom_line(data=r3data100,aes(x=time,y=Convexity),col="black")
p7 <- p7 + geom_ribbon(data=r3data100,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") + theme2 + labs(y="",x="") +
  ylim(moge100$convex %>% m1, moge100$convex %>% m2)
# p7 <- p7 + geom_line(data=r3data100,aes(x=time,y=Convexityr),col="red")
# p7 <- p7 + geom_ribbon(data=r3data100,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")


p8 <- ggplot(data=moge100) + geom_point(aes(x=Time,y=slope),alpha=0.3)
p8 <- p8 + geom_line(data=r4data100,aes(x=time,y=Slope),col="black")
p8 <- p8 + geom_ribbon(data=r4data100,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") + theme+ labs(y="",x="Time") +
  ylim(moge100$slope %>% m1, moge100$slope %>% m2)


grid.draw(rbind(gtable:::cbind_gtable(ggplotGrob(p1), ggplotGrob(p5), "last"),
        gtable:::cbind_gtable(ggplotGrob(p2), ggplotGrob(p6), "last"),
        gtable:::cbind_gtable(ggplotGrob(p3), ggplotGrob(p7), "last"),
        gtable:::cbind_gtable(ggplotGrob(p4), ggplotGrob(p8), "last"),
        size="first"))

dev.off()

#
# r2_100 <- gamm(Moist ~  s(Time,bs="tp",k=4), random = list(site=~1), data=moge100, correlation = corAR1(),select=TRUE)
# summary(r2_100$gam)
