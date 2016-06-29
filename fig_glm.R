rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")


######
#GLM negative binomial
######

library(MASS)

# prepare data set
ab.data <- as.data.frame(sapply(D20m,function(x)apply(x,2,sum)))
ab.data$sp <- rownames(ab.data)
trait.temp <- data.frame(sp=rownames(trait),
                         moist=trait$Moist,
                         slope=trait$sp.slope.mean,
                         slope.sd = trait$sp.slope.sd,
                         convex=trait$sp.convex.mean,
                         convex.sd=trait$sp.convex.sd,
                         WSG=trait$WSG,
                         slope10=trait$slope_size_10,
                         slope20=trait$slope_size_20,
                         slope30=trait$slope_size_30,
                         slope40=trait$slope_size_40,
                         slope50=trait$slope_size_50,
                         slope60=trait$slope_size_60,
                         slope70=trait$slope_size_70,
                         slope80=trait$slope_size_80,
                         slope90=trait$slope_size_90,
                         slope100=trait$slope_size_100,
                         convex10=trait$convex_size_10,
                         convex20=trait$convex_size_20,
                         convex30=trait$convex_size_30,
                         convex40=trait$convex_size_40,
                         convex50=trait$convex_size_50,
                         convex60=trait$convex_size_60,
                         convex70=trait$convex_size_70,
                         convex80=trait$convex_size_80,
                         convex90=trait$convex_size_90,
                         convex100=trait$convex_size_100)

ab.t.data <- merge(ab.data,trait.temp,by="sp")
rownames(ab.t.data) <- ab.t.data$sp
ab.t.data2 <- na.omit(ab.t.data)


ab.t.data.r <- ab.t.data2[!(rownames(ab.t.data2) %in% c("PIPECO","POULAR")),]

# ab.t.data2$int <- ab.t.data2$WSG * ab.t.data2$slope
# ab.t.data$int <- ab.t.data$WSG * ab.t.data$slope

library(scales)

pdf("~/Dropbox/MS/TurnoverBCI/fig/glm_fig.pdf", width=6, height=2)
par(mfrow=c(1,4))
par(mar=c(0,0,0,0))
par(oma=c(3,4,2,2))

plot(census_2010/census_1982 ~ WSG, ab.t.data2,pch=16,cex=log(census_1982)/5,axes=F,log="y",col=alpha("black",0.5))
abline(h=1,lty=2)
box()
axis(2,tick=FALSE,line=-0.8)
axis(2,tcl=0.2, labels = FALSE)
axis(1,tick=FALSE,line=-0.8)
axis(1,tcl=0.2, labels = FALSE)

mtext(expression(paste("Wood density (g ",cm^-3,")")),side=1, line=1.5,cex=0.8)
mtext("No. of individuals in 2010 / \n No. of individuals in 1982",side=2, line=1.5,cex=0.8)

plot(census_2010/census_1982 ~ moist, ab.t.data2,pch=16,cex=log(census_1982)/5,axes=F,log="y",col=alpha("black",0.5))
abline(h=1,lty=2)
box()
axis(1,tick=FALSE,line=-0.8)
axis(1,tcl=0.2, labels = FALSE)
mtext("Moisture",side=1, line=1.5,cex=0.8)

plot(census_2010/census_1982 ~ convex, ab.t.data2,pch=16,cex=log(census_1982)/5,axes=F,log="y",col=alpha("black",0.5))
abline(h=1,lty=2)
box()
axis(1,tick=FALSE,line=-0.8)
axis(1,tcl=0.2, labels = FALSE)
mtext("Convexity (m)",side=1, line=1.5,cex=0.8)

plot(census_2010/census_1982 ~ slope, ab.t.data2,pch=16,cex=log(census_1982)/5,axes=F,log="y",col=alpha("black",0.5))
abline(h=1,lty=2)
box()
axis(1,tick=FALSE,line=-0.8)
axis(1,tcl=0.2, labels = FALSE)
mtext("Slope (degrees)",side=1, line=1.5,cex=0.8)
dev.off()




pdf("~/Dropbox/MS/TurnoverBCI/fig/glm_fig_n.pdf", width=6, height=2)
par(mfrow=c(1,4))
par(mar=c(0,0,0,0))
par(oma=c(3,4,2,2))

plot(census_2010/census_1982 ~ WSG, ab.t.data2,pch=16,cex=log(census_1982)/5,axes=F,col=alpha("black",0.5))
abline(h=1,lty=2)
box()
axis(2,tick=FALSE,line=-0.8)
axis(2,tcl=0.2, labels = FALSE)
axis(1,tick=FALSE,line=-0.8)
axis(1,tcl=0.2, labels = FALSE)

mtext(expression(paste("WSG (g ",cm^-3,")")),side=1, line=1.5,cex=0.8)
mtext("No. of individuals in 2010 / \n No. of individuals in 1982",side=2, line=1.5,cex=0.8)

plot(census_2010/census_1982 ~ moist, ab.t.data2,pch=16,cex=log(census_1982)/5,axes=F,col=alpha("black",0.5))
abline(h=1,lty=2)
box()
axis(1,tick=FALSE,line=-0.8)
axis(1,tcl=0.2, labels = FALSE)
mtext("Moisture",side=1, line=1.5,cex=0.8)

plot(census_2010/census_1982 ~ convex, ab.t.data2,pch=16,cex=log(census_1982)/5,axes=F,col=alpha("black",0.5))
abline(h=1,lty=2)
box()
axis(1,tick=FALSE,line=-0.8)
axis(1,tcl=0.2, labels = FALSE)
mtext("Convexity (m)",side=1, line=1.5,cex=0.8)

plot(census_2010/census_1982 ~ slope, ab.t.data2,pch=16,cex=log(census_1982)/5,axes=F,col=alpha("black",0.5))
abline(h=1,lty=2)
box()
axis(1,tick=FALSE,line=-0.8)
axis(1,tcl=0.2, labels = FALSE)
mtext("Slope (degrees)",side=1, line=1.5,cex=0.8)
dev.off()
