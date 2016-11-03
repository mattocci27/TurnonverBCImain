rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")

####
#kernel density
#####
library(ggplot2)
library(gridExtra)
theme_set(theme_bw())

kernel.20 <- data.frame(WSG=unlist(WSG20),
                       convex=unlist(convex20),
                       slope=unlist(slope20),
                       moist=unlist(Moist20),
                       time=c(rep(1982,50),
                              rep(1985,50),
                              rep(1990,50),
                              rep(1995,50),
                              rep(2000,50),
                              rep(2005,50),
                              rep(2010,50)))

kernel.100 <- data.frame(WSG=unlist(WSG100),
                       #convex=unlist(convex.100.rare[[10]),
                       #slope=unlist(slope.100.rare[[10]),
                       convex=unlist(convex100),
                       slope=unlist(slope100),
                       moist=unlist(Moist100),
                       time=c(rep(1982,50),
                              rep(1985,50),
                              rep(1990,50),
                              rep(1995,50),
                              rep(2000,50),
                              rep(2005,50),
                              rep(2010,50)))

temp <- sapply(WSG.ind,length)
k.ind.WSG <- data.frame(WSG=unlist(WSG.ind),
                        time=c(rep(1982,temp[1]),
                              rep(1985,temp[2]),
                              rep(1990,temp[3]),
                              rep(1995,temp[4]),
                              rep(2000,temp[5]),
                              rep(2005,temp[6]),
                              rep(2010,temp[7])))

temp <- sapply(Moist.ind,length)
k.ind.moist <- data.frame(moist=unlist(Moist.ind),
                        time=c(rep(1982,temp[1]),
                              rep(1985,temp[2]),
                              rep(1990,temp[3]),
                              rep(1995,temp[4]),
                              rep(2000,temp[5]),
                              rep(2005,temp[6]),
                              rep(2010,temp[7])))
# temp <- sapply(slope.ind.rare[[1]],length)
temp <- sapply(slope.ind,length)
k.ind.slope <- data.frame(slope=unlist(slope.ind.rare[[10]]),
                        time=c(rep(1982,temp[1]),
                              rep(1985,temp[2]),
                              rep(1990,temp[3]),
                              rep(1995,temp[4]),
                              rep(2000,temp[5]),
                              rep(2005,temp[6]),
                              rep(2010,temp[7])))

# temp <- sapply(convex.ind.rare[[1]],length)
temp <- sapply(convex.ind, length)
k.ind.convex <- data.frame(convex=unlist(convex.ind.rare[[10]]),
                        time=c(rep(1982,temp[1]),
                              rep(1985,temp[2]),
                              rep(1990,temp[3]),
                              rep(1995,temp[4]),
                              rep(2000,temp[5]),
                              rep(2005,temp[6]),
                              rep(2010,temp[7])))




postscript("~/Dropbox/MS/TurnoverBCI/density_WSG_all2.eps",width=6,height=4,paper="special")
theme = theme(axis.text.x = element_text(size=rel(0.6)), axis.text.y = element_text(size=rel(0.6)),axis.title.y = element_text(size=rel(0.6)),axis.title.x = element_text(size=rel(0.6)),
              plot.margin= unit(c(0,0,0,0), units = "lines"))

p1 <- ggplot(kernel.100, aes(x=WSG))
p1 <- p1 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.55,0.65) + labs(y="Denstiy for 1ha subplots", x="") + theme

p2 <- ggplot(kernel.20, aes(x=WSG))
p2 <- p2 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.53,0.68)  + labs(y="Denstiy for 0.04ha subplots",x="")+ theme

p3 <- ggplot(k.ind.WSG, aes(x=WSG))
p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(y="Density for each tree",x = expression(paste("Wood density (g ",cm^-3,")")))+ theme(legend.position=c(0.22,0.6)) + guides(colour=FALSE)+ theme

p4 <- ggplot(kernel.100, aes(x=moist))
p4 <- p4 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.3,0.9)  + guides(colour=FALSE)  + labs(y="",x="")+ theme

p5 <- ggplot(kernel.20, aes(x=moist))
p5 <- p5 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0,1.2)  + guides(colour=FALSE)  + labs(y="",x="")+ theme

p6 <- ggplot(k.ind.moist, aes(x=moist))
p6 <- p6 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Moisture") + guides(colour=FALSE) + xlim(-2.1,4)+ theme

p7 <- ggplot(kernel.100, aes(x=convex))
p7 <- p7 + geom_density(adjust=1,aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.03,0.09)  + guides(colour=FALSE) + labs(y="",x="")+ theme


p8 <- ggplot(kernel.20, aes(x=convex))
p8 <- p8 + geom_density(adjust=1,aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.03,0.09)  + guides(colour=FALSE) + labs(y="",x="")+ theme


p9 <- ggplot(k.ind.convex, aes(x=convex))
p9 <- p9 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Convexity (m)") + guides(colour=FALSE) + xlim(-0.15,0.25)+ theme

p10 <- ggplot(kernel.100, aes(x=slope))
p10 <- p10 + geom_density(adjust=1,aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(4,6)  + guides(colour=FALSE)+ labs(y="",x="")+ theme

p11 <- ggplot(kernel.20, aes(x=slope))
p11 <- p11 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(4,6)  + guides(colour=FALSE)+ labs(y="",x="")+ theme

p12 <- ggplot(k.ind.slope, aes(x=slope))
p12 <- p12 + geom_density(adjust=4, aes(colour=as.factor(time))) + xlim(0.6,15)+ labs(y=" ",x="Slope (degrees)") + guides(colour=FALSE)+ theme



grid.draw(rbind(p1, p2, p3, size="first"))

grid.arrange(p1,p4,p7,p10,
      p2,p5,p8,p11,
      p3,p6,p9,p12,ncol=4)

dev.off()


library(cowplot)
plot_grid(p1, p4, p7, p10,
      p2, p5, p8, p11,
      p3, p6, p9, p12, ncol = 4, align = "v")


postscript("~/Dropbox/MS/TurnoverBCI/label.eps",width=6,height=4,paper="special")
p1 <- ggplot(kernel.100, aes(x=WSG))
p1 <- p1 + geom_density(aes(colour=as.factor(time))) + xlim(0.55,0.65) + labs(y="Denstiy for 1ha subplots", x="") + theme

print(p1)
dev.off()








##################################


#for rarefied convex and slope

# postscript("~/Dropbox/MS/TurnoverBCI/density_WSG_all2.eps",width=6,height=4,paper="special")
# theme = theme(axis.text.x = element_text(size=rel(0.6)), axis.text.y = element_text(size=rel(0.6)),axis.title.y = element_text(size=rel(0.6)),axis.title.x = element_text(size=rel(0.6)),
#               plot.margin= unit(c(0,0,0,0), units = "lines"))
#
# p1 <- ggplot(kernel.100, aes(x=WSG))
# p1 <- p1 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.55,0.65) + labs(y="Denstiy for 1ha subplots", x="") + theme
#
# p2 <- ggplot(kernel.20, aes(x=WSG))
# p2 <- p2 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.53,0.68)  + labs(y="Denstiy for 0.04ha subplots",x="")+ theme
#
#  #labs(y=expression(paste("Density for 400-",m^2," subplots")), x="")
# #
# # p3 <- ggplot(k.ind.WSG, aes(x=WSG))
# # p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(colour = "Color key",y="Density for each tree")+ theme
#
# p3 <- ggplot(k.ind.WSG, aes(x=WSG))
# p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(y="Density for each tree")+ theme(legend.position=c(0.22,0.6)) + guides(colour=FALSE)+ theme
#
#
# p4 <- ggplot(kernel.100, aes(x=moist))
# p4 <- p4 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.3,0.9)  + guides(colour=FALSE)  + labs(y="",x="")+ theme
#
# p5 <- ggplot(kernel.20, aes(x=moist))
# p5 <- p5 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0,1.2)  + guides(colour=FALSE)  + labs(y="",x="")+ theme
#
# p6 <- ggplot(k.ind.moist, aes(x=moist))
# p6 <- p6 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Moisture") + guides(colour=FALSE) + xlim(-2.1,4)+ theme
#
# p7 <- ggplot(kernel.100, aes(x=convex))
# p7 <- p7 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.03,0.09)  + guides(colour=FALSE) + labs(y="",x="")+ theme
#
# p8 <- ggplot(kernel.20, aes(x=convex))
# p8 <- p8 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0,0.11)  + guides(colour=FALSE) + labs(y="",x="")+ theme
#
# p9 <- ggplot(k.ind.convex, aes(x=convex))
# p9 <- p9 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Convexity") + guides(colour=FALSE) + xlim(-0.2,0.28)+ theme
#
# p10 <- ggplot(kernel.100, aes(x=slope))
# p10 <- p10 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(7,10)  + guides(colour=FALSE)+ labs(y="",x="")+ theme
#
# p11 <- ggplot(kernel.20, aes(x=slope))
# p11 <- p11 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(7,10.5)  + guides(colour=FALSE)+ labs(y="",x="")+ theme
#
# p12 <- ggplot(k.ind.slope, aes(x=slope))
# p12 <- p12 + geom_density(adjust=4, aes(colour=as.factor(time))) + xlim(0.6,18)+ labs(y=" ",x="Slope") + guides(colour=FALSE)+ theme
#
#
# grid.arrange(p1,p4,p7,p10,
#       p2,p5,p8,p11,
#       p3,p6,p9,p12,ncol=4)
#
# dev.off()
