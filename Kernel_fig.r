rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")



temp1 <- unlist(WSG100)
temp2 <- unlist(WSG.ind)

ks.test(temp1,temp2)


####
#kernel density
#####
library(ggplot2)
library(gridExtra)
library(gird)
theme_set(theme_bw())

vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)
}
kernel.20 <- data.frame(WSG=unlist(WSG20),
                       convex=unlist(convex.20.rare[[10]]),
                       slope=unlist(slope.20.rare[[10]]),
                       moist=unlist(Moist20),
                       time=c(rep(1982,50),
                              rep(1985,50),
                              rep(1990,50),
                              rep(1995,50),
                              rep(2000,50),
                              rep(2005,50),
                              rep(2010,50)))


kernel.100 <- data.frame(WSG=unlist(WSG100),
                       convex=unlist(convex.100.rare[[10]]),
                       slope=unlist(slope.100.rare[[10]]),
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
temp <- sapply(slope.ind.rare[[1]],length)
k.ind.slope <- data.frame(slope=unlist(slope.ind.rare[[10]]),
                        time=c(rep(1982,temp[1]),
                              rep(1985,temp[2]),
                              rep(1990,temp[3]),
                              rep(1995,temp[4]),
                              rep(2000,temp[5]),
                              rep(2005,temp[6]),
                              rep(2010,temp[7])))

temp <- sapply(convex.ind.rare[[1]],length)
k.ind.convex <- data.frame(convex=unlist(convex.ind.rare[[10]]),
                        time=c(rep(1982,temp[1]),
                              rep(1985,temp[2]),
                              rep(1990,temp[3]),
                              rep(1995,temp[4]),
                              rep(2000,temp[5]),
                              rep(2005,temp[6]),
                              rep(2010,temp[7])))




postscript("~/Dropbox/MS/TurnoverBCI/density_WSG_all.eps",width=6,height=4,paper="special")
theme = theme(axis.text.x = element_text(size=rel(0.6)), axis.text.y = element_text(size=rel(0.6)),axis.title.y = element_text(size=rel(0.6)),axis.title.x = element_text(size=rel(0.6)),
              plot.margin= unit(c(0,0,0,0), units = "lines"))

p1 <- ggplot(kernel.100, aes(x=WSG))
p1 <- p1 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.55,0.65) + labs(y="Denstiy for 1ha subplots", x="") + theme 

p2 <- ggplot(kernel.20, aes(x=WSG))
p2 <- p2 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.53,0.68)  + labs(y="Denstiy for 0.04ha subplots",x="")+ theme

 #labs(y=expression(paste("Density for 400-",m^2," subplots")), x="")

p3 <- ggplot(k.ind.WSG, aes(x=WSG))
p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(colour = "Color key",y="Density for each tree")+ theme

p3 <- ggplot(k.ind.WSG, aes(x=WSG))
p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(y="Density for each tree")+ theme(legend.position=c(0.22,0.6)) + guides(colour=FALSE)+ theme


# ,legend.key.size = unit(10,"pt")

p4 <- ggplot(kernel.100, aes(x=moist))
p4 <- p4 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.3,0.9)  + guides(colour=FALSE)  + labs(y="",x="")+ theme

p5 <- ggplot(kernel.20, aes(x=moist))
p5 <- p5 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0,1.2)  + guides(colour=FALSE)  + labs(y="",x="")+ theme

p6 <- ggplot(k.ind.moist, aes(x=moist))
p6 <- p6 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Moisture") + guides(colour=FALSE) + xlim(-2.1,4)+ theme


p7 <- ggplot(kernel.100, aes(x=convex))
p7 <- p7 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.03,0.09)  + guides(colour=FALSE) + labs(y="",x="")+ theme

p8 <- ggplot(kernel.20, aes(x=convex))
p8 <- p8 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0,0.11)  + guides(colour=FALSE) + labs(y="",x="")+ theme

p9 <- ggplot(k.ind.convex, aes(x=convex))
p9 <- p9 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Convexity") + guides(colour=FALSE) + xlim(-0.2,0.28)+ theme

p10 <- ggplot(kernel.100, aes(x=slope))
p10 <- p10 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(7,10)  + guides(colour=FALSE)+ labs(y="",x="")+ theme


p11 <- ggplot(kernel.20, aes(x=slope))
p11 <- p11 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(7,10.5)  + guides(colour=FALSE)+ labs(y="",x="")+ theme

p12 <- ggplot(k.ind.slope, aes(x=slope))
p12 <- p12 + geom_density(adjust=4, aes(colour=as.factor(time))) + xlim(0.6,18)+ labs(y=" ",x="Slope") + guides(colour=FALSE)+ theme


grid.arrange(p1,p4,p7,p10,
             p2,p5,p8,p11,
             p3,p6,p9,p12,ncol=4)

dev.off()





postscript("~/Dropbox/MS/TurnoverBCI/label.eps",width=6,height=4,paper="special")
p1 <- ggplot(kernel.100, aes(x=WSG))
p1 <- p1 + geom_density(aes(colour=as.factor(time))) + xlim(0.55,0.65) + labs(y="Denstiy for 1ha subplots", x="") + theme 

print(p1)
dev.off()








##################################





theme = theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)),
              plot.margin= unit(c(0,0,0,0), units = "lines"))

p1 <- ggplot(kernel.100, aes(x=WSG))
p1 <- p1 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.55,0.65) + labs(y="Denstiy for 1ha subplots", x="") + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8))) 



# p1 <- ggplot(kernel.100, aes(x=WSG))
# p1 <- p1 + geom_density(aes(colour=as.factor(time)))  + xlim(0.55,0.65) + labs(y="Denstiy for 1ha subplots", x="") + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8))) 

# p1 + guides(fill = guide_legend(keywidth = 20,keyheight = 10))


# + labs(y=expression(paste("Density for 10000-",m^2,"subplots")), x="") #+ theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)),plot.margin=(c(1,1,-0.5,1), "cm"))

# p1

p2 <- ggplot(kernel.20, aes(x=WSG))
p2 <- p2 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.53,0.68)  + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8))) + labs(y="Denstiy for 0.04ha subplots",x="")

 #labs(y=expression(paste("Density for 400-",m^2," subplots")), x="")

p3 <- ggplot(k.ind.WSG, aes(x=WSG))
p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(colour = "Color key",y="Density for each tree")+ theme(legend.position=c(0.22,0.6)) + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8))) +


p3 <- ggplot(k.ind.WSG, aes(x=WSG))
p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(y="Density for each tree")+ theme(legend.position=c(0.22,0.6)) + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8))) + guides(colour=FALSE)


# ,legend.key.size = unit(10,"pt")

p4 <- ggplot(kernel.100, aes(x=moist))
p4 <- p4 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.3,0.9)  + guides(colour=FALSE)  + labs(y="",x="") + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))

p5 <- ggplot(kernel.20, aes(x=moist))
p5 <- p5 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0,1.2)  + guides(colour=FALSE)  + labs(y="",x="") + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))

p6 <- ggplot(k.ind.moist, aes(x=moist))
p6 <- p6 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Moisture") + guides(colour=FALSE) + xlim(-2.1,4) + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))


p7 <- ggplot(kernel.100, aes(x=convex))
p7 <- p7 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.03,0.09)  + guides(colour=FALSE) + labs(y="",x="") + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))

p8 <- ggplot(kernel.20, aes(x=convex))
p8 <- p8 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0,0.11)  + guides(colour=FALSE) + labs(y="",x="") + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))

p9 <- ggplot(k.ind.convex, aes(x=convex))
p9 <- p9 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Convex") + guides(colour=FALSE) + xlim(-0.2,0.28) + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))

p10 <- ggplot(kernel.100, aes(x=slope))
p10 <- p10 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(7,10)  + guides(colour=FALSE)+ labs(y="",x="") + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))


p11 <- ggplot(kernel.20, aes(x=slope))
p11 <- p11 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(7,10.5)  + guides(colour=FALSE)+ labs(y="",x="") + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))

p12 <- ggplot(k.ind.slope, aes(x=slope))
p12 <- p12 + geom_density(adjust=4, aes(colour=as.factor(time))) + xlim(0.6,18)+ labs(y=" ",x="Slope") + guides(colour=FALSE) + theme(axis.text.x = element_text(size=rel(0.8)), axis.text.y = element_text(size=rel(0.8)),axis.title.y = element_text(size=rel(0.8)),axis.title.x = element_text(size=rel(0.8)))


grid.arrange(p1,p4,p7,p10,
             p2,p5,p8,p11,
             p3,p6,p9,p12,ncol=4)



grid.arrange(p1,p4,p7,p10,
             p2,p5,p8,p11,
             ncol=4,nrow=2,
             widths=c(3,3,3,3),
             heights=c(3,3))



dev.off()


print(p1,vp=vplayout(1,1))
print(p4,vp=vplayout(1,2))
print(p7,vp=vplayout(1,3))
print(p10,vp=vplayout(1,4))
print(p2,vp=vplayout(2,1))
print(p5,vp=vplayout(2,2))
print(p8,vp=vplayout(2,3))
print(p11,vp=vplayout(2,4))

# print(p1,vp=vplayout(1,1))
# print(p2,vp=vplayout(1,2))
# print(p3,vp=vplayout(2,1))
# print(p4,vp=vplayout(2,2))
# print(p5,vp=vplayout(3,1))
# print(p6,vp=vplayout(3,2))
# print(p7,vp=vplayout(4,1))
# print(p8,vp=vplayout(4,2))

# print(p1,vp=vplayout(1,1))
# print(p2,vp=vplayout(2,1))
# print(p3,vp=vplayout(1,2))
# print(p4,vp=vplayout(2,2))
# print(p5,vp=vplayout(1,3))
# print(p6,vp=vplayout(2,3))
# print(p7,vp=vplayout(1,4))
# print(p8,vp=vplayout(2,4))


# ##moist
# pdf("~/Dropbox/TurnoverNew/density_moist.pdf",width=15,height=5)
# pushViewport(viewport(layout=grid.layout(1, 3)))

# p1 <- ggplot(kernel.20, aes(x=moist))
# p1 <- p1 + geom_density(aes(colour=as.factor(time)))  + labs(title=expression(paste("1250 400-",m^2," subplots (No. of individuals)")))+ guides(colour=FALSE)

# p2 <- ggplot(kernel.100, aes(x=moist))
# p2 <- p2 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE)  + xlim(0.3,0.9)+ labs(title=expression(paste("50 10000-",m^2," subplots (No. of individuals)")))

# p3 <- ggplot(k.ind.moist, aes(x=moist))
# p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(colour = "Color key",title ="214100-247100 trees")+ theme(legend.position=c(0.15,0.6))

# print(p1,vp=vplayout(1,2))
# print(p2,vp=vplayout(1,3))
# print(p3,vp=vplayout(1,1))
# dev.off()

# ##convex
# pdf("~/Dropbox/TurnoverNew/density_convex.pdf",width=15,height=5)
# pushViewport(viewport(layout=grid.layout(1, 3)))

# p1 <- ggplot(kernel.20, aes(x=convex))
# p1 <- p1 + geom_density(aes(colour=as.factor(time)))  + labs(title=expression(paste("1250 400-",m^2," subplots (No. of individuals)")))+ guides(colour=FALSE)

# p2 <- ggplot(kernel.100, aes(x=convex))
# p2 <- p2 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE)   + xlim(0.02,0.09)+labs(title=expression(paste("50 10000-",m^2," subplots (No. of individuals)")))

# p3 <- ggplot(k.ind.convex, aes(x=convex))
# p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(colour = "Color key",title ="214100-247100 trees")+ theme(legend.position=c(0.15,0.6))

# print(p1,vp=vplayout(1,2))
# print(p2,vp=vplayout(1,3))
# print(p3,vp=vplayout(1,1))
# dev.off()


# ##slope
# pdf("~/Dropbox/TurnoverNew/density_slope.pdf",width=15,height=5)
# pushViewport(viewport(layout=grid.layout(1, 3)))

# p1 <- ggplot(kernel.20, aes(x=slope))
# p1 <- p1 + geom_density(aes(colour=as.factor(time)))  + labs(title=expression(paste("1250 400-",m^2," subplots (No. of individuals)")))+ guides(colour=FALSE)

# p2 <- ggplot(kernel.100, aes(x=slope))
# p2 <- p2 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE)  + xlim(7,10)+ labs(title=expression(paste("50 10000-",m^2," subplots (No. of individuals)")))

# p3 <- ggplot(k.ind.slope, aes(x=slope))
# p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(colour = "Color key",title ="214100-247100 trees")+ theme(legend.position=c(0.15,0.6))

# print(p1,vp=vplayout(1,2))
# print(p2,vp=vplayout(1,3))
# print(p3,vp=vplayout(1,1))
# dev.off()

##WSG
# postscript("~/Dropbox/MS/TurnoverBCI/density_WSG.eps",width=5.5,height=11,paper="special")
pushViewport(viewport(layout=grid.layout(2, 4)))
# pushViewport(viewport(layout=grid.layout(2, 4)))


temp1 <- kernel.100[,-2:-4]
temp2 <- kernel.20[,-2:-4]

temp3 <- k.ind.WSG

temp1$scale <- "1ha"
temp2$scale <- "0.04ha"
temp3$scale <- "trees"


library("scales")
integer_breaks <- function(n = 5, ...) {
  breaker <- pretty_breaks(n, ...)
  function(x) {
     breaks <- breaker(x)
     breaks[breaks == floor(breaks)]
  }
}

moge <- rbind(temp1,
              temp2,
              temp3)

temp1 <- kernel.100
temp2 <- kernel.20
temp1$scale <- "1ha"
temp2$scale <- "0.04ha"

moge <- rbind(temp1,
              temp2)


moge2 <- data.frame(val=c(moge$WSG,
                  moge$convex,
                  moge$slope,
                  moge$moist),
                    time = rep(moge$time,4),
                    trait=rep(c("WSG","convex","slope","moist"),each=9100))


p <- ggplot(moge, aes(x=WSG))
p <- p + geom_density(adjust=1,aes(colour=as.factor(time))) + facet_grid(~scale ,scales="free")  + scale_x_continuous(limits=c(0.5,0.65))
p


scale_y_continuous(breaks = integer_breaks())
