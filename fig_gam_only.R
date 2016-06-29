rm(list = ls()) # This clears everything from memory.
load("~/Dropbox/BCI_Turnover/gam_fig.RData")

library(grid)
vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)
}

theme_set(theme_bw())


pdf("~/Dropbox/MS/TurnoverBCI/gamm.pdf",width=4.5,height=9,paper="special")

theme = theme(axis.text.x = element_text(size=rel(0.6)),
    axis.text.y = element_text(size=rel(0.6)),
    axis.title.y = element_text(size=rel(0.8)),
    axis.title.x = element_text(size=rel(0.8)),
    # plot.background=element_rect(fill="red"),
    plot.margin= unit(c(1,0.5,0,0), units = "lines"))

theme2 = theme(axis.text.x = element_blank(),
    axis.text.y = element_text(size=rel(0.6)),
    axis.title.y = element_text(size=rel(0.8)),
    axis.title.x = element_text(size=rel(0.8)),
    # plot.background=element_rect(fill="red"),
    plot.margin= unit(c(1,0.5,-1,0), units = "lines"))

p1 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=WSG),alpha=0.3)+ theme2

p1 <- p1 + labs(x="",title="0.04ha subplots",
    y = expression(paste("Wood density (g ",cm^-3,")")))

p1 <- p1 + geom_line(data=r1data,aes(x=time,y=WSG),col="black")
p1 <- p1 + geom_ribbon(data=r1data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
# p1 <- p1 + geom_line(data=r1data,aes(x=time,y=WSGr),col="red")
# p1 <- p1 + geom_ribbon(data=r1data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")




p2 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=Moist),alpha=0.3)+ theme2+ labs(x="",y="Moisture")
p2 <- p2 + geom_line(data=r2data,aes(x=time,y=Moist),col="black")
p2 <- p2 + geom_ribbon(data=r2data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
# p2 <- p2 + geom_line(data=r2data,aes(x=time,y=WSGr),col="red")+ labs(x="")
# p2 <- p2 + geom_ribbon(data=r2data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")

p3 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=convex),alpha=0.3)+ theme2+ labs(x="",y="Convexity (m)")
p3 <- p3 + geom_line(data=r3data,aes(x=time,y=Convexity),col="black")
p3 <- p3 + geom_ribbon(data=r3data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
# p3 <- p3 + geom_line(data=r3data,aes(x=time,y=Convexityr),col="red")
# p3 <- p3 + geom_ribbon(data=r3data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red") + theme2+ labs(x="")


p4 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=slope),alpha=0.3)+ theme+ labs(x="Time",y="Slope (degrees)")
p4 <- p4 + geom_line(data=r4data,aes(x=time,y=Slope),col="black")
p4 <- p4 + geom_ribbon(data=r4data,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
# p4 <- p4 + geom_line(data=r4data,aes(x=time,y=WSGr),col="red")
# p4 <- p4 + geom_ribbon(data=r4data,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")

p5 <- ggplot(data=moge100) + geom_point(aes(x=Time,y=WSG),alpha=0.3)
p5 <- p5 + geom_line(data=r1data100,aes(x=time,y=WSG),col="black")
p5 <- p5 + geom_ribbon(data=r1data100,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray")
p5 <- p5 + geom_line(data=r1data100,aes(x=time,y=WSGr),col="blue")
p5 <- p5 + geom_ribbon(data=r1data100,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="blue") + theme2 + labs(y="",x="",title="1ha subplots")



p6 <- ggplot(data=moge100) + geom_point(aes(x=Time,y=Moist),alpha=0.3)
p6 <- p6 + geom_line(data=r2data100,aes(x=time,y=Moist),col="black")
p6 <- p6 + geom_ribbon(data=r2data100,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") + theme2 + labs(y="",x="")
# p6 <- p6 + geom_line(data=r2data100,aes(x=time,y=Moistr),col="red")
# p6 <- p6 + geom_ribbon(data=r2data100,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")

p7 <- ggplot(data=moge100[moge100$site!=28,]) + geom_point(aes(x=Time,y=convex),alpha=0.3)
p7 <- p7 + geom_line(data=r3data100,aes(x=time,y=Convexity),col="black")
p7 <- p7 + geom_ribbon(data=r3data100,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") + theme2 + labs(y="",x="")
# p7 <- p7 + geom_line(data=r3data100,aes(x=time,y=Convexityr),col="red")
# p7 <- p7 + geom_ribbon(data=r3data100,aes(ymin=lower2,ymax=upper2,x=time),alpha=0.3,fill="red")


p8 <- ggplot(data=moge100) + geom_point(aes(x=Time,y=slope),alpha=0.3)
p8 <- p8 + geom_line(data=r4data100,aes(x=time,y=Slope),col="black")
p8 <- p8 + geom_ribbon(data=r4data100,aes(ymin=lower1,ymax=upper1,x=time),alpha=0.5,fill="gray") + theme+ labs(y="",x="Time")


grid.draw(rbind(gtable:::cbind_gtable(ggplotGrob(p1), ggplotGrob(p5), "last"),
        gtable:::cbind_gtable(ggplotGrob(p2), ggplotGrob(p6), "last"),
        gtable:::cbind_gtable(ggplotGrob(p3), ggplotGrob(p7), "last"),
        gtable:::cbind_gtable(ggplotGrob(p4), ggplotGrob(p8), "last"),
        size="first"))

dev.off()
