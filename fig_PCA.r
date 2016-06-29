rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")


tr <- data.frame(WD=trait$WSG,
                 Moist=trait$Moist,
                 Convexity=trait$sp.convex.mean,
                 Slope=trait$sp.slope.mean)




rownames(tr)<-tr$name
a<-princomp(scale(na.omit(tr)))
X <- a$loadings
# Plot the frame
plot(X, asp=1, type="n",ylim=c(-1, 1), xlim=c(-1,1))
abline(v=0, lty=3)
abline(h=0, lty=3)
# Plot arrows: see ?arrows for the syntax
arrows(0, 0, X[,1], X[,2], len=0.1, col="blue") # Label the arrows
#arrows(0, 0, X[,2], X[,3], len=0.1, col="blue") # Label the arrows
text(1.1*X, rownames(X), col="blue",xpd=TRUE)


cor(na.omit(tr),method = "spearman")





postscript("~/Dropbox/MS/TurnoverBCI/fig/ab_change.eps",width=6,height=6,paper="special")
par(mfrow=c(1,3),mar=c(2,4,2,2))
barplot(as.numeric(na.omit(moistab$index)), main = "Moisture",col=ifelse(moistab$delta_ab>0,"orange","purple"),border=ifelse(moistab$delta_ab>0,"orange","purple"))
barplot(as.numeric(na.omit(convexab$index)), main = "Convexity",col=ifelse(convexab$delta_ab>0,"orange","purple"),border=ifelse(convexab$delta_ab>0,"orange","purple"),
ylab="Contribuiton index")
barplot(as.numeric(na.omit(slopeab$index)), main = "Slope",col=ifelse(slopeab$delta_ab>0,"orange","purple"),border=ifelse(slopeab$delta_ab>0,"orange","purple"))
par(mfrow=c(1,1))

dev.off()

par(mfrow=c(1,2))
barplot()
barplot(1:5,loremwd=1)

write.csv(WSGab, "~/Dropbox/MS/TurnoverBCI/fig0/WSGab.csv")
write.csv(moistab, "~/Dropbox/MS/TurnoverBCI/fig0/moistab.csv")
write.csv(convexab, "~/Dropbox/MS/TurnoverBCI/fig0/convexab.csv")
write.csv(slope100ab, "~/Dropbox/MS/TurnoverBCI/fig0/slope100ab.csv")
