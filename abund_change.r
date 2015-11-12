rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")
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

#this may be useful way to detect species.
#using the product of delta abundance and deviaiton from mean (or median) trait for each species
WSGab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    delta_ab2 = ab.t.data$census_2010 - ab.t.data$census_1982,
                    delta_ab3 = ab.t.data$census_2010/ab.t.data$census_1982,
                    WSG = ab.t.data$WSG,
                    WSG_delta =ab.t.data$WSG - mean(ab.t.data$WSG,na.rm=T))
WSGab$index <- as.numeric(scale(WSGab$delta_ab)) * as.numeric(scale(WSGab$WSG_delta))

WSGab <- WSGab[order(WSGab$index),]

##make trees
library(FD)
WSGab2 = data.frame(na.omit(WSGab))
t.dis <- as.matrix(gowdis(as.matrix(WSGab2$WSG)))
rownames(t.dis) <- WSGab2$sp
colnames(t.dis) <- WSGab2$sp

dend <- hclust(as.dist(t.dis),method="average")

# dend <- color_branches(dend, k=6)
#
# plot(dend)

plot(tree, show.tip.label=FALSE)

tree <- as.phylo(dend)

tree.trait = WSGab2$delta_ab
names(tree.trait) = WSGab2$sp
phylosignal(tree.trait[tree$tip.label],tree)

tree <- as.phylo(dend)
plot(tree, show.tip.label=FALSE,type="fan",
    edge.color = c("red","blue","green"))
#
# tree$edge.length <- ifelse(tree$edge.length < 1.025e-03,tree$edge.length+0.01,tree$edge.length)
# plot(tree, show.tip.label=FALSE,type="fan")



library(phytools)
n= 3
moge = tapply(WSGab2$delta_ab, cut(WSGab2$delta_ab,n),median)
low1 = as.numeric(sapply(strsplit(names(moge),"\\(|,"),"[",2))
upper1 = as.numeric(sapply(strsplit(names(moge),"]|,"),"[",2))

WSGab2$temp = 1
WSGab2$temp2 = 1

low1 = c(-1,-0.004,-0.001,0,0.001,0.004)
upper1 = c(-0.004,-0.001,0,0.001,0.004,1)
my.col = rev(heat.colors(10))

my.col = c("blue","sky blue","light blue",
      "pink","salmon","red")

n=6
for (i in 1:n){
  WSGab2$temp = ifelse(WSGab2$delta_ab < upper1[i] &  WSGab2$delta_ab >= low1[i],my.col[i],WSGab2$temp)

  WSGab2$temp2 = ifelse(WSGab2$delta_ab < upper1[i] &  WSGab2$delta_ab >= low1[i],i,WSGab2$temp2)
}

plot(tree,"fan",cex=0.4,show.tip.label = FALSE,lwd=4)
# tiplabels(pch=22, col=WSGab2$delta_ab)
# plot(tree,cex=0.4,show.tip.label = FALSE)
# plot(tree,cex=0.3)

x = as.characterWSGab2$temp2)
x = WSGab2$delta_ab
names(x) = WSGab2$sp
obj = contMap(tree, x,type="fan",res=100,lwd=3,
outline=FALSE,legend=TRUE,plot=FALSE)


plotTree.wBars(obj$tree, x,scale=1,lwd=1)

plotTree.wBars(obj$tree, x,scale=1,colors=obj$col,method="plotSimmap",lwd=2)

plotTree.wBars(obj$tree, x, type = "fan",scale=1,colors=obj$col,method="plotSimmap",lwd=2)


# tree2 <-make.simmap(tree, x, model="ER", nsim=1)


plotSimmap(tree2, type="fan",ftype="off")






plotBranchbyTrait(tree,x, type="fan",mode="tips")

xx$cols[]<-heat.colors(1001)
plot(xx,type="fan",res=6,lwd=3,
outline=FALSE,legend=FALSE,show.tip.label=FALSE)

data(anoletree)
plotSimmap(anoletree,type="fan",part=0.5,fsize=0.9, ftype="i")


contMap(tree, x= rnorm(236))

tiplabels(pch = 22,
      col = WSGab2$temp,
      bg = WSGab2$temp,
      cex = 1.2)




WSGab2$temp = ifelse(WSGab2$delta_ab < -0.0283, 1,WSGab2$temp)
WSGab2$temp = ifelse(WSGab2$delta_ab > -0.0283&WSGab2$delta_ab <= -0.0134,3,WSGab2$temp)
WSGab2$temp = ifelse(WSGab2$delta_ab > -0.0134&WSGab2$delta_ab <= -0.00842,4,WSGab2$temp)


heatmap(cbind(WSGab2$delta_ab+1,WSGab2$delta_ab+1))

# moistab
moistab <- data.frame(sp = ab.t.data2$sp,
                    delta_ab = ab.t.data2$census_2010/sum(ab.t.data2$census_2010) - ab.t.data2$census_1982/sum(ab.t.data2$census_1982),
                    moist = ab.t.data2$moist,
                    moist_delta =ab.t.data2$moist - mean(ab.t.data2$moist))
moistab$index <- as.numeric(scale(moistab$delta_ab)) * as.numeric(scale(moistab$moist_delta))

# moistab$index2 <- moistab$delta_ab * moistab$moist_delta

moistab <- moistab[order(moistab$index),]

# convex100ab
convexab <- data.frame(sp = ab.t.data2$sp,
                    delta_ab = ab.t.data2$census_2010/sum(ab.t.data2$census_2010) - ab.t.data2$census_1982/sum(ab.t.data2$census_1982),
                    convex = ab.t.data2$convex,
                    convex_delta =ab.t.data2$convex - mean(ab.t.data2$convex))
convexab$index <- as.numeric(scale(convexab$delta_ab)) * as.numeric(scale(convexab$convex_delta))

# convexab$index2 <- convexab$delta_ab * convexab$_delta

convexab <- convexab[order(convexab$index),]

# slope100ab
slope100ab <- data.frame(sp = ab.t.data2$sp,
                    delta_ab = ab.t.data2$census_2010/sum(ab.t.data2$census_2010) - ab.t.data2$census_1982/sum(ab.t.data2$census_1982),
                    slope100 = ab.t.data2$slope100,
                    slope100_delta =ab.t.data2$slope100 - mean(ab.t.data2$slope100))
slope100ab$index <- as.numeric(scale(slope100ab$delta_ab)) * as.numeric(scale(slope100ab$slope100_delta))

slope100ab <- slope100ab[order(slope100ab$index),]



#using the product of delta abundance and deviaiton from mean (or median) trait for each species
WSGab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    WSG = ab.t.data$WSG,
                    WSG_delta =ab.t.data$WSG - mean(ab.t.data$WSG,na.rm=T))
WSGab$index <- WSGab$delta_ab * WSGab$WSG_delta*100

WSGab <- WSGab[order(WSGab$index),]

# moistab
moistab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    moist = ab.t.data$moist,
                    moist_delta =ab.t.data$moist - mean(ab.t.data$moist,na.rm=T))
moistab$index <- moistab$delta_ab * moistab$moist_delta*100

moistab <- moistab[order(moistab$index),]

# convex100ab
convexab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    convex = ab.t.data$convex,
                    convex_delta =ab.t.data$convex - mean(ab.t.data$convex,na.rm=T))
convexab$index <- convexab$delta_ab * convexab$convex_delta*100

convexab <- convexab[order(convexab$index),]
# convexab <- convexab[order(convexab$convex),]

# slope100ab
slope100ab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    slope100 = ab.t.data$slope100,
                    slope100_delta =ab.t.data$slope100 - mean(ab.t.data$slope100,na.rm=T))
slope100ab$index <- slope100ab$delta_ab * slope100ab$slope100_delta*100

slope100ab <- slope100ab[order(slope100ab$index),]


postscript("~/Dropbox/MS/TurnoverBCI/ab_change.eps",width=6,height=6,paper="special")
par(mfrow=c(2,2),mar=c(2,4,2,2))
barplot(as.numeric(na.omit(WSGab$index)), main = "WSG",xlab="", ylab= "Contribution index",col=ifelse(WSGab$delta_ab>0,"orange","purple"),border=ifelse(WSGab$delta_ab>0,"orange","purple"))
barplot(as.numeric(na.omit(moistab$index)), main = "Moisture",col=ifelse(moistab$delta_ab>0,"orange","purple"),border=ifelse(moistab$delta_ab>0,"orange","purple"))
barplot(as.numeric(na.omit(convexab$index)), main = "Convexity",col=ifelse(convexab$delta_ab>0,"orange","purple"),border=ifelse(convexab$delta_ab>0,"orange","purple"),
ylab="Contribuiton index")
barplot(as.numeric(na.omit(slope100ab$index)), main = "Slope",col=ifelse(slope100ab$delta_ab>0,"orange","purple"),border=ifelse(slope100ab$delta_ab>0,"orange","purple"))
par(mfrow=c(1,1))

dev.off()


write.csv(WSGab, "~/Dropbox/MS/TurnoverBCI/fig0/WSGab.csv")
write.csv(moistab, "~/Dropbox/MS/TurnoverBCI/fig0/moistab.csv")
write.csv(convexab, "~/Dropbox/MS/TurnoverBCI/fig0/convexab.csv")
write.csv(slope100ab, "~/Dropbox/MS/TurnoverBCI/fig0/slope100ab.csv")

head(WSGab)



barplot(as.numeric(na.omit(WSGab$index)), main = "WSG",xlab="", ylab= "",col=ifelse(WSGab$delta_ab>0,"orange","purple"),border=ifelse(WSGab$delta_ab>0,"orange","purple"))







barplot(moistab$moge, main = "Moist",col=ifelse(moistab$moge>0,"orange","purple"))

par(mfrow=c(1,2))


barplot(as.numeric(na.omit(WSGab$moge)), main = "WSG",col=ifelse(WSGab$delta_ab>0,"orange","purple"),border=ifelse(WSGab$delta_ab>0,"orange","purple"))

barplot(as.numeric(na.omit(convexab$moge)), main = "Convex",col=ifelse(convexab$delta_ab>0,"orange","purple"),border=ifelse(convexab$delta_ab>0,"orange","purple"))
par(mfrow=c(1,1))


par(mfrow=c(2,2))
barplot(WSGab$delta_ab, main="WSG",ylab ="abundance in 2010 - abundance in 1982",xlab="low WSG <---------> high WSG")
abline(v=101,lty=2)

barplot(moistab$delta_ab, main="Moist")
abline(v=nrow(moistab)/2, lty=2)

barplot(slope100ab$delta_ab, main="Slope")
abline(v=nrow(slope100ab)/2, lty=2)

barplot(convex100ab$delta_ab, main="Convex")
abline(v=nrow(convex100ab)/2, lty=2)

par(mforw=c(1,1))

# 198 XYL1MA      881  0.153073437
# 8   ALSEBL     1385  0.122604261
# 21  CALOLO     1231  0.094878789
head(WSGab)
head(moistab)
head(convexab)
head(slope100ab)

tail(na.omit(WSGab))
tail(na.omit(moistab))
tail(na.omit(convexab))
tail(na.omit(slope100ab))




aa <- scale(na.omit(convex100ab)$moge)

barplot(as.numeric(aa))






WSGab$temp <- ifelse(WSGab$moge > 0, 1, 0)
sum(WSGab$temp,na.rm=T)

nrow(WSGab)


temp <- ab.t.data$WSG
na.omit(temp)



kk <- convexab[convexab$convex< -0.08 | convexab$convex >0.08,]
kk <- convexab[convexab$convex> -0.05 & convexab$convex <0.05,]


nrow(na.omit(kk))


hist(scale(convexab$convex))




ab2<-cbind(ab.t.data["POULAR",2:8],
ab.t.data["PIPECO",2:8])

par(mfrow=c(1,3))
barplot(as.matrix(ab.t.data["POULAR",2:8]))
barplot(as.matrix(ab.t.data["PIPECO",2:8]))
barplot(as.matrix(ab.t.data["PSYCHO",2:8]))
par(mfrow=c(1,1))



library(circlize)
library(dendextend)
library(colorspace)
dend <- iris[1:30,-5] %>% dist %>% hclust %>% as.dendrogram

dend2 = dend



# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend2) <-
   rainbow_hcl(3)[sort_levels_values(
      as.numeric(iris[,5])[order.dendrogram(dend)]
   )]

# We shall add the flower type to the labels:
labels(dend2) <-paste(as.character(iris[,5])[order.dendrogram(dend)],
                           "(",labels(dend),")",
                           sep = "")


dend2 <- color_branches(dend2, k=3) #, groupLabels=iris_species)

par(mfrow=c(1,2))
plot(dend)
circlize_dendrogram(dend2)

%>% set("branches_lwd", c(1.5,1,1.5)) %>%
   set("branches_lty", c(1,1,3,1,1,2)) %>%
   set("labels_colors") %>% set("labels_cex", c(.9,1.2))



require(colorspace)
d_SIMS <- dist(firstpointsample5[,-1])
hc_SIMS <- hclust(d_SIMS)
labels(hc_SIMS)
dend_SIMS <- as.dendrogram(hc_SIMS)
SIMS_groups <- rev(levels(firstpointsample5[, 1]))
dend_SIMS <- color_branches(dend_SIMS, k = 3, groupLabels = SIMS_groups)
is.character(labels(dend_SIMS))
plot(dend_SIMS)
labels_colors(dend_SIMS) <- rainbow_hcl(3)[sort_levels_values(as.numeric(firstpointsample5[,1])[order.dendrogram(dend_SIMS)])]
labels(dend_SIMS) <- paste(as.character(firstpointsample5[, 1])[order.dendrogram(dend_SIMS)],"(", labels(dend_SIMS), ")", sep = "")
dend_SIMS <- hang.dendrogram(dend_SIMS, hang_height = 0.1)
dend_SIMS <- assign_values_to_leaves_nodePar(dend_SIMS, 0.5,"lab.cex")
par(mar = c(3, 3, 3, 7))
plot(dend_SIMS, main = "Clustered SIMS dataset\n (the labels give the true m/z groups)",horiz = TRUE, nodePar = list(cex = 0.007))
legend("topleft", legend = SIMS_groups, fill = rainbow_hcl(3))






data(anoletree)


tree2 <-make.simmap(tree, x, model="ER", nsim=1)

plotSimmap(anoletree,type="fan",part=0.5,fsize=0.9, ftype="i")



no colors provided. using the following legend:
    CG     GB      Non-     TC     TG        Tr        TW
"black"  "red"  "green3" "blue" "cyan" "magenta"  "yellow"

Note: type="fan" is in development.
Many options of type="phylogram" are not yet available.

> ss<-sort(unique(getStates(anoletree,"tips")))
> add.simmap.legend(colors=setNames(palette()[1:length(ss)],ss))
Click where you want to draw the legend





require(phytools)
data(anoletree)
x<-getStates(anoletree,"tips")
tree<-drop.tip.simmap(anoletree,names(x)[which(x=="Non-")])
x<-getStates(tree,"tips")
ecomorph.trees<-make.simmap(tree,x,nsim=1,model="ER")
