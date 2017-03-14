#######
#BCI data
######
rm(list = ls()) # This clears everything from memory.

library(foreach)
# setwd("~/Dropbox/temporal/")
source("~/Dropbox/BCI_Turnover/TurnoverSource20150611.r")

## BCI topo data
## this URL does not work (3/14/2017)
source("http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/files/source/spatial/quadfunc.r")
source("http://ctfs.arnarb.harvard.edu/Public/CTFSRPackage/files/source/topography/slope.r")


elev <- read.delim("~/Dropbox/BCI_TREE/50ha/bci50ha_elevdata_5by5.txt")
elev2 <- readelevdata(elev)
elev.list <- elev.to.list(elev)
topo <-allquadratslopes(elev.list)
topo$x <- rep(1:50,each=25)
topo$y <- rep(1:25,50)
topo$temp <- 1:1250
topo <- topo[order(topo$slope),]
topo$slope_rank <- 1:1250

topo <- topo[order(topo$convex),]
topo$convex_rank <- 1:1250

topo <- topo[order(topo$temp),]

## BCI trait data
# I should change this later
trait_data <- read.csv("~/Dropbox/BCI_TREE/BCITRAITS_20101220.csv")
trait_data$sp2 <- paste(trait_data$GENUS., trait_data$SPECIES.,sep="_")
trait_data2 <- read.csv("~/Dropbox/BCI_TREE/BCI LEAF ELEMENTAL COMPOSITION.csv")
t <-data.frame(SP=trait_data$SP.,sp2=trait_data$sp2,log.H=log10(trait_data$HEIGHT_AVG), log.LA = log10(trait_data$LEAFAREA_AVD), log.SM=log10(trait_data$SEED_DRY), log.SLA = log10(1/(trait_data$LMALAM_AVI/10000)), WSG=trait_data$SG100C_AVG)
t2<-data.frame(SP=trait_data2$SP, LIGHT=trait_data2$LIGHT ,Cmass=trait_data2$C, Nmass=trait_data2$N,Pmass=trait_data2$P)
t2.2 <-t2[t2$LIGHT=="SHADED",]
trait <- merge(t2.2, t, by="SP")
trait <- trait[-150,-2]

moist <- read.delim("~/Dropbox/BCI_Turnover/Condit_2013_PNAS_FullSpeciesResponses from Bettina.txt")
moist$sp2<-paste(sapply(strsplit(as.character(moist$Latin),"\\s"),"[",1),
      sapply(strsplit(as.character(moist$Latin),"\\s"),"[",2),
      sep="_")

trait <- merge(trait, moist,by="sp2",all=T)
trait <- trait[is.na(trait$SP)!=T,]

##BCI communtiy data

for (i in 1:7){
	load(paste("~/Dropbox/BCI_TREE/50ha/bci.full",i,".rdata",sep=""))
}

bci.full <- list(bci.full1,
                 bci.full2,
                 bci.full3,
                 bci.full4,
                 bci.full5,
                 bci.full6,
                 bci.full7)

D <- bci.full1
D$sp <- toupper(D$sp)

#survive
D$status1 <- as.factor(bci.full[[1]][,"status"])
D$a1 <- ifelse(bci.full[[1]][,"status"]=="A",1,0)# use "A" as survivors

for (i in 2:7){
bci.full[[i]][,"status"] <- as.factor(bci.full[[i]][,"status"])
D <- cbind(D,bci.full[[i]][,"status"])
suv <- ifelse(bci.full[[i]][,"status"]=="A",1,0)# use "A" as survivors
D <- cbind(D,suv)


dead <- ifelse(bci.full[[i-1]][,"status"]!="D"&bci.full[[i]][,"status"]=="D",1,0)
rec <- as.factor(ifelse(bci.full[[i-1]][,"status"]=="P"&bci.full[[i]][,"status"]=="A","Y","N"))
rec2 <- ifelse(rec=="Y",1,0)
D <- cbind(D,dead,rec,rec2)
}

##col names
temp <- list("status","a","d","rec","r")
temp2 <- sapply(temp,function(x){paste(x,2:7,sep="")})
colnames(D)[23:52] <- as.vector(t(temp2))

for (i in 1:7){
	D <- cbind(D, bci.full[[i]][,"dbh"])
}

colnames(D)[53:59] <- paste("dbh",1:7,sep="")
##

#100m
D$gx100 <- as.integer(D$gx/100)+1
D$gy100 <- as.integer(D$gy/100)+1
D$quad100 <- as.factor(paste(D$gx100,D$gy100,sep="_"))


a.temp <- paste("a",1:7,sep="")
dbh.temp <-paste("dbh",1:7,sep="")

D20m <- list()
D100m <- list()

D20ba <- list()
D100ba <- list()


for (i in 1:7){
	D20m[[i]] <- tapply(D[,a.temp[i]], list(D$quadrat,D$sp), sum, na.rm=T)
	D20m[[i]][is.na(D20m[[i]])] <- 0
	D20m[[i]] <- D20m[[i]][-1,]

	D100m[[i]] <- tapply(D[,a.temp[i]], list(D$quad100,D$sp), sum, na.rm=T)
	D100m[[i]][is.na(D100m[[i]])] <- 0
	D100m[[i]] <- D100m[[i]][-51,]

	# Basal area: m2 / ha
	D20ba[[i]] <- tapply(pi*(D[,dbh.temp[i]]/10/2)^2/100^2, list(D$quadrat,D$sp), sum)
	D20ba[[i]][is.na(D20ba[[i]])] <- 0
	D20ba[[i]] <- D20ba[[i]][-1,]

	D100ba[[i]] <- tapply(pi*(D[,dbh.temp[i]]/10/2)^2/100^2, list(D$quad100,D$sp), sum)
	D100ba[[i]][is.na(D100ba[[i]])] <- 0
	D100ba[[i]] <- D100ba[[i]][-51,]
}

names(D20m) <- names(D100m) <- names(D20ba) <- names(D100ba) <- c("census_1982",
                                                                  "census_1985",
                                                                  "census_1990",
                                                                  "census_1995",
                                                                  "census_2000",
                                                                  "census_2005",
                                                                  "census_2010")


rownames(topo) <- rownames(D20m[[1]])

###################################

##topo original values

temp.com <- D20m[["census_1982"]]
temp.sp <- apply(temp.com,2,sum)
temp.sp2 <- names(temp.sp[temp.sp>0])

topo.com <- D20m[["census_1982"]][,temp.sp2]


p.abund <- t(t(topo.com)/apply(topo.com ,2,sum))
p.abund[p.abund==0] <- NA

sp.slope <- p.abund*topo$slope
sp.slope.mean <- apply(sp.slope,2,sum,na.rm=T)
sp.slope.sd <- apply(sp.slope,2,sd,na.rm=T)

sp.convex <- p.abund*topo$convex
sp.convex.mean <- apply(sp.convex,2,sum,na.rm=T)
sp.convex.sd <- apply(sp.convex,2,sd,na.rm=T)

##################
# rarefaction
##################
#topo 10 - 100 steepest subplots
n.sample <- seq(10,100,length=10)

library(doSNOW)
cl_sock<-makeCluster(4, type="SOCK")

registerDoSNOW(cl_sock)

system.time(rare.slope<-foreach(i = 1:length(n.sample),.combine=cbind) %dopar% slope.rare2(n.sample=n.sample[i],n.runs=10000,temp.com,topo.com))



# moge<-slope.rare(n.sample=100,n.runs=100,temp.com,topo.com)

rownames(rare.slope) <- names(sp.slope.mean)
colnames(rare.slope) <- as.vector(rbind(paste("slope_size_",n.sample,sep=""),
                                  paste("convex_size_",n.sample,sep="")))



rare.slope.data <- data.frame(SP=rownames(rare.slope),
                              sp.slope.mean,
                              sp.slope.sd,
                              sp.convex.mean,
                              sp.convex.sd,
                              rare.slope)

trait <- merge(trait,rare.slope.data,by="SP",all=T)
rownames(trait) <- trait$SP



#community mean
WSG.ind <- list()
WSG20 <- list()
WSG100 <- list()
# WSG20.rm <- list()
# WSG100.rm <- list()

Moist.ind <- list()
Moist20 <- list()
Moist100 <- list()

slope.ind <- list()
slope20 <- list()
slope100 <- list()

convex.ind <- list()
convex20 <- list()
convex100 <- list()



# Moist20.rm <- list()
# Moist100.rm <- list()

for (i in 1:7){
	WSG20[[i]] <- com.mean.ab(D20m[[i]],trait,"WSG")
	WSG100[[i]] <- com.mean.ab(D100m[[i]],trait,"WSG")
	# WSG20.rm[[i]] <- com.mean.ab(D20m[[i]][,-c(242,255,151)],trait,"WSG") #check
	# WSG100.rm[[i]] <- com.mean.ab(D100m[[i]][,-c(242,255,151)],trait,"WSG")


	Moist20[[i]] <- com.mean.ab(D20m[[i]],trait,"Moist")
	Moist100[[i]] <- com.mean.ab(D100m[[i]],trait,"Moist")

	slope20[[i]] <- com.mean.ab(D20m[[i]],trait,"sp.slope.mean")
	slope100[[i]] <- com.mean.ab(D100m[[i]],trait,"sp.slope.mean")

	convex20[[i]] <- com.mean.ab(D20m[[i]],trait,"sp.convex.mean")
	convex100[[i]] <- com.mean.ab(D100m[[i]],trait,"sp.convex.mean")


	# Moist20.rm[[i]] <- com.mean.ab(D20m[[i]][,-c(242,255,151)],trait,"Moist") #check
	# Moist100.rm[[i]] <- com.mean.ab(D100m[[i]][,-c(242,255,151)],trait,"Moist")

	temp <- apply(D20m[[i]],2,sum)
	temp.data <- data.frame(SP=names(temp),abund=temp)
	temp2 <- merge(temp.data,trait,by="SP",all=T)

	temp.WSG <- na.omit(data.frame(WSG=temp2[,"WSG"],abund=temp2$abund))
	WSG.ind[[i]] <- rep(temp.WSG[,"WSG"],temp.WSG$abund)

	temp.Moist <- na.omit(data.frame(Moist=temp2[,"Moist"],abund=temp2$abund))
	Moist.ind[[i]] <- rep(temp.Moist[,"Moist"],temp.Moist$abund)

	temp.slope <- na.omit(data.frame(slope=temp2[,"sp.slope.mean"],abund=temp2$abund))
	slope.ind[[i]] <- rep(temp.slope[,"slope"],temp.slope$abund)

	temp.convex <- na.omit(data.frame(convex=temp2[,"sp.convex.mean"],abund=temp2$abund))
	convex.ind[[i]] <- rep(temp.convex[,"convex"],temp.convex$abund)
}



temp <- paste("slope_size_", seq(10,100,by=10),sep="")

slope.100.rare <- list(Moist100,Moist100,Moist100,Moist100,Moist100,Moist100,Moist100,Moist100,Moist100,Moist100)
for (i in 1:10) {
	for (j in 1:7){
	slope.100.rare[[i]][[j]]<-com.mean.ab(D100m[[j]],trait,temp[i])
	}
}


slope.20.rare <- list(Moist20,Moist20,Moist20,Moist20,Moist20,Moist20,Moist20,Moist20,Moist20,Moist20)
for (i in 1:10) {
	for (j in 1:7){
	slope.20.rare[[i]][[j]]<-com.mean.ab(D20m[[j]],trait,temp[i])
	}
}


slope.ind.rare <- list(Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind)

slope_size <- paste("slope_size_", seq(10,100,by=10),sep="")


for (i in 1:10){
	for (j in 1:7){
		temp <- apply(D20m[[j]],2,sum)
		temp.data <- data.frame(SP=names(temp),abund=temp)
		temp2 <- merge(temp.data,trait,by="SP",all=T)

		temp.slope <- na.omit(data.frame(slope=temp2[,slope_size[i]],abund=temp2$abund))
		slope.ind.rare[[i]][[j]] <- rep(temp.slope[,"slope"],temp.slope$abund)
	}
}



######
temp <- paste("convex_size_", seq(10,100,by=10),sep="")

convex.100.rare <- list(Moist100,Moist100,Moist100,Moist100,Moist100,Moist100,Moist100,Moist100,Moist100,Moist100)
for (i in 1:10) {
	for (j in 1:7){
	convex.100.rare[[i]][[j]]<-com.mean.ab(D100m[[j]],trait,temp[i])
	}
}


convex.20.rare <- list(Moist20,Moist20,Moist20,Moist20,Moist20,Moist20,Moist20,Moist20,Moist20,Moist20)
for (i in 1:10) {
	for (j in 1:7){
	convex.20.rare[[i]][[j]]<-com.mean.ab(D20m[[j]],trait,temp[i])
	}
}


convex.ind.rare <- list(Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind,Moist.ind)

convex_size <- paste("convex_size_", seq(10,100,by=10),sep="")


for (i in 1:10){
	for (j in 1:7){
		temp <- apply(D20m[[j]],2,sum)
		temp.data <- data.frame(SP=names(temp),abund=temp)
		temp2 <- merge(temp.data,trait,by="SP",all=T)

		temp.convex <- na.omit(data.frame(convex=temp2[,convex_size[i]],abund=temp2$abund))
		convex.ind.rare[[i]][[j]] <- rep(temp.convex[,"convex"],temp.convex$abund)
	}
}



save.image("~/Dropbox/BCI_Turnover/turnover_data.RData")
