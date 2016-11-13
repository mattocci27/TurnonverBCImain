#
com.mid.ab<-function(samp,trait,traitname)
{

		N<-dim(samp)[1]
		X<-paste(traitname)
		comMean<-numeric(N)
		for (i in 1:N){
			sppInSample <- names(samp[i,samp[i,] > 0] )
			sppInSample2 <- names(samp[i,samp[i,] < 1])
			sp.abund <- samp[i,samp[i,] > 0]

			if (length(sppInSample) > 0) {
			sppInTrait<-trait[sppInSample,]
			temp <- cbind(sppInTrait,sp.abund)
			temp2 <- temp[is.na(temp[,X])==F,]

			alltrait <- rep(temp2[,X],times=temp2[,"sp.abund"])

			comMean[i] <- median(alltrait,na.rm=T)

			}

			else {
				comMean[i] <- NA
			}
		}
	comMean
}


com.mean.ab<-function(samp,trait,traitname)
{

		N<-dim(samp)[1]
		X<-paste(traitname)
		comMean<-numeric(N)
		for (i in 1:N){
			sppInSample <- names(samp[i,samp[i,] > 0] )
			sppInSample2 <- names(samp[i,samp[i,] < 1])
			sp.abund <- samp[i,samp[i,] > 0]

			if (length(sppInSample) > 0) {
			sppInTrait<-trait[sppInSample,]
			temp <- cbind(sppInTrait,sp.abund)
			temp2 <- temp[is.na(temp[,X])==F,]
			comMean[i] <- sum(temp2[,X]*temp2[,"sp.abund"])/sum(temp2[,"sp.abund"])

			}

			else {
				comMean[i] <- NA
			}
		}
	comMean
}

# rarafuction for slope
slope.rare<-function(n.sample, n.runs, temp.com=temp.com,topo.com=topo.com){

sp.slope.quad <- matrix(numeric(0),ncol(topo.com),n.runs)
sp.convex.quad <- matrix(numeric(0),ncol(topo.com),n.runs)

temp.site <- rownames(topo[topo$slope_rank > (1250-n.sample), ])
samp.site <- rownames(topo[topo$slope_rank <= (1250-n.sample),])

for (i in 1:n.runs){
	samp.site.name <- c(temp.site, sample(samp.site,n.sample))
	temp.com <- topo.com[samp.site.name,]

	temp.topo <- topo[samp.site.name,]


	p.abund <- t(t(temp.com)/apply(temp.com,2,sum))
	p.abund[p.abund==0] <- NA

	temp.slope <- p.abund*temp.topo$slope
	temp.convex <- p.abund*temp.topo$convex

	sp.slope.quad[,i] <- apply(temp.slope,2,sum,na.rm=T)
	sp.convex.quad[,i] <- apply(temp.convex,2,sum,na.rm=T)

	}

	sp.slope.quad[sp.slope.quad==0]<-NA
	sp.convex.quad[sp.convex.quad==0]<-NA

	res1 <- apply(sp.slope.quad,1,mean,na.rm=T)
	res2 <- apply(sp.convex.quad,1,mean,na.rm=T)
	return(cbind(res1,res2))

}



convex.rare<-function(n.sample, n.runs, temp.com=temp.com,topo.com=topo.com){

sp.slope.quad <- matrix(numeric(0),ncol(topo.com),n.runs)
sp.convex.quad <- matrix(numeric(0),ncol(topo.com),n.runs)

temp.site <- rownames(topo[topo$slope_rank > (1250-n.sample), ])
samp.site <- rownames(topo[topo$slope_rank <= (1250-n.sample),])

for (i in 1:n.runs){
	samp.site.name <- c(temp.site, sample(samp.site,n.sample))
	temp.com <- topo.com[samp.site.name,]

	temp.topo <- topo[samp.site.name,]


	p.abund <- t(t(temp.com)/apply(temp.com,2,sum))
	p.abund[p.abund==0] <- NA

	temp.slope <- p.abund*temp.topo$slope
	temp.convex <- p.abund*temp.topo$convex

	sp.slope.quad[,i] <- apply(temp.slope,2,sum,na.rm=T)
	sp.convex.quad[,i] <- apply(temp.convex,2,sum,na.rm=T)

	}

	sp.slope.quad[sp.slope.quad==0]<-NA
	sp.convex.quad[sp.convex.quad==0]<-NA

	res1 <- apply(sp.slope.quad,1,mean,na.rm=T)
	res2 <- apply(sp.convex.quad,1,mean,na.rm=T)
	return(rbind(res1,res2))

}



slope.rare2<-function(n.sample, n.runs, temp.com=temp.com,topo.com=topo.com){

sp.slope.quad <- matrix(numeric(0),ncol(topo.com),n.runs)
sp.convex.quad <- matrix(numeric(0),ncol(topo.com),n.runs)

temp.site <- rownames(topo[topo$slope_rank > (1250-n.sample), ])
samp.site <- rownames(topo[topo$slope_rank <= (1250-n.sample),])

for (i in 1:n.runs){
	samp.site.name <- c(sample(temp.site,n.sample,replace=T), sample(samp.site,n.sample,replace=T))
	temp.com <- topo.com[samp.site.name,]

	temp.topo <- topo[samp.site.name,]


	p.abund <- t(t(temp.com)/apply(temp.com,2,sum))
	p.abund[p.abund==0] <- NA

	temp.slope <- p.abund*temp.topo$slope
	temp.convex <- p.abund*temp.topo$convex

	sp.slope.quad[,i] <- apply(temp.slope,2,sum,na.rm=T)
	sp.convex.quad[,i] <- apply(temp.convex,2,sum,na.rm=T)

	}

	sp.slope.quad[sp.slope.quad==0]<-NA
	sp.convex.quad[sp.convex.quad==0]<-NA

	res1 <- apply(sp.slope.quad,1,mean,na.rm=T)
	res2 <- apply(sp.convex.quad,1,mean,na.rm=T)
	return(cbind(res1,res2))

}

convex.rare2<-function(n.sample, n.runs, temp.com=temp.com,topo.com=topo.com){

sp.slope.quad <- matrix(numeric(0),ncol(topo.com),n.runs)
sp.convex.quad <- matrix(numeric(0),ncol(topo.com),n.runs)

temp.site <- rownames(topo[topo$slope_rank > (1250-n.sample), ])
samp.site <- rownames(topo[topo$slope_rank <= (1250-n.sample),])

for (i in 1:n.runs){
	samp.site.name <- c(sample(temp.site,n.sample,replace=T), sample(samp.site,n.sample,replace=T))
	temp.com <- topo.com[samp.site.name,]

	temp.topo <- topo[samp.site.name,]


	p.abund <- t(t(temp.com)/apply(temp.com,2,sum))
	p.abund[p.abund==0] <- NA

	temp.slope <- p.abund*temp.topo$slope
	temp.convex <- p.abund*temp.topo$convex

	sp.slope.quad[,i] <- apply(temp.slope,2,sum,na.rm=T)
	sp.convex.quad[,i] <- apply(temp.convex,2,sum,na.rm=T)

	}

	sp.slope.quad[sp.slope.quad==0]<-NA
	sp.convex.quad[sp.convex.quad==0]<-NA

	res1 <- apply(sp.slope.quad,1,mean,na.rm=T)
	res2 <- apply(sp.convex.quad,1,mean,na.rm=T)
	return(rbind(res1,res2))

}

temp.dpw<-function(samp1,samp2,t_dis_m){

	temp.sp1 <- c(rownames(t_dis_m),colnames(samp1))
	temp.sp1 <- temp.sp1[duplicated(temp.sp1)]
	temp.sp2 <- c(rownames(t_dis_m),colnames(samp1))
	temp.sp2 <- temp.sp2[duplicated(temp.sp2)]

	samp1 <- samp1[,temp.sp1]
	samp2 <- samp2[,temp.sp2]


	delta<-NULL
	for (i in 1:nrow(samp1)){
		samp.temp<-rbind(samp1[i,], samp2[i,])

		##pk1
		test1<-as.matrix(samp.temp[1,samp.temp[1,]>0])
		test1<-test1/sum(test1)
		##pk2
		test2<-as.matrix(samp.temp[2,samp.temp[2,]>0])
		test2<-test2/sum(test2)

		pp<-test1%*%t(test2)

		r.n<-rownames(pp)
		c.n<-colnames(pp)
		t_dis2<-t_dis_m[r.n, c.n]

		delta1<-sum(apply(t_dis2, 1, mean, na.rm=T)*test1)
		delta2<-sum(apply(t_dis2, 2, mean, na.rm=T)*test2)

		delta[i] <- (delta1+delta2)/2
			}
	delta
}

ses.temp.dpw<-function(samp1, samp2, t_dis_m,runs=999){

	dpw.rand<-t(replicate(runs,temp.dpw(samp1,samp2,my.null(t_dis_m))))
	dpw.obs<-temp.dpw(samp1,samp2,t_dis_m)

	dpw.rand.mean <- apply(X = dpw.rand, MARGIN = 2, FUN = mean,
        na.rm = TRUE)
    dpw.rand.sd <- apply(X = dpw.rand, MARGIN = 2, FUN = sd,
        na.rm = TRUE)
    dpw.obs.z <- (dpw.obs - dpw.rand.mean)/dpw.rand.sd
    dpw.obs.rank <- apply(X = rbind(dpw.obs, dpw.rand), MARGIN = 2,
        FUN = rank)[1, ]
    dpw.obs.rank <- ifelse(is.na(dpw.rand.mean), NA, dpw.obs.rank)
    data.frame(ntaxa = specnumber(samp1), dpw.obs, dpw.rand.mean,
        dpw.rand.sd, dpw.obs.rank, dpw.obs.z, dpw.obs.p = dpw.obs.rank/(runs +
            1), runs = runs, row.names = rownames(samp1))
}

my.null <- function(t_dis_m){
	temp<-sample(rownames(t_dis_m),nrow(t_dis_m))
	rownames(t_dis_m)<-colnames(t_dis_m)<-temp
	t_dis_m
}


####
# Joe and my idea
####


# rec_m <- rec2_m
ind.null<-function(rec_m){

	res <- r2dtable(1, rowSums(rec_m), colSums(rec_m))[[1]]
	rownames(res) <- rownames(rec_m)
	colnames(res) <- colnames(rec_m)

	res

}

ses.temp.dpw2<-function(samp1, samp2, dead, rec_m, t_dis_m,runs=999){
	# rec_m <- samp2 - samp1x
	dpw.rand<-t(replicate(runs,temp.dpw(samp1,samp1-dead+ind.null(rec_m),t_dis_m)))

	dpw.obs<-temp.dpw(samp1,samp2,t_dis_m)

	dpw.rand.mean <- apply(X = dpw.rand, MARGIN = 2, FUN = mean,
        na.rm = TRUE)
    dpw.rand.sd <- apply(X = dpw.rand, MARGIN = 2, FUN = sd,
        na.rm = TRUE)
    dpw.obs.z <- (dpw.obs - dpw.rand.mean)/dpw.rand.sd
    dpw.obs.rank <- apply(X = rbind(dpw.obs, dpw.rand), MARGIN = 2,
        FUN = rank)[1, ]
    dpw.obs.rank <- ifelse(is.na(dpw.rand.mean), NA, dpw.obs.rank)
    data.frame(ntaxa = specnumber((samp1)), dpw.obs, dpw.rand.mean,
        dpw.rand.sd, dpw.obs.rank, dpw.obs.z, dpw.obs.p = dpw.obs.rank/(runs + 1), runs = runs, row.names = rownames(samp1))
}




plot.nmds <- function(samp, range=c("north","south","all"), axis = c("1-2","1-3","2-3"), engine, k = 3, trymax =50, n.census = 7){

com.nmds <- metaMDS(samp,engine=engine,k=k, trymax=trymax)
if (axis == "1-2") {
		axis1 <- "MDS1"
		axis2 <- "MDS2"
		lab1 <- "NMDS axis1"
		lab2 <- "NMDS axis2"
	} else if (axis == "1-3") {
		axis1 <- "MDS1"
		axis2 <- "MDS3"
		lab1 <- "NMDS axis1"
		lab2 <- "NMDS axis3"
	} else if (axis == "2-3") {
		axis1 <- "MDS2"
		axis2 <- "MDS3"
		lab1 <- "NMDS axis2"
		lab2 <- "NMDS axis3"
	}

if (range=="north"){
		range2 <- grep("_4$|_5$",rownames(samp))
	} else if(range=="south"){
		range2 <- grep("_1$|_2$",rownames(samp))
	} else if(range=="all") {range2 <- 1:nrow(samp)}


plot(com.nmds$points[range2,paste(axis2)] ~ com.nmds$points[range2,paste(axis1)],xlim=c(-0.4,0.4),ylim=c(-0.4,0.4),type="n",xlab=paste(lab1), ylab=paste(lab2), main = paste(range))

	n.samp <- length(range2)/n.census

	for (i in 1:(n.census-1)) {
	N1 <- n.samp * (i-1) + 1
	N1.2 <- n.samp * i

	N2 <- n.samp * i + 1
	N2.2 <- n.samp * (i+1)

	x1 <- com.nmds$points[range2,paste(axis1)][N1:N1.2]
	y1 <- com.nmds$points[range2,paste(axis2)][N1:N1.2]
	x2 <- com.nmds$points[range2,paste(axis1)][N2:N2.2]
	y2 <- com.nmds$points[range2,paste(axis2)][N2:N2.2]

	arrows(x1,y1,x2,y2,length=0.05,col=arrow.col[i])
	}

}

plot.nmds.env <- function(samp, trait, range=c("north","south","all"), axis = c("1-2","1-3","2-3"), engine, k = 3, trymax =50, n.census = 7){

if (axis == "1-2") {
		axis1 <- "MDS1"
		axis2 <- "MDS2"
		lab1 <- "NMDS axis1"
		lab2 <- "NMDS axis2"
		choices <- 1:3
		choice <- c(1,2)
	} else if (axis == "1-3") {
		axis1 <- "MDS1"
		axis2 <- "MDS3"
		lab1 <- "NMDS axis1"
		lab2 <- "NMDS axis3"
		choices <- 1:3
		choice <- c(1,3)

	} else if (axis == "2-3") {
		axis1 <- "MDS2"
		axis2 <- "MDS3"
		lab1 <- "NMDS axis2"
		lab2 <- "NMDS axis3"
		choices <- 1:3
		choice <- c(2,3)
	}

if (range=="north"){
		range2 <- grep("_4$|_5$",rownames(samp))
	} else if(range=="south"){
		range2 <- grep("_1$|_2$",rownames(samp))
	} else if(range=="all") {range2 <- 1:nrow(samp)}


# n.samp <- length(range2)/n.census
# samp.ori <- samp[1:n.samp,]
com.nmds <- metaMDS(samp,engine=engine,k=k, trymax=trymax)
fit1 <- envfit(com.nmds,trait, choices = choices)

plot(com.nmds$points[range2,paste(axis2)] ~ com.nmds$points[range2,paste(axis1)],xlim=c(-0.4,0.4),ylim=c(-0.4,0.4),type="n",xlab=paste(lab1), ylab=paste(lab2), main = paste(range))

plot(fit1, choice = choice)

}




nmds.boot <- function(nmds.res, n.census, n.rep){
NMDS1.diff <- NULL
NMDS2.diff <- NULL
NMDS3.diff <- NULL
nmds1.boot <- matrix(numeric(0),nrow=(n.census-1),ncol=n.rep)
nmds2.boot <- matrix(numeric(0),nrow=(n.census-1),ncol=n.rep)
nmds3.boot <- matrix(numeric(0),nrow=(n.census-1),ncol=n.rep)


 n.samp <- nrow(nmds.res$points)/n.census


for (i in 1:(n.census-1)) {
	N1 <- n.samp * (i-1) + 1
	N1.2 <- n.samp * i

	N2 <- n.samp * i + 1
	N2.2 <- n.samp * (i+1)

	before1 <- nmds.res$points[,"MDS1"][N1:N1.2]
	before2 <- nmds.res$points[,"MDS2"][N1:N1.2]
	before3 <- nmds.res$points[,"MDS3"][N1:N1.2]
	after1 <- nmds.res$points[,"MDS1"][N2:N2.2]
	after2 <- nmds.res$points[,"MDS2"][N2:N2.2]
	after3 <- nmds.res$points[,"MDS3"][N2:N2.2]

	temp1 <- before1 - after1
	temp1[temp1>0] <- 1
	temp1[temp1<0] <- 0

	temp2 <- before2 - after2
	temp2[temp2>0] <- 1
	temp2[temp2<0] <- 0

	temp3 <- before3 - after3
	temp3[temp3>0] <- 1
	temp3[temp3<0] <- 0

	NMDS1.diff[i] <- n.samp-sum(temp1)
	NMDS2.diff[i] <- n.samp-sum(temp2)
	NMDS3.diff[i] <- n.samp-sum(temp3)

	nmds1.boot[i,]<-sample(before1-after1,n.rep,replace=T)
	nmds2.boot[i,]<-sample(before2-after2,n.rep,replace=T)
	nmds3.boot[i,]<-sample(before3-after3,n.rep,replace=T)
}

nmds1.mean <- apply(nmds1.boot,1,mean)
nmds2.mean <- apply(nmds2.boot,1,mean)
nmds3.mean <- apply(nmds3.boot,1,mean)



nmds1.lower <- apply(nmds1.boot,1,function(x)quantile(x,0.025))
nmds2.lower <- apply(nmds2.boot,1,function(x)quantile(x,0.025))
nmds3.lower <- apply(nmds3.boot,1,function(x)quantile(x,0.025))

nmds1.upper <- apply(nmds1.boot,1,function(x)quantile(x,0.975))
nmds2.upper <- apply(nmds2.boot,1,function(x)quantile(x,0.975))
nmds3.upper <- apply(nmds3.boot,1,function(x)quantile(x,0.975))


year.temp <- c("1981-1985",
               "1985-1990",
               "1990-1995",
               "1995-2000",
               "2000-2005",
               "2005-2010")


year <- year.temp[1:(n.census-1)]

nmds1.p<-nmds2.p<-nmds3.p <-NULL
for (i in 1:(n.census-1)) nmds1.p[i] <- binom.test(NMDS1.diff[i],n.samp)$p.value
for (i in 1:(n.census-1)) nmds2.p[i] <- binom.test(NMDS2.diff[i],n.samp)$p.value
for (i in 1:(n.census-1)) nmds3.p[i] <- binom.test(NMDS3.diff[i],n.samp)$p.value



data.frame(Census.interval= c("NMDS.axis.1",year,"NMDS.axis.2",year,"NMDS.axis.3",year),
           Number.of.subplots.with.negative.change=c(NA,NMDS1.diff,NA,NMDS2.diff,NA,NMDS3.diff),
           Binomial.P.value=c(NA,nmds1.p,NA,nmds2.p,NA,nmds3.p),
           Mean.rate.of.change=c(NA,nmds1.mean,NA,nmds2.mean,NA,nmds3.mean),
           Lower.95per.CI=c(NA,nmds1.lower,NA,nmds2.lower,NA,nmds3.lower),
           Upper.95per.CI=c(NA,nmds1.upper,NA,nmds2.upper,NA,nmds3.upper))

}
