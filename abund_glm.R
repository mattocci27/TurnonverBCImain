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

# GLM analysis
res.nb <- glm.nb(census_2010 ~  scale(slope100) + offset(log(census_1982)),data=ab.t.data[ab.t.data$census_1982>50,])
summary(res.nb)

res <- glm.nb(census_2010 ~ scale(WSG)+scale(moist)+scale(slope100)+scale(convex)+ offset(log(census_1982)), data= ab.t.data2)


res <- glm.nb(census_2010 ~ scale(WSG)+scale(moist)+scale(slope100)+scale(convex)+ scale(WSG):scale(slope100)  + offset(log(census_1982)), data= ab.t.data2)


res2 <- glm.nb(census_2010 ~ scale(WSG)+scale(slope100)+scale(convex)+ scale(WSG):scale(slope100)  + offset(log(census_1982)), data= ab.t.data2)


res3 <- glm.nb(census_2010 ~ scale(WSG)+scale(moist)+scale(slope100)+ offset(log(census_1982)), data= ab.t.data)



res <- glm.nb(census_2010 ~ scale(convex)+ offset(log(census_1982)), data= ab.t.data)
# res <- glm(census_2010 ~ scale(WSG)+scale(moist)+scale(slope100)+scale(convex)+ scale(WSG)*scale(slope100)  + offset(log(census_1982)), data= ab.t.data,family=poisson)


summary(res)





res.ori <- glm.nb(census_2010 ~ scale(WSG)+scale(moist)+scale(slope)+scale(convex)+ offset(log(census_1982)), data= ab.t.data2)
summary(res.ori)


res.r <- glm.nb(census_2010 ~ scale(WSG)+scale(moist)+scale(slope100)+scale(convex100)+ offset(log(census_1982)), data= ab.t.data.r)

res.r <- glm.nb(census_2010 ~ scale(WSG)+scale(moist)+scale(slope100)+scale(convex100)+ scale(WSG)*scale(slope100) + scale(slope100) * scale(convex100) + offset(log(census_1982)), data= ab.t.data.r)

summary(res.r)



###
library(ggplot2)

p1 <- ggplot(ab.t.data,aes(y=census_2010/census_1982,x=WSG,size=log10(census_1982)))
p1 <- p1 + geom_point() + labs(y="Relative abundance change (1982-2010)",title="No. of individuals")+ geom_hline(yintercept=1,lty=2) + scale_size_continuous(range = c(2,6))+labs(size="log10[Abundace]")+ theme(legend.position=c(0.8,0.25)) + scale_y_log10()#+theme(legend.title = element_text(size = 8))
p1
dev.off()


ab.t.data2$int <- ab.t.data2$WSG * ab.t.data2$slope100
ab.t.data$int <- ab.t.data$WSG * ab.t.data$slope100



par(mfrow=c(2,1))
plot(census_2010/census_1982 ~ WSG, ab.t.data2,pch=16,log="y")
abline(h=1,lty=2)

plot(census_2010/census_1982 ~ moist, ab.t.data2,pch=16,log="y")
abline(h=1,lty=2)

plot(census_2010/census_1982 ~ slope100, ab.t.data2,pch=16,log="y")
abline(h=1,lty=2)

plot(census_2010/census_1982 ~ convex, ab.t.data,pch=16,log="y",cex=log(census_1982)/5)

plot(census_2010/census_1982 ~ int, ab.t.data,pch=16,log="y",cex=log(census_1982)/5)
abline(h=1,lty=2)

plot(census_2010/census_1982 ~ int, ab.t.data2,pch=16,log="y",cex=log(census_1982)/5)

abline(h=1,lty=2)



p1 <- ggplot(rare.slope.data,aes(y=slope_size_100, x=sp.slope.mean))
p1 <- p1 + geom_point()+labs(x="Species mean slope", y="Rarefied species mean slope (100)") + geom_abline(slope=1,lty=2)

p1


par(mfrow=c(1,2))
plot(slope_size_100~sp.slope.mean,rare.slope.data)
plot(slope_size_10~sp.slope.mean,rare.slope.data)
par(mfrow=c(1,1))