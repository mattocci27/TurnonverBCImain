rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")

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

ab.t.data2 <- na.omit(ab.t.data)

# temp.data<-D[D$status1=="A",]

ab.t.data2$inc <- as.factor(ifelse(ab.t.data2$census_2010>ab.t.data2$census_1982,"UP","Down"))

# plot(convex~inc,data=ab.t.data2)
library(MASS)

summary((ab.t.data2$census_2010 - mean(ab.t.data2$census_2010))^2)

summary((ab.t.data2$census_2010 - ab.t.data2$census_1982)^2)



res_all <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])

res_WSG <- glm.nb(census_2010~WSG + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])
res_moist <- glm.nb(census_2010~moist + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])
res_convex <- glm.nb(census_2010~convex + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])
res_slope <- glm.nb(census_2010~slope + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])

res_WSG2 <- glm.nb(census_2010 ~
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])

res_moist2 <- glm.nb(census_2010 ~WSG
            # + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])


res_convex2 <- glm.nb(census_2010 ~WSG
            + moist
            # + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])


res_slope2 <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            # + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])

res_slope3 <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            # + WSG:slope
            + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])

res_convex3 <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:moist
            # + WSG:convex
            + WSG:slope
            + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])

res_moist3 <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            + slope
            # + WSG:moist
            + WSG:convex
            + WSG:slope
            + offset(log(census_1982)), data=ab.t.data2[ab.t.data2$census_1982>1,])

r2.func = function(res,n){
  data = ab.t.data2[ab.t.data2$census_1982 > n, ]
  y = data$census_2010/data$census_1982
  residuals = y - res$fitted.values/data$census_1982
  # residuals = y - exp(res$linear.predictors)/data$census_1982
  1 - sum(residuals^2)/sum((y-mean(y))^2)
}

# hist(res_WSG$fitted.values/data$census_1982)
#
# plot(res_WSG$fitted.values, data$census_2010,log="xy")
#
# plot(res_WSG$fitted.values, data$census_2010,log="xy")
#
#
# plot(res_all$fitted.values, data$census_2010,log="xy")

r2.func(res_all,n=1)
r2.func(res_WSG,n=1)
r2.func(res_moist,n=1)
r2.func(res_convex,n=1)
r2.func(res_slope,n=1)

r2.func(res_WSG2,n=1) - r2.func(res_all,n=1)
r2.func(res_moist2,n=1) - r2.func(res_all,n=1)
r2.func(res_convex2,n=1) - r2.func(res_all,n=1)
r2.func(res_slope2,n=1) - r2.func(res_all,n=1)


r2.func(res_moist3,n=1) - r2.func(res_all,n=1)
r2.func(res_convex3,n=1) - r2.func(res_all,n=1)
r2.func(res_slope3,n=1) - r2.func(res_all,n=1)
