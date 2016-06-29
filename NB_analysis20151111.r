rm(list = ls()) # This clears everything from memory.
library(dplyr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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



model <-"
     data{
        int<lower=0> n_sample;
        vector[n_sample] WSG;
        vector[n_sample] moist;
        vector[n_sample] slope;
        vector[n_sample] convex;
        vector[n_sample] moist_int;
        vector[n_sample] slope_int;
        vector[n_sample] convex_int;
        vector[n_sample] relative_ab;
        int y[n_sample];
        //int census_1982[n_sample];
        vector[n_sample] census_1982;
     }
     parameters{
        vector[8] beta;
        real<lower=0> phi;
     }

     transformed parameters {
        vector<lower=0>[n_sample] mu;
        mu <- exp(beta[1]
          + beta[2] * WSG
          + beta[3] * moist
          + beta[4] * slope
          + beta[5] * convex
          + beta[6] * moist_int
          + beta[7] * slope_int
          + beta[8] * convex_int
          + log(census_1982));
     }
     model{
          real temp1;
          real temp2;
          real temp3;
          real temp_like;
          for (i in 1:n_sample){
               temp1 <- lgamma(y[i] + 1/phi) - lgamma(1/phi) - lgamma(y[i]+1);
               temp2 <- -(1/phi)*log(1+mu[i]*phi);
               temp3 <- y[i]*log(mu[i]/(1/phi + mu[i]));
               //temp_like <- (temp1+temp2+temp3)*(1-relative_ab[i]);
               temp_like <- (temp1+temp2+temp3)*relative_ab[i]*100;
               lp__ <- lp__ + temp_like;
          }
          phi ~ cauchy(0, 5);
          beta ~ normal(0, 10);
     }
"



list.data_log <- list(n_sample=nrow(ab.t.data2),
                  WSG=as.numeric(scale(ab.t.data2$WSG)),
                  moist=as.numeric(scale(ab.t.data2$moist)),
                  slope=as.numeric(scale(ab.t.data2$slope)),
                  convex=as.numeric(scale(ab.t.data2$convex)),
                  moist_int = as.numeric(scale(ab.t.data2$moist * ab.t.data2$WSG)),
                  slope_int = as.numeric(scale(ab.t.data2$slope * ab.t.data2$WSG)),
                  convex_int = as.numeric(scale(ab.t.data2$convex * ab.t.data2$WSG)),
                  y=ab.t.data2$census_2010,
                  census_1982=ab.t.data2$census_1982,
                  relative_ab = log(ab.t.data2$census_1982)/sum(log(ab.t.data2$census_1982)))

list.data <- list(n_sample=nrow(ab.t.data2),
                  WSG=as.numeric(scale(ab.t.data2$WSG)),
                  moist=as.numeric(scale(ab.t.data2$moist)),
                  slope=as.numeric(scale(ab.t.data2$slope)),
                  convex=as.numeric(scale(ab.t.data2$convex)),
                  moist_int = as.numeric(scale(ab.t.data2$moist * ab.t.data2$WSG)),
                  slope_int = as.numeric(scale(ab.t.data2$slope * ab.t.data2$WSG)),
                  convex_int = as.numeric(scale(ab.t.data2$convex * ab.t.data2$WSG)),
                  y=ab.t.data2$census_2010,
                  census_1982=ab.t.data2$census_1982,
                  relative_ab = ab.t.data2$census_1982/sum(ab.t.data2$census_1982))


ab.t.data3 = ab.t.data2[ab.t.data2$census_1982>100,]

list.data_ori <- list(n_sample=nrow(ab.t.data2),
                  WSG=as.numeric(scale(ab.t.data2$WSG)),
                  moist=as.numeric(scale(ab.t.data2$moist)),
                  slope=as.numeric(scale(ab.t.data2$slope)),
                  convex=as.numeric(scale(ab.t.data2$convex)),
                  moist_int = as.numeric(scale(ab.t.data2$moist * ab.t.data2$WSG)),
                  slope_int = as.numeric(scale(ab.t.data2$slope * ab.t.data2$WSG)),
                  convex_int = as.numeric(scale(ab.t.data2$convex * ab.t.data2$WSG)),
                  y=ab.t.data2$census_2010,
                  census_1982=ab.t.data2$census_1982,
                  relative_ab =rep(1/nrow(ab.t.data2),nrow(ab.t.data2)))


# list.data_ori <- list(n_sample=nrow(ab.t.data3),
#                   WSG=as.numeric(scale(ab.t.data3$WSG)),
#                   moist=as.numeric(scale(ab.t.data3$moist)),
#                   slope=as.numeric(scale(ab.t.data3$slope)),
#                   convex=as.numeric(scale(ab.t.data3$convex)),
#                   moist_int = as.numeric(scale(ab.t.data3$moist * ab.t.data3$WSG)),
#                   slope_int = as.numeric(scale(ab.t.data3$slope * ab.t.data3$WSG)),
#                   convex_int = as.numeric(scale(ab.t.data3$convex * ab.t.data3$WSG)),
#                   y=ab.t.data3$census_2010,
#                   census_1982=ab.t.data3$census_1982,
#                   relative_ab =rep(1/nrow(ab.t.data3),nrow(ab.t.data3)))
#


par(mfrow=c(1,2))
hist(list.data$relative_ab*100)
hist(list.data_log$relative_ab*100)
par(mfrow=c(1,1))



system.time(fit<- stan(model_code = model,
            data=list.data,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit_log<- stan(model_code = model,
            data=list.data_log,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit_ori<- stan(model_code = model,
            data=list.data_ori,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit2<- stan(fit = fit,
            data=list.data,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 3))


system.time(fit2_log<- stan(fit = fit_log,
            data=list.data_log,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 3))

system.time(fit2_ori<- stan(fit = fit_ori,
            data=list.data_ori,
            iter = 2000,
            warmup = 1000,
            thin = 1,
            chains = 3))

fit.summary <- data.frame(summary(fit2)$summary)
fit.summary.log <- data.frame(summary(fit2_log)$summary)
fit.summary.ori <- data.frame(summary(fit2_ori)$summary)




1 - sum((list.data$y - fit.summary.ori[10:211,"mean"])^2) /sum( (list.data$y - mean(list.data$census_1982)) ^2)

plot(list.data$y , fit.summary.ori[10:211,"mean"],log="xy")

# plot(WSG_slope2~WSG_slope,ab.t.data2)

save.image("NB_res20151111_replace.RData")


#####
#figure and table
####


load("NB_res20151019_replace.RData")

write.csv(fit.summary,"temp1_replace.csv")
write.csv(fit.summary.log,"temp2_replace.csv")
write.csv(fit.summary.ori,"temp3_replace.csv")

res.mid <- fit.summary.ori[1:8,"X50."]
res.lo <- fit.summary.ori[1:8,"X2.5."]
res.up <- fit.summary.ori[1:8,"X97.5."]
                                    t

# res.mid <- fit.summary.ori[1:8,"X50."]
# res.lo <- fit.summary.ori[1:8,"X2.5."]
# res.up <- fit.summary.ori[1:8,"X97.5."]

# res.mid <- fit.summary.log[1:8,"X50."]
# res.lo <- fit.summary.log[1:8,"X2.5."]
# res.up <- fit.summary.log[1:8,"X97.5."]


plot.data <- data.frame(y=1:8,
                        res.mid=rev(res.mid),
                        res.low=rev(res.lo),
                        res.up=rev(res.up))

pdf("~/Dropbox/MS/TurnoverBCI/fig0/fig_effect.pdf",height=5.5,width=6)
par(oma=c(2,2,2,2),
    mar=c(4,6,4,4))

plot(y~res.mid, plot.data, xlim=c(-2,2),type="n",yaxt="n",ylab="",xlab="Effect")
axis(2,1:8, c("WSG x Convexity", "WSG x Slope", "WSG x Moist","Convexity", "Slope","Moist","WSG","Intercept"),las=2)
# axis(2,at=pretty(-0.3,0.4), labels=sprintf("%1.2f",pretty(-0.3,0.4)))
# points(y~res.mid, plot.data[1:6,],cex=1.5)

points(y~res.mid, plot.data,cex=1.5,pch=ifelse(plot.data$res.lo>0 | plot.data$res.up<0,16,1))


# points(y~res.mid, plot.data[7,],pch=16,cex=1.5)
arrows(plot.data$res.lo,plot.data$y, plot.data$res.up,plot.data$y,length=0)
abline(v=0,lty=2)

dev.off()






model <-"
     data{
          int<lower=0> n_sample;
          vector[n_sample] WSG;
          vector[n_sample] moist;
          vector[n_sample] slope10;
          vector[n_sample] convex;
          vector[n_sample] relative_ab;
          vector[n_sample] wsd;
          int y[n_sample];
          int census_1982[n_sample];

     }

     parameters{
          real beta1;
          real beta2;
          real beta3;
          real beta4;
          real beta5;
          real<lower=0> phi;
     }

     transformed parameters {
          real<lower=0> mu[n_sample];
          for (i in 1:n_sample) {
               mu[i] <- exp(beta1 + beta2 * convex[i] + beta3 * moist[i] + beta4*slope10[i]+beta5*WSG[i] + log(census_1982[i]));
                    }
     }
     model{
          real temp1;
          real temp2;
          real temp3;
          real temp_like;

          for (i in 1:n_sample){
               temp1 <- lgamma(y[i] + 1/phi) - lgamma(1/phi) - lgamma(y[i]+1);
               temp2 <- -(1/phi)*log(1+mu[i]*phi);
               temp3 <- y[i]*log(mu[i]/(1/phi + mu[i]));
               temp_like <- (temp1+temp2+temp3)*(1-relative_ab[i])/wsd[i];
               lp__ <- lp__ + temp_like;
          }

          phi ~ uniform(0,1.0E+4);
          beta1 ~ normal(0,1.0E+4);
          beta2 ~ normal(0,1.0E+4);
          beta3 ~ normal(0,1.0E+4);
          beta4 ~ normal(0,1.0E+4);
          beta5 ~ normal(0,1.0E+4);
     }
"

list.data_sd <- list(n_sample=nrow(ab.t.data2),
                  WSG=as.numeric(scale(ab.t.data2$WSG)),
                  moist=as.numeric(scale(ab.t.data2$moist)),
                  slope10=as.numeric(scale(ab.t.data2$slope100)),
                  convex=as.numeric(scale(ab.t.data2$convex)),
                  y=ab.t.data2$census_2010,
                  census_1982=ab.t.data2$census_1982,
                  relative_ab =ab.t.data2$census_1982/sum(ab.t.data2$census_1982),
                  wsd =ab.t.data2$convex.sd)



system.time(fit_sd<- stan(model_code = model,
            data=list.data_sd,
            iter = 1,
            warmup = 0,
            thin = 1,
            chains = 1))

system.time(fit2_sd<- stan(fit = fit_sd,
            data=list.data_sd,
            iter = 400,
            warmup = 200,
            thin = 1,
            chains = 3))

fit.summary.sd <- data.frame(summary(fit2_sd)$summary)


traceplot(fit2_sd,par="beta5")




library(snowfall)
sfInit(parallel=T,cpu=3)
sfLibrary(rstan)
sfExportAll()

n.chains <- 3
# 28.760
system.time(res_par <- sfLapply(1:n.chains,function(x){stan(fit=fit,
               data=list.data,
               iter=2000,
                   warmup=1000,
                   thin=1,
                   chains=1,
                   verbose = FALSE,
                   refresh = -1,
                   chain_id=x)}))

fit2 <- sflist2stanfit(res_par)

fit.summary <- data.frame(summary(fit2)$summary)


traceplot(fit2,par="beta4")


# temp<-ab.t.data2[ab.t.data2$census_1982>100,]

library(ggplot2)
library(MASS)
library(grid)


vplayout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)
}


pdf("abund_convex.pdf", width=12,height=5)

pushViewport(viewport(layout=grid.layout(1, 2)))


p1 <- ggplot(ab.t.data2,aes(y=census_2010/census_1982,x=convex, size=log10(census_1982)))
p1 <- p1 + geom_point() + labs(y="Relative abundance change (1982-2010)")+ geom_hline(yintercept=1,lty=2) + scale_size_continuous(range = c(2,6)) + ylim(0,10) + scale_y_log10()+ theme(legend.position=c(0.85,0.25))

p2 <- ggplot(ab.t.data2,aes(y=census_2010/census_1982,x=convex100, size=log10(census_1982)))
p2 <- p2 + geom_point() + labs(y="Relative abundance change (1982-2010)")+ geom_hline(yintercept=1,lty=2) + scale_size_continuous(range = c(2,6)) + ylim(0,10) + scale_y_log10()+ guides(size=FALSE)

print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(1,2))

dev.off()






# p + geom_abline(intercept= .1483, slope = -0.02561)


res.nb <- glm.nb(census_2010 ~  scale(slope10) + offset(log(census_1982)),data=ab.t.data[ab.t.data$census_1982>50,])
summary(res.nb)



res <- glm.nb(census_2010 ~ scale(WSG)+scale(moist)+scale(slope100)+scale(convex100)+ offset(log(census_1982)), data= ab.t.data2)
summary(res)



library(smatr)
plot(convex2010~convex1982,data=moge4)
abline(a=0,b=1,lty=2)

temp<-line.cis(y=moge4$convex2010,x=moge4$convex1982)

abline(a=temp[1,1],b=temp[2,1])


# summary(lm(log(census_2010/census_1982) ~ slope10, data=ab.t.data2))




# mu <- 400
# size <- 2
# phi <- 1/size
# data <- rnbinom(500, mu = mu, size = 1/phi)

# n_sample<-200

# beta1 <- 1.1
# beta2 <- 0.1
# beta3 <- -0.6
# beta4 <- 0
# beta5 <- 0

# WSG <- rnorm(n_sample)
# moist <- rnorm(n_sample)
# slope10 <- rnorm(n_sample)
# convex <- rnorm(n_sample)

# census_1982<-as.integer(exp(rnorm(n_sample,3,2)))
# census_1982[census_1982==0] <-1


# hist(census_1982)

# mu <- exp(beta1+ beta2*WSG + beta3*moist + beta4*slope10 + beta5*convex + log(census_1982))

# y<-rnbinom(n_sample, mu=mu,size=10)

# plot(y/census_1982 ~ WSG,log="y")


# list.data <- list(n_sample=n_sample,
#                   WSG=WSG,
#                   moist=moist,
#                   slope10=slope100,
#                   convex=convex100,
#                   y=y,
#                   census_1982=census_1982,
#                   relative_ab=log(census_1982)/sum(log(census_1982)))
#                   #relative_ab=rep(1,n_sample))





# system.time(fit<- stan(model_code = model,
#             data=list.data,
#             iter = 1,
#             warmup = 0,
#             thin = 1,
#             chains = 1))


# system.time(fit2<- stan(fit = fit,
#             data=list.data,
#             iter = 200,
#             warmup = 100,
#             thin = 1,
#             chains = 3))




# fit.summary <- data.frame(summary(fit2)$summary)


# traceplot(fit2,par="beta1")
