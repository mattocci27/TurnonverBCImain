###
### This simulation shows that the pattens in the 50ha scale
### can be created by temporal autocorrelation alone.

library(mgcv)

set.seed(5)
n <- 7
x <- 1:n

before <- proc.time()
p <- NULL
r2 <- NULL
dat2 <- NULL

p_range <- c(0.1, 0.5, 1, 10)
#phi_range <- c(0, 1, 0.25, 0.5, 0.75, 1)

plot(y ~ x, xlab = expression(x[t]), ylab = expression(y[t]), ylim=c(-10,10))
for (j in 1:1500){
  y <- NULL
  y[1] <- 0
  for (i in 2:n){
    y[i] <- rnorm(1, 1 * y[i-1], 0.1)
  }

#  y <- rep(0,n)
#  y <- y + arima.sim(list(ar=0.85), n=n)
#  y <- as.numeric(y)
#  y <- y - y[1]
  #plot(y ~ x)
  dat <- data.frame(x=x, y=y)
  e <- try(gamm(y ~ s(x, k=4), data = dat, correlation = corCAR1(form=~x)),
             silent = TRUE)
  if (class(e) == "try-error") {
    res <- NA
  } else {
    res <- e
    p[j] <- res$gam %>% summary %>% .$s.pv
    r2[j] <- res$gam %>% summary %>% .$r.sq
    dat2 <- rbind(dat2, dat)
  #lines(fitted(res$lme) ~ x, lty = "solid", col = "midnightblue", lwd = 2)
  }
  #res <- gamm(y ~ s(x, k=4), data = dat, correlation = corCAR1(form=~x))
}

after <- proc.time()
after - before

r22 <- r2[!is.na(r2)] %>% sample(1000)
r2 <- p[!is.na(p)] %>% sample(1000)

par(mfrow=c(1,2))
hist(r22, xlab = "R2 values")
hist(p2, xlab = "P values")

dat2 <- dat2 %>% mutate(n = rep((1:(nrow(dat2)/7)), each = 7))

save.image("art.rda")

load("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/analysis/art.rda")

ggplot(dat2 %>% filter(n <= 50), aes(x=x,y=y, col = n %>% as.factor)) +
  #geom_point() +
  geom_smooth(se = FALSE) +
  guides(col = FALSE) +
  xlab("Time") +
  ggtitle("50 simulations") +
  ylab("Imaginary community means at 50ha scale") +
  theme_bw()

aa <- dat2 %>% filter(n <= 50) %>%
  gamm(y ~ s(x,k=4), data =., random = list(n = ~ 1), correlation = corCAR1(form=~x))


# site and tempo

  y <- matrix(rep(0,350),nrow=7)
  y[1,] <- 0
  for (i in 2:n){
    for (j in 2:50)
    y[i,j] <- rnorm(1, 1 * y[i-1, j] + 0.3*y[i,j-1], 1)
  }

dat <- data_frame(y=as.vector(t(y)), x = rep(1:7, each=50)) %>%
  mutate(site = rep(1:50, 7))

res <- gamm(y ~ s(x,k=4), data = dat, random = list(site = ~ 1), correlation = corCAR1(form=~x))

summary(res$gam)
plot(y~x,dat)


t(y) %>% levelplot

plot(y ~ x, dat2)


moge <- dat2 %>% filter(x == 7 & y > 0)
quantile(moge$y, 0.025)

quantile(dat2$y, 0.025)
quantile(dat2$y, 0.975)


plot(y ~ x, dat)

quantile(r2, 0.975)



# check
gam(y ~ s(x, k=4), data = dat, correlation = corCAR1(form = ~x)) %>% summary

gam(y ~ s(x, k=4), data = dat, correlation = corCAR1()) %>% summary

gam(y ~ s(x, k=4), data = dat) %>% summary


dat$site <- 1 %>% as.factor


ys <- ts(y)
acf(ys)


x <- 1:7
y <- NULL
y[1] <- 0
for (i in 2:7){
  y[i] <- rnorm(1,y[i-1], 1)
}

n <- 7
time <- 1:n
x <- time/n
y <- rep(5,n)
y <- y + arima.sim(list(ar=0.85), n=n)
y <- as.numeric(y)
y <- y - y[1]

#y <- arima.sim(list(order = c(1,1,0), ar=1.2e-11), n=6) %>% as.numeric
dat <- data.frame(x=x, y=y)
m1 <- smooth.spline(x,y)
m2 <- gam(y ~ s(x, k=4), data = dat)
m3 <- gamm(y ~ s(x, k=4), data = dat, correlation = corCAR1(form = ~ time))
#m4 <- gamm(y ~ s(x, k=4), data = dat, correlation = corCAR1())

plot(y ~ x, xlab = expression(x[t]), ylab = expression(y[t]))
lines(fitted(m1) ~ x, lty = "solid", col = "darkolivegreen", lwd = 2)
lines(fitted(m2) ~ x, lty = "solid", col = "red", lwd = 2)
lines(fitted(m3$lme) ~ x, lty = "solid", col = "midnightblue", lwd = 2)
#lines(fitted(m4$lme) ~ x, lty = "solid", col = "orange", lwd = 2)
summary(m3$lme)

# use Ferguson
test <- bckftg.alg(y, as.data.frame(x), lime = )

###
setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/source.R")
load("gam_fig.RData")

# library(MASS)

data_frame(WSG100 = WSG100, WSG20 = WSG20, WSG_ind = WSG.ind)

str(WSG100)
str(WSG20)
str(WSG.ind)

time <- c(1982, 1985, 1990, 1995, 2000, 2005, 2010)
# rep_ind <- sapply(WSG.ind, length)


fig_dat <- data_frame(WSG = c(unlist(WSG100) - WSG100[[1]],
  unlist(WSG20) - WSG20[[1]]),
  Moist = c(unlist(Moist100) - Moist100[[1]],
    unlist(Moist20) - Moist20[[1]]),
  Convex = c(unlist(convex100) - convex100[[1]],
    unlist(convex20) - convex20[[1]]),
  Slope = c(unlist(slope100) - slope100[[1]],
    unlist(slope20) - slope20[[1]]),
  Time = c(rep(time, each = 50),
    rep(time, each = 1250)),
  size = rep(c("1ha", "0.04ha"), c(50*7, 1250*7)))


fig_dat2 <- fig_dat %>%
  tidyr::gather(., "trait", "val", 1:4)



fig_dat3 <- fig_dat2 %>%
  group_by(Time, trait) %>%
  summarise(val = mean(val))


fig_dat3 <- data_frame(Time = fig_dat3$Time,
    size = "52ha",
    trait = fig_dat3$trait,
    val = fig_dat3$val)

obs <- fig_dat3 %>% tidyr::spread(trait, val) %>%
  mutate(Convex = -Convex)
  #mutate(Convex = scale(Convex)) %>%
  #mutate(Convex = Convex - Convex[1]) %>%
  #mutate(Moist = scale(Moist)) %>%
  #mutate(Moist = Moist - Moist[1]) %>%
  #mutate(Slope = scale(Slope)) %>%
  #mutate(Slope = Slope - Slope[1]) %>%
  #mutate(WSG = scale(WSG)) %>%
  #mutate(WSG = WSG - WSG[1])

obs2 <- obs %>%
  tidyr::gather(trait, val, 3:6)

ggplot(obs2, aes(x=Time,y=val)) +
  geom_point() +
  facet_wrap(~trait,scale ="free") +
  geom_smooth(se=FALSE)
#  geom_hline(yintercept = 3.74, linetype=2) +
 # geom_hline(yintercept = -3.51, linetype=2)

#corCAR check
moge <- data.frame(WSG = unlist(WSG100),
                   Moist = unlist(Moist100),
                   slope = unlist(slope100),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time) %>%
  mutate(WSG = WSG - WSG100[[1]]) %>%
  mutate(Moist = Moist - Moist100[[1]]) %>%
  mutate(convex = convex - convex100[[1]]) %>%
  mutate(slope = slope - slope100[[1]])

moge$Time2 <- rep(1:7, each = 50)

obs$Time2 <- 1:7

#obs <- obs %>% filter(Time != 1982)
m1 <- gam(Convex ~  s(Time,k=4), data=obs)
m2 <- gamm(Convex ~  s(Time,k=4), data=obs, correlation = corAR1(form = ~Time2))
m3 <- gamm(Convex ~  s(Time,k=6), data=obs)

plot(Convex ~ Time, data=obs, xlab = expression(x[t]), ylab = expression(y[t]))
lines(fitted(m1) ~ obs$Time, lty = "solid", col = "darkolivegreen", lwd = 2)
lines(fitted(m2$lme) ~ obs$Time, lty = "solid", col = "midnightblue", lwd = 2)

summary(m2$lme)

moge <- data.frame(WSG = unlist(WSG100),
                   Moist = unlist(Moist100),
                   slope = unlist(slope100),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time)

moge$Time2 <- rep(1:7, each = 50)

m1 <- gam(WSG ~  s(Time,k=4), data=moge)

m2 <- gamm(WSG ~  s(Time,k=4), random = list(site = ~ 1),
           data=moge, correlation = corAR1(form = ~Time2))

m3 <- gamm(WSG ~  s(Time,k=4),
           random = list(site = ~ 1),
           data=moge, correlation = corAR1(form = ~Time2 |site))

summary(m2$gam)

plot(WSG ~ Time, data=moge, xlab = expression(x[t]), ylab = expression(y[t]))
lines(fitted(m1) ~ moge$Time, lty = "solid", col = "darkolivegreen", lwd = 2)
lines(fitted(m2$lme) ~ moge$Time, lty = "solid", col = "midnightblue", lwd = 2)

f1 <- sum((moge$WSG - fitted(m2$lme))^2)

f2 <- sum((moge$WSG - mean(moge$WSG))^2)


plot(fitted(m3$lme) ~ moge$Time, lty = "solid", col = "midnightblue", lwd = 2)


a2 <- fitted(m2$lme) %>% as.numeric
a3 <- fitted(m3$lme) %>% as.numeric


## CV again

train <- tail(obs,6)
test <- head(obs,1)

train_m <- gamm(WSG ~  s(Time,k=4), data=train, correlation = corAR1(form = ~Time2))
pred <- predict(train_m$gam, test)
resd <- (pred - test$WSG)^2


moge$WSG %>% t %>% matrix(ncol=7) %>% levelplot(asp="fill")

moge2 <- moge %>% tidyr::gather(trait, val, 1:4)

ggplot(moge2, aes(x=Time,y=val, fill = site %>% as.factor)) +
  #geom_point() +
  facet_wrap(~trait, scales="free") +
  geom_smooth(se = FALSE, col = "gray") +
  guides(col = FALSE, fill = FALSE) +
  xlab("Time") +
  ggtitle("50 simulations") +
  ylab("Imaginary community means at 50ha scale") +
  theme_bw()
