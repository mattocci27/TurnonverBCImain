library(mgcv)

set.seed(5)
x <- 1:7

before <- proc.time()
p <- NULL
r2 <- NULL
dat2 <- NULL


plot(y ~ x, xlab = expression(x[t]), ylab = expression(y[t]), ylim=c(-5,5))
for (j in 1:100){
  y <- NULL
  y[1] <- 0
  for (i in 2:7){
    y[i] <- rnorm(1,y[i-1], 0.2)
  }

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
  lines(fitted(res$lme) ~ x, lty = "solid", col = "midnightblue", lwd = 2)
  }
  #res <- gamm(y ~ s(x, k=4), data = dat, correlation = corCAR1(form=~x))

}

after <- proc.time()
after - before


dat2 <- dat2 %>% mutate(n = rep((1:(nrow(dat2)/7)), each = 7))

save.image("art.rda")

load("~/Dropbox/MS/TurnoverBCI/TurnoverBCImain/analysis/art.rda")

ggplot(dat2 %>% filter(n <= 20), aes(x=x,y=y, col = n %>% as.factor)) +
  #geom_point() +
  geom_smooth(se = FALSE) +
  guides(col = FALSE) +
  theme_bw()


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
dat <- data.frame(x=x,y=y)
m1 <- smooth.spline(x,y)
m2 <- gam(y ~ s(x, k=4), data = dat)
m3 <- gamm(y ~ s(x, k=4), data = dat, correlation = corCAR1(form = ~ y))
m4 <- gamm(y ~ s(x, k=4), data = dat, correlation = corCAR1())

plot(y ~ x, xlab = expression(x[t]), ylab = expression(y[t]))
lines(fitted(m1) ~ x, lty = "solid", col = "darkolivegreen", lwd = 2)
lines(fitted(m2) ~ x, lty = "solid", col = "red", lwd = 2)
lines(fitted(m3$lme) ~ x, lty = "solid", col = "midnightblue", lwd = 2)
#lines(fitted(m4$lme) ~ x, lty = "solid", col = "orange", lwd = 2)

# use Ferguson
test <- bckftg.alg(y, as.data.frame(x), lime = )

###
setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")
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

fig_dat3 <- data_frame(Time = fig_dat3$Time,
    size = "52ha",
    trait = fig_dat3$trait,
    val = fig_dat3$val)

obs <- fig_dat3 %>% tidyr::spread(trait, val) %>%
  mutate(Convex = scale(Convex)) %>%
  mutate(Convex = Convex - Convex[1]) %>%
  mutate(Moist = scale(Moist)) %>%
  mutate(Moist = Moist - Moist[1]) %>%
  mutate(Slope = scale(Slope)) %>%
  mutate(Slope = Slope - Slope[1]) %>%
  mutate(WSG = scale(WSG)) %>%
  mutate(WSG = WSG - WSG[1])

obs2 <- obs %>%
  tidyr::gather(trait, val, 3:6)

ggplot(obs2, aes(x=Time,y=val)) +
  geom_point() +
  facet_grid(.~trait) +
  geom_smooth(se=FALSE) +
  geom_hline(yintercept = 3.74, linetype=2) +
  geom_hline(yintercept = -3.51, linetype=2)


#corCAR check
moge <- data.frame(WSG = unlist(WSG100),
                   Moist = unlist(Moist100),
                   slope = unlist(slope.100.rare[10]),
                   convex = unlist(convex100),
                   site = as.factor(rep(1:50,7)),
                   Time) %>%


m1 <- gam(Moist ~  s(Time,k=4), data=obs)
m2 <- gamm(Moist ~  s(Time,k=4), data=obs, correlation = corAR1(form = ~Time))

plot(Moist ~ Time, data=obs, xlab = expression(x[t]), ylab = expression(y[t]))
lines(fitted(m1) ~ obs$Time, lty = "solid", col = "darkolivegreen", lwd = 2)
lines(fitted(m2$lme) ~ obs$Time, lty = "solid", col = "midnightblue", lwd = 2)


m1 <- gam(Moist ~  s(Time,k=4), data=moge)
m2 <- gamm(Moist ~  s(Time,k=4), random = list(site = ~ 1), data=moge, correlation = corAR1(form = ~Time))


plot(Moist ~ Time, data=moge, xlab = expression(x[t]), ylab = expression(y[t]))
lines(fitted(m1) ~ obs$Time, lty = "solid", col = "darkolivegreen", lwd = 2)
lines(fitted(m2$lme) ~ obs$Time, lty = "solid", col = "midnightblue", lwd = 2)


