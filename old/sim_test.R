rm(list = ls()) # This clears everything from memory.
library(FD)
library(picante)
library(gdata)
load("~/Dropbox/temporal/BCI_temporal.rdata")
source("~/Dropbox/temporal/tempo_source2.r")

# abundance signal
set.seed(15)
n_sp <- 100
n_site <- 50

trait <- rnorm(n_sp) %>% sort
abund <- rlnorm(n_sp, -1.2 * trait + 3, 0.5) %>% as.integer + 1

t_dat <- data.frame(sp = paste("sp", 1:n_sp, sep = "_"),
  trait = trait,
  abund = abund)

site <- paste("site", 1:n_site, sep = "_")

site_dat <- NULL
for (i in 1:n_sp){
  moge <- sample(site, t_dat[i, "abund"], replace = T)
  site_dat <- c(site_dat, moge)
}

dat <- data_frame(sp = rep(t_dat$sp, t_dat$abund),
  site = site_dat)

#
com_m <- table(dat$site, dat$sp)

#com2
com_m2 <- com_m
com_m2[, 1:10] <- com_m2[, 1:10] * 5
com_m2[, 91:100] <- com_m2[, 91:100] * 0.2
com_m2 <- apply(com_m2, 2, as.integer)


names(trait) <- t_dat$sp
dis_m <- dist(trait) %>% as.matrix

system.time(nate_res <- ses.temp.dpw(com_m, com_m2, dis_m, runs = 99))

hist(nate_res$dpw.obs.z)



# no abundance signal
set.seed(15)
n_sp <- 100
n_site <- 50

trait <- rnorm(n_sp) %>% sort
abund <- rlnorm(n_sp, 3.12, 1.5) %>% as.integer + 1

t_dat <- data.frame(sp = paste("sp", 1:n_sp, sep = "_"),
  trait = trait,
  abund = abund)

site <- paste("site", 1:n_site, sep = "_")

site_dat <- NULL
for (i in 1:n_sp){
  moge <- sample(site, t_dat[i, "abund"], replace = T)
  site_dat <- c(site_dat, moge)
}

dat <- data_frame(sp = rep(t_dat$sp, t_dat$abund),
  site = site_dat)

#
com_m <- table(dat$site, dat$sp)
com_m[, 1:2] <- 100
#com2
com_m2 <- com_m
com_m2[, 1:2] <- 1
com_m2[, 91:100] <- com_m2[, 91:100]
com_m2 <- apply(com_m2, 2, as.integer)

names(trait) <- t_dat$sp
dis_m <- dist(trait) %>% as.matrix

system.time(nate_res <- ses.temp.dpw(com_m, com_m2, dis_m, runs = 99))

hist(nate_res$dpw.obs.z)
