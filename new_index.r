rm(list = ls()) # This clears everything from memory.

library(dplyr)

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")
# prepare data set
ab.data <- as.data.frame(sapply(D20m,function(x)apply(x,2,sum)))


sapply(D100m, function(x) x[, "HYBAPR"])

ab100m <- sapply(D100m, function(x) apply(x, 1, sum))
moge <- sapply(D100m, function(x) x[, "HYBAPR"])

temp100 <- moge / ab100m
temp100 <- temp100[, 7] - temp100[, 1]


ab20m <- sapply(D20m, function(x) apply(x, 1, sum))
moge <- sapply(D20m, function(x) x[, "HYBAPR"])

temp20 <- moge / ab20m
temp20 <- temp20[, 7] - temp20[, 1]

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


mean(temp100)
mean(temp20)
ab.t.data2 %>% filter(sp == "HYBAPR")


WSG_sp <- ab.t.data %>% select(sp, WSG) %>% na.omit

WSG_D100m_1982<- D100m[[1]][, WSG_sp$sp]
WSG_D100m_2010<- D100m[[7]][, WSG_sp$sp]

ab1982 <- WSG_D100m_1982/ apply(WSG_D100m_1982, 1, sum)
ab2010 <- WSG_D100m_2010 / apply(WSG_D100m_2010, 1, sum)

WSG_100m <- t(t(ab2010 - ab1982) * (WSG_sp$WSG - mean(WSG_sp$WSG)))

WSG_100m <- WSG_100m[, order(-apply(WSG_100m, 2, sum))]


temp <- WSG_sp[order(WSG_sp$WSG), ]
WSG_100m <- WSG_100m[, order(-apply(WSG_100m, 2, sum))]

WSG_100m <- WSG_100m[, order(temp$sp)]

WSG_100m_dat <- data.frame(val = as.vector(WSG_100m),
  site = rep(rownames(WSG_100m), ncol(WSG_100m)),
  sp = rep(colnames(WSG_100m), each = nrow(WSG_100m))
  ) %>%
  mutate(sp  = factor(sp, levels = unique(sp)))



WSG_D20m_1982<- D20m[[1]][, WSG_sp$sp]
WSG_D20m_2010<- D20m[[7]][, WSG_sp$sp]

ab1982 <- WSG_D20m_1982/ apply(WSG_D20m_1982, 1, sum)
ab2010 <- WSG_D20m_2010 / apply(WSG_D20m_2010, 1, sum)

WSG_20m <- t(t(ab2010 - ab1982) * (WSG_sp$WSG - mean(WSG_sp$WSG)))

WSG_20m <- WSG_20m[, order(-apply(WSG_20m, 2, sum))]


temp <- WSG_sp[order(WSG_sp$WSG), ]
WSG_20m <- WSG_20m[, order(-apply(WSG_20m, 2, sum))]

WSG_20m <- WSG_20m[, order(temp$sp)]

WSG_20m_dat <- data.frame(val = as.vector(WSG_20m),
  site = rep(rownames(WSG_20m), ncol(WSG_20m)),
  sp = rep(colnames(WSG_20m), each = nrow(WSG_20m))
  ) %>%
  mutate(sp  = factor(sp, levels = unique(sp)))

ex_dat <- data.frame(val = as.vector(mapply(rnorm, 50, seq(-0.008, 0.008, length = 200), sd = 0.0005)),
  site = rep(1:50, 200),
  sp = rep(1:200, each = 50))

  levelplot(val ~  sp * site, data = ex_dat, col.regions = heat.colors(100), cut = 50)


library(lattice)

levelplot(val ~  sp * site, data = WSG_100m_dat, col.regions = heat.colors(100), cut = 5)

levelplot(val ~  sp * site, data = WSG_20m_dat, col.regions = heat.colors(100), cut = 50)

mm <- ab2010 - ab1982

ab.data$sp <- rownames(ab.data)

mm <- matrix(1:9, nrow = 3)

mm2 <- t(mm) * 1:3
t(mm2)

# spA = 0.1 100 -> 90 -> 0 -> 0
# spB = 0.5 10 -> 13 -> 8 -> 9
# spC = 0.8 5 -> 4 -> 4 -> 7
library(mgcv)

dat <- data.frame(sp = c(rep("spA", 190), rep("spB", 40), rep("spC", 20)),
     trait = c(rep(0.1, 190), rep(0.5, 40), rep(0.8, 20)),
     census = c(rep(1, 100), rep(2, 90), rep(1, 10), rep(2, 13),
      rep(3, 8), rep(4, 9), rep(1, 5), rep(2, 4), rep(3, 4), rep(4, 7)),
     site = rep(c("siteA", "siteB", "siteC", "siteD", "siteE"), each = 50))

res <- gam(trait ~ s(census, k = 3), data = dat)

res2 <- gamm(trait ~ s(census, k =3), data = dat, random = list(site = ~1))

p <- ggplot(dat, aes(x = census, y = trait, colour = sp)) +
  geom_jitter(width = 0.2)
print(p)

p <- ggplot(dat, aes(x = census, y = trait, colour = sp)) +
  geom_point()
print(p)

r1.r <- gamm(WSG ~  s(Time,k=4), random = list(site=~1), data=moge20, correlation = corAR1())



# unlist(WSG100.ind)

moge <- c(rep(ab.t.data2$WSG, ab.t.data2$census_1982),
  rep(ab.t.data2$WSG, ab.t.data2$census_1985),
  rep(ab.t.data2$WSG, ab.t.data2$census_1990),
  rep(ab.t.data2$WSG, ab.t.data2$census_1995),
  rep(ab.t.data2$WSG, ab.t.data2$census_2000),
  rep(ab.t.data2$WSG, ab.t.data2$census_2005),
  rep(ab.t.data2$WSG, ab.t.data2$census_2010))


moge1 <- D100m[[1]][, ab.t.data2$sp] %>% as_data_frame %>%
  mutate(site = rownames(D100m[[1]])) %>%
  tidyr::gather("sp", "abund", 1:202)

dat1 <- data_frame(site = rep(moge1$site, moge1$abund),
      sp = rep(moge1$sp, moge1$abund))

moge2 <- D100m[[2]][, ab.t.data2$sp] %>% as_data_frame %>%
  mutate(site = rownames(D100m[[2]])) %>%
  tidyr::gather("sp", "abund", 1:202)

dat2 <- data_frame(site = rep(moge2$site, moge2$abund),
      sp = rep(moge2$sp, moge2$abund))


moge3 <- D100m[[3]][, ab.t.data2$sp] %>% as_data_frame %>%
  mutate(site = rownames(D100m[[3]])) %>%
  tidyr::gather("sp", "abund", 1:202)

dat3 <- data_frame(site = rep(moge3$site, moge3$abund),
      sp = rep(moge3$sp, moge3$abund))


moge4 <- D100m[[4]][, ab.t.data2$sp] %>% as_data_frame %>%
  mutate(site = rownames(D100m[[4]])) %>%
  tidyr::gather("sp", "abund", 1:202)

dat4 <- data_frame(site = rep(moge4$site, moge4$abund),
      sp = rep(moge4$sp, moge4$abund))


moge5 <- D100m[[5]][, ab.t.data2$sp] %>% as_data_frame %>%
  mutate(site = rownames(D100m[[5]])) %>%
  tidyr::gather("sp", "abund", 1:202)

dat5 <- data_frame(site = rep(moge5$site, moge5$abund),
      sp = rep(moge5$sp, moge5$abund))


moge6 <- D100m[[6]][, ab.t.data2$sp] %>% as_data_frame %>%
  mutate(site = rownames(D100m[[6]])) %>%
  tidyr::gather("sp", "abund", 1:202)

dat6 <- data_frame(site = rep(moge6$site, moge6$abund),
      sp = rep(moge6$sp, moge6$abund))


moge7 <- D100m[[7]][, ab.t.data2$sp] %>% as_data_frame %>%
  mutate(site = rownames(D100m[[7]])) %>%
  tidyr::gather("sp", "abund", 1:202)

dat7 <- data_frame(site = rep(moge7$site, moge7$abund),
      sp = rep(moge7$sp, moge7$abund))

gam_dat <- rbind(dat1, dat2, dat3, dat4, dat5, dat6, dat7)

gam_dat <- gam_dat %>% as_data_frame %>%
  mutate(Time = rep(c(1982, 1985, 1990, 1995, 2000, 2005, 2010), c(nrow(dat1), nrow(dat2), nrow(dat3), nrow(dat4), nrow(dat5), nrow(dat6), nrow(dat7))))

gam_dat <- left_join(gam_dat, ab.t.data2, by = "sp")

system.time(gam_ind <- gamm(WSG ~  s(Time, k=4), data = gam_dat, correlation = corAR1()))



system.time(gamm_ind <- gamm(WSG ~  s(Time,k=4), random = list(site = ~1), data = gam_dat, correlation = corAR1()))

system.time(lme_ind <- lmer(WSG ~ Time + (Time|site), data = gam_dat))

system.time(lme_ind2 <- lmer(WSG ~ Time + (Time|sp), data = gam_dat))

# =====================================================
#this may be useful way to detect species.
#using the product of delta abundance and deviaiton from mean (or median) trait for each species
WSGab <- data.frame(sp = ab.t.data$sp,
        delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) -     ab.t.data$census_1982/sum(ab.t.data$census_1982),
        delta_ab2 = ab.t.data$census_2010 - ab.t.data$census_1982,
        delta_ab3 = ab.t.data$census_2010/ab.t.data$census_1982,
        WSG = ab.t.data$WSG,
        WSG_delta =ab.t.data$WSG - mean(ab.t.data$WSG,na.rm=T))
WSGab$index <- as.numeric(scale(WSGab$delta_ab)) * as.numeric(scale(WSGab$WSG_delta))

WSGab <- WSGab[order(WSGab$index),]



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
slopeab <- data.frame(sp = ab.t.data2$sp,
                    delta_ab = ab.t.data2$census_2010/sum(ab.t.data2$census_2010) - ab.t.data2$census_1982/sum(ab.t.data2$census_1982),
                    slope = ab.t.data2$slope,
                    slope_delta =ab.t.data2$slope - mean(ab.t.data2$slope))
slopeab$index <- as.numeric(scale(slopeab$delta_ab)) * as.numeric(scale(slopeab$slope_delta))

slopeab <- slopeab[order(slopeab$index),]



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

# slopeab
slopeab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    slope = ab.t.data$slope,
                    slope_delta =ab.t.data$slope - mean(ab.t.data$slope,na.rm=T))
slopeab$index <- slopeab$delta_ab * slopeab$slope_delta*100

slopeab <- slopeab[order(slopeab$index),]


# sp list for appendix ====================================================

taxa <- read.csv("~/Dropbox/MS/TurnoverBCI/nomenclature_R_20120305_Rready-2.csv")

sp_list <- WSGab %>%
  mutate(Wood_density = round(index, 4)) %>%
  mutate(Abundance_change = round(delta_ab, 4)) %>%
  select(sp, Abundance_change, Wood_density, -index) %>%
  full_join(., moistab, by = "sp") %>%
  mutate(Moisture = round(index, 4)) %>%
  select(-index) %>%
  full_join(., convexab, by = "sp") %>%
  mutate(Convexity = round(index, 4)) %>%
  select(-index) %>%
  full_join(., slopeab, by = "sp") %>%
  mutate(Slope = round(index, 4), sp6 = sp) %>%
  left_join(., taxa, by = "sp6") %>%
  select(sp, family, genus, species,
    Abundance_change, Wood_density, Moisture, Convexity, Slope) %>%
  arrange(sp) %>%
  mutate(species = paste(genus, species)) %>%
  select(-genus)

write.csv(sp_list, "/Users/mattocci/Dropbox/MS/TurnoverBCI/sp_list.csv")




postscript("~/Dropbox/MS/TurnoverBCI/fig/ab_change.eps",width=3,height=6)
par(lwd=1)
barplot(as.numeric(na.omit(WSGab$index)), main = "WSG",ylab="", xlab= "Contribution index",col=ifelse(WSGab$delta_ab>0,"gray","black"),border=ifelse(WSGab$delta_ab>0,"gray","black"),horiz=T)
par(lwd=1)
dev.off()



postscript("~/Dropbox/MS/TurnoverBCI/fig_current/ab_change.eps", width = 6, height = 6, paper = "special")
par(mfrow=c(1, 3), mar = c(4, 2, 2, 2))

barplot(as.numeric(na.omit(moistab$index)),
  main = "Moisture",
  col = ifelse(moistab$delta_ab>0,"gray","black"),
  border = ifelse(moistab$delta_ab>0,"gray","black"),
  horiz = T,
  xlab= "Contribution index")

barplot(as.numeric(na.omit(convexab$index)),
  main = "Convexity",
  col = ifelse(convexab$delta_ab>0,"gray","black"),
  border = ifelse(convexab$delta_ab>0,"gray","black"),
  horiz = T,
  xlab= "Contribution index")

barplot(as.numeric(na.omit(slopeab$index)),
  main = "Slope",
  col = ifelse(slopeab$delta_ab>0,"gray","black"),
  border = ifelse(slopeab$delta_ab>0,"gray","black"),
  horiz = T,
  xlab= "Contribution index")

par(mfrow=c(1,1))

dev.off()

par(mfrow=c(1,2))
barplot()
barplot(1:5,loremwd=1)

write.csv(WSGab, "~/Dropbox/MS/TurnoverBCI/fig0/WSGab.csv")
write.csv(moistab, "~/Dropbox/MS/TurnoverBCI/fig0/moistab.csv")
write.csv(convexab, "~/Dropbox/MS/TurnoverBCI/fig0/convexab.csv")
write.csv(slope100ab, "~/Dropbox/MS/TurnoverBCI/fig0/slope100ab.csv")
