rm(list = ls()) # This clears everything from memory.

library(dplyr)
library(ggplot2)

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
                    WSG_delta =ab.t.data$WSG - mean(WSG100[[1]])) %>%
                    mutate(WSG_delta2 = WSG - mean(WSG, na.rm = T))

WSGab$index <- WSGab$delta_ab * WSGab$WSG_delta*100

WSGab <- WSGab[order(WSGab$index),]

# moistab
moistab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    moist = ab.t.data$moist,
                    moist_delta = ab.t.data$moist - mean(Moist100[[1]]))
moistab$index <- moistab$delta_ab * moistab$moist_delta*100

moistab <- moistab[order(moistab$index),]

# convex100ab
convexab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    convex = ab.t.data$convex,
                    convex_delta =ab.t.data$convex - mean(convex100[[1]]))
convexab$index <- convexab$delta_ab * convexab$convex_delta*100

convexab <- convexab[order(convexab$index),]
# convexab <- convexab[order(convexab$convex),]

# slopeab
slopeab <- data.frame(sp = ab.t.data$sp,
                    delta_ab = ab.t.data$census_2010/sum(ab.t.data$census_2010) - ab.t.data$census_1982/sum(ab.t.data$census_1982),
                    slope = ab.t.data$slope,
                    slope_delta =ab.t.data$slope - mean(slope100[[1]]))
slopeab$index <- slopeab$delta_ab * slopeab$slope_delta*100

slopeab <- slopeab[order(slopeab$index),]


# sp list for appendix ====================================================

taxa <- read.csv("~/Dropbox/MS/TurnoverBCI/nomenclature_R_20120305_Rready-2.csv")

sp_list <- WSGab %>%
  mutate(Wood_density = round(index, 4)) %>%
  mutate(Abundance_change = round(delta_ab, 4)) %>%
  dplyr::select(sp, Abundance_change, Wood_density, -index) %>%
  full_join(., moistab, by = "sp") %>%
  mutate(Moisture = round(index, 4)) %>%
  dplyr::select(-index) %>%
  full_join(., convexab, by = "sp") %>%
  mutate(Convexity = round(index, 4)) %>%
  dplyr::select(-index) %>%
  full_join(., slopeab, by = "sp") %>%
  mutate(Slope = round(index, 4), sp6 = sp) %>%
  left_join(., taxa, by = "sp6") %>%
  dplyr::select(sp, family, genus, species,
    Abundance_change, Wood_density, Moisture, Convexity, Slope) %>%
  arrange(sp) %>%
  mutate(species = paste(genus, species)) %>%
  dplyr::select(-genus)

write.csv(sp_list, "/Users/mattocci/Dropbox/MS/TurnoverBCI/sp_list.csv")



#
# par(mfrow = c(2,2))
# moge <- sp_list$Wood_density %>% sort(decreasing = T)
# wsg_posi <- moge[moge>0]
# plot(1:length(wsg_posi), 100 *cumsum(wsg_posi)/sum(moge,na.rm=T), type = "b",
#   ylab = "cumulative contributoin index of wood density (%)", xlab = "Number of species", ylim = c(0, max(100 *cumsum(wsg_posi)/sum(moge,na.rm=T))))
# abline(h = 100, lty = 2)
#
# moge <- sp_list$Moisture %>% sort(decreasing = T)
# moist_posi <- moge[moge>0]
# plot(1:length(moist_posi), 100 *cumsum(moist_posi)/sum(moge,na.rm=T), type = "b", ylab = "cumulative contributoin index of moisture (%)", xlab = "Number of species", ylim = c(0, max(100 *cumsum(moist_posi)/sum(moge,na.rm=T))))
# abline(h = 100, lty = 2)
#
#
# moge <- sp_list$Convexity %>% sort(decreasing = T)
# convex_posi <- moge[moge>0]
# plot(1:length(convex_posi), 100 *cumsum(convex_posi)/sum(moge,na.rm=T), type = "b", ylab = "cumulative contributoin index of convexity (%)", xlab = "Number of species",ylim = c(0, max(100 *cumsum(convex_posi)/sum(moge,na.rm=T))))
# abline(h = 100, lty = 2)
#
#
# moge <- sp_list$Slope %>% sort(decreasing = F)
# slope_posi <- moge[moge<0]
# plot(1:length(slope_posi), 100 *cumsum(slope_posi)/sum(moge,na.rm=T), type = "b", ylab = "cumulative contributoin index of slope (%)", xlab = "Number of species", ylim = c(0, max(100 *cumsum(slope_posi)/sum(moge,na.rm=T))))
# abline(h = 100, lty = 2)
# par(mfrow = c(1,1))


## don't use %
fig_dat <- sp_list %>%
  arrange(desc(Wood_density)) %>%
  mutate(wsg_sp = 1:nrow(sp_list)) %>%
  mutate(wsg_posi = cumsum(Wood_density) /sum(Wood_density, na.rm = T) * 100) %>%
  mutate(wsg_posi = cumsum(Wood_density)) %>%

  arrange(desc(Moisture)) %>%
  mutate(moist_sp = 1:nrow(sp_list)) %>%
  mutate(moist_posi = cumsum(Moisture) /sum(Moisture, na.rm = T) * 100) %>%
  mutate(moist_posi = cumsum(Moisture)) %>%

  arrange(desc(Convexity)) %>%
  mutate(convex_sp = 1:nrow(sp_list)) %>%
  mutate(convex_posi = cumsum(Convexity) /sum(Convexity, na.rm = T) * 100) %>%
  mutate(convex_posi = cumsum(Convexity)) %>%

  arrange(Slope) %>%
  mutate(slope_sp = 1:nrow(sp_list)) %>%
  mutate(slope_nega = cumsum(Slope) /sum(Slope, na.rm = T) * 100) %>%
  mutate(slope_nega = cumsum(Slope))

temp1 <- fig_dat %>%
  tidyr::gather("trait", "val", c(wsg_posi, moist_posi, convex_posi, slope_nega))

temp2 <- fig_dat %>%
  tidyr::gather("trait2", "n_sp", c(wsg_sp, moist_sp, convex_sp, slope_sp))

fig_dat2 <- temp1 %>%
  mutate(n_sp = temp2$n_sp) %>%
  mutate(trait = factor(trait, levels = c("wsg_posi", "moist_posi", "convex_posi", "slope_nega"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood density", "Moisture", "Convexity", "Slope")))



fig_dat3 <- fig_dat2 %>%
  filter(trait != "wsg_posi" | Wood_density > 0) %>%
  filter(trait != "moist_posi" | Moisture > 0) %>%
  filter(trait != "convex_posi" | Convexity > 0) %>%
  filter(trait != "slope_nega" | Slope < 0)

dummy1 <- data_frame(x = 1,
    trait2 = "Wood density",
    val = c(0, max(fig_dat3 %>% filter(trait == "wsg_posi") %>% .$val)))
dummy2 <- data_frame(x = 1,
  trait2 = "Moisture",
  val = c(0, max(fig_dat3 %>% filter(trait == "moist_posi") %>% .$val)))
dummy3 <- data_frame(x = 1,
    trait2 = "Convexity",
    val = c(0, max(fig_dat3 %>% filter(trait == "convex_posi") %>% .$val)))
dummy4 <- data_frame(x = 1,
    trait2 = "Slope",
    val = c(0, max(fig_dat3 %>% filter(trait == "slope_nega") %>% .$val)))

dummy <- bind_rows(dummy1, dummy2, dummy3, dummy4) %>%
  mutate(n_sp =1) %>%
  mutate(trait2 = factor(trait2, levels = c("Wood density", "Moisture", "Convexity", "Slope"))) %>%
  mutate(census_1982 = 1) %>%
  mutate(inc = "Increased")

dummy_line <- data_frame(x = 1,
    trait2 = c("Wood density", "Moisture", "Convexity", "Slope"),
    n_sp = 1,
    h = c(sum(fig_dat$Wood_density, na.rm = T),
      sum(fig_dat$Moisture, na.rm = T),
      sum(fig_dat$Convexity, na.rm = T),
      sum(fig_dat$Slope, na.rm = T))) %>%
        mutate(trait2 = factor(trait2, levels = c("Wood density", "Moisture", "Convexity", "Slope"))) %>%
    mutate(census_1982 = 1) %>%
    mutate(inc = "Increased")

fig_dat4 <- left_join(fig_dat3, ab.data, by = "sp") %>%
  mutate(inc = ifelse(census_2010 - census_1982 > 0, "Increased", "Decreased"))

##ver colour
postscript("~/Dropbox/MS/TurnoverBCI/fig/fig4_abs.eps", width = 6, height = 6.5)

  ggplot(fig_dat4, aes(x = n_sp, y= val, colour = inc)) +
    geom_line(col = "black") +
    geom_point() +
    facet_wrap(~ trait2, scale = "free") +
    theme_bw() +
    geom_blank(data = dummy) +
    geom_hline(data = dummy_line, aes(yintercept = h), lty = 2) +
    # geom_hline(yintercept = 100, lty = 2) +
    ylab("Cumulative contributoin index") +
    xlab("Number of species") +
    geom_text(data = fig_dat4 %>% filter(n_sp <6),
      aes(label = species), hjust= -0.2, vjust = 1,
      fontface="italic", size = 3) +
    geom_segment(data = fig_dat4 %>% filter(n_sp <6),
      mapping = aes(x = n_sp + 10, y = val - 0.05, xend = n_sp + 2, yend= val),
      arrow=arrow(length = unit(0.05, "inches")), size=0.25) +
    theme(legend.position = "bottom") +
    scale_colour_manual(values = c("red3", "royalblue2")) +
    guides(colour = guide_legend(title = "Abundance"))

dev.off()

###
# WSG
# Mean   :0.5586

yy<-dnorm(seq(-0.2,0.8, length=100), mean = 0.399, sd = 0.128)
xx <- seq(-0.2,0.8, length=100)
plot(yy ~ xx , type = "l")

yy<-dnorm(seq(-0.2,0.8, length=100), mean = 0.399 + 0.003, sd = 0.128)
xx <- seq(-0.2,0.8, length=100)
points(yy ~ xx , type = "l", col = "blue")

##ver white
postscript("~/Dropbox/MS/TurnoverBCI/fig/fig4_abs.eps", width = 6, height = 6.5)

  ggplot(fig_dat4, aes(x = n_sp, y= val)) +
    geom_line(col = "black") +
    geom_point() +
    facet_wrap(~ trait2, scale = "free") +
    theme_bw() +
    geom_blank(data = dummy) +
    geom_hline(data = dummy_line, aes(yintercept = h), lty = 2) +
    # geom_hline(yintercept = 100, lty = 2) +
    ylab("Cumulative contributoin index") +
    xlab("Number of species") +
    geom_text(data = fig_dat4 %>% filter(n_sp <6),
      aes(label = species), hjust= -0.2, vjust = 1,
      fontface="italic", size = 3) +
    geom_segment(data = fig_dat4 %>% filter(n_sp <6),
      mapping = aes(x = n_sp + 10, y = val - 0.05, xend = n_sp + 2, yend= val),
      arrow=arrow(length = unit(0.05, "inches")), size=0.25)

dev.off()
