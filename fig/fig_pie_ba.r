rm(list = ls()) # This clears everything from memory.

library(dplyr)
library(ggplot2)

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")
# prepare data set
ab.data <- as.data.frame(sapply(D20ba,function(x)apply(x,2,sum)))
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

write.csv(sp_list, "/Users/mattocci/Dropbox/MS/TurnoverBCI/sp_list_ba.csv")



# ========================================
WSGab <- WSGab %>%
  mutate(WSG_index = index)

moistab <- moistab %>%
  mutate(moist_index = index)

convexab <- convexab %>%
  mutate(convex_index = index)

slopeab <- slopeab %>%
  mutate(slope_index = index)

fig_dat <- full_join(WSGab, moistab, by = "sp") %>%
  full_join(., convexab, by = "sp") %>%
  full_join(., slopeab, by = "sp") %>%
  tidyr::gather(., "trait", "val", c(WSG_index, slope_index, moist_index, convex_index)) %>%
  dplyr::select(., c(sp, trait, val)) %>%
  na.omit %>%
  mutate(sig = ifelse(val < 0, "Negative", "Positive"))  %>%
  mutate(val2 = abs(val)) %>%
  mutate(trait_sig = paste(trait, sig, sep = "_"))

  fig_dat %>% group_by(trait) %>%
   summarise(mean = sum(val2, na.rm = T))

temp0 <- tapply(fig_dat$val2, fig_dat$trait, mean, na.rm = T) %>% rep(each = 2)

temp <- fig_dat %>% group_by(trait, sig) %>%
  summarise(mean = mean(val2, na.rm = T)) %>%
  mutate(trait_sig = paste(trait, sig, sep = "_")) %>%
  as_data_frame %>%
  dplyr::select(., c(mean, trait_sig)) %>%
  mutate(mean2 = mean / temp0)

# temp[3,1] <- 100

# high slope
# PIPECA

# large change
# PSYCLI

# high abund
# HYBAPR
# FARAOC

sp_vec <- c("PIPECO", "POULAR", "PIPECA", "PSYCLI", "HYBAPR", "FARAOC")

# moist
# TET2PA

#convex
# SWARS1
# ALSEBL

# slope
# HYBAPR
library(grDevices)

n <- 2
hues <- seq(15, 375, length=n+1)
cols_hex <- sort(hcl(h=hues, l=65, c=100)[1:n])

sp_vec2 <- c("PIPECO", "POULAR", "TET2PA", "SWARS1", "ALSEBL", "HYBAPR")

sp_vec2 <- c("PIPECO", "POULAR", "QUARAS", "CAVAPL", "TET2PA", "HYBAPR", "FARAOC")

fig_dat2 <- full_join(fig_dat, temp, by = "trait_sig") %>%
  arrange(val2) %>%
  mutate(trait = factor(trait, levels = c("WSG_index", "moist_index", "convex_index", "slope_index"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood density", "Moisture", "Convexity", "Slope"))) %>%
  mutate(sig = factor(sig, levels = c("Positive", "Negative"))) %>%
  mutate(col = ifelse(sp == "POULAR", "POULAR",
    ifelse(sp == "PIPECO", "PIPECO", "Other species"))) %>%
  mutate(col = factor(col, levels = c("PIPECO", "POULAR", "Other species"))) %>%
  mutate(sp2 = ifelse(sp %in% sp_vec, as.character(sp), "Other species")) %>%
  mutate(sp3 = ifelse(sp %in% sp_vec2, as.character(sp), "Other species")) %>%
  mutate(sp2 = factor(sp2, levels = c("FARAOC", "HYBAPR", "PIPECA", "PIPECO", "POULAR", "PSYCLI", "Other species"))) %>%
   mutate(sp3 = factor(sp3, levels = c("PIPECO", "POULAR", "QUARAS", "CAVAPL", "TET2PA", "HYBAPR", "FARAOC", "Other species")))





lab_dat <- data_frame(lab = paste("(", letters[1:12], ")", sep = ""),
    y = 20,
    x = rep(c(0.21, -1.9, -0.29, 0.5),  3),
    size = rep(c("50ha", "1ha", "0.04ha"), each = 4),
    trait = rep(c("WSG", "moist", "convex", "slope"), 3),
    sp2 = "Other species") %>%
    mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha"))) %>%
    mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
    mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Convexity~(m)", "Slope~(degrees)")))

pdf("~/Dropbox/MS/TurnoverBCI/fig/fig4_BA.pdf", width = 6, height = 3.6)
lab_dat <- fig_dat2 %>% count(trait2, sig) %>% as.data.frame %>%
  mutate(x = 1.4, y = 0.8) %>%
  mutate(n2 = paste("n =", n, sep = " "))

  ggplot(fig_dat2) +
  geom_bar(aes(y = val2, x = mean2/2,
    fill = as.factor(col), width = mean2), position = "fill", stat="identity", color = "white", size = 0.01) +
  geom_text(data = lab_dat, aes(label = n2, x = 1.2, y = 1),
    size = 4, vjust = 0) +
  coord_polar(theta="y") +
  facet_grid(sig ~ trait2) +
  # scale_fill_gradient(low = "blue", high = "red") +
  # guides(fill = FALSE) +
  # scale_fill_manual(values = ifelse(levels(fig_dat2$sp) == "POULAR", "red", "gray")) +
  # scale_fill_manual(values = c("PIPECO" = "#F8766D", "POULAR" = "#00C0AF", "Other species" = "gray"), name = "") +
  scale_fill_manual(values = c(cols_hex, "gray"),
    guide = guide_legend(title.position = "top",
      title = "Species",)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     axis.ticks = element_blank()) +
  xlab("") + ylab("") +
  theme(legend.position = "bottom",
    legend.margin = unit(-0.2, "cm"))

dev.off()



pdf("~/Dropbox/MS/TurnoverBCI/fig/BA_pie.pdf", width = 6, height = 3.6)
# 6 species
n <- 7
hues <- seq(15, 375, length=n+1)
cols_hex <- sort(hcl(h=hues, l=65, c=100)[1:n])


lab_dat <- fig_dat2 %>% count(trait2, sig) %>% as.data.frame %>%
  mutate(x = 1.4, y = 0.8) %>%
  mutate(n2 = paste("n =", n, sep = " "))

  ggplot(fig_dat2) +
  geom_bar(aes(y = val2, x = mean2/2,
    fill = as.factor(sp3), width = mean2), position = "fill", stat="identity", color = "white", size = 0.01) +
  geom_text(data = lab_dat, aes(label = n2, x = 1.2, y = 1),
    size = 4, vjust = 0) +
  coord_polar(theta="y") +
  facet_grid(sig ~ trait2) +
  # scale_fill_gradient(low = "blue", high = "red") +
  # guides(fill = FALSE) +
  # scale_fill_manual(values = ifelse(levels(fig_dat2$sp) == "POULAR", "red", "gray")) +
  # scale_fill_manual(values = c("PIPECO" = "#F8766D", "POULAR" = "#00C0AF", "Other species" = "gray"), name = "") +
  scale_fill_manual(values = c(cols_hex, "gray"),
    guide = guide_legend(title.position = "top",
      title = "Species",)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
     axis.text.y = element_blank(),
     axis.ticks = element_blank()) +
  xlab("") + ylab("") +
  theme(legend.position = "bottom",
    legend.margin = unit(-0.2, "cm"))
dev.off()
