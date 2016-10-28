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


p0 <- barplot(as.numeric(na.omit(slopeab$index)),
  main = "Slope",
  col = ifelse(slopeab$delta_ab>0,"gray","black"),
  border = ifelse(slopeab$delta_ab>0,"gray","black"),
  horiz = T,
  xlab= "Contribution index",
  ylim = c(0, 360))

postscript("~/Dropbox/MS/TurnoverBCI/fig_current/ab_change.eps", width = 6, height = 6, paper = "special")

par(mfrow=c(1, 4), mar = c(4, 4, 2, 2))

p <- barplot(as.numeric(na.omit(WSGab$index)),
  main = "Wood density",
  ylab = "Species ranked by contribution index",
  xlab = "Contribution index",
  col = ifelse(WSGab$delta_ab>0,"gray","black"),
  border = ifelse(WSGab$delta_ab>0,"gray","black"),
  horiz = T,
  ylim = c(0, 360))
  # axis(2, tick = FALSE,line = -0.8, cex.axis = 0.9)
  axis(2, tcl = 0.2, labels = FALSE, at = p0)

barplot(as.numeric(na.omit(moistab$index)),
  main = "Moisture",
  col = ifelse(moistab$delta_ab>0,"gray","black"),
  border = ifelse(moistab$delta_ab>0,"gray","black"),
  horiz = T,
  xlab= "Contribution index",
  ylim = c(0, 360))

barplot(as.numeric(na.omit(convexab$index)),
  main = "Convexity",
  col = ifelse(convexab$delta_ab>0,"gray","black"),
  border = ifelse(convexab$delta_ab>0,"gray","black"),
  horiz = T,
  xlab= "Contribution index",
  ylim = c(0, 360))

barplot(as.numeric(na.omit(slopeab$index)),
  main = "Slope",
  col = ifelse(slopeab$delta_ab>0,"gray","black"),
  border = ifelse(slopeab$delta_ab>0,"gray","black"),
  horiz = T,
  xlab= "Contribution index",
  ylim = c(0, 360))

par(mfrow=c(1,1))

dev.off()


moge <- full_join(WSGab, moistab, by = "sp")

ggplot(WSGab, x = index, fill = sp) + geom_bar()



par(mfrow=c(1,2))
barplot()
barplot(1:5,loremwd=1)

write.csv(WSGab, "~/Dropbox/MS/TurnoverBCI/fig0/WSGab.csv")
write.csv(moistab, "~/Dropbox/MS/TurnoverBCI/fig0/moistab.csv")
write.csv(convexab, "~/Dropbox/MS/TurnoverBCI/fig0/convexab.csv")
write.csv(slope100ab, "~/Dropbox/MS/TurnoverBCI/fig0/slope100ab.csv")




barplot(as.numeric(na.omit(WSGab$index)), main = "WSG",ylab="", xlab= "Contribution index",col=ifelse(WSGab$delta_ab>0,"gray","black"),border=ifelse(WSGab$delta_ab>0,"gray","black"),horiz=T)

moge <- na.omit(WSGab$index) %>% as.numeric


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
  mutate(sp3 = factor(sp3, levels = c("ALSEBL", "HYBAPR", "PIPECO", "POULAR", "SWARS1", "TET2PA", "Other species")))





  lab_dat <- data_frame(lab = paste("(", letters[1:12], ")", sep = ""),
      y = 20,
      x = rep(c(0.21, -1.9, -0.29, 0.5),  3),
      size = rep(c("50ha", "1ha", "0.04ha"), each = 4),
      trait = rep(c("WSG", "moist", "convex", "slope"), 3),
      sp2 = "Other species") %>%
      mutate(size = factor(size, levels = c("50ha", "1ha", "0.04ha"))) %>%
      mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
      mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moisture", "Convexity~(m)", "Slope~(degrees)")))



postscript("~/Dropbox/MS/TurnoverBCI/fig/ab_change_pi.eps", width = 6, height = 3, paper = "special")

pdf("~/Dropbox/MS/TurnoverBCI/fig/ab_change_pi.pdf", width = 6, height = 3.6)

lab_dat <- fig_dat2 %>% count(trait2, sig) %>% as.data.frame %>%
  mutate(x = 1.4, y = 0.8) %>%
  mutate(n2 = paste("n =", n, sep = " "))

  ggplot(fig_dat2) +
  geom_bar(aes(y = val2, x = mean2/2,
    fill = as.factor(sp2), width = mean2), position = "fill", stat="identity", color = "white", size = 0.01) +
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



val <- fig_dat %>% filter(sp == "PIPECO" | sp == "POULAR") %>%
.$val

arrow_pos <- data.frame(
  trait2 = rep(c("Wood density", "Slope", "Moisture", "Convexity"), each = 2),
  sp = rep(c("PIPECO", "POULAR"), 4),
  val = val
  )

# Calculate the y positions for the labels and arrows
# For the myd data frame, obtain counts within each bin, but separately for each class
bwidth <- 30   # Set binwidth
Min <- floor(min(fig_dat$val)/bwidth) * bwidth
Max <- ceiling(max(fig_dat$val)/bwidth) * bwidth

Min <- -0.5
Max <- 0.5

# Function to do the counting
# func <- function(df) {
#    tab = as.data.frame(table(cut(df$val, breaks = seq(Min, Max, bwidth), right = FALSE)))
#    tab$upper = Min + bwidth * (as.numeric(rownames(tab)))
#    return(tab)
#    }
#
# # Apply the function to each class in myd data frame
# TableOfCounts <- plyr::ddply(fig_dat2, .(trait2), function(df) func(df))
#
# # Transfer counts of arrow_pos
# arrow_pos$upper <- (floor(arrow_pos$val/bwidth) * bwidth) + bwidth
# arrow_pos <- merge(arrow_pos, TableOfCounts, by = c("trait2", "upper"))
# arrow_pos$xvar <- (arrow_pos$upper - .5 * bwidth)      # x position of the arrow is at the midpoint of the bin
# # arrow_pos$trait2=factor(as.character(arrow_pos$trait),
    # levels=c("1", "2", "3", "4")) # Gets rid of warnings.

arrow_pos$Freq2 <- rep(c(30,60), 4)

pdf("~/Dropbox/MS/TurnoverBCI/fig/ab_change_hist.pdf", width = 6, height = 6, paper = "special")
ggplot(fig_dat2, aes(x = val)) +
  geom_histogram() +
  facet_wrap( ~ trait2, scale = "free") +
  theme_bw() +
  ylab("No. of spcies") +
  xlab("Contribution index") +

  geom_text(data = arrow_pos, aes(label=sp, x=val, y=Freq2 + 30, colour = sp), size=4) +
  geom_segment(data=arrow_pos,
     aes(x=val, xend=val, y=Freq2 + 20, yend=5, colour = sp),
     arrow=arrow(length=unit(2, "mm"))) +
  guides(colour = FALSE)
dev.off()
