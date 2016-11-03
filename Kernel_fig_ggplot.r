rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")

####
#kernel density
#####
library(ggplot2)
library(gridExtra)
library(dplyr)
theme_set(theme_bw())

kernel.20 <- data.frame(WSG=unlist(WSG20),
             convex=unlist(convex20),
             slope=unlist(slope20),
             moist=unlist(Moist20),
             time=c(rep(1982,50),
                    rep(1985,50),
                    rep(1990,50),
                    rep(1995,50),
                    rep(2000,50),
                    rep(2005,50),
                    rep(2010,50)))

kernel.100 <- data.frame(WSG=unlist(WSG100),
             #convex=unlist(convex.100.rare[[10]),
             #slope=unlist(slope.100.rare[[10]),
             convex=unlist(convex100),
             slope=unlist(slope100),
             moist=unlist(Moist100),
             time=c(rep(1982,50),
                    rep(1985,50),
                    rep(1990,50),
                    rep(1995,50),
                    rep(2000,50),
                    rep(2005,50),
                    rep(2010,50)))

temp <- sapply(WSG.ind,length)
k.ind.WSG <- data.frame(WSG=unlist(WSG.ind),
            time=c(rep(1982,temp[1]),
                  rep(1985,temp[2]),
                  rep(1990,temp[3]),
                  rep(1995,temp[4]),
                  rep(2000,temp[5]),
                  rep(2005,temp[6]),
                  rep(2010,temp[7])))

temp <- sapply(Moist.ind,length)
k.ind.moist <- data.frame(moist=unlist(Moist.ind),
            time=c(rep(1982,temp[1]),
                  rep(1985,temp[2]),
                  rep(1990,temp[3]),
                  rep(1995,temp[4]),
                  rep(2000,temp[5]),
                  rep(2005,temp[6]),
                  rep(2010,temp[7])))
# temp <- sapply(slope.ind.rare[[1]],length)
temp <- sapply(slope.ind,length)
k.ind.slope <- data.frame(slope=unlist(slope.ind),
            time=c(rep(1982,temp[1]),
                  rep(1985,temp[2]),
                  rep(1990,temp[3]),
                  rep(1995,temp[4]),
                  rep(2000,temp[5]),
                  rep(2005,temp[6]),
                  rep(2010,temp[7])))

# temp <- sapply(convex.ind.rare[[1]],length)
temp <- sapply(convex.ind, length)
k.ind.convex <- data.frame(convex=unlist(convex.ind),
            time=c(rep(1982,temp[1]),
                  rep(1985,temp[2]),
                  rep(1990,temp[3]),
                  rep(1995,temp[4]),
                  rep(2000,temp[5]),
                  rep(2005,temp[6]),
                  rep(2010,temp[7])))



temp100 <- kernel.100 %>%
  mutate(size = "1ha")

temp20 <- kernel.20 %>%
  mutate(size = "0.04ha")

fig_dat <- bind_rows(temp100, temp20) %>%
  tidyr::gather(., "trait", "val", 1:4)

temp1 <- data_frame(time = k.ind.WSG$time,
    size = "individual",
    trait = "WSG",
    val = k.ind.WSG[,1])

temp2 <- data_frame(time = k.ind.moist$time,
    size = "individual",
    trait = "moist",
    val = k.ind.moist[,1])

temp3 <- data_frame(time = k.ind.convex$time,
    size = "individual",
    trait = "convex",
    val = k.ind.convex[,1])

temp4 <- data_frame(time = k.ind.slope$time,
    size = "individual",
    trait = "slope",
    val = k.ind.slope[,1])

fig_dat2 <- bind_rows(fig_dat, temp1, temp2, temp3, temp4) %>%
  mutate(size = factor(size, levels = c("1ha", "0.04ha", "individual"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
  mutate(size2 = factor(size, labels = c("1ha", "0.04ha", "Individual"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Convexity~(m)", "Slope~(degrees)")))


dummy1 <- data_frame(time = 1982,
    size = "1ha",
    trait = "WSG",
    val = c(0.55, 0.65))
dummy2 <- data_frame(time = 1982,
    size = "1ha",
    trait = "moist",
    val = c(0.3, 0.9))

dummy3 <- data_frame(time = 1982,
    size = "individual",
    trait = "moist",
    val = c(-2, 4))

dummy4 <- data_frame(time = 1982,
    size = "1ha",
    trait = "slope",
    val = c(4, 6))

dummy5 <- data_frame(time = 1982,
    size = "0.04ha",
    trait = "slope",
    val = c(4, 6))


dummy0 <- fig_dat2 %>%
    filter(., size != "0.04ha" | trait != "convex" | (val > 0.035 & val < 0.085)) %>%
    filter(., size != "individual" | trait != "convex" | (val > -0.15 & val < 0.25)) %>%
    filter(., size != "0.04ha" | trait != "slope" | (val > 4 & val < 6)) %>%
    filter(., size != "individual" | trait != "slope" | (val > 2 & val < 8))


dummy0 %>% filter(., trait == "convex" & size == "0.04ha") %>% summary

dummy <- bind_rows(dummy0, dummy1, dummy2, dummy3, dummy4, dummy5) %>%
  mutate(size = factor(size, levels = c("1ha", "0.04ha", "individual"))) %>%
  mutate(trait = factor(trait, levels = c("WSG", "moist", "convex", "slope"))) %>%
  mutate(size2 = factor(size, labels = c("1ha", "0.04ha", "Individual"))) %>%
  mutate(trait2 = factor(trait, labels = c("Wood~density ~(g~cm^{-3})", "Moist", "Convexity~(m)", "Slope~(degrees)")))

# dummy6 <- fig_dat2 %>% filter(., trait == "convex" & size == "0.04ha") %>%
#
# dummy7 <- fig_dat2 %>% filter(., trait != "convex" & size != "0.04ha")  %>%
#   bind_rows(., dummy6)


# postscript("~/Dropbox/MS/TurnoverBCI/fig/mogemoge.eps",
#   width = 10, height = 6.66, paper = "special")

pdf("~/Dropbox/MS/TurnoverBCI/fig/mogemoge.pdf",
  width = 10, height = 6.66, paper = "special")

ggplot(dummy0, aes(x = val)) +
  facet_wrap(~ size2 + trait2, scale = "free",
  labeller = labeller(trait2 = label_parsed, size2 = label_value)) +
  geom_density(data = filter(dummy0, size != "individual"),adjust = 1,  aes(colour = as.factor(time))) +
  geom_blank(data = dummy) +
  geom_density(data = filter(dummy0, size == "individual"), adjust = 4, aes(colour = as.factor(time))) +
  ylab("Density") +
  xlab("Trait values") +
  # theme(strip.text.x = element_text(hjust = 0.5, vjust = 0.5),
  #   panel.margin.x = unit(0.5, "lines"))
  theme(panel.margin.x = unit(0.5, "lines"))

dev.off()


aa <- rnorm(1000, 0, 0.1)
plot(density(aa))

levels(diamonds$color) <- paste0(" \n", levels(diamonds$color) , "\n ")


P <- ggplot(diamonds, aes(carat, price, fill = ..x..)) +
      xlim(0, 2) + stat_binhex(na.rm = TRUE)+
      facet_wrap(~ color + cut)
P +  theme(strip.text = element_text(size=9, lineheight=0, hjust = 1))



###use grid again ==================================
library(cowplot)


postscript("~/Dropbox/MS/TurnoverBCI/fig/mogemoge.eps",
  width = 6, height = 4)

  theme <- theme(axis.text.x = element_text(size = 7, angle = 45),
    axis.text.y = element_text(size = 7),
    axis.title.y = element_text(size = 8, margin = margin(0, -5, 0, 0)),
    axis.title.x = element_text(size = 8, margin = margin(-10, 0, 0, 0)),
    plot.margin = unit(c(0.1, 0.5, 0.1, 0.1), units = "lines")
    # panel.margin = unit(0, "lines")
    )

  p1 <- ggplot(kernel.100, aes(x=WSG))
  p1 <- p1 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.55,0.65) + labs(y="Denstiy (1ha)", x="") + theme

  p2 <- ggplot(kernel.20, aes(x=WSG))
  p2 <- p2 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.53,0.68)  + labs(y="Denstiy (0.04ha)",x="")+ theme

  p3 <- ggplot(k.ind.WSG, aes(x=WSG))
  p3 <- p3 + geom_density(adjust=4, aes(colour=as.factor(time)))  + labs(y="Density (each tree)",x = expression(paste("Wood density (g ",cm^-3,")")))+ theme(legend.position=c(0.22,0.6)) + guides(colour=FALSE)+ theme

  p4 <- ggplot(kernel.100, aes(x=moist))
  p4 <- p4 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.3,0.9)  + guides(colour=FALSE)  + labs(y="",x="")+ theme

  p5 <- ggplot(kernel.20, aes(x=moist))
  p5 <- p5 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0,1.2)  + guides(colour=FALSE)  + labs(y="",x="")+ theme

  p6 <- ggplot(k.ind.moist, aes(x=moist))
  p6 <- p6 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Moisture") + guides(colour=FALSE) + xlim(-2.1,4)+ theme

  p7 <- ggplot(kernel.100, aes(x=convex))
  p7 <- p7 + geom_density(adjust=1,aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.03,0.09)  + guides(colour=FALSE) + labs(y="",x="")+ theme


  p8 <- ggplot(kernel.20, aes(x=convex))
  p8 <- p8 + geom_density(adjust=1,aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(0.03,0.09)  + guides(colour=FALSE) + labs(y="",x="")+ theme


  p9 <- ggplot(k.ind.convex, aes(x=convex))
  p9 <- p9 + geom_density(adjust=4, aes(colour=as.factor(time))) + labs(y="",x="Convexity (m)") + guides(colour=FALSE) + xlim(-0.15,0.25)+ theme

  p10 <- ggplot(kernel.100, aes(x=slope))
  p10 <- p10 + geom_density(adjust=1,aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(4,6)  + guides(colour=FALSE)+ labs(y="",x="")+ theme

  p11 <- ggplot(kernel.20, aes(x=slope))
  p11 <- p11 + geom_density(aes(colour=as.factor(time))) + guides(colour=FALSE) + xlim(4,6)  + guides(colour=FALSE)+ labs(y="",x="")+ theme

  p12 <- ggplot(k.ind.slope, aes(x=slope))
  p12 <- p12 + geom_density(adjust=4, aes(colour=as.factor(time))) + xlim(2,8)+ labs(y=" ",x="Slope (degrees)") + guides(colour=FALSE)+ theme

  xoff <- .25 # relative x position of label, within one plot
  yoff <- .93 # relative y position of label, within one plot

  plot_grid(p1, p4, p7, p10,
        p2, p5, p8, p11,
        p3, p6, p9, p12,
        # p1, p4, p7, p10,
        # p1, p4, p7, p10,
        ncol = 4,
        align = "hv"
      ) +
  draw_plot_label(label = paste("(", letters[1:12], ")", sep = ""),
          x = rep((xoff + 0:3)/4, 3),
          y = 1 - (1 - yoff + rep(0:2, each = 4))/3,
          hjust = .5,
          vjust = .5,
          size = 9,
          fontface = 1)

dev.off()
