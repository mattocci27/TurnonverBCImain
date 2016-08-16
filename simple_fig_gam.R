library(dplyr)
library(ggplot2)

load("gam_fig.RData")

pdf("~/Dropbox/MS/TurnoverBCI/gamm.pdf",width=4.5,height=9,paper="special")

p1 <- ggplot(data=moge20) + geom_point(aes(x=Time,y=WSG),alpha=0.3)+ theme2 + labs(x="",title="0.04ha subplots", y = "Wood density")
p1 <- p1 + geom_line(data=r1data,aes(x=time,y=WSG),col="black")
p1 <- p1 + geom_ribbon(data = r1data, aes(ymin=lower1,ymax=upper1,x=time),
  alpha=0.5, fill="gray")



fig_20 <- moge20 %>%
  tidyr::gather("Trait", "val", 1:4) %>%
  mutate(size = "0.04-ha")

fig_100 <- moge100 %>%
  tidyr::gather("Trait", "val", 1:4) %>%
  mutate(size = "1-ha")

bind_rows(fig_20, fig_100) %>% ggplot(.) +
  geom_point(aes(x = Time, y = val), alpha = 0.3) +
  facet_grid(Trait ~ size, scale = "free")
