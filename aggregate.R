rm(list = ls()) # This clears everything from memory.
library(dplyr)
ilibrary(ggplot2)
setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20150611.RData")
source("TurnoverSource20150611.r")

# PIPECO
# POULAR

ini_20 <- D20m[[1]][, c("PIPECO", "POULAR")] %>% as.data.frame
ini_100 <- D100m[[1]][, c("PIPECO", "POULAR")] %>% as.data.frame

aft_20 <- D20m[[7]][, c("PIPECO", "POULAR")] %>% as.data.frame
aft_100 <- D100m[[7]][, c("PIPECO", "POULAR")] %>% as.data.frame



# aft_20[ini_20[,1] == 0, ]

# ini_100 <- ini_100 / apply(ini_100, 2, sum)
# ini_20 <- ini_20 / apply(ini_20, 2, sum)
# aft_100 <- aft_100 / apply(aft_100, 2, sum)
# aft_20<- aft_20/ apply(aft_20, 2, sum)

res_20 <- (aft_20) / (ini_20) %>% as_data_frame
res_100 <- (aft_100) / (ini_100) %>% as_data_frame


res_20[is.infinite(res_20[, 1]),] %>% nrow
res_20[is.infinite(res_20[, 2]),] %>% nrow

#
# aft_20[aft_20[ini_20[,1] == 0, "PIPECO"] > 0, ]
#
# res_20[aft_20[ini_20[,1] == 0, "site"][[1]], ]
#
# res_20 %>%
#   filter(site == aft_20[ini_20[,1] == 0, "site"][[1]])

# # 0 / 0 should be 1
res_20[is.na(res_20)] <- 1

#inf is removed from figures

dat_100 <- res_100 %>%
  tidyr::gather("sp", "val", 1:2) %>%
  mutate(scale = "1-ha")
dat_20 <- res_20 %>%
  tidyr::gather("sp", "val", 1:2) %>%
  mutate(scale = "0.04-ha")

pdf("~/Dropbox/MS/TurnoverBCI/agg.pdf", width = 6, height = 3)
bind_rows(dat_100, dat_20) %>%
  ggplot(., aes(x = val, fill = scale, colour = scale)) +
  facet_grid( ~ sp, scales = "free") +
  # geom_density(aes(y = ..scaled..), alpha = 0.3) +
  geom_density(alpha = 0.3) +
  theme_bw() +
  xlab("No. of individuals in 2010 / No. of individuals  in 1982") +
  ylab("Density")
dev.off()
