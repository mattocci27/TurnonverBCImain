
setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")
library(cvTools)
library(MASS)
ab_data <- as.data.frame(sapply(D100m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)

trait.temp <- data.frame(sp=rownames(trait),
  moist=trait$Moist,
  slope=trait$sp.slope.mean,
  concav= -trait$sp.convex.mean,
  WSG=trait$WSG)

# Rick style
ab_t_data <- merge(ab_data,trait.temp,by="sp") %>%
  mutate(rate = log(census_2010/census_1982)/(2010-1982))

par(mfrow=c(2,2))
plot(rate ~ WSG, ab_t_data2)
plot(rate ~ moist, ab_t_data2)
plot(rate ~ convex, ab_t_data2)
plot(rate ~ slope, ab_t_data2)
par(mfrow=c(1,1))

ab_t_data2 <- ab_t_data %>%
  filter(is.finite(rate) == TRUE) %>%
  mutate(WSG = as.numeric(scale(WSG))) %>%
  mutate(slope = as.numeric(scale(slope))) %>%
  mutate(convex = as.numeric(scale(convex))) %>%
  mutate(moist = as.numeric(scale(moist))) %>%
  na.omit()


ab_t_data2 %>%
  filter(WSG < quantile(WSG, 0.25)) %>%
  lm(rate ~ WSG, .) %>% summary

ab_t_data2 %>%
  gam(rate ~ s(slope, k = 20), data = .) %>% summary


ab_t_data %>%
  gam(census_2010 ~ s(convex, k = 20), data = .,
      family = nb(),
      offset = log(census_1982)) %>% plot


plot(rate ~ convex, ab_t_data)

ab_t_data3 <- ab_t_data %>% gather(trait, val,9:14)

ggplot(ab_t_data3 %>% filter(trait != "convex.sd" & trait != "slope.sd"),
       aes(x=val, y = rate, col = trait)) +
  facet_wrap(~trait, scales = "free") +
  geom_point() +
  geom_smooth()

ab_t_data2 %>%
  filter(rate > quantile(rate, 0.75)) %>%
  plot(rate ~ WSG, .)


res_ori <- lm(rate ~ WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + convex:slope
            + moist:slope
            + convex:moist, ab_t_data2)

stepAIC(res_ori)

res_aic <- lm(rate ~ WSG
            + convex
            + slope
            + WSG:slope, ab_t_data2)

data <- ab_t_data  %>% filter(is.finite(rate) == TRUE)
data <- ab_t_data2
