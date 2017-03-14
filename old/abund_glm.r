rm(list = ls()) # This clears everything from memory.

setwd("~/Dropbox/BCI_Turnover")
load("BCI_turnover20141213.RData")
library(dplyr)
ab_data <- as.data.frame(sapply(D20m,function(x)apply(x,2,sum)))
ab_data$sp <- rownames(ab_data)


trait.temp <- data.frame(sp=rownames(trait),
  moist=trait$Moist,
  slope=trait$sp.slope.mean,
  slope.sd = trait$sp.slope.sd,
  convex=trait$sp.convex.mean,
  convex.sd=trait$sp.convex.sd,
  WSG=trait$WSG)

ab_t_data <- merge(ab_data,trait.temp,by="sp")

ab_t_data2 <- na.omit(ab_t_data)

# temp.data<-D[D$status1=="A",]

ab_t_data2$inc <- as.factor(ifelse(ab_t_data2$census_2010>ab_t_data2$census_1982,"UP","Down"))

# plot(convex~inc,data=ab.t.data2)
library(MASS)


ab_t_data2 <- ab_t_data %>%
  na.omit() %>%
  filter(census_1982 != 0 | census_2010 != 0) %>%
  mutate(census_1982 = census_1982 + 1) %>%
  mutate(census_2010 = census_2010 + 1) %>%
  mutate(WSG_moist = as.numeric(scale(WSG*moist))) %>%
  mutate(WSG_slope = as.numeric(scale(WSG*slope))) %>%
  mutate(WSG_convex = as.numeric(scale(WSG*convex))) %>%
  mutate(slope_moist = as.numeric(scale(slope*moist))) %>%
  mutate(convex_moist = as.numeric(scale(convex*moist))) %>%
  mutate(slope_convex = as.numeric(scale(slope*convex))) %>%
  mutate(WSG = as.numeric(scale(WSG))) %>%
  mutate(slope = as.numeric(scale(slope))) %>%
  mutate(convex = as.numeric(scale(convex))) %>%
  mutate(moist = as.numeric(scale(moist)))

r2.func = function(res, data){
  y = data$census_2010/data$census_1982
  residuals = y - res$fitted.values/data$census_1982
  1 - sum(residuals^2) / sum((y - mean(y))^2)
}


res2 <- glm.nb(census_2010 ~ WSG
            + moist
            + convex
            + slope
            + WSG_moist
            + WSG_convex
            + WSG_slope
            + slope_convex
            + slope_moist
            + convex:moist
            + offset(log(census_1982)),
            data = ab_t_data2)

summary(res2)
r2.func(res2, data = ab_t_data2)


res3 <- glm.nb(census_2010 ~WSG
            + moist
            + convex
            + slope
            + WSG:moist
            + WSG:convex
            + WSG:slope
            + convex:slope
            + moist:slope
            + convex:moist
            + WSG:convex:slope
            + WSG:moist:slope
            + WSG:convex:moist
            + slope:convex:moist
            + offset(log(census_1982)),
            data = ab_t_data2)

stepAIC(res2)

res_tr <- glm.nb(census_2010 ~ WSG
            + moist
            + convex
            + slope
            + WSG:slope
            + convex:moist
            + offset(log(census_1982)),
            data = ab_t_data2)

summary(res_tr)
r2_obs <- r2.func(res_tr, data = ab_t_data2)

1 - (1 - r2_obs) * (nrow(ab_t_data2) - 1) / (nrow(ab_t_data2) - 6 - 1)

r2_func <- function(res, data){
  # data <- ab_t_data2[ab_t_data2$ab1982 >= n, ]
  y <- data$census_2010/data$census_1982
  residuals <- y - res$fitted/data$census_1982
  # residuals = y - exp(res$linear.predictors)/data$census_1982
  1 - sum(residuals^2) / sum((y - mean(y))^2)
}

r2_func(res_tr, ab_t_data2)

data <- moge

r2.func(res_all, data = ab_t_data2)


temp <- c("WSG", "moist", "slope", "convex")
moge2 <- NULL
moge3 <- NULL
before <- proc.time()
for (j in 1:4){
  moge <- ab_t_data2
  for (i in 1:99){
    moge[, temp[j]] <- sample(moge[, temp[j]])
    res <- glm.nb(census_2010 ~WSG
                + moist
                + convex
                + slope
                + WSG:slope
                + convex:moist
                + offset(log(census_1982)),
                data = moge)
    moge2[i] <- r2.func(res, data = moge)
  }
  moge3 <- cbind(moge2, moge3)
}

after <- proc.time()
after - before




obs_rank <- apply(rbind(r2_obs, moge3), 2, rank)[1,]
(1 - (obs_rank /100)) * 2 # one tail

moge2 <- NULL
for (i in 1:99){
  moge <- ab_t_data2
  moge$WSG <- sample(moge$WSG)
  moge$moist <- sample(moge$moist)
  moge$convex <- sample(moge$convex)
  moge$slope <- sample(moge$slope)
  res <- glm.nb(census_2010 ~WSG
              + moist
              + convex
              + slope
              + WSG:slope
              + convex:moist
              + offset(log(census_1982)),
              data = moge)
  moge2[i] <- r2.func(res, data = moge)
}
c(r2_obs,moge2) %>% rank



moge2 <- NULL

for (i in 1:99){
  temp1 <- ab_t_data2 %>%
    dplyr::select(sp, census_1982, census_2010)
  temp2 <- ab_t_data2 %>%
    dplyr::select(sp, WSG, moist, convex, slope)

  temp1$sp <- sample(temp1$sp) %>% as.character

  moge <- full_join(temp1, temp2, by = "sp")
  # moge <- ab_t_data2
  # moge$WSG <- sample(moge$WSG)
  # moge$moist <- sample(moge$moist)
  # moge$convex <- sample(moge$convex)
  # moge$slope <- sample(moge$slope)
  res <- glm.nb(census_2010 ~WSG
              + moist
              + convex
              + slope
              + WSG:moist
              + WSG:convex
              + WSG:slope
              + convex:slope
              + moist:slope
              + convex:moist
              + offset(log(census_1982)),
              data = moge)
  moge2[i] <- r2.func(res, data = moge)
}

c(r2_obs,moge2) %>% rank

res <- glm.nb(census_2010 ~WSG
            + offset(log(census_1982)),
            data = moge)
