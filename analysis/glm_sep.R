
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

ab_t_data <- merge(ab_data, trait.temp, by="sp")

ab_t_data2 <- ab_t_data %>%
  na.omit()

ab_t_data2 <- ab_t_data
## restrcited trait value

var_name <- "WSG"
temp <- quantile(ab_t_data2[,var_name], 0.8, na.rm = T) %>% as.numeric

temp_dat <- ab_t_data2[ab_t_data2[,var_name] > temp,]

form <- paste("(census_2010 + 1) ~ ", var_name,
             " + offset(log(census_1982 + 1))", sep = "") %>% noquote

res_temp <- glm.nb(form,
            data = temp_dat)

summary(res_temp)



p_func <- function(var_name, q = 0.2){
  var_name <- var_name
  temp <- quantile(ab_t_data2[,var_name], q, na.rm = T) %>% as.numeric

  temp_dat <- ab_t_data2[ab_t_data2[,var_name] < temp,]

  form <- paste("(census_2010 + 1) ~ ", var_name,
                " + offset(log(census_1982 + 1))", sep = "") %>% noquote

  res_temp <- glm.nb(form,
                     data = temp_dat)
  summary(res_temp)$coefficients[2,4]
}

p_func("concav", q = 0.2)
p_func("moist", q = 0.2)
p_func("slope", q = 0.2)
p_func("WSG", q = 0.2)

plot((census_2010 + 1)/(census_1982+1) ~ WSG, temp_dat, log = "y")
abline(h=1)


dat <- dat %>% filter(!is.na(WSG))
glmfit <- glm.nb((census_2010 + 1) ~ WSG +
                 offset(log(census_1982 + 1)),
               data = ab_t_data)

cv_glm_s <- function(dat, glmfit, K = 10){
  cv <- NULL
  SS <- NULL
  PREDS <- NULL
  temp <- cvFolds(nrow(dat), K = K, type = "random")
  temp <- data.frame(ID = temp$subsets, gr = temp$which) %>%
    arrange(ID)
  temp <- data.frame(dat, temp)

  Call <- glmfit$call

  for (i in 1:K){
    Call$data <- filter(temp, gr != i)
    d_glm <- eval.parent(Call)

    test_dat <- temp %>% filter(gr == i)
    fitted <- predict(d_glm, test_dat, type = "response")
    fitted2 <- fitted / (test_dat$census_1982 + 1)
    yy <- (test_dat$census_2010 + 1)/ (test_dat$census_1982 + 1)

    SS[i] <- (yy - mean(yy, na.rm = T))^2 %>% mean
    PREDS[i] <- (yy - fitted2)^2 %>% mean(na.rm = T)
  }
  # 1 - mean(PREDS, na.rm = T) / mean(SS, na.rm = T)
  mean_ <- mean(1 - PREDS/SS, na.rm = T)
  se_ <- sd(1 - PREDS/SS, na.rm = T) / sqrt(K)
  upper <- mean_ + se_ * 1.96
  lower <- mean_ - se_ * 1.96
  data.frame(mean = mean_, lower = lower, upper = upper)
}


temp <- quantile(ab_t_data[,var_name], 0.9, na.rm = T) %>% as.numeric
dat <- dat %>%
  filter(WSG > temp) %>%
  filter(!is.na(WSG))
glmfit <- glm.nb((census_2010 + 1) ~ WSG +
                 offset(log(census_1982 + 1)),
               data = ab_t_data)

cv_glm_s(dat, glmfit, K = 10)


var_name <- "WSG"
temp <- quantile(ab_t_data2[,var_name], 0.8, na.rm = T) %>% as.numeric

temp_dat <- ab_t_data2[ab_t_data2[,var_name] > temp,]

form <- paste("(census_2010 + 1) ~ ", var_name,
             " + offset(log(census_1982 + 1))", sep = "") %>% noquote

res_temp <- glm.nb(form,
            data = temp_dat)

summary(res_temp)






## reviewers's suggetion
# this does not make sence
rev_dat <- ab_t_data2 %>% mutate(temp = (census_2010 + 1)/(census_1982 + 1))

rev_dat %>% filter(temp < quantile(temp, 0.25)) %>%
  glm.nb(census_2010 ~ WSG
            + moist
            + concav
            + slope
            + WSG:slope
            + moist:concav
            + offset(log(census_1982)),
            data = .) %>%
  summary


# does this make sence?
rev_dat %>% filter(temp < quantile(temp, 0.9)) %>%
  plot(temp ~ moist, ., log = "y")





ab_t_data3 <- ab_t_data %>% gather(trait, val, 9:12)

moge <- ab_t_data3 %>% group_by(trait) %>%
  summarize(l1 = quantile(val, 0.1, na.rm = TRUE),
            l25 = quantile(val, 0.2, na.rm = TRUE),
            h1 = quantile(val, 0.9, na.rm = TRUE),
            h25 = quantile(val, 0.8, na.rm = TRUE)
) %>% as.data.frame


# data
t1l1 <- ab_t_data3 %>% filter(trait == "concav") %>%
  filter(val <= moge[1, "l1"])

t2l1 <- ab_t_data3 %>% filter(trait == "moist") %>%
  filter(val <= moge[2, "l1"])

t3l1 <- ab_t_data3 %>% filter(trait == "slope") %>%
  filter(val <= moge[3, "l1"])

t4l1 <- ab_t_data3 %>% filter(trait == "WSG") %>%
  filter(val <= moge[4, "l1"])

l1 <- bind_rows(t1l1,t2l1,t3l1,t4l1) %>%
  mutate(q ="l1")

t1l25 <- ab_t_data3 %>% filter(trait == "concav") %>%
  filter(val <= moge[1, "l25"])

t2l25 <- ab_t_data3 %>% filter(trait == "moist") %>%
  filter(val <= moge[2, "l25"])

t3l25 <- ab_t_data3 %>% filter(trait == "slope") %>%
  filter(val <= moge[3, "l25"])

t4l25 <- ab_t_data3 %>% filter(trait == "WSG") %>%
  filter(val <= moge[4, "l25"])

l25 <- bind_rows(t1l25,t2l25,t3l25,t4l25) %>%
  mutate(q ="l25")


t1h1 <- ab_t_data3 %>% filter(trait == "concav") %>%
  filter(val >= moge[1, "h1"])

t2h1 <- ab_t_data3 %>% filter(trait == "moist") %>%
  filter(val >= moge[2, "h1"])

t3h1 <- ab_t_data3 %>% filter(trait == "slope") %>%
  filter(val >= moge[3, "h1"])

t4h1 <- ab_t_data3 %>% filter(trait == "WSG") %>%
  filter(val >= moge[4, "h1"])

h1 <- bind_rows(t1h1,t2h1,t3h1,t4h1) %>%
  mutate(q ="h1")

t1h25 <- ab_t_data3 %>% filter(trait == "concav") %>%
  filter(val >= moge[1, "h25"])

t2h25 <- ab_t_data3 %>% filter(trait == "moist") %>%
  filter(val >= moge[2, "h25"])

t3h25 <- ab_t_data3 %>% filter(trait == "slope") %>%
  filter(val >= moge[3, "h25"])

t4h25 <- ab_t_data3 %>% filter(trait == "WSG") %>%
  filter(val >= moge[4, "h25"])

h25 <- bind_rows(t1h25,t2h25,t3h25,t4h25) %>%
  mutate(q ="h25")

ab_t_data4 <- bind_rows(l1,l25,h1,h25)

ggplot(ab_t_data4,
       aes(x = val, y = (census_2010 + 1)/(census_1982 + 1))) +
  geom_point() +
  facet_grid(q ~ trait, scales = "free_x") +
  scale_y_log10()



pdf("~/Dropbox/MS/TurnoverBCI/TurnoverBCI_MS/ver2/fig_X.pdf", width = 7, height = 7, paper = "special")

ab_t_data4 %>%
  mutate(trait = factor(trait, levels = c("WSG", "moist", "concav", "slope"))) %>%
  mutate(trait2 = factor(trait,
    labels = c("Wood~density ~(g~cm^{-3})",
               "Moisture",
               "Concavity~(m)",
               "Slope~(degrees)"))) %>%
  mutate(q = factor(q, levels = c("h1", "h25", "l25", "l1"))) %>%
  mutate(q2 = factor(q, labels = c("Upper 10% quantile","Upper 20% quantile","Lower 20% qunatile","Lower 10% qunatile"))) %>%
ggplot(.,
       aes(x = val, y = (census_2010 + 1)/(census_1982 + 1))) +
  geom_point() +
 # geom_smooth() +
  geom_hline(yintercept = 1, lty = 2) +
  facet_wrap(q2 ~ trait2, scales = "free_x",
             labeller = labeller(trait2 = label_parsed, size = label_value)) +
  scale_y_log10() +
  ylab("(No. of individual in 2010 + 1) / \n (No. of individual in 1982 + 1)") +
  xlab("Trait values") +
  theme_bw() +
  theme(
        strip.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        axis.text.x = element_text(size = 6, angle = 45),
        axis.text.y = element_text(size = 6),
        legend.text = element_text(size = 7))

dev.off()









