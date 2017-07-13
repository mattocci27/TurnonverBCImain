set.seed(5)
N1 <- as.integer(rlnorm(300, mean = log(300)))
summary(N1)
hist(N1)


r1 <- rlnorm(300, mean = log(1) - (1^2) / 2, 0)
summary(r1)

r2 <- rlnorm(300, mean = log(2) - (1^2) / 2, 0)
summary(r2)


Nt1 <- rlnorm(300, log(r1 * N1), 0.1)
Nt2 <- rlnorm(300, log(r2 * N1), 0.1)


# demographic 
beta3 <- 0.1
beta <- rbeta(300, 0.2, 1)

hist(beta)
beta2 <- rnorm(300, 1, 0.2)
beta4 <- rnorm(300, 1, 0.2)

Nt3 <- NULL
for (i in 1:300) {
Nt3[i] <- rbinom(1, N1[i], (1 - beta[i]))+ rpois(1, N1[i] * beta[i] * beta2[i])
}

for (i in 1:300) {
Nt3[i] <- rbinom(1, N1[i], 1 - beta3) + rpois(1, N1[i] * beta3)
}

dif1 <- (Nt1 - N1)^2
dif2 <- (Nt2 - N1)^2
dif3 <- (Nt3 - N1)^2


dif1 <- (Nt1 / N1)
dif2 <- (Nt2 / N1)
dif3 <- (Nt3 / N1)

data_frame(dif1, dif2, dif3,  N1) %>%
  tidyr::gather(dif, val, 1:3) %>%
  mutate(dif = as.factor(dif)) %>%
  ggplot(., aes(x = N1, y = val, col = dif, fill = dif)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()



N1 <- apply(D1_m, 2, sum)
N2 <- apply(D2_m, 2, sum)

R2 <- apply(rec2_m, 2, sum)


moge <- data_frame(N1 = N1, N2 = N2, R2 = R2) %>%
  mutate(rec_rate = R2 / N1) %>%
  mutate(mor_rate = (N1 + R2 - N2) / N1)


plot(rec_rate ~ mor_rate, moge, log = "xy")


trait %>%
  filter(SP %in% c("FARAOC", "HYBAPR", "PIPECO", "POULAR"))
