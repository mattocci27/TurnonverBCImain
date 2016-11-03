library(dplyr)
# p-sim
argv <- commandArgs(trailingOnly = TRUE)
n_sim <- as.numeric(argv[1])
n_size <- as.numeric(argv[2])

n_sim <- 100
p_res1 <- numeric(n_sim)
p_res2 <- numeric(n_sim)
p_res3 <- numeric(n_sim)
p_res4 <- numeric(n_sim)
p_res5 <- numeric(n_sim)

p_res1s <- numeric(n_sim)
p_res2s <- numeric(n_sim)
p_res3s <- numeric(n_sim)
p_res4s <- numeric(n_sim)
p_res5s <- numeric(n_sim)

before <- proc.time()
for (i in 1:n_sim){
  n_size <- 100

  x1 <- rnorm(n_size)
  x2 <- rnorm(n_size)
  x3 <- rnorm(n_size)
  x4 <- rnorm(n_size)
  x5 <- rnorm(n_size)
  yy <- rnorm(n_size)

  res5 <- lm(yy ~ x1 * x2 * x3 * x4 * x5) %>%
    summary %>%
    .$coefficients

  res5s <- lm(yy ~ x1 + x2 + x3 + x4 + x5) %>%
    summary %>%
    .$coefficients

  res4 <- lm(yy ~ x1 * x2 * x3 * x4) %>%
    summary %>%
    .$coefficients

  res4s <- lm(yy ~ x1 + x2 + x3 + x4) %>%
    summary %>%
    .$coefficients


  res3 <- lm(yy ~ x1 * x2 * x3) %>%
    summary %>%
    .$coefficients

  res3s <- lm(yy ~ x1 + x2 + x3) %>%
    summary %>%
    .$coefficients

  res2 <- lm(yy ~ x1 * x2) %>%
    summary %>%
    .$coefficients

  res2s <- lm(yy ~ x1 + x2) %>%
    summary %>%
    .$coefficients

  res1 <- lm(yy ~ x1) %>%
    summary %>%
    .$coefficients

  res1s <- lm(yy ~ x1) %>%
    summary %>%
    .$coefficients

  if(min(res1[, "Pr(>|t|)"]) < 0.05) p_res1[i] <- 1
  if(min(res1s[, "Pr(>|t|)"]) < 0.05) p_res1s[i] <- 1
  if(min(res2[, "Pr(>|t|)"]) < 0.05) p_res2[i] <- 1
  if(min(res2s[, "Pr(>|t|)"]) < 0.05) p_res2s[i] <- 1
  if(min(res3[, "Pr(>|t|)"]) < 0.05) p_res3[i] <- 1
  if(min(res3s[, "Pr(>|t|)"]) < 0.05) p_res3s[i] <- 1
  if(min(res4[, "Pr(>|t|)"]) < 0.05) p_res4[i] <- 1
  if(min(res4s[, "Pr(>|t|)"]) < 0.05) p_res4s[i] <- 1
  if(min(res5[, "Pr(>|t|)"]) < 0.05) p_res5[i] <- 1
  if(min(res5s[, "Pr(>|t|)"]) < 0.05) p_res5s[i] <- 1
}
after <- proc.time()
after - before

sum(p_res1) / n_sim
sum(p_res2) / n_sim
sum(p_res3) / n_sim
sum(p_res4) / n_sim
sum(p_res5) / n_sim

sum(p_res1s) / n_sim
sum(p_res2s) / n_sim
sum(p_res3s) / n_sim
sum(p_res4s) / n_sim
sum(p_res5s) / n_sim




for (i in 1:n_sim){
  n_size <- 1000

  x1 <- rnorm(n_size)
  x2 <- rnorm(n_size)
  yy <- rnorm(n_size)
  res <- lm(yy ~ x1 * x2) %>%
    summary %>%
    .$coefficients

  res2 <- lm(yy ~ x1 + x2) %>%
    summary %>%
    .$coefficients

  if(min(res[, "Pr(>|t|)"]) < 0.05) p_res[i] <- 1
  if(min(res2[, "Pr(>|t|)"]) < 0.05) p_res2[i] <- 1

}

p_res
p_res2

xx <- rnorm(100)
yy <- rnorm(100, xx, 1)

lm(yy ~ xx) %>% summary

nn <- seq(2, 100, length = 100)

tt <- sqrt(nn) * 0.2

p_mat <- cbind(pnorm(tt), pnorm(tt, lower.tail = F))
p_vec <- apply(p_mat, 1, min)

plot(p_vec ~ nn, type = "l")
abline(h = 0.05, lty = 2)

library(lattice)
library(dplyr)
n_size <- 30
nn <- seq(3, 100, length = n_size)
rho <- seq(0, 0.5, length = n_size)
# tt <- rho / (1 - rho^2) * sqrt(nn - 2)
# p_mat <- cbind(pnorm(tt), pnorm(tt, lower.tail = F))
# p_vec <- apply(p_mat, 1, min)

g <- expand.grid(rho = rho, nn = nn)
tt <- g$rho / (1 - g$rho^2) * sqrt(g$nn - 2)
p_mat <- cbind(pnorm(tt), pnorm(tt, lower.tail = F))
p_vec <- apply(p_mat, 1, min)

moge <- data.frame(nn = g$nn, rho = g$rho, p_vec = p_vec)

moge2 <- moge %>%
  mutate(p_vec = 0.05) %>%
  bind_rows(., moge) %>%
  mutate(gr = rep(1:2, each = n_size^2)) %>%
  mutate(gr = as.factor(gr))


mycolors_trans <- rgb(c(255,0,0),
          c(0,0,255),
          c(0,255,0), alpha = 150, maxColorValue = 255)


wireframe(p_vec ~ nn * rho, moge2, groups = gr,
  screen = list(z = 260, x = -60),
  # drape = TRUE,
  col.groups = mycolors_trans,
  scales = list(arrows = FALSE),
    )

#
# moge <- list(n_sample = nn,
#       rho = rho,
#       p_val = matrix(p_vec, nrow = 100))
#
# p <- plot_ly(x = moge$n_sample, y = moge$rho, z = moge$p_val, type = "surface") %>%
#   add_surface(z = matrix(rep(0.05, 100^2), nrow = 100),  opacity = 1)
#
# Sys.setenv("plotly_username" = "mattocci27")
# Sys.setenv("plotly_api_key" = "4159kcaaqv")
# plotly_IMAGE(p, format = "eps", out_file = "~/Desktop/moge.eps")
#
