library(mgcv)
x <- 1:6

before <- proc.time()
p <- NULL
r2 <- NULL

for (j in 1:1000){
  y <- NULL
  y[1] <- 0
  for (i in 2:6){
    y[i] <- rnorm(1,y[i-1], 1)
  }

  #plot(y ~ x)
  dat <- data.frame(x=x, y=y)
  res <- gam(y ~ s(x, k=4), data = dat, correlation = corCAR1())

  p[j] <- res %>% summary %>% .$s.pv
  r2[j] <- res %>% summary %>% .$r.sq
}

after <- proc.time()
after - before

plot(y ~ x, dat)
