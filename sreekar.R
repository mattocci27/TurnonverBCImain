
rlnorm(100, 1, 1)

library(dplyr)
mu <- 100
sigma <- 10
mean_ <- log(mu) - sigma^2 / 2

n_quad <- 9
n_sp <- 50 # gamma

mat <- matrix(numeric(n_sp * n_quad), nrow=n_quad)
rownames(mat) <- paste("quad", 1:n_quad, sep = "_")
colnames(mat) <- paste("sp", 1:n_sp, sep = "_")

library(dplyr)

n_quad <- 9 #
n_sp <- 50 # gamma

mu <- 15 # median (mean?) of species abundance
sigma <- 1 # sd

sp_ab <- rlnorm(n_sp, log(mu), sigma) %>% as.integer

mat <- NULL

temp0 <- data_frame(quad =  paste("quad", 1:n_quad, sep = "_"), temp = 0)
for (i in 1:n_sp){
  temp <- data_frame(quad = paste("quad", sample(1:n_quad, sp_ab[i], replace = T), sep = "_") ) %>% count(quad)

  temp2 <- full_join(temp, temp0, by = "quad")
  temp2[is.na(temp2) == T] <- 0
  temp3 <- temp2 %>% arrange(quad)
  mat <- cbind(mat, temp3$n)
}

rownames(mat) <- paste("quad", 1:n_quad, sep = "_")
colnames(mat) <- paste("sp", 1:n_sp, sep = "_")
