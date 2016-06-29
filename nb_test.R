set.seed(15)
n_sp <- 300
abund1 <- rlnorm(n_sp, log(1000))
trait <- rnorm(n_sp)
mu <- abund1 * exp(0.2 * trait)
abund2 <- rlnorm(n_sp, log(mu), 2)

summary(abund1)
summary(abund2)

plot(trait, abund1/abund2, log = "y")
