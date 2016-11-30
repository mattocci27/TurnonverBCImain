

xx <- 1:7

mu <- 0.004 * xx


aa <- sapply(1:1250, function(x) rnorm(1:7, mu, sd = 0.4))

aa <- t(aa)
aa <- aa - aa[,1]

dat <- data_frame(y = as.numeric(aa), x = rep(1:7, each = 1250))

dat %>% group_by(x) %>% summarise(mean = mean(y))

plot(y ~ x, dat)
lm(y ~ scale(x) , dat) %>% summary


##
y2 <- apply(aa, 2, sum)
y2


# moge20
moge20 %>% group_by(Time) %>%
  summarise(WSG = mean(WSG)) %>%
  plot(WSG ~ Time,.)


moge100 %>% group_by(Time) %>%
  summarise(WSG = mean(WSG)) %>%
  plot(WSG ~ Time,.)


sapply(WSG100, mean)
WSG100

  WSG100.rm[[i]] <- com.mean.ab(D100m[[i]][,!(colnames(D100m[[1]]),trait,"WSG")



samp <- d1
traitname <- "WSG"
com.mean.ab <- function(samp,trait,traitname)
{

  N<-dim(samp)[1]
  #
  N <- length(samp)
  X<-paste(traitname)
  comMean<-numeric(N)
  for (i in 1:N){
  sppInSample <- names(samp[i,samp[i,] > 0] )
  sppInSample2 <- names(samp[i,samp[i,] < 1])
  sp.abund <- samp[i,samp[i,] > 0]

  if (length(sppInSample) > 0) {
  sppInTrait<-trait[sppInSample,]
  temp <- cbind(sppInTrait,sp.abund)
  temp2 <- temp[is.na(temp[,X])==F,]
  comMean[i] <- sum(temp2[,X]*temp2[,"sp.abund"])/sum(temp2[,"sp.abund"])

  }
  else {
  comMean[i] <- NA
  }
  }
  comMean
}

com.mean.ab(d1, trait, "WSG")

comWSG <- NULL
for (i in 1:7){
  d1 <- apply(D100m[[i]], 2, sum)
  moge <- data_frame(ab = d1, SP = names(d1)) %>%
    full_join(., trait, by = "SP") %>%
    mutate(re_ab = ab / sum(ab, na.rm = T))
  com_trait <- moge$re_ab * moge$sp.convex.mean
  comWSG[i]<- sum(com_trait, na.rm = T)
}

plot(1:7, comWSG - comWSG[1])

res <- list()
for (i in 1:7){
  d1 <- apply(D100m[[i]], 2, sum)

sp_ab <- d1[d1 > 0]
mat <- NULL
n_quad <- 1250
n_sp <- length(sp_ab)

temp0 <- data_frame(quad =  paste("quad", 1:n_quad, sep = "_"), temp = 0)
for (j in 1:n_sp){
  temp <- data_frame(quad = paste("quad", sample(1:n_quad, sp_ab[j], replace = T), sep = "_") ) %>% count(quad)

  temp2 <- full_join(temp, temp0, by = "quad")
  temp2[is.na(temp2) == T] <- 0
  temp3 <- temp2 %>% arrange(quad)
  mat <- cbind(mat, temp3$n)

}

rownames(mat) <- paste("quad", 1:n_quad, sep = "_")
colnames(mat) <- names(sp_ab)

res[[i]] <- com.mean.ab(mat, trait, "WSG")

}


data_frame(numeric())
sapply(res, mean)

fig_dat <- data_frame(trait = unlist(res) - unlist(res[[1]]), time = rep(1:7, each =1250))

fig_dat %>% plot(trait ~ time,.)

gam_res <- gam(trait ~  s(time,k=4), data=fig_dat,
  correlation = corAR1(form = ~time))

plot(gam_res)
###
## if there is no species aggreagaoin, we still can see the com pattern


res <- list()
for (i in 1:7){
  d1 <- apply(D100m[[i]], 2, sum)

sp_ab <- d1[d1 > 0]
sp_ab <- sp_ab[names(sp_ab) !="PIPECO" & names(sp_ab) !="POULAR" & names(sp_ab) !="SWARS1" & names(sp_ab) !="XYL1MA" & names(sp_ab) !="MOURMY" & names(sp_ab) !="OCOTWH"]


mat <- NULL
n_quad <- 1250
n_sp <- length(sp_ab)

temp0 <- data_frame(quad =  paste("quad", 1:n_quad, sep = "_"), temp = 0)
for (j in 1:n_sp){
  temp <- data_frame(quad = paste("quad", sample(1:n_quad, sp_ab[j], replace = T), sep = "_") ) %>% count(quad)

  temp2 <- full_join(temp, temp0, by = "quad")
  temp2[is.na(temp2) == T] <- 0
  temp3 <- temp2 %>% arrange(quad)
  mat <- cbind(mat, temp3$n)

}

rownames(mat) <- paste("quad", 1:n_quad, sep = "_")
colnames(mat) <- names(sp_ab)
#
res[[i]] <- com.mean.ab(mat, trait, "WSG")

}


data_frame(numeric())
plot(1:7, sapply(res, mean))

fig_dat <- data_frame(trait = unlist(res) - unlist(res[[1]]), time = rep(1:7, each =1250))

fig_dat %>% plot(trait ~ time,.)

gam_res <- gam(trait ~  s(time,k=4), data=fig_dat,
  correlation = corAR1(form = ~time))

plot(gam_res)
