module load ufrc
srundev --time=60 --mem-per-cpu=2gb --cpus-per-task=4
module load gcc/5.2.0 R/3.2.2

library(vegan)
x <-  matrix(runif(640*300), nrow=640)
system.time(com.nmds.all2 <- metaMDS(x, engine = "monoMDS",
  k = 3, trymax = 50))

system.time(com.nmds.all2 <- metaMDS(x, engine = "monoMDS",
  k = 3, trymax = 50, parallel = 4))

  system.time(com.nmds.all2 <- metaMDS(x, engine = "monoMDS",
    k = 3, trymax = 50, parallel = 8))
