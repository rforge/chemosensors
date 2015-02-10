### inc
library(devtools)
load_all()

library(gridExtra)

### sim
set.seed(1)
set <- c("A 0.02", "C 0.5")
sc <- Scenario(T = set, nT = 3, V = set, nV = 3, randomize = TRUE)

#conc <- getConc(sc)
#cf <- sdata.frame(sc)

cf <- sdata.frame(sc)
conc <- getConc(sc, cf = cf)

sa <- SensorArray(num = 1:4)
sdata <- predict(sa, conc = conc)

df1 <- sdata.frame(sa, cf = cf, sdata = sdata, feature = "step")
df2 <- sdata.frame(sa, conc = conc, sdata = sdata, feature = "step")

p1 <- plotPCA(sa, conc = df1[, gnames(sa)], sdata = df1[, snames(sa)])

p2 <- plotPCA(sa, conc = df2[, gnames(sa)], sdata = df2[, snames(sa)])

grid.arrange(p1, p2)
