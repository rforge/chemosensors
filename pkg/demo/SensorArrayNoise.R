
sa <- SensorArray(model="plsr", enableSorption=FALSE, num=1:17)

data(UNIMANshort)
conc <- UNIMANshort$C
X <- UNIMANshort$dat

# -0- short UNIMAN dataset (200 samples)
mod <- prcomp(X, center=TRUE, scale=FALSE)
#col <- ccol(sa, conc=C, concUnits="perc")
#scoreplot(mod, pch=20, col=col)

# -1- default csd (0.1), default ssd (0.1), dsd = 0
plot(sa, "prediction", conc=conc, mod=mod)

# -2- csd = 0.5, ssd =0.5, dsd = 0.5
csd(sa) <- 0.5
ssd(sa) <- 0.5
dsd(sa) <- 0.5
plot(sa, "prediction", conc=conc, mod=mod)
