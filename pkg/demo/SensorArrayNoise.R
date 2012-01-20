
sa <- SensorArray(model="plsr", enableSorption=FALSE, num=1:17)

data(UNIMANshort, package="chemosensors") # 'C', 'dat'
conc <- C
X <- dat

# -0- short UNIMAN dataset (200 samples)
mod <- prcomp(X, center=TRUE, scale=FALSE)
col <- ccol(sa, conc=C, concUnits="perc")
scoreplot(mod, pch=20, col=col)

# -1- default csd/ssd 
plot(sa, "prediction", conc=conc, mod=mod)

# -2- csd = 0.5, ssd =0.5
csd(sa) <- 0.5
ssd(sa) <- 0.5
plot(sa, "prediction", conc=conc, mod=mod)
