
# load UNIMAN data
data("UNIMANshort", package="chemosensors") #-> 'C', 'dat'

# filter for only maximum conc. levels
C.max <- apply(C, 2, max)
ind <- which(apply(C, 1, function(x, C.max) sum(x == C.max) > 0, C.max))

C <- C[ind, ]
dat <- dat[ind, ]

print(head(C))

# create 100 sensors (replicas from 17 UNIMAN sensors)
sa <- SensorArray(nsensors=100) 
nsd(sa) <- 0 # noise-free mode

sdata <- predict(sa, conc=C)

# plot 
opar <- par(mfrow=c(2, 2))

gases <- c(1, 3)

plotAffinitySpace(y='points', conc=C, sdata=dat, gases=gases, main="17 UNIMAN sensors \n Affinity Space A and C")
plotAffinitySpace(sa, 'points', conc=C, sdata=sdata, gases=gases, main="100 virtual sensors \n Affinity Space A and C")

plotAffinitySpace(y='density', conc=C, sdata=dat, gases=gases, main="2D Density")
plotAffinitySpace(sa, 'density', conc=C, sdata=sdata, gases=gases, main="2D Density")

gases <- c(2, 1)
plotAffinitySpace(y='points', conc=C, sdata=dat, gases=gases, main="Affinity Space A and B")
plotAffinitySpace(sa, 'points', conc=C, sdata=sdata, gases=gases, main="Affinity Space A and B")

gases <- c(2, 3)
plotAffinitySpace(y='points', conc=C, sdata=dat, gases=gases, main="Affinity Space B and C")
plotAffinitySpace(sa, 'points', conc=C, sdata=sdata, gases=gases, main="Affinity Space B and C")

par(opar)

