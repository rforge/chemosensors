
### parameters
num <- 1:17
sind <- 1
gases <- 1

csd <- 0.1
ssd <- 0.1
dsd <- 0.1

n <- 100
n0 <- 10

enableSorption <- TRUE

### 3 arrays
sa1 <- SensorArray(num=num, enableSorption=enableSorption, csd=csd, ssd=0, dsd=0)
sa2 <- SensorArray(num=num, enableSorption=enableSorption, csd=0, ssd=ssd, dsd=0)
sa3 <- SensorArray(num=num, enableSorption=enableSorption, csd=0, ssd=0, dsd=dsd)

### concentration matrix
conc <- concSample(sa1, "const", gases=gases, n=n)

conc0 <- mean(conc[, gases])
cdat <- c(rep(0, n0), rep(conc0, n), rep(0, n0))

### generate data
sdata1 <- predict(sa1, conc)
sdata2 <- predict(sa2, conc)
sdata3 <- predict(sa3, conc)

### plot data
dat <- cbind(sdata1[, sind], sdata2[, sind], sdata3[, sind])
ylim <- range(as.numeric(dat))
xlim <- c(-n0, n+n0)

main <- paste("Sensor", sind, "Signal:", c("csd", "ssd", "dsd"), c(csd, ssd, dsd))

opar <- par(mfrow=c(4, 1), bty="n", mar=c(3, 3, 1, 1), oma=c(2, 2, 2, 2))

plot(cdat, t='l', ylim=c(-0.2 * conc0, 1.2 * conc0), axes=FALSE, 
  main=paste("Gas", LETTERS[gases], "Pulse"))
axis(2, at=c(0, conc0), labels=c(0, conc0), las=2, tick=TRUE)

for(i in 1:ncol(dat)) {
  plot(dat[, i], t='l', ylim=ylim, xlim=xlim, axes=FALSE, 
    main=main[i], ylab="", xlab="")
  axis(2, at=ylim, labels=round(ylim, 2), las=2, tick=TRUE)
}

xt <- seq(0, n, by=100)
axis(1, at=xt, labels=xt, tick=TRUE)

par(opar)
