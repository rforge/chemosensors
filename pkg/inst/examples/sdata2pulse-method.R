# sdata2pulse
tunit <- 60

sa <- SensorArray(num = 1, tunit = tunit, csd = 1, ssd = 0, dsd = 0)
sm <- as(sa, "SensorModel")
cn <- as(sa, "ConcNoiseModel")

sc <- Scenario(T = c("A", "B", "C", "B, C"), tunit = tunit)
conc <- getConc(sc)
conc <- predict(cn, conc)

sdata <- predict(sm@dataModel[[1]], C = conc, out = "gas") 

colnames(sdata) <- sa@gnames
p1 <- ggplot(melt(sdata), aes(X1, value, group = X2, color = X2)) + geom_line()
p1

sdata.pulse <- sdata2pulse(sm, conc, sdata)

sdata.out <- predict(as(sm, "SensorDynamics"), conc = conc, sdata = sdata.pulse)
sdata.out <- sdata.out[, , 1]

sdata.delta <- sdata.out - sdata.pulse

mf1 <- melt(sdata, varnames = c("sample", "gas"))
mf2 <- melt(sdata.pulse, varnames = c("sample", "gas"))
mf3 <- melt(sdata.out, varnames = c("sample", "gas"))
mf4 <- melt(sdata.delta, varnames = c("sample", "gas"))
mf5 <- melt(sdata + sdata.delta, varnames = c("sample", "gas"))

mf <- rbind(data.frame(mf1, sdata = "sdata"), 
 data.frame(mf2, sdata = "sdata.pulse"),
 data.frame(mf3, sdata = "sdata.out"),
 data.frame(mf4, sdata = "sdata.delta"),
 data.frame(mf5, sdata = "sdata + sdata.delta"))
    
p2 <- ggplot(mf, aes(x = sample, y = value)) + geom_line(aes(color = as.factor(gas))) +
  facet_grid(sdata ~ .)

# class SensorDynamics
# sensor dynamics: default initialization
sdyn <- SensorDynamics()

show(sdyn)
print(sdyn)

#plot(sdyn)

# SensorDynamics as a part of SensorModel
sm <- SensorModel(tunit=60)
sdyn <- as(sm, "SensorDynamics")

# sensor data in response to 3 pulses
tunit <- 60

s <- Sensor(tunit = tunit, csd = 1, ssd = 0, dsd = 0)
sm <- as(s, "SensorModel")
cn <- as(s, "ConcNoiseModel")

sc <- Scenario(T = c("A", "C", "A, C"), tunit = tunit)
conc <- getConc(sc)

conc.noised <- predict(cn, conc)

mf1 <- melt(conc, varnames = c("sample", "gas"))
mf2 <- melt(conc.noised, varnames = c("sample", "gas"))
mf <- rbind(data.frame(mf1, conc = "conc"), 
 data.frame(mf2, conc = "conc.noise"))
p1 <- ggplot(mf, aes(x = sample, y = value)) + geom_line(aes(color = as.factor(gas))) +
  facet_grid(conc ~ .)

concUnits(s)
conc.noised.perc100 <- 100 * conc.noised
sdata <- predict(s@dataModel[[1]], C = conc.noised.perc100, out = "gas")
 
mf <- melt(sdata, varnames = c("sample", "gas"))
p2 <- ggplot(mf, aes(x = sample, y = value)) + geom_line(aes(color = as.factor(gas)))
p2

sdata.pulse <- sdata2pulse(s, conc.noised, sdata)

sdata.out <- predict(as(sm, "SensorDynamics"), conc = conc.noised, sdata = sdata.pulse)
sdata.out <- sdata.out[, , 1]

sdata.delta <- sdata.out - sdata.pulse

mf1 <- melt(sdata, varnames = c("sample", "gas"))
mf2 <- melt(sdata.pulse, varnames = c("sample", "gas"))
mf3 <- melt(sdata.out, varnames = c("sample", "gas"))
mf4 <- melt(sdata.delta, varnames = c("sample", "gas"))
mf5 <- melt(sdata + sdata.delta, varnames = c("sample", "gas"))

mf <- rbind(data.frame(mf1, sdata = "sdata"), 
 data.frame(mf2, sdata = "sdata.pulse"),
 data.frame(mf3, sdata = "sdata.out"),
 data.frame(mf4, sdata = "sdata.delta"),
 data.frame(mf5, sdata = "sdata + sdata.delta"))
    
p3 <- ggplot(mf, aes(x = sample, y = value)) + geom_line(aes(color = as.factor(gas))) +
  facet_grid(sdata ~ .)
p3

