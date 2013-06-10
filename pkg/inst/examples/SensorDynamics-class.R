### Sensor dynamics: default initialization
sdyn <- SensorDynamics()

sdyn # equivalent to: show(sdyn)

print(sdyn)

#plot(sdyn)

### SensorDynamics as a part of SensorModel
sm <- SensorModel(tunit = 60)
sdyn <- as(sm, "SensorDynamics")

### Demo 1: response to pulses of different concentrations
set.seed(5)
sa <- SensorArray(tunit = 60, csd = 1, ssd = 0, dsd = 0)

# mixture at low concentrations
p1 <- plotSignal(sa, set = c("A 0.01", "C 0.1", "A 0.01, C 0.1"), main = "Low concentrations")
p1

# mixture at high concentrations
p2 <- plotSignal(sa, set = c("A 0.05", "C 1", "A 0.05, C 1"), main = "High concentrations")
p2

###
sa <- SensorArray(tunit = 60, nsd = 0)

p3 <- plotSignal(sa, set = c("A", "B", "C"))
p3

p4 <- plotPCA(sa, set = c("A", "B", "C"))
p4
