# sensor dynamics: default initialization
sdyn <- SensorDynamics()

show(sdyn)
print(sdyn)

plot(sdyn)

# SensorDynamics as a part of SensorModel
sm <- SensorModel(tunit=60)
sdyn <- as(sm, "SensorDynamics")
