
# create a sensor
s <- Sensor(num=1)

# compute affinities
Ka <- affinity(s)
Kd <- 1 / Ka

print(Ka)
print(Kd)

# plot sensor response at conc. range of type 'inc' (see 'concSample' for more details)
#  - add lines related to computation of affinities
plot(s, Ka=Ka, type="inc", affinity=TRUE)
plot(s, Ka=Ka, type="inc", affinity=TRUE, xlim=c(0, 0.1))

#sa <- SensorArray(num=1:17, gases=c(1, 3))
#plot(sa, 'affinityMap')
