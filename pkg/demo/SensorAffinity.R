
# concentration matrix of 3 gas classes: A, C and AC
conc <- matrix(0, 300, 3)
conc[1:100, 1] <- 0.05 #  A
conc[101:200, 3] <- 1 # C
conc[201:300, 1] <- 0.05 # AC
conc[201:300, 3] <- 1 # AC

conc <- conc[sample(1:nrow(conc)  ), ]

# two sensor arrays composed of different sensors
sa1 <- SensorArray(num=1:5) # sensors with affinity A < C
sa2 <- SensorArray(num=c(13, 14, 17)) # sensors with affinity A > C

# PCA scoreplots of sensors array data in response to the concentration matrix
plot(sa1, "prediction", conc=conc)
plot(sa2, "prediction", conc=conc)
