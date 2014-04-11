### parameters
tunit <- 10
nsensors <- 20
dsd <- 0

### scenario
set.AC <- c('A 0.01', 'A 0.05', 'C 0.1', 'C 1', 'A 0.01, C 0.1', 'A 0.05, C 1')

sc <- Scenario(set.AC, tunit = tunit)

conc <- getConc(sc)

### sensor array
sa <- SensorArray(nsensors = nsensors, tunit = tunit, dsd = dsd)

### generate data
sdata <- predict(sa, conc, cores = 2)

### extract features
df <- sdata.frame(sa, conc = conc, sdata = sdata, feature = "step")


