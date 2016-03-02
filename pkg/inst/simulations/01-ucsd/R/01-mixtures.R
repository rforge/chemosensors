# For further details, please see example code and table fo parameters in 
# http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0088839

### include
library(chemosensors)

### parameters
tunit <- 60 # 60 is recomended value 
nsensors <- 2 # configure sensors types with two parameters, `nsensors` and `num` (`num` default value is 1:17)
dsd <- 0.01 # disable drift by setting `dsd = 0` 

nrep <- 2 # number of repetitions per gas class

set.AC <- c('A 0.01', 'A 0.05', 'C 0.1', 'C 1', 'A 0.01, C 0.1', 'A 0.05, C 1') # set of gas classes

### scenario
set <- rep(set.AC, nrep) # repeat each class `nrep` times

sc <- Scenario(set, tunit = tunit, randomize = TRUE) # `tunit` must be the same as in `SensorArray`

conc <- getConc(sc)

### sensor array
sa <- SensorArray(nsensors = nsensors, tunit = tunit, dsd = dsd)

### generate data
sdata <- predict(sa, conc, cores = 1) # use `cores = 2` for parallel computing in 2 cores, library `multicore` will be loaded silently

### data exploratory graphics
plotSignal(sa, conc = conc, sdata = sdata) # plot transients

plotPCA(sa, conc = conc, sdata = sdata) # plot PCA scores of transients

plotPCA(sa, conc = conc, sdata = sdata, feature = "ss") # plot PCA scores of `ss` steady-state features

### save data to CSV
write.table(conc, sep = ",", quote = FALSE, row.names = FALSE, file = "conc.csv")
write.table(sdata, sep = ",", quote = FALSE, row.names = FALSE, file = "sdata.csv")

# write two matrices in one file
dat <- cbind(conc, sdata) # see help of cbind by typing in R `?cbind`
write.table(dat, sep = ",", quote = FALSE, row.names = FALSE, file = "dat.csv")

