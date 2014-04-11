# For further details, please see example code and table fo parameters in 
# http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0088839

### include
library(chemosensors)

### parameters
tunit <- 0 # 60 is recomended value; disable temporal dynamics by setting `tunit = 0` (`tunit` default value is 0)
nsensors <- 4 # configure sensors types with two parameters, `nsensors` and `num` (`num` default value is 1:17)
num <- 1:4
dsd <- 0.005 # disable drift by setting `dsd = 0` 

### conc. matrix
conc <- matrix(0, nrow = 300 * 3, ncol = 3)
conc[61:360, 1] <- 0.01
conc[121:420, 2] <- 0.01
conc[181:420, 3] <- 0.01


### sensor array
sa <- SensorArray(nsensors = nsensors, num = num, tunit = tunit, dsd = dsd)

### generate data
sdata <- predict(sa, conc, cores = 1) # use `cores = 2` for parallel computing in 2 cores, library `multicore` will be loaded silently

### data exploratory graphics
plotSignal(sa, conc = conc, sdata = sdata) # plot transients

plotPCA(sa, conc = conc, sdata = sdata) # plot PCA scores of transients

### save data to CSV
write.table(conc, sep = ",", quote = FALSE, row.names = FALSE, file = "conc.csv")
write.table(sdata, sep = ",", quote = FALSE, row.names = FALSE, file = "sdata.csv")

# write two matrices in one file
dat <- cbind(conc, sdata) # see help of cbind by typing in R `?cbind`
write.table(dat, sep = ",", quote = FALSE, row.names = FALSE, file = "dat.csv")

