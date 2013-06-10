
### 1) a concentration matrix of three gases (tunit 4)
sa <- SensorArray(tunit = 4)

set <- c("A 0.1", "B 0.1", "C 1")
sc <- Scenario(set, tunit = 4)
conc <- getConc(sc)

head(conc)

sdata <- predict(sa, conc)

sf <- sdata2df(sa, sdata)

head(sf)
