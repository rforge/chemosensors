
### 1) a concentration matrix of three gases (tunit 1)
sa <- SensorArray() 

conc.values <- concMax(sa)
conc <- diag(conc.values)

conc 

cf <- conc2df(sa, conc)
cf

### 2) a concentration matrix of three gases (tunit = 4)
sa <- SensorArray(tunit = 4)

set <- c("A 0.1", "B 0.1", "C 1")
sc <- Scenario(set, tunit = 4)
conc <- getConc(sc)

head(conc)

cf <- conc2df(sa, conc)
head(cf)

