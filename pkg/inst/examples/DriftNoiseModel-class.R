# model: default initialization
dn <- DriftNoiseModel()

# get information about the model
show(dn)
print(dn)

plot(dn)  

# model: custom parameters
# - many sensors
dn <- DriftNoiseModel(dsd=0.5, ndcomp=3, num=1:17)

print(dn)

plot(dn)

# method plot
#  - plot types 'y': barplot, noise, walk
dn <- DriftNoiseModel() # default model

plot(dn, "noise", main="plot(dn, 'noise')") 
# default plot type, i.e. 'plot(dn)' does the same plotting

data(UNIMANshort)
X <- UNIMANshort$dat[, num(dn)]
plot(dn, "pc", X, main="plot(dn, 'pc', X)")

### example with a SensorArray
set.seed(1)
sa <- SensorArray(num = 1:5)

set <- c("A 0.01", "A 0.05", "C 0.1", "C 1")
sc <- Scenario(rep(set, 10))
conc <- getConc(sc)

sdata <- predict(sa, conc)

p1 <- plotPCA(sa, conc = conc, sdata = sdata, air = FALSE, 
  main = "feature: transient")
p1

p2 <- plotPCA(sa, conc = conc, sdata = sdata, feature = "ss", 
  main = "feature: steady-state")
p2

p3 <- plotPCA(sa, conc = conc, sdata = sdata, feature = "step", 
  main = "feature: step")
p3
