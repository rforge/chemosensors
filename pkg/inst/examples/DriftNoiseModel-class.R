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

plot(dn, "noise", main="plot(dn, 'noise')") # default plot type, i.e. 'plot(dn)' does the same plotting

data(UNIMANshort, package="chemosensors")
X <- dat[, num(dn)]
plot(dn, "pc", X, main="plot(dn, 'pc', X)")

