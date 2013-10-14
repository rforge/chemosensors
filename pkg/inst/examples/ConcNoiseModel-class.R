# model: default initialization
cn <- ConcNoiseModel()

# get information about the model
show(cn)
print(cn)

plot(cn)  

# model: custom parameters
# - many sensors
cn <- ConcNoiseModel(csd=0.5, gases=c(1, 3))

print(cn)

plot(cn)

# method plot
#  - plot types 'y': noise
cn <- ConcNoiseModel() # default model

plot(cn, "noise", main="plot(cn, 'noise')") 
# default plot type, i.e. 'plot(cn)' does the same plotting
