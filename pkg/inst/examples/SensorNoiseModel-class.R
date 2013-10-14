# model: default initialization
sn <- SensorNoiseModel()

# get information about the model
show(sn)
print(sn)

plot(sn)  

# model: custom parameters
# - many sensors
sn <- SensorNoiseModel(ssd=0.5, num=1:17, gases=c(1, 2, 3))

print(sn)

plot(sn)

# method plot
#  - plot types 'y': barplot, noise, walk
sn <- SensorNoiseModel() # default model

plot(sn, "barplot", main="plot(sn, 'barplot')") 
# default plot type, i.e. 'plot(sn)' does the same plotting

plot(sn, "noise", main="plot(sn, 'noise')")

set.seed(6) # make results reproducible
plot(sn, "walk", n=100, k=5, main="plot(sn, 'walk', n=100, k=5)")
  
