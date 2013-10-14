# sorption model: default initialization
sm <- SorptionModel()

# get information about the model
show(sm)
print(sm)

plot(sm)  

# model: custom parameters
# almost linear model
sm <- SorptionModel(alpha=0.01, gases=c(1, 3)) 
plot(sm, main="Almost linear model, non-linearity 0.01")

# non-linear model
sm <- SorptionModel(alpha=0.3, gases=c(1, 3)) 
plot(sm, main="Non-linear model, non-linearity 0.5")

# saturated model
sm <- SorptionModel(alpha=0.5, gases=c(1, 3)) 
plot(sm, main="Saturated model, non-linearity 1")

# model with UNIMAN sorption parameters
sm <- SorptionModel(Knorm=FALSE, gases=c(1, 3)) 
plot(sm, main="Model with UNIMAN parameters (no normalization)")

# method plot
#  - plot types 'y': response, data, predict
sm <- SorptionModel() # default model

plot(sm, "response", main="plot(sm, 'response')") 
# default plot type, i.e. 'plot(sm)' does the same plotting

plot(sm, "data", main="plot(sm, 'data')")

plot(sm, "predict", main="plot(sm, 'predict')")
