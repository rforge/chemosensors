
# sensor model: default initialization
sm <- SensorModel()

# get information about the model
show(sm)
print(sm)

print(coef(sm)) # sensitivity coefficients

plot(sm)  

# get available model names
model.names <- SensorModelNames()
print(model.names)

# sensor model: custom parameters
sm <- SensorModel(num=7, model="plsr", gases=c(1, 3))

print(sm)

#plot(sm, uniman=TRUE) # add UNIMAN reference data (the model was build from)

# method plot
#  - plot types 'y': response, predict
sm <- SensorModel() # default sensor model

plot(sm, "response", main="plot(sm, 'response')") 
# default plot type, i.e. 'plot(sm)' does the same plotting

conc <- concSample(sm, "range", gases=1, n=10)
plot(sm, "predict", conc, gases=1, main="plot(sm, 'predict', conc, gases=1)")
