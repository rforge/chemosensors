
# model: default initialization
sam <- SensorArrayModel()

# get information about the model
show(sam)
print(sam)

print(coef(sam)) # sensitivity coefficients

plot(sam)  

# model: custom parameters
sam <- SensorArrayModel(num=1:17) # 17 UNIMAN virtual sensors
plot(sam, main="17 UNIMAN virtual sensors")

sam <- SensorArrayModel(num=15:17, model="plsr", gases=c(1, 3))
print(sam)
plot(sam, uniman=TRUE) # add UNIMAN reference data (the model was build from)

sam <- SensorArrayModel(num=c(rep(1, 10), rep(5, 10)), gases=c(1, 2))
plot(sam, main="Array of sensors from two families: num 1 and 5")

# method plot
#  - plot types 'y': response
sam <- SensorArrayModel() # default sensor model

plot(sam, "response", main="plot(sam, 'response')") # default plot type, i.e. 'plot(sam)' does the same plotting  
