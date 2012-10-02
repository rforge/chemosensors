
# array: default initialization
sa <- SensorArray()

# get information about the array
show(sa)
print(sa)

print(coef(sa)) # array coefficients

plot(sa)  

# model: custom parameters
sa <- SensorArray(num=1:17) # 17 UNIMAN virtual sensors
plot(sa, main="17 UNIMAN virtual sensors")

sa <- SensorArray(num=15:17, alpha=0.01, model="plsr", gases=c(1, 3)) # array with quite linear sensors

print(sa)

plot(sa, uniman=TRUE, main="Array of more linear sensors") # add UNIMAN reference data (the models were build from)

# method plot
#  - plot types 'y': response
sa <- SensorArray() # default sensor model

plot(sa, "response", main="plot(sa, 'response')") # default plot type, i.e. 'plot(sa)' does the sae plotting  
