
# array: default initialization
sa <- SensorArray()

# get information about the array
show(sa)
print(sa)

print(coef(sa)) # array coefficients

#plot(sa)  

# model: custom parameters
sa <- SensorArray(num=1:17) # 17 UNIMAN virtual sensors
plot(sa, main="17 UNIMAN virtual sensors")

# array with quite linear sensors
sa <- SensorArray(num=15:17, alpha=0.01, model="mvr") 
print(sa)

# add UNIMAN reference data (the models were build from)
p1 <- plotResponse(sa, main="Array of more linear sensors") 
