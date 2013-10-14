
# model: default initialization
sm <- SensorModel()

# get information about the model
show(sm)
print(sm)

print(coef(sm)) # sensitivity coefficients

plot(sm)  

