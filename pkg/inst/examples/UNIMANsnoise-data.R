
data(UNIMANsnoise, package="chemosensors")

str(UNIMANsnoise)

Bsd <- UNIMANsnoise$Bsd

str(Bsd)

# SD values for beta-coefficients in the case of 'SensorModel/mvr'
bsd <- Bsd[["SensorModel"]][["mvr"]]

print(dim(bsd)) # 3 gases, 17 sensors

print(apply(bsd, 1, summary)) # summary per gas

barplot(bsd, names.arg=1:ncol(bsd), main="SD of coefficients ~ Sensors")
