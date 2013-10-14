
data(UNIMANsnoise)

str(UNIMANsnoise, max.level = 2)

str(UNIMANsnoise$Bsd$Sensor, max.level = 1)

# SD parameters for a particular data model 'plsr'
Bsd <- UNIMANsnoise$Bsd$Sensor$plsr

# plot #1
df <- melt(Bsd, varnames = c("gas", "sensor"))

df <- mutate(df,
  gas = LETTERS[gas], 
  sensor = factor(paste("S", sensor, sep = ""), levels = paste("S", 1:17, sep = "")))
  
p1 <- qplot(sensor, value, data = df, geom = "bar") + 
  facet_grid(gas ~ ., scales = "free_y") +
  labs(x = "sensor", y = "sd parameter", title = "Sensor Noise in data model 'plsr'")
p1  
  

# plot #2
Bsd.norm <- t(apply(Bsd, 1, function(x) x / max(x)))

df <- melt(Bsd.norm, varnames = c("gas", "sensor"))

df <- mutate(df,
  gas = LETTERS[gas], 
  sensor = factor(paste("S", sensor, sep = ""), levels = paste("S", 1:17, sep = "")))

p2 <- ggplot(df, aes(x = sensor, y = value, fill = gas)) + 
  geom_bar(position = "stack") +
  labs(x = "sensor", y = "sd parameter (normalized acroos gases)")
p2

# plot PCA plots for sensors different in the noise level
set.seed(10)
sa1 <- SensorArray(model = "plsr", num = c(4, 7, 14), csd = 0, ssd = 1, dsd = 0)

p3 <- plotPCA(sa1, set = rep(c("A", "B", "C"), 10), air = FALSE) + 
  labs(title = "Less noisy sensors")
p3

sa2 <- SensorArray(model = "plsr", num = c(1, 5, 17), csd = 0, ssd = 1, dsd = 0)

p4 <- plotPCA(sa2, set = rep(c("A", "B", "C"), 10), air = FALSE) + 
  labs(title = "More noisy sensors")
p4
