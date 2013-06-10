
data(UNIMANsorption, package="chemosensors")

# print the list of loaded data variables
str(UNIMANsorption)

dim(UNIMANsorption$qkc)

str(UNIMANsorption$qkc)

### Langmuir parameter K
K <- UNIMANsorption$qkc[, , "K"]

mf <- melt(K, varnames = c("sensor", "gas"))

p1 <- qplot(sensor, value, data = mf, geom = "line", color = gas) +
  ylab("Langmuir parameter K")
p1

p2 <- qplot(sensor, value, data = mf, geom = "bar", stat = "identity") + 
  facet_grid(gas ~ ., scale = "free_y") + 
  ylab("Langmuir parameter K")
p2

### Langmuir parameter KCmin
KCmin <- UNIMANsorption$qkc[, , "KCmin"]

mf <- melt(KCmin, varnames = c("sensor", "gas"))

p3 <- qplot(sensor, value, data = mf, geom = "line", color = gas) +
  ylab("Langmuir parameter KCmin")
p3

p4 <- qplot(sensor, value, data = mf, geom = "bar", stat = "identity") + 
  facet_grid(gas ~ .) + 
  ylab("Langmuir parameter KCmin")
p4

### Langmuir parameter KCmax
KCmax <- UNIMANsorption$qkc[, , "KCmax"]

mf <- melt(KCmax, varnames = c("sensor", "gas"))

p5 <- qplot(sensor, value, data = mf, geom = "line", color = gas) +
  ylab("Langmuir parameter KCmax")
p5

p6 <- qplot(sensor, value, data = mf, geom = "bar", stat = "identity") + 
  facet_grid(gas ~ .) + 
  ylab("Langmuir parameter KCmax")
p6

### summary plot for K*
require(gridExtra)
grid.arrange(p1, p3, p5, ncol = 1)

### plot to group sensors based on affinities A vs. C
df <- as.data.frame(K)
df <- mutate(df,
  sensor = 1:nrow(df),
  sensor.group = ifelse(A > C, "More affinity to A", "More affinity to C"))

mf <- melt(K, varnames = c("sensor", "gas"))

p7 <- ggplot(mf, aes(x = factor(sensor), y = value, fill = gas)) + 
  geom_bar(position = "dodge") +
  xlab("sensor") + ylab("Langmuir parameter K")
p7

p8 <- ggplot(df, aes(reorder(x = factor(sensor), A - C), y = A - C, fill = sensor.group)) + 
  geom_bar(position = "identity") + coord_flip() +
  xlab("sensor") + ylab("Difference in K between A and C")
p8

### UNIMAN affinities K in polar plot
mf <- melt(UNIMANsorption$qkc[, , "K"], varnames = c("sensor", "gas"))

p9 <- qplot(sensor, value, color = gas, data = mf, geom = "line") + coord_polar()
p9


