
### look at 17 sensor types
sa <- SensorArray(num = 1:17)
aff <- computeAffinity(sa)

df <- data.frame(num = num(sa), A = aff[, 1], C = aff[, 3])
p1 <- ggplot(df, aes(A, C, label = num)) + geom_text()

### test `beta`
# default value of `beta` 2
sa <- SensorArray(num = rep(c(1, 4, 17), 5))
aff <- computeAffinity(sa)

df <- data.frame(num = num(sa), A = aff[, 1], C = aff[, 3])
p2 <- ggplot(df, aes(A, C, label = num)) + geom_text()

# value of `beta` 10
sa <- SensorArray(num = rep(c(1, 4, 17), 5), beta = 10)
aff <- computeAffinity(sa)

df <- data.frame(num = num(sa), A = aff[, 1], C = aff[, 3])
p3 <- ggplot(df, aes(A, C, label = num)) + geom_text()

### 17 sensor types + `beta` 10
sa <- SensorArray(num = rep(1:17, 5), beta = 10)
aff <- computeAffinity(sa)

df <- data.frame(num = num(sa), A = aff[, 1], C = aff[, 3])
df <- mutate(df, 
  group = ifelse(num == 2, "2", ifelse(num == 17, "17", ifelse(num == 4, "4", "other"))))
p4 <- ggplot(df, aes(A, C, label = num, color = group)) + geom_text()
