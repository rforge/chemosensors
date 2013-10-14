# scenario object (empty): default initialization 
sc <- Scenario(tunit = 60)

# add data to 
add(sc) <- list("A", 0.05, 2)
add(sc) <- list("B", 0.05, 2)
add(sc) <- list("C", 1, 2)
add(sc) <- list(c("A", "B", "C"), c(0.02, 0.04, 0.5), 2)

# scenario object: custom initialization
set <- c("A 0.05", "B 0.05", "C 1", "A 0.02, B 0.04, C 0.5") 
sc <- Scenario(tunit = 60, T = set, nT = 2, V = set, nV = 2)

# a real example of Scenario
sc <- Scenario(name = "Regression", tunit = 60, concUnits = "perc",
  T = c("A 0.01", "A 0.02", "A 0.05"), nT = 30,
  V = c("A 0.01", "A 0.02", "A 0.05"), nV = 30,
  randomize = TRUE)
sc

add(sc) <- list("A", 0.03, 30, "T")
add(sc) <- list("A", 0.04, 30, "T")
sc

# get information about the sensor
show(sc)
print(sc)

## Not run:
plot(sc)  
plot(sc, facet = FALSE, concUnits = 'norm')
## End(Not run)

# extract conc. matrix from scenatio
conc <- getConc(sc)

print(head(conc))
