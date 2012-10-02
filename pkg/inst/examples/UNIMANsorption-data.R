
data(UNIMANsorption, package="chemosensors")

# print the list of loaded data variables
str(UNIMANsorption)

dim(UNIMANsorption$qkc)

str(UNIMANsorption$qkc)

barplot(UNIMANsorption$qkc[, , "K"], beside=TRUE, main="Affinity parameter K ~ UNIMAN sensors")

barplot(UNIMANsorption$qkc[, , "KCmin"], beside=TRUE, main="Parameter KCmin ~ UNIMAN sensors")

barplot(UNIMANsorption$qkc[, , "KCmax"], beside=TRUE, main="Parameter KCmax ~ UNIMAN sensors")
