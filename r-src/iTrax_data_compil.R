library(data.table)

list.files("data_martin/ITRAX/01")

tab = read.table(file = "data_martin/ITRAX/01/AUD20_result.txt", sep =",", header = TRUE)
tab = tab[, 3:16]
