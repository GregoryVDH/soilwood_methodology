library(data.table)

run_ref = 1
run = 5
elt = 8 # K

fileList = list.files("../../data/data_martin/la-icpms/mean_profiles")
dfList = list()
n = length(fileList)
c = 1
for (i in c(run_ref, run)) {
    tab = fread(file = paste0("../../data/data_martin/la-icpms/mean_profiles/", fileList[i]), sep = ";", header = TRUE)
    tab = tab[tab$sample_name == "Oak_ref", ]
    if (nrow(tab) > 0) {
        tab$sample_name = i
        dfList[[c]] = tab
        c = c + 1
    }
}

nrows = min( c(nrow(dfList[[1]]), nrow(dfList[[2]])) )
df = data.frame(
    x = dfList[[2]][[elt]][1:nrows],
    y = dfList[[1]][[elt]][1:nrows]
)
plot(
    df$x, df$y, 
    xlim = c(0, 0.5), ylim = c(0, 0.5),
    xlab = "Oak reference sample of series n°5",
    ylab = "Oak reference sample of series n°1",
    pch = 21, bg = "red", col = "black"
)
