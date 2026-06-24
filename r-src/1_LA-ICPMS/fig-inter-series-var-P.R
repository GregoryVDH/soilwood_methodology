

fileList = list.files("../../data/data_martin/la-icpms/mean_profiles")
dfList = list()
n = length(fileList)
for (i in seq_len(n)) {
    tab = fread(file = paste0("../../data/data_martin/la-icpms/mean_profiles/", fileList[i]), sep = ";", header = TRUE)
    tab = tab[tab$sample_name == "Oak_ref", ]
    if (nrow(tab) > 0) {
        tab$sample_name = i
        dfList[[i]] = tab
    }
}

df_smooth = list()
for (i in seq_len(n)) {
    df_smooth[[i]] = as.data.table(dfList[[i]][1:40,])
    df_smooth[[i]]$dist = seq(1, 40, 1)
}

for (i in seq_len(n)) {
    for (r in 1:nrow(df_smooth[[i]])) {
        for (elt in 3:19) {
            df_smooth[[i]][[elt]][r] = mean(dfList[[i]][[elt]][ abs(dfList[[i]]$dist - df_smooth[[i]]$dist[r]) <= 1 ], na.rm = TRUE)
        }
    }
}


elt = 7
par(mar = c(4,4,1,1))
plot(c(0,40), c(0,0.4), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
usr <- par("usr")
rect(
  usr[1], usr[3],
  usr[2], usr[4],
  col = "grey90",
  border = NA
)
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = cpal[i]) }
legend("topright", legend = seq_len(n), lty = 1, col = cpal[1:n], lwd = 2)
par(default_par)





fileList = list.files("../../data/data_martin/la-icpms/mean_profiles_noCorr")
dfList = list()
n = length(fileList)
for (i in seq_len(n)) {
    tab = fread(file = paste0("../../data/data_martin/la-icpms/mean_profiles_noCorr/", fileList[i]), sep = ";", header = TRUE)
    tab = tab[tab$sample_name == "Oak_ref", ]
    if (nrow(tab) > 0) {
        tab$sample_name = i
        dfList[[i]] = tab
    }
}

df_smooth = list()
for (i in seq_len(n)) {
    df_smooth[[i]] = as.data.table(dfList[[i]][1:40,])
    df_smooth[[i]]$dist = seq(1, 40, 1)
}

for (i in seq_len(n)) {
    for (r in 1:nrow(df_smooth[[i]])) {
        for (elt in 3:19) {
            df_smooth[[i]][[elt]][r] = mean(dfList[[i]][[elt]][ abs(dfList[[i]]$dist - df_smooth[[i]]$dist[r]) <= 1 ], na.rm = TRUE)
        }
    }
}


elt = 7
par(mar = c(4,4,1,1))
plot(c(0,40), c(0,0.4), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
usr <- par("usr")
rect(
  usr[1], usr[3],
  usr[2], usr[4],
  col = "grey90",
  border = NA
)
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = cpal[i]) }
legend("topright", legend = seq_len(n), lty = 1, col = cpal[1:n], lwd = 2)
par(default_par)