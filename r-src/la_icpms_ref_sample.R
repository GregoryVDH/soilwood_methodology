#  %%

library(data.table)
rm(list = ls(all = TRUE))

meanList = function(inputList, nrow, ncol) {
    m = 0
    count = 0
    for (i in seq_len(length(inputList))) {
        if (!is.na(inputList[[i]][[ncol]][nrow])) {
            m = m + inputList[[i]][[ncol]][nrow]
            count = count + 1
        }
    }
    if (count == 0) {m = NA}
    return(m / count)
}

sdList = function(inputList, nrow, ncol) {
    m = meanList(inputList, nrow, ncol)
    if (!is.na(m)) {
        sd = 0
        count = 0
        for (i in seq_len(length(inputList))) {
            if (!is.na(inputList[[i]][[ncol]][nrow])) {
                sd = sd + (m - inputList[[i]][[ncol]][nrow])**2
                count = count + 1
            }
        }
        if (count == 0) {sd = NA}
        sd = (sd / count)**0.5
    } else {
        sd = NA
    }
    return(sd)
}

# c25 <- c(
#   "dodgerblue2", "#E31A1C", # red
#   "green4",
#   "#6A3D9A", # purple
#   "#FF7F00", # orange
#   "black", "gold1",
#   "skyblue2", "#FB9A99", # lt pink
#   "palegreen2",
#   "#CAB2D6", # lt purple
#   "#FDBF6F", # lt orange
#   "gray70", "khaki2",
#   "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
#   "darkturquoise", "green1", "yellow4", "yellow3",
#   "darkorange4", "brown"
# )

fileList = list.files("./data/data_martin/la-icpms/mean_profiles")
dfList = list()
n = length(fileList)
for (i in seq_len(n)) {
    tab = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles/", fileList[i]), sep = ";", header = TRUE)
    tab = tab[tab$sample_name == "Oak_ref", ]
    if (nrow(tab) > 0) {
        tab$sample_name = i
        dfList[[i]] = tab
    }
}


# dfList_lod = list()
# n = length(fileList)
# for (i in seq_len(n)) {
#     tab = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles_lod/", fileList[i]), sep = ";", header = TRUE)
#     tab = tab[tab$sample_name == "Oak_ref", ]
#     if (nrow(tab) > 0) {90
#         tab$sample_name = i
#         dfList_lod[[i]] = tab
#     }
# }

# for (i in seq_len(n)) {
#     for (elt in 3:19){
#         exclude = dfList[[i]][[elt]] < dfList_lod[[i]][[elt]]
#         dfList[[i]][[elt]][exclude] = NA
#     }
# }

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


# elt = 8
# plot(c(0,40), c(0,0.3), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
# for (i in seq_len(n)) { 
#     plotdata = df_smooth[[i]][[elt]]
#     # plotdata = plotdata - mean(df_smooth[[i]][[elt]][df_smooth[[i]]$dist >= 20 & df_smooth[[i]]$dist <= 30])
#     # plotdata = plotdata / max(plotdata, na.rm = TRUE)
#     lines(
#         plotdata, 
#         col = c25[i], 
#         lwd = 2
#     ) 
# }


cpal = c(
  "dodgerblue2", 
  "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", 
  "gold1",
  "skyblue2", 
  "brown",
  "#FB9A99", # lt pink
  "darkturquoise", 
  "palegreen2",
  "maroon", 
  "orchid1", 
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", 
  "khaki2",
  "deeppink1", 
  "blue1", 
  "steelblue4",
  "green1", 
  "yellow4", 
  "yellow3",
  "darkorange4"
)
# %%
elt_list = c(3,4,7,8,9,11,12,15,16,17,18)
maxVal = c(4, 2.5, 0.3, NA, 0.5, NA, 3, 0.7, 1.5, 6, NA)
# pdf(file = "./pdf/baseline_correction_C_norm.pdf", title = "baseline_correction_C_norm")
for (j in seq_len(length(elt_list))) {
    elt = elt_list[j]
    if (is.na(maxVal[j])) {
        maxVal[j] = 0
        for (i in seq_len(n)) {
            maxVal[j] = max(maxVal[j], max(df_smooth[[i]][[elt]], na.rm = TRUE))
        }
    }
    plot(
        c(0,40), c(0,maxVal[j]), 
        xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", 
        col = "white", main = colnames(df_smooth[[1]])[elt])
    usr <- par("usr")
    rect(
    usr[1], usr[3],
    usr[2], usr[4],
    col = "grey90",
    border = NA
    )
    for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = cpal[i], lwd =2) }
    legend("topright", legend = seq_len(n), lty = 1, col = cpal[1:n], lwd = 2)
}
# dev.off()

#  %%

for (elt in 3:19) {
    plot(
        df_smooth[[1]]$dist, df_smooth[[1]][[elt]] * 1.5, 
        xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", main = colnames(df_smooth[[1]])[elt],
        col = "white"
    )
    for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = c25[i]) }
}

# Potassium offset
elt = 8
correction = array(data = 0, dim = n)
for (i in seq_len(n)) {
    correction[i] = median(df_smooth[[1]][[elt]] - df_smooth[[i]][[elt]], na.rm = TRUE)
}
plot(c(0,40), c(0,0.3), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white", main = "no correction")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = c25[i]) }
plot(c(0,40), c(0,0.3), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] + correction[i], col = c25[i]) }

# Potassium max-min standardisation
meanVal = array(data = NA, dim = n)
medianVal = array(data = NA, dim = n)
for (i in seq_len(n)) {
    meanVal[i] = mean(df_smooth[[i]][[elt]], na.rm = TRUE)
    medianVal[i] = median(df_smooth[[i]][[elt]], na.rm = TRUE)
}

plot(c(0,40), c(0,2.5), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white", main = "mean correction")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] / (meanVal[i]), col = c25[i]) }
plot(c(0,40), c(0,4), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white", main = "median correction")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] / (medianVal[i]), col = c25[i]) }



# Magnesium
elt = 4
correction = array(data = 0, dim = n)
for (i in seq_len(n)) {
    correction[i] = median(df_smooth[[1]][[elt]][1:30] - df_smooth[[i]][[elt]][1:30], na.rm = TRUE)
}
plot(c(0,40), c(0,3), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white", main = "no correction")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = c25[i]) }
plot(c(0,40), c(0,3), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white",main = "offset")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] + correction[i], col = c25[i]) }
meanVal = array(data = NA, dim = n)
medianVal = array(data = NA, dim = n)
for (i in seq_len(n)) {
    meanVal[i] = mean(df_smooth[[i]][[elt]][1:30], na.rm = TRUE)
    medianVal[i] = median(df_smooth[[i]][[elt]][1:30], na.rm = TRUE)
}

plot(c(0,40), c(0,2.5), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white", main = "mean correction")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] / (meanVal[i]), col = c25[i]) }
plot(c(0,40), c(0,4), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white", main = "median correction")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] / (medianVal[i]), col = c25[i]) }


# Phosphorus
elt = 7
correction = array(data = 0, dim = n)
for (i in seq_len(n)) {
    correction[i] = median(df_smooth[[1]][[elt]] - df_smooth[[i]][[elt]], na.rm = TRUE)
}
plot(c(0,40), c(0,0.6), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = c25[i]) }
plot(c(0,40), c(0,0.6), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] + correction[i], col = c25[i]) }
meanVal = array(data = NA, dim = n)
medianVal = array(data = NA, dim = n)
for (i in seq_len(n)) {
    meanVal[i] = mean(df_smooth[[i]][[elt]], na.rm = TRUE)
    medianVal[i] = median(df_smooth[[i]][[elt]], na.rm = TRUE)
}
plot(c(0,40), c(0,4), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white", main = "mean correction")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] / (meanVal[i]), col = c25[i]) }
plot(c(0,40), c(0,5), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white", main = "median correction")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] / (medianVal[i]), col = c25[i]) }


# Calcium
elt = 9
correction = array(data = 0, dim = n)
for (i in seq_len(n)) {
    correction[i] = median(df_smooth[[1]][[elt]] - df_smooth[[i]][[elt]], na.rm = TRUE)
}
plot(c(0,40), c(0,0.6), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = c25[i]) }
plot(c(0,40), c(0,0.6), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] + correction[i], col = c25[i]) }

# Manganese
elt = 11
colnames(df_smooth[[1]])[elt]
correction = array(data = 0, dim = n)
for (i in seq_len(n)) {
    correction[i] = median(df_smooth[[1]][[elt]] - df_smooth[[i]][[elt]], na.rm = TRUE)
}
plot(c(0,40), c(0,2), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = c25[i]) }
plot(c(0,40), c(0,2), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] + correction[i], col = c25[i]) }

# iron
elt = 12
colnames(df_smooth[[1]])[elt]
correction = array(data = 0, dim = n)
for (i in seq_len(n)) {
    correction[i] = median(df_smooth[[1]][[elt]] - df_smooth[[i]][[elt]], na.rm = TRUE)
}
plot(c(0,40), c(0,2), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]], col = c25[i]) }
plot(c(0,40), c(0,2), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { lines(df_smooth[[i]]$dist, df_smooth[[i]][[elt]] + correction[i], col = c25[i]) }




# ==============================================
# 
## raw data => not standardised by C signal
# 
# ==============================================

fileList = list.files("./data/data_martin/la-icpms/mean_profiles_raw/")
dfList = list()
n = length(fileList)
for (i in seq_len(n)) {
    tab = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles_raw/", fileList[i]), sep = ";", header = TRUE)
    tab = tab[tab$sample_name == "Oak_ref", ]
    if (nrow(tab) > 0) {
        tab$sample_name = i
        dfList[[i]] = tab
    }
}


dfList_lod = list()
n = length(fileList)
for (i in seq_len(n)) {
    tab = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles_raw_lod/", fileList[i]), sep = ";", header = TRUE)
    tab = tab[tab$sample_name == "Oak_ref", ]
    if (nrow(tab) > 0) {90
        tab$sample_name = i
        dfList_lod[[i]] = tab
    }
}

for (i in seq_len(n)) {
    for (elt in 3:19){
        exclude = dfList[[i]][[elt]] < dfList_lod[[i]][[elt]]
        dfList[[i]][[elt]][exclude] = NA
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


elt = 8
plot(c(0,40), c(0,100000), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal", col = "white")
for (i in seq_len(n)) { 
    plotdata = df_smooth[[i]][[elt]]
    # plotdata = df_smooth[[i]][[elt]] - mean(df_smooth[[i]][[elt]][df_smooth[[i]]$dist >= 20 & df_smooth[[i]]$dist <= 30])
    # plotdata = plotdata / max(plotdata, na.rm = TRUE)
    lines(
        plotdata, 
        col = c25[i], 
        lwd = 2
    ) 
}



# %% 
# Compare series 1 and 5 for all samples other than Oak_ref

library(data.table)
fileList = list.files("./data/data_martin/la-icpms/mean_profiles_noCorr")
tab1 = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles_noCorr/", fileList[1]), sep = ";", header = TRUE)
tab2 = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles_noCorr/", fileList[3]), sep = ";", header = TRUE)

sn1 = unique(tab1$sample_name)
sn2 = unique(tab2$sample_name)

n = 11
elt = 11

df_smooth1 = list()
df_smooth2 = list()
for (i in seq_len(n)) {
    df_smooth1[[i]] = tab1[1:60]
    df_smooth1[[i]]$dist = seq(1, 60, 1)
    df_smooth2[[i]] = tab2[1:60]
    df_smooth2[[i]]$dist = seq(1, 60, 1)
}

for (i in seq_len(n)) {
    for (r in 1:nrow(df_smooth1[[i]])) {
        temp = tab1[tab1$sample_name == sn1[i]]
        df_smooth1[[i]][[elt]][r] = mean(
            temp[[elt]][ abs(temp$dist - df_smooth1[[i]]$dist[r]) <= 1 ],
            na.rm = TRUE
        )
        temp = tab2[tab2$sample_name == sn2[i]]
        df_smooth2[[i]][[elt]][r] = mean(
            temp[[elt]][ abs(temp$dist - df_smooth2[[i]]$dist[r]) <= 1 ],
            na.rm = TRUE
        )
    }
}


# minVal1 = array(dim = n-1, data = NA)
# minVal2 = array(dim = n-1, data = NA)
# for (i in 1:(n - 1)) {
#     minVal1[i] = min(df_smooth1[[i]][[elt]][1:30], na.rm = TRUE)
#     minVal2[i] = min(df_smooth2[[i]][[elt]][1:30], na.rm = TRUE)
# }
# maxVal1 = array(dim = n-1, data = NA)
# maxVal2 = array(dim = n-1, data = NA)
# for (i in 1:(n - 1)) {
#     maxVal1[i] = max(df_smooth1[[i]][[elt]][1:30], na.rm = TRUE)
#     maxVal2[i] = max(df_smooth2[[i]][[elt]][1:30], na.rm = TRUE)
# }
# cat("min series 01: ", mean(minVal1), " - min series 05: ", mean(minVal2))
# cat("\n")
# cat("max series 01: ", mean(maxVal1), " - max series 05: ", mean(maxVal2))

x0 = min(df_smooth2[[n]][[elt]][1:30], na.rm = TRUE)
x1 = max(df_smooth2[[n]][[elt]][1:30], na.rm = TRUE)
y1 = max(df_smooth1[[n]][[elt]][1:30], na.rm = TRUE) / max(df_smooth2[[n]][[elt]][1:30], na.rm = TRUE)
y0 = min(df_smooth1[[n]][[elt]][1:30], na.rm = TRUE) / min(df_smooth2[[n]][[elt]][1:30], na.rm = TRUE)

slope = (y1 - y0) / (x1 - x0)
corrFact = slope * (df_smooth2[[n]][[elt]] - x0) + y0

plot(df_smooth1[[n]]$dist, df_smooth1[[n]][[elt]], type = 'o')
lines(df_smooth2[[n]]$dist, df_smooth2[[n]][[elt]], col = 'blue', type = "o")
lines(df_smooth2[[n]]$dist, df_smooth2[[n]][[elt]] * corrFact, col = 'red', type = "o")


x0 = quantile(df_smooth2[[n]][[elt]][1:30], probs = 0.25, na.rm = TRUE)
x1 = quantile(df_smooth2[[n]][[elt]][1:30], na.rm = TRUE, probs = 0.85)
y1 = quantile(df_smooth1[[n]][[elt]][1:30], na.rm = TRUE, probs = 0.85) / quantile(df_smooth2[[n]][[elt]], na.rm = TRUE, probs = 0.85)
y0 = quantile(df_smooth1[[n]][[elt]][1:30], probs  = 0.25,  na.rm = TRUE) / quantile(df_smooth2[[n]][[elt]], na.rm = TRUE, probs = 0.25)
slope = (y1 - y0) / (x1 - x0)
corrFact2 = slope * (df_smooth2[[n]][[elt]] - x0) + y0
lines(df_smooth2[[n]]$dist, df_smooth2[[n]][[elt]] * corrFact2, col = 'darkgreen', type = "o")


x = df_smooth2[[n]][[elt]] * corrFact2 
y = df_smooth1[[n]][[elt]]
summary(lm(y ~ x))
x = df_smooth2[[n]][[elt]] * corrFact
summary(lm(y ~ x+0))

plot(df_smooth2[[n]]$dist, df_smooth2[[n]][[elt]] * corrFact - df_smooth1[[n]][[elt]], type ="o", ylim = c(-0.05,0.3))
lines(df_smooth2[[n]]$dist, df_smooth2[[n]][[elt]] * corrFact2 - df_smooth1[[n]][[elt]], type ="o", col = "red")
abline(h = 0)
mean(abs(df_smooth2[[n]][[elt]] * corrFact2 - df_smooth1[[n]][[elt]])[3:30], na.rm = TRUE)
mean(abs(df_smooth2[[n]][[elt]] * corrFact - df_smooth1[[n]][[elt]])[3:30], na.rm = TRUE)


# %%

i = 2
mean(df_smooth[[1]][[elt]] / df_smooth[[i]][[elt]], na.rm = TRUE)
i = i + 1

nrows = 1e9
for (i in seq_len(n)) {
    nrows = min(c(nrows, nrow(dfList[[i]])), na.rm = TRUE)
}
elt = 8
df2 = data.frame(dist = array(dim = nrows, data = NA), dist_sd = NA, mean = NA, stdev = NA, rsd = NA)
for (r in seq_len(nrows)) {
    df2$dist[r] = meanList(dfList, r, 2)
    df2$dist_sd[r] = sdList(dfList, r, 2)
    df2$mean[r] = meanList(dfList, r, elt)
    df2$stdev[r] = sdList(dfList, r, elt)
}
df2$rsd = df2$stdev / df2$mean
plot(df2$mean, df2$stdev)
abline(a=0, b = 1)
abline(a=0, b = 0.5)
abline(a=0, b = 0.25)

mean(dfList[[1]][[elt]][seq_len(nrows)] / dfList[[2]][[elt]][seq_len(nrows)], na.rm = TRUE)
mean(dfList[[1]][[elt]][seq_len(nrows)] / dfList[[3]][[elt]][seq_len(nrows)], na.rm = TRUE)






tab = fread(file = "./data/data_martin/la-icpms/individual_profiles/Series_01_mean.csv")
tab = tab[tab$sample_name == "AUD_20", ]

lod = fread(file = "./data/data_martin/la-icpms/individual_profiles_lod/Series_01_mean.csv")
lod = lod[lod$sample_name == "AUD_20", ]

for (elt in 3:19){
    exclude = tab[[elt]] < lod[[elt]]
    tab[[elt]][exclude] = NA
}

nrows = min(c(nrow(tab[tab$rep == 1, ]), nrow(tab[tab$rep == 2, ]), nrow(tab[tab$rep == 3, ])))

mean(tab[[elt]][tab$rep == 1][seq_len(nrows)] / tab[[elt]][tab$rep == 2][seq_len(nrows)], na.rm = TRUE)
mean(tab[[elt]][tab$rep == 1][seq_len(nrows)] / tab[[elt]][tab$rep == 3][seq_len(nrows)], na.rm = TRUE)
mean(tab[[elt]][tab$rep == 2][seq_len(nrows)] / tab[[elt]][tab$rep == 3][seq_len(nrows)], na.rm = TRUE)

tab = fread(file = "./data/data_martin/la-icpms/mean_profiles/Series_01_mean.csv")
tab = tab[tab$sample_name == "AUD_20", ]
lod = fread(file = "./data/data_martin/la-icpms/mean_profiles_lod/Series_01_mean.csv")
lod = lod[lod$sample_name == "AUD_20", ]

for (elt in 3:19){
    exclude = tab[[elt]] < lod[[elt]]
    tab[[elt]][exclude] = NA
}
mean(tab$stdev.K39 / tab$mean.K39, na.rm = TRUE)




# %%
s = 11
tab1 = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles/", fileList[1]), sep = ";", header = TRUE)
tab2 = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles/", fileList[s]), sep = ";", header = TRUE)

sn1 = unique(tab1$sample_name)
sn2 = unique(tab2$sample_name)

n = 10
elt = 11
df_smooth1 = list()
df_smooth2 = list()
for (i in seq_len(n)) {
    df_smooth1[[i]] = tab1[1:60]
    df_smooth1[[i]]$dist = seq(1, 60, 1)
    df_smooth2[[i]] = tab2[1:60]
    df_smooth2[[i]]$dist = seq(1, 60, 1)
}

for (i in seq_len(n)) {
    for (r in 1:nrow(df_smooth1[[i]])) {
        temp = tab1[tab1$sample_name == sn1[i]]
        df_smooth1[[i]][[elt]][r] = mean(
            temp[[elt]][ abs(temp$dist - df_smooth1[[i]]$dist[r]) <= 1 ],
            na.rm = TRUE
        )
        temp = tab2[tab2$sample_name == sn2[i]]
        df_smooth2[[i]][[elt]][r] = mean(
            temp[[elt]][ abs(temp$dist - df_smooth2[[i]]$dist[r]) <= 1 ],
            na.rm = TRUE
        )
    }
}

K1 = array(data = 0, dim = 60)
K2 = array(data = 0, dim = 60)
for (r in 1:60) {
    for (i in 1:n) {
        K1[r] = K1[r] + df_smooth1[[i]][[elt]][r]
        K2[r] = K2[r] + df_smooth2[[i]][[elt]][r]
    }
    K1[r] = K1[r] / n
    K2[r] = K2[r] / n
}


plot(1:60, K1, type = 'l', ylim = c(0, 3), xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal")
lines(1:60, K2, col = "red")
legend("topright", legend = c("series 1", paste0("series ", s)), lty = 1, lwd = 2, col = c("black", "red"))



# %%