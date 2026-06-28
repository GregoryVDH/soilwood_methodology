library(data.table)


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

corrFct = function(x, par, n) {
    if (n == 2) {
        return(par[1] * x**2 + par[2] * x)
    } else if (n == 3) {
        return(par[1] + par[2] * x**2 + par[3] * x)
    } else {
        return(array(data = NA, dim = length(x)))
    }
}

#  %%
elt = 8
ref_run = 1

run = 5
    df = data.frame(
        x = df_smooth[[run]][[elt]][1:30],
        x2 = df_smooth[[run]][[elt]][1:30]**2,
        y = df_smooth[[ref_run]][[elt]][1:30]
    )
    df = na.omit(df)
    mod = lm(df$y ~ df$x2 + df$x)
    if (summary(mod)$coefficients[1,4] > 0.05) {
        mod = lm(df$y ~ df$x2 + df$x + 0)
    }
    # Check if the polynome is bell shaped within the calibration range
    while (TRUE) {
        if (length(mod$coefficients) == 3 ) {
            a = 2 * mod$coefficients[2]
            b = mod$coefficients[3]
        } else {
            a = 2 * mod$coefficients[1]
            b = mod$coefficients[2]
        }
        if (max(df$x) * a + b < 0) {
            df = df[df$x != max(df$x), ]
        } else {
            break
        }
        mod = lm(df$y ~ df$x2 + df$x)
        if (summary(mod)$coefficients[1,4] > 0.05) {
            mod = lm(df$y ~ df$x2 + df$x + 0)
        }
    }

    par(mar = c(4,4,1,1))
    plot(
        df$x, df$y, xlab = "Oak reference - Series 05", ylab = "Oak reference - Series 01",
        pch = 21, bg = "firebrick", col = "black"
    )
    x_min = min(df$x)
    x_max = max(df$x)
    x_step = (x_max - x_min) / 100
    x_array = seq(x_min, x_max, x_step)
    if (nrow(summary(mod)$coefficients) == 2) {
        lines(x_array, (x_array**2) * mod$coefficients[1] + x_array * mod$coefficients[2])
    } else {
        lines(x_array, 
        (x_array**2) * mod$coefficients[2] + 
            x_array * mod$coefficients[3] +
            mod$coefficients[1]
        )
    }

coeff = mod$coefficients
par(mar = c(4,4,1,1))
plot(
    df_smooth[[ref_run]]$dist, df_smooth[[ref_run]][[elt]], type ="o", 
    col = "black", lwd = 2, pch = 21, bg = "black", ylim =c(0, 0.3),
    xlab = "distance to bark (mm)", ylab = "1/C * ICPMS signal"
)
lines(
    df_smooth[[run]]$dist, 
    df_smooth[[run]][[elt]], 
    type ="o", bg = "firebrick", lwd = 2, col = "firebrick", pch = 21
)
lines(
    df_smooth[[run]]$dist, 
    corrFct(df_smooth[[run]][[elt]], mod$coefficients, length(coeff)), 
    type ="o", bg = "chartreuse3", lwd = 1, col = "chartreuse3", pch = 21
)

par(default_par)