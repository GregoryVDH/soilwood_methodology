rm(list = ls(all = TRUE))
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
elt = 7
ref_run = 1
plot(df_smooth[[ref_run]]$dist, df_smooth[[ref_run]][[elt]], type ="o", col = cpal[ref_run], lwd = 2)

for (run in 2:12) {
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

    plot(df$x, df$y)
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
    summary(mod)

    coeff = mod$coefficients
    # lines(
    #     df_smooth[[run]]$dist, 
    #     corrFct(df_smooth[[run]][[elt]], summary(mod)$coefficients[,1], length(coeff)), 
    #     type ="o", col = cpal[run], lwd = 2
    # )
}


# %%


library(data.table)
library(signal)
library(zoo)
source("./r-src/1_LA-ICPMS/fct.R")

fileList = list.files("./data/data_martin/la-icpms/mean_profiles_noCorr")
tab1 = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles_noCorr/", fileList[1]), sep = ";", header = TRUE)
tab2 = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles_noCorr/", fileList[3]), sep = ";", header = TRUE)

elt = 8
nrows = min(
    c(
        nrow(tab2[tab2$sample_name == "Oak_ref"]), 
        nrow(tab1[tab1$sample_name == "Oak_ref"])
    ), 
    na.rm = TRUE
)
df = data.frame(
    dist = tab2$dist[tab2$sample_name == "Oak_ref"][1:nrows],
    x = tab2[[elt]][tab2$sample_name == "Oak_ref"][1:nrows],
    x2 = NA,
    y = tab1[[elt]][tab1$sample_name == "Oak_ref"][1:nrows]
)

sgf_int = 161
element_filled = na.approx(df$x, na.rm = FALSE, rule = 2)
df$x_sgf <- sgolayfilt(element_filled, p = 2, n = sgf_int)
element_filled_y = na.approx(df$y, na.rm = FALSE, rule = 2)
df$y_sgf <- sgolayfilt(element_filled_y, p = 2, n = sgf_int)


res = fit_monotonic_poly(
    data = df,
    x_name = "x_sgf",
    y_name = "y_sgf",
    direction = "increasing"
)

plot(df$dist, df$y_sgf)
lines(df$dist, res$fitted_values, type = "l", col = "red", lwd = 2)
mean(abs(df$y_sgf - res$fitted_values))
res$coefficients

# %%

res$fitted_values
beta = res$coefficients


plot(df$x_sgf, df$y_sgf, xlim = c(0, x_max), ylim = c(0, x_max))
lines(x_array, beta[1] + beta[2] * x_array + beta[3] * x_array**2)




df$x2 = df$x**2
mod = lm(df$y ~ df$x2 + df$x)
if (summary(mod)$coefficients[1,4] > 0.05) {
    mod = lm(df$y ~ df$x2 + df$x + 0)
}
x_min = min(df$x, na.rm = TRUE)
x_max = max(df$x, na.rm = TRUE)
x_step = (x_max - x_min) / 100
x_array = seq(x_min, x_max, x_step)
points(df$x, df$y, col = "red")
if (nrow(summary(mod)$coefficients) == 2) {
    lines(x_array, (x_array**2) * mod$coefficients[1] + x_array * mod$coefficients[2], col = "red", lwd = 2)
} else {
    lines(x_array, 
    (x_array**2) * mod$coefficients[2] + 
        x_array * mod$coefficients[3] +
        mod$coefficients[1],
        col = "red", lwd = 2
    )
}



# plot(smoothed_sg, smoothed_sg_y)
plot(df$dist, df$x, type ="o")
lines(df$dist, df$x_mobMean, col = "blue", lwd = 2)
lines(df$dist, df$x_sgf, type = "l", col = "red", lwd = 2)


df$x2_sgf = df$x_sgf**2
mod = lm(df$y_sgf ~ df$x2_sgf + df$x_sgf)
if (summary(mod)$coefficients[1,4] > 0.05) {
    mod = lm(df$y_sgf ~ df$x2_sgf + df$x_sgf + 0)
}
x_min = min(df$x, na.rm = TRUE)
x_max = max(df$x, na.rm = TRUE)
x_step = (x_max - x_min) / 100
x_array = seq(x_min, x_max, x_step)
plot(df$x_sgf, df$y_sgf, xlim = c(0, x_max), ylim = c(0, x_max))
if (nrow(summary(mod)$coefficients) == 2) {
    lines(x_array, (x_array**2) * mod$coefficients[1] + x_array * mod$coefficients[2])
    df$pred = (df$x**2) * mod$coefficients[1] + df$x * mod$coefficients[2]
} else {
    lines(x_array, 
    (x_array**2) * mod$coefficients[2] + 
        x_array * mod$coefficients[3] +
        mod$coefficients[1]
    )
    df$pred = (df$x**2) * mod$coefficients[2] + df$x * mod$coefficients[3] + mod$coefficients[1]
}

if (nrow(summary(mod)$coefficients) == 2) {
    df$pred = (df$x**2) * mod$coefficients[1] + df$x * mod$coefficients[2]
} else {
    df$pred = (df$x**2) * mod$coefficients[2] + df$x * mod$coefficients[3] + mod$coefficients[1]
}
plot(df$dist, df$y, type = "l")
lines(df$dist, df$x, col = "red", lwd = 1, lty = 2)
lines(df$dist, df$pred, col = "blue", lwd = 2)


# %%

library(data.table)
library(signal)
library(zoo)
source("./r-src/1_LA-ICPMS/fct.R")

odd_int = function(num) {
    if (floor(num/2) == num / 2) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

fileList = list.files("./data/data_martin/la-icpms/mean_profiles")
tab1 = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles/", fileList[1]), sep = ";", header = TRUE)
# tab2 = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles/", fileList[3]), sep = ";", header = TRUE)

elt = 8
df = data.frame(
    dist = tab1$dist[tab1$sample_name == "Oak_ref"],
    x = tab1[[elt]][tab1$sample_name == "Oak_ref"]
)

int_win = 1000
resolution = 25
if (odd_int(floor(int_win / resolution))) {
    n_sgf = floor(int_win / resolution)
} else {
    n_sgf = floor(int_win / resolution) + 1
}

element_filled = na.approx(df$x, na.rm = FALSE, rule = 2)
df$x_sgf <- sgolayfilt(element_filled, p = 2, n = n_sgf)

plot(df$dist, df$x, col = "grey70", cex = 0.8, type ="o")
lines(df$dist, df$x_sgf, col = "red")

# %%

plot(df$dist, df$x - df$x_sgf, type = "o")

num = 46

