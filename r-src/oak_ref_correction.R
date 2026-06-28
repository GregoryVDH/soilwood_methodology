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

n_sgf = function(int_win, resolution = 25) {
    if (odd_int(floor(int_win / resolution))) {
        n_sgf = floor(int_win / resolution)
    } else {
        n_sgf = floor(int_win / resolution) + 1
    }
    return(n_sgf)
}

min_nrow = function(df1, df2, sample = "Oak_ref") {
    return(
        min(
            c(
                nrow(df1[df1$sample_name == sample]), 
                nrow(df2[df2$sample_name == sample])
            ), 
            na.rm = TRUE
        )
    ) 
}


# %% Read input data

fileList = list.files("./data/data_martin/la-icpms/mean_profiles")
dfList = list()
n = length(fileList)
for (i in seq_len(n)) {
    dfList[[i]] = fread(file = paste0("./data/data_martin/la-icpms/mean_profiles/", fileList[i]), sep = ";", header = TRUE)
}

# %%

dfCorr = dfList

elt = 8
minVal = 1e12
maxVal = -1e12
for (run in seq_len(n)) {
    t = dfList[[run]][[elt]]
    minVal = min(c(minVal, min(t, na.rm = TRUE)))
    maxVal = max(c(maxVal, max(t, na.rm = TRUE)))
}
rm(t)

run_ref = 1
run = 5
nrows = min_nrow(dfList[[run]], dfList[[run_ref]])
df = data.frame(
    x = dfList[[run]][[elt]][dfList[[run]]$sample_name == "Oak_ref"][1:nrows],
    y = dfList[[run_ref]][[elt]][dfList[[run_ref]]$sample_name == "Oak_ref"][1:nrows]
)

int_win = 4000
element_filled = na.approx(df$x, na.rm = FALSE, rule = 2)
df$x_sgf <- sgolayfilt(element_filled, p = 2, n = n_sgf(int_win))
element_filled = na.approx(df$y, na.rm = FALSE, rule = 2)
df$y_sgf <- sgolayfilt(element_filled, p = 2, n = n_sgf(int_win))

res = fit_poly(
    data = df,
    x_name = "x_sgf",
    y_name = "y_sgf",
    x_min = minVal,
    x_max = maxVal,
    direction = "increasing"
)

# plot(df$x_sgf, df$y_sgf, xlim = c(minVal, maxVal), ylim = c(minVal, maxVal))
# lines(
#     seq(minVal, maxVal, (maxVal-minVal) / 1000),
#     res$coefficients[1] + res$coefficients[2] * seq(minVal, maxVal, (maxVal-minVal) / 1000) + res$coefficients[3] * seq(minVal, maxVal, (maxVal-minVal) / 1000)**2 
# )
beta = res$coefficients
dfCorr[[run]][[elt]] = beta[1] + beta[2] * dfList[[run]][[elt]] + beta[3] * dfList[[run]][[elt]]**2


nameList = unique(dfCorr[[run]]$sample_name)
i = 2
tab = dfCorr[[run]][dfCorr[[run]]$sample_name == nameList[i]]
tab2 = dfList[[run]][dfList[[run]]$sample_name == nameList[i]]
plot(tab$dist, tab[[elt]], type = "l")
lines(tab2$dist, tab2[[elt]], col = "red")
i = i +1