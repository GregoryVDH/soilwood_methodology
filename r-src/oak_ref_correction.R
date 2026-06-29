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

run_ref = 10

run_seq = seq_len(n)
run_seq[which(run_seq == run_ref)] = NA
run_seq = run_seq[!is.na(run_seq)]
for (elt in 3:19) {
    for (run in run_seq) {
        cat("\r", "Element: ", (elt-2), " / 17 - run: ", which(run == run_seq), " / 11")
        # Find minimum and maximum value for all samples in the series
        minVal = 1e12
        maxVal = -1e12
        t = dfList[[run]][[elt]]
        minVal = min(c(minVal, min(t, na.rm = TRUE)))
        maxVal = max(c(maxVal, max(t, na.rm = TRUE)))
        rm(t)

        nrows = min_nrow(dfList[[run]], dfList[[run_ref]])
        df = data.table(
            dist = dfList[[run]]$dist[dfList[[run]]$sample_name == "Oak_ref"][1:nrows], 
            x = dfList[[run]][[elt]][dfList[[run]]$sample_name == "Oak_ref"][1:nrows],
            y = dfList[[run_ref]][[elt]][dfList[[run_ref]]$sample_name == "Oak_ref"][1:nrows]
        )
        df = df[df$dist < 30] 

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

        beta = res$coefficients
        dfCorr[[run]][[elt]] = beta[1] + beta[2] * dfList[[run]][[elt]] + beta[3] * dfList[[run]][[elt]]**2
    }
}
cat("\n")
if (!dir.exists("./data/data_martin/la-icpms/mean_profile_oakRefCorr")) {
    dir.create("./data/data_martin/la-icpms/mean_profile_oakRefCorr")
}

for (i in seq_len(n)) {
    data.table::fwrite(
        x = dfCorr[[i]], 
        file = paste0("./data/data_martin/la-icpms/mean_profile_oakRefCorr/", fileList[i]),
        append = FALSE,
        sep = ";",
        row.names = FALSE,
        col.names = TRUE
    )
}
