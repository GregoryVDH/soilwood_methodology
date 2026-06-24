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