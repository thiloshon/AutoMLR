split_data <- function(data) {
    if (nrow(data) < 500) {
        message("Small Data detected")
        return(0.9)
    } else if (nrow(data) < 50000) {
        message("Medium data detected")
        return(0.8)
    } else if (nrow(data) < 120000) {
        message("Large data detected")
        return(0.7)
    } else {
        message("Extremely large data detected")
        return(0.6)
    }
}

recommend_preprocessing <- function(data, algorithm, breed) {
    preproc <-
        data.frame(
            id = character(),
            label = character(),
            applied_on = character(),
            pre_split = logical(),
            stringsAsFactors = F
        )

    pre01 <- outlierPre(data, F)
    if (length(pre01) > 0){
        preproc[1,] <- c("outlier-detection", "Outlier Removal", paste(pre01, collapse = ", "), TRUE)
        preproc[2,] <- c("outlier-detection2", "Outlier Removal2", paste(pre01, collapse = ", "), TRUE)
    }

    return(preproc)
}

outlierPre <- function(data, perform = T) {
    # beyond 97th %ile based on chi-squared scores
    # (squares of differences between values and mean divided by variance)
    outliers <-
        outliers::scores(data[, sapply(data, is.numeric)] , type = "chisq", prob = 0.97)

    if (perform) {
        return(data[!outliers, ])
    } else {
        return(colnames(outliers)[colSums(outliers == T, na.rm = T) > 0])
    }
}

factor <- function(data) {
    for (var in colnames(data)) {
        # factorization score algorithm
        # if values are binary or
        # distinct values / records is lower than threshold
        # it has high probability of being a factor variable
        distinct_vals <- unique(data[, var])
        score <- distinct_vals / nrow(data)

        if (distinct_vals == 2 | score < 0.05) {
            data[, var] <- as.factor(data[, var])
        }
    }

    return(data)
}

impute <- function(data) {
    na_ratio <- as.data.frame(colMeans(is.na(data)))
    na_ratio <- na_ratio[(na_ratio[, 2] != 0), ]

    cols <- na.ratio[(na_ratio[, 2] < 0.45), 1]

    imp <-
        impute(
            data,
            target = cols,
            classes = list(factor = imputeMode(), integer = imputeMean()),
            dummy.classes = c("integer", "factor"),
            dummy.type = "numeric"
        )

    return(imp$data)
}

normalize <- function(data){
    cols <- vector()
    for (var in colnames(data)) {
        skew <- e1071::skewness(data[,var])
        if (abs(skew) > 0.4){
            cols <- c(cols, var)
        }
    }

    trainTask <- normalizeFeatures(data, target = cols ,method = "standardize")
}



