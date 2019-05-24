#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
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

recommend_evaluation <- function(data, type, target) {
    if (type == "classification") {
        factored.predictor <- as.factor(as.vector(data[, target]))

        classes.count <- as.data.frame(table(factored.predictor))
        classes.count <- classes.count[order(-classes.count$Freq), ]

        majority.class.size <- classes.count[1, 2]
        minority.class.size <- classes.count[nrow(classes.count), 2]

        imbalanceRatio <- minority.class.size / majority.class.size

        if (imbalanceRatio > 0.4){
            return("Accuracy")
        } else {
            return("Balanced Error Rate")
        }


    } else {
        return("Mean Absolute Error")
    }
}

#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
recommend_preprocessing <- function(data, algorithm, breed) {
    preproc <-
        data.frame(
            id = character(),
            label = character(),
            applied_on = character(),
            pre_split = logical(),
            stringsAsFactors = F
        )

    pre <- identifierPre(data, F)
    if (length(pre) > 0) {
        preproc[nrow(preproc) + 1,] <-
            c("identifierPre",
              "Removing Identifiers",
              paste(pre, collapse = ", "),
              TRUE)
    }

    pre <- factorPre(data, F)
    if (length(pre) > 0) {
        preproc[nrow(preproc) + 1,] <-
            c("factorPre",
              "Factorization",
              paste(pre, collapse = ", "),
              TRUE)
    }

    pre <- outlierPre(data, F)
    if (length(pre) > 0) {
        preproc[nrow(preproc) + 1,] <-
            c("outlierPre",
              "Outlier Removal",
              paste(pre, collapse = ", "),
              TRUE)
    }

    pre <- imputePre(data, F)
    if (length(pre) > 0) {
        preproc[nrow(preproc) + 1,] <-
            c("imputePre",
              "Imputing Missing Values",
              paste(pre, collapse = ", "),
              TRUE)
    }

    pre <- skewPre(data, F)
    if (length(pre) > 0) {
        preproc[nrow(preproc) + 1,] <-
            c("skewPre",
              "Fixing Skeweness",
              paste(pre, collapse = ", "),
              TRUE)
    }

    pre <- normalizePre(data, F)
    if (length(pre) > 0) {
        preproc[nrow(preproc) + 1,] <-
            c("normalizePre",
              "Normalization ",
              paste(pre, collapse = ", "),
              TRUE)
    }

    if(breed){

    }

    return(preproc)
}

#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
outlierPre <- function(data, perform = T) {
    # beyond 97th %ile based on chi-squared scores
    # (squares of differences between values and mean divided by variance)
    outliers <-
        outliers::scores(data[, sapply(data, is.numeric)] , type = "chisq", prob = 0.97)

    if (perform) {
        cols <-
            colnames(data)[sapply(data, class) %in% c('integer', 'numeric')]

        for (col in cols) {
            outliers <- boxplot(data[, c(col)], plot = FALSE)$out
            temp <- data[, col]
            log <- temp %in% outliers
            temp[log] <- NA
            data[, col] <- temp
        }

        d <- imputePre(data)

        return(d)

    } else {
        return(colnames(outliers)[colSums(outliers == T, na.rm = T) > 0])
    }
}

#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
factorPre <- function(data, perform = T) {
    retVar <- vector()

    for (var in colnames(data)) {
        # factorization score algorithm
        # if values are binary or
        # distinct values / records is lower than threshold
        # it has high probability of being a factor variable
        distinct_vals <- length(unique(data[, var]))
        score <- distinct_vals / nrow(data)

        if (perform) {
            # if (distinct_vals == 2 | score < 0.05) {
            if (distinct_vals < 5 | score < 0.005) {
                data[, var] <- as.factor(data[, var])
            }
        } else {
            if (distinct_vals < 5 | score < 0.005) {
                retVar <- c(retVar, var)
            }
        }
    }

    if (perform) {
        return(data)
    } else {
        return(retVar)
    }
}

#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
imputePre <- function(data, perform = T) {
    # columns <-
    #     sapply(data, class) %in% c('integer', 'factor', 'numeric')

    na_ratio <- as.data.frame(colMeans(is.na(data)))
    na_ratio$cols <- colnames(na_ratio)

    if (!any(na_ratio[, 1] != 0)) {
        # No missing values in dataset
        return()
    }

    na_ratio <- na_ratio[(na_ratio[, 1] != 0), ]

    cols <- rownames(na_ratio)[(na_ratio[, 1] < 0.45)]

    if (perform) {
        imp <-
            impute(
                data,
                classes = list(
                    factor = imputeMode(),
                    integer = imputeMean(),
                    numeric = imputeMedian()
                ),
                dummy.classes = c("integer", "factor"),
                dummy.type = "numeric"
            )
        return(imp$data)
    } else {
        return(cols)
    }
}

#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
identifierPre <- function(data, perform = T) {
    names <- tolower(colnames(data))
    cols <- names[endsWith(names, "id")]

    if (perform) {
        return(data[,!(names(data) %in% cols)])
    } else {
        return(cols)
    }
}

#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
skewPre <- function(data, perform = T) {
    cols <-
        colnames(data)[sapply(data, class) %in% c('integer', 'numeric')]
    returnCol <- vector()

    for (var in cols) {
        skew <- e1071::skewness(data[, var], na.rm = T)
        if (!is.na(skew) & abs(skew) > 0.5) {
            returnCol <- c(returnCol, var)
        }
    }

    if (perform) {
        for (var in cols) {
            skew <- e1071::skewness(data[, var], na.rm = T)
            if (!is.na(skew) & abs(skew) > 0.4) {
                data[,var] <- log1p(data[,var])
            }
        }
        return(data)

    } else {
        return(returnCol)
    }

}

#' Create the package default Questionnaire.
#'
#' @return BdQuestionContainer object with default Questions
#'
#' @examples
#'
#' customQuestionnaire <- create_default_questionnaire()
#'
#' @export
normalizePre <- function(data, perform = T) {
    cols <-
        colnames(data)[sapply(data, class) %in% c('integer', 'numeric')]

    if (perform) {
        dat <-
            normalizeFeatures(data , method = "standardize")
        return(dat)
    } else {
        return(cols)
    }
}

insignificancePre <- function(data, perform = T) {
    data <- data[, sapply(data, class) %in% c('integer', 'numeric')]
    print(abs(cor(data)))
    generateFilterValuesData(trainTask, method = c("information.gain", "chi.squared"))
}
