#' Suggest algorithms based on Decision tree method.
#'
#' @return expected performance score for the dataset and algorithm
#'
#' @examples
#'
#' val <- suggest_learner_manual(data, "classsification", "Species")
#'
#' @export
suggest_learner_manual <-
    function(dataset, type = "regression", target) {
        # assign scores to each algorithm for number of characteristics from -10 to +10
        # When suggesting, keep adding scores based on requirements
        # select highest ranking algorithm for pipeline creation

        if (nrow(dataset) == 0) {
            stop("Empty Dataset")
        }

        # Manual Rules from spread sheet
        # Thanks to scikit-learn

        number_of_records <- nrow(dataset)

        if (number_of_records < 50) {
            message("Not enough data")
            return(list("Collect more records"))
        }

        requiredScores <- vector()

        if (category_pred(type)) {
            message("Catgorical Data")

            requiredScores <- c(requiredScores, "categorical_data")

            if (label_data(type)) {
                # ------ Classification Algorithms ------

                message("Labelled Data")
                requiredScores <- c(requiredScores, "labelled_data")

                if (number_of_records > 100000) {
                    message("Large dataset")
                    requiredScores <-
                        c(requiredScores, "large_data")

                    if (text_data()) {
                        message("Text Data")
                        requiredScores <-
                            c(requiredScores, "text_data")
                    } else {
                        message("Non textual data")
                    }
                } else {
                    message("Medium data")
                    requiredScores <-
                        c(requiredScores, "medium_data")
                }

                # ------ END OF Classification Algorithms ------
            } else {
                # ------ Clustering Algorithms ------
                message(" Non labelled data")
                requiredScores <-
                    c(requiredScores, "unlabelled_data")

                if (category_known(type)) {
                    message("Known categories")
                    requiredScores <-
                        c(requiredScores, "known_clusters")

                    if (number_of_records > 100000) {
                        message("Large dataset")
                        requiredScores <-
                            c(requiredScores, "large_data")
                    } else {
                        message("Medium data")
                        requiredScores <-
                            c(requiredScores, "medium_data")
                    }

                }
                else {
                    message("Unknown categories")
                    requiredScores <-
                        c(requiredScores, "unknown_clusters")

                    if (number_of_records > 100000) {
                        message("Large dataset")
                        requiredScores <-
                            c(requiredScores, "large_data")
                    } else {
                        message("Medium data")
                        requiredScores <-
                            c(requiredScores, "medium_data")
                    }

                    # ------ END OF Clustering Algorithms ------
                }
            }
        }

        else {
            message("Continous data")
            requiredScores <- c(requiredScores, "continous_data")

            # ------ Regression Algorithms

            if (number_of_records > 100000) {
                message("Large dataset")
                requiredScores <- c(requiredScores, "large_data")
            } else {
                message("Medium dataset")
                requiredScores <- c(requiredScores, "medium_data")
            }
            # ------ END OF Regression Algorithms
        }

        algorithms_manual <-
            read.csv("functions/algorithms_scoring.csv")
        algorithms_manual[is.na(algorithms_manual)] <- 0

        scoreboard <-
            algorithms_manual[, c(
                "algorithms_id",
                "algorithms_name",
                "meta_name",
                "meta_influenze",
                "type",
                requiredScores
            )]

        # Cleanign scoreboard
        scoreboard <-
            scoreboard[scoreboard$type == type |
                           scoreboard$type == "both",]
        scoreboard2 <- getProperties(dataset, type, target)
        mix <- scoreboard$algorithms_id

        if (type == "classification") {
            clean <- sub("classif.", "", scoreboard2$class)
        } else {
            clean <- sub("regr.", "", scoreboard2$class)
        }

        return(scoreboard[mix %in% clean, ])
    }


#' find if data is categorical.
#'
#' @export
category_pred <- function(type) {
    if (type != "regression") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' find if data is labelled
#'
#' @export
label_data <- function(type) {
    if (type != "classification") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' find if data is known category
#'
#' @export
category_known <- function(type) {
    if (type != "classification") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#' Get properties to filter algorithms.
#'
#' @export
getProperties <- function(data, type, target) {
    props <- vector()
    tempData <- data[,!(names(data) %in% target)]
    tempData <- factorPre(tempData)

    if (any(sapply(tempData, class) %in% c('numeric'))) {
        props <- c(props, "numerics")
    }

    if (any(sapply(tempData, class) %in% c('factor'))) {
        props <- c(props, "factors")
    }

    if (any(colMeans(is.na(tempData)) != 0)) {
        props <- c(props, "missings")
    }

    if (type == "classification") {
        temp <- as.factor(data[, target])
        if (length(levels(temp)) == 2) {
            props <- c(props, "twoclass")
        } else {
            props <- c(props, "multiclass")
        }
    }

    suppressWarnings(listLearners(
        ifelse(type == "classification", "classif", "regr"),
        properties = props,
        quiet = T,
        warn.missing.packages = T
    ))
}
