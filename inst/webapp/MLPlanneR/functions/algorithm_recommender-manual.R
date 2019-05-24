# assign scores to each algorithm for number of characteristics from -10 to +10
# When suggesting, keep adding scores based on requirements
# select highest ranking algorithm for pipeline creation



# Decision tree method.
suggest_learner_manual <-
    function(dataset, type = "regression", target) {
        if (nrow(dataset) == 0) {
            stop("Empty Dataset")
        }

        # Manual Rules from spread sheet
        # Thanks to scikit-learn


        # TODO: Get input type and decide (text / audio)

        number_of_records <- nrow(dataset)

        if (number_of_records < 50) {
            print("Not enough data")
            return(list("Collect more records"))
        }

        requiredScores <- vector()

        if (category_pred(type)) {
            print("Catgorical Data")

            requiredScores <- c(requiredScores, "categorical_data")

            if (label_data(type)) {
                # ------ Classification Algorithms ------
                print("Labelled Data")
                requiredScores <- c(requiredScores, "labelled_data")

                if (number_of_records > 100000) {
                    print("Large dataset")
                    requiredScores <-
                        c(requiredScores, "large_data")

                    if (text_data()) {
                        print("Text Data")
                        requiredScores <-
                            c(requiredScores, "text_data")
                    } else {
                        print("Non textual data")
                    }
                } else {
                    print("Medium data")
                    requiredScores <-
                        c(requiredScores, "medium_data")
                }

                # ------ END OF Classification Algorithms ------
            } else {
                # ------ Clustering Algorithms ------
                print(" Non labelled data")
                requiredScores <-
                    c(requiredScores, "unlabelled_data")

                if (category_known(type)) {
                    print("Known categories")
                    requiredScores <-
                        c(requiredScores, "known_clusters")

                    if (number_of_records > 100000) {
                        print("Large dataset")
                        requiredScores <-
                            c(requiredScores, "large_data")
                    } else {
                        print("Medium data")
                        requiredScores <-
                            c(requiredScores, "medium_data")
                    }

                }
                else {
                    print("Unknown categories")
                    requiredScores <-
                        c(requiredScores, "unknown_clusters")

                    if (number_of_records > 100000) {
                        print("Large dataset")
                        requiredScores <-
                            c(requiredScores, "large_data")
                    } else {
                        print("Medium data")
                        requiredScores <-
                            c(requiredScores, "medium_data")
                    }

                    # ------ END OF Clustering Algorithms ------
                }
            }
        }

        else {
            print("Continous data")
            requiredScores <- c(requiredScores, "continous_data")

            # ------ Regression Algorithms

            if (number_of_records > 100000) {
                print("Large dataset")
                requiredScores <- c(requiredScores, "large_data")
            } else {
                print("Medium dataset")
                requiredScores <- c(requiredScores, "medium_data")
            }
            # ------ END OF Regression Algorithms
        }

        algorithms_manual <-
            read.csv("functions/algorithms scoring.csv")
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
                           scoreboard$type == "both", ]
        scoreboard2 <- getProperties(dataset, type, target)
        mix <- scoreboard$algorithms_id

        if(type == "classification"){
            clean <- sub("classif.", "", scoreboard2$class)
        } else {
            clean <- sub("regr.", "", scoreboard2$class)
        }

        return(scoreboard[mix %in% clean,])
    }


category_pred <- function(type) {
    if (type != "regression") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

label_data <- function(type) {
    if (type != "classification") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

category_known <- function(type) {
    if (type != "classification") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

getProperties <- function(data, type, target) {
    props <- vector()
    tempData <- data[, !(names(data) %in% target)]
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
