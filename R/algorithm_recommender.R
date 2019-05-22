


#' Recommending learners based on dataset.
#'
#' Recommends machine learning algorithm best fitting for the given
#' dataset and learning type.
#'
#' @param dataset The dataset to train as a data frame
#' @param type Type of machine learning, either regression or classsification
#' @param predictor The target feature as character
#'
#' @return data frame of algorithms and thier ranks
#'
#' @examples
#'
#' ranking <- suggest_learner(iris, "classification", "Species")
#' ranking <- suggest_learner(cars, "regression", "mpg")
#'
#' @export
suggest_learner <-
    function(dataset, type = "regression", predictor) {
        if (nrow(dataset) == 0) {
            stop("Empty Dataset")
        }

        number_of_records <- nrow(dataset)

        if (number_of_records < 50) {
            print("Not enough data")
            return(list("Collect more records"))
        }


        scoreboard <- suggest_learner_manual(dataset, type = type)

        # removing non type algorithms

        scoreboard <- scoreboard[scoreboard$type == type | scoreboard$type == "both", ]


        #scoreboard$expected.accuracy <-
            #suggest_learner_meta(dataset, type = type, predictor = predictor, scoreboard$meta_name)

        print(scoreboard)


        scoreboard$sum <- rowSums(scoreboard[, 6:ncol(scoreboard)])

        scoreboard <- scoreboard[order(-scoreboard$sum),]

        return(scoreboard)
    }
