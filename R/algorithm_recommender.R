#' Suggest algorithms based on hybrid approach.
#'
#' @return expected performance score for the dataset and algorithms
#'
#' @examples
#'
#' val <- suggest_learner(data, "classsification", "Species")
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

        # get recommendations
        scoreboard <-
            suggest_learner_manual(dataset, type = type, predictor)
        temp <-
            suggest_learner_meta(dataset,
                                 type = type,
                                 predictor = predictor,
                                 scoreboard$meta_name)

        # add metainfluenzer score
        scoreboard$expected.accuracy <- temp
        scoreboard$expected.accuracy <-
            scoreboard$expected.accuracy * (scoreboard$meta_influenze / max(scoreboard$meta_influenze))
        scoreboard$expected.accuracy <-
            scoreboard$expected.accuracy * 10
        scoreboard$sum <- rowSums(scoreboard[, 6:ncol(scoreboard)])
        scoreboard <- scoreboard[order(-scoreboard$sum), ]

        message("Scores of algorithms: ")
        print(scoreboard)

        return(scoreboard)
    }
