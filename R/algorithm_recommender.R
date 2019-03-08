# Decision tree method.
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

        scoreboard$expected.accuracy <-
            suggest_learner_meta(dataset, type = type, predictor = predictor)


        scoreboard$sum <- rowSums(scoreboard[, 3:ncol(scoreboard)])

        scoreboard <- scoreboard[order(-scoreboard$sum),]

        return(scoreboard)
    }
