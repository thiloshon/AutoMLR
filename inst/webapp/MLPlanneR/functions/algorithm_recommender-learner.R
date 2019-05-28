#' Suggest algorithms based on meta learninng technique.
#'
#' @return expected performance score for the dataset and algorithm
#'
#' @examples
#'
#' val <- suggest_learner_meta(data, "classsification", "Species", "nnet")
#'
#' @export
suggest_learner_meta <-
    function(dataset, type = "classification", predictor, algorithms) {
        # load learned model
        load("functions/algorithm-recommender-model.RData")

        if (is.na(predictor)) {
            stop("Give a predictor variable")
        }

        expec_perf <- 7
        dataset <- as.data.frame(dataset)
        factored.predictor <- as.factor(dataset[, predictor])

        # Create meta to pass for prediction
        classes.count <- as.data.frame(table(factored.predictor))
        classes.count <- classes.count[order(-classes.count$Freq),]

        majority.class.size <- classes.count[1, 2]
        minority.class.size <- classes.count[nrow(classes.count), 2]

        max.nominal.att.distinct.values <- 5
        number.of.classes <- nrow(classes.count)
        number.of.features <- ncol(dataset)
        number.of.instances <- nrow(dataset)
        number.of.instances.with.missing.values <-
            sum(complete.cases(dataset), na.rm = TRUE)
        number.of.missing.values <-
            sum(as.vector(is.na(dataset)), na.rm = TRUE)
        number.of.numeric.features <-
            sum(as.vector(unlist(lapply(
                dataset, is.numeric
            ))), na.rm = TRUE)

        number.of.symbolic.features <-
            sum(as.vector(unlist(lapply(
                dataset, is.factor
            ))), na.rm = TRUE)


        # Create prediction dataset
        learners <-
            data.frame(
                majority.class.size  = rep(majority.class.size, length(algorithms)),
                minority.class.size  = rep(minority.class.size, length(algorithms)),
                max.nominal.att.distinct.values  = rep(max.nominal.att.distinct.values, length(algorithms)),
                number.of.classes  = rep(number.of.classes, length(algorithms)),
                number.of.features  = rep(number.of.features, length(algorithms)),
                number.of.instances  = rep(number.of.instances, length(algorithms)),
                number.of.instances.with.missing.values  = rep(
                    number.of.instances.with.missing.values,
                    length(algorithms)
                ),
                number.of.missing.values  = rep(number.of.missing.values, length(algorithms)),
                number.of.numeric.features  = rep(number.of.numeric.features, length(algorithms)),
                number.of.symbolic.features  = rep(number.of.symbolic.features, length(algorithms))

            )

        learners$flow_name_fixed <- algorithms

        # get expected performance
        predicted.accuracies <- tryCatch({
            predict(nnet.model, newdata = learners)
        },

        error = function(error_message) {
            return(expec_perf)
        })

        return(expec_perf)
    }
