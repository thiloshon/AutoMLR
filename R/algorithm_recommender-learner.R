suggest_learner_meta <-
    function(dataset, type = "classification", predictor) {
        load("inst/algorithm-recommender-model.RData")

        if (is.na(predictor)) {
            stop("Give a predictor variable")
        }

        factored.predictor <- as.factor(dataset[,predictor])

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
            sum(as.vector(unlist(
                lapply(OpenMLMasterTrainingDataNoNAs, is.numeric)
            )), na.rm = TRUE)

        number.of.symbolic.features <-
            sum(as.vector(unlist(
                lapply(OpenMLMasterTrainingDataNoNAs, is.factor)
            )), na.rm = TRUE)


        #
        learners <-
            data.frame(
                majority.class.size  = rep(majority.class.size, 107),
                minority.class.size  = rep(minority.class.size, 107),
                max.nominal.att.distinct.values  = rep(max.nominal.att.distinct.values, 107),
                number.of.classes  = rep(number.of.classes, 107),
                number.of.features  = rep(number.of.features, 107),
                number.of.instances  = rep(number.of.instances, 107),
                number.of.instances.with.missing.values  = rep(number.of.instances.with.missing.values, 107),
                number.of.missing.values  = rep(number.of.missing.values, 107),
                number.of.numeric.features  = rep(number.of.numeric.features, 107),
                number.of.symbolic.features  = rep(number.of.symbolic.features, 107)

            )

        learners$flow_name_fixed <- unique(OpenMLMasterTrainingDataNoNAs$flow_name_fixed)

        predicted.accuracies <- predict(nnet.model, newdata = learners)

        return(predicted.accuracies)

    }
