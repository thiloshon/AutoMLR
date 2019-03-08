library(tibble)

PipeLine <-
    setRefClass(
        "PipeLine",
        fields = list(
            learner = "character",
            preprocessing = "character",
            cross.validation = "character"
        ),
        methods = list(
            initialize = function(learner = character()) {
                .self$learner <- learner
            },

            addPreprocessing = function(preproc) {
                .self$preprocessing <- c(.self$preprocessing, preproc)
            },
            addValidation = function(validation) {
                .self$cross.validation <- c(.self$cross.validation, validation)
            },

            tuneParameters = function() {

            },

            train = function() {

            },

            test = function() {

            }



        )
    )



MLPlan <-
    setRefClass(
        "MLPlan",
        fields = list(
            data = "data.frame",
            type = "character",
            target = "character",
            data.meta = "list",
            ml.pipelines = "PipeLine"

        ),
        methods = list(
            initialize = function(data = tibble(),
                                  type = "classification",
                                  target) {
                .self$data <- as.data.frame(data)
                .self$type <- type
                .self$target <- target
                .self$data.meta <- getMeta(data)
            },

            getMeta = function(dataset) {
                dataset <- .self$data
                predictor <- .self$target

                if (.self$type == "classification") {
                    factored.predictor <- as.factor(as.vector(dataset[, predictor]))
                    classes.count <-
                        as.data.frame(table(factored.predictor))
                    classes.count <-
                        classes.count[order(-classes.count$Freq),]

                    .self$data.meta$number.of.classes <-
                        nrow(classes.count)
                    .self$data.meta$majority.class.size <-
                        classes.count[1, 2]
                    .self$data.meta$minority.class.size <-
                        classes.count[nrow(classes.count), 2]

                } else {
                    # Regression

                }

                .self$data.meta$number.of.features <- ncol(dataset)
                .self$data.meta$number.of.instances <- nrow(dataset)
                .self$data.meta$number.of.instances.with.missing.values <-
                    sum(complete.cases(dataset), na.rm = TRUE)
                .self$data.meta$number.of.missing.values <-
                    sum(as.vector(is.na(dataset)), na.rm = TRUE)
                .self$data.meta$number.of.numeric.features <-
                    sum(as.vector(unlist(lapply(
                        dataset, is.numeric
                    ))), na.rm = TRUE)
                .self$data.meta$number.of.symbolic.features <-
                    sum(as.vector(unlist(lapply(
                        dataset, is.factor
                    ))), na.rm = TRUE)



            },

            addPipe = function(pipeline) {
                # .self$ml.pipelines <- c(.self$ml.pipelines, pipeline)
            },

            learn = function() {

            },

            evaluate = function() {

            },

            deploy = function() {

            }

        )
    )
