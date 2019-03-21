library(tibble)
library(mlr)

MLPlan <-
    setRefClass(
        "MLPlan",
        fields = list(
            data = "data.frame",
            type = "character",
            target = "character",
            data.meta = "list",
            ml.pipelines = "list"

        ),
        methods = list(
            initialize = function(type = "classification") {
                .self$type <- type
            },

            addData = function(data) {
                .self$data <- as.data.frame(data)
            },

            addTarget = function(target) {
                print(target)
                print(is.numeric(target))

                if (is.numeric(target)){
                    .self$target <- names(.self$data)[target]
                }else {
                    .self$target <- target
                }

                getMeta()
            },

            getMeta = function() {
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
                .self$ml.pipelines <- c(.self$ml.pipelines, pipeline)
            },

            preprocess = function() {
                for (pipe in .self$ml.pipelines) {
                    for (preproc in pipe$preprocessing) {

                    }

                }
            },

            learn = function() {

            },

            evaluate = function() {

            },

            deploy = function() {

            }

        )
    )

PipeLine <-
    setRefClass(
        "PipeLine",
        fields = list(
            id = "character",
            learner = "character",
            preprocessing = "character",
            cross.validation = "character",
            data = "data.frame",
            train.split = "numeric"
        ),
        methods = list(
            initialize = function(learner = character(),
                                  id = paste("model-", as.character(sample(1:10 ^ 6, 1)), sep = "")) {
                .self$id <- id
                .self$learner <- learner
            },

            addSplit = function(percentage){
                .self$train.split <- percentage
            },

            addPreprocessing = function(preproc, target = NULL) {
                if (is.null(target)) {
                    .self$preprocessing <- c(.self$preprocessing, preproc)
                } else {
                    .self$preprocessing <-
                        c(.self$preprocessing, paste(preproc, target, sep = "~"))
                }
            },
            addValidation = function(validation) {
                .self$cross.validation <- c(.self$cross.validation, validation)
            },


            updateRows = function(oldRows, newRows) {
                if (is.numeric(oldRows)) {
                    #remove by index
                } else if (is.logical(oldRows)) {
                    #remove by logical
                }
            },

            removeRows = function(rows) {

            },

            updateCols = function(target, newCols) {
                if (is.numeric(oldRows)) {
                    #remove by index
                } else if (is.logical(oldRows)) {
                    #remove by logical
                }
            },

            removeCols = function(target, rows) {

            },

            tuneParameters = function() {

            },

            train = function() {

            },

            test = function() {

            }



        )
    )





plan <-
    MLPlan(type = "classification")
plan$addData(sample.data.iris)
plan$addTarget("Species")

plan$addPipe(pipe)
