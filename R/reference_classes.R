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

            printSelf = function() {
                print("Machine Learning Plan Object")
                print("Data: ")
                print(.self$data)
                print("Type: ")
                print(.self$type)
                print("Target: ")
                print(.self$target)
                print("Data Meta: ")
                print(.self$data.meta)
                print("Pipelines: ")

                for (pipe in .self$ml.pipelines) {
                    pipe$printSelf()
                }
            },

            addData = function(data) {
                .self$data <- as.data.frame(data)
            },

            addTarget = function(target) {
                if (is.numeric(target)) {
                    .self$target <- names(.self$data)[target]
                } else {
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
                        classes.count[order(-classes.count$Freq), ]

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

            train = function() {
                for (pipe in .self$ml.pipelines) {

                    if (.self$type == "classification"){
                        classif.task = mlr::makeClassifTask(id = pipe$id, data = .self$data, target = .self$target)
                        pipe$addMLRTask(classif.task)

                        # Classification tree, set it up for predicting probabilities
                        classif.lrn = mlr::makeLearner(pipe$learner, predict.type = "response", fix.factors.prediction = TRUE)
                        pipe$addMLRLearner(classif.lrn)

                        mod = mlr::train(classif.lrn, classif.task)
                        pipe$addMLRModel(mod)

                    } else if(.self$type == "regression"){
                        regr.task = mlr::makeRegrTask(id = pipe$id, data = .self$data, target = .self$target)
                        pipe$addMLRTask(regr.task)

                        # Regression gradient boosting machine, specify hyperparameters via a list
                        regr.lrn = mlr::makeLearner(pipe$learner)
                        pipe$addMLRLearner(regr.lrn)

                        mod = mlr::train(regr.lrn, regr.task)
                        pipe$addMLRModel(mod)
                    }
                }
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
            train.split = "numeric",
            mlr.task = "list",
            mlr.learner = "list",
            mlr.model = "list"
        ),
        methods = list(
            initialize = function(learner = character(),
                                  id = paste("model-", as.character(sample(1:10 ^ 6, 1)), sep = "")) {
                .self$id <- id
                .self$learner <- learner
            },

            addSplit = function(percentage) {
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

            addMLRTask = function(mlr.task) {
                .self$mlr.task <- list(mlr.task)
            },

            addMLRLearner = function(mlr.learner) {
                .self$mlr.learner <- list(mlr.learner)
            },

            addMLRModel = function(mlr.model) {
                .self$mlr.model <- list(mlr.model)
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

            },
            printSelf = function() {
                print("Machine Learning Pipeline Object")
                print("ID: ")
                print(.self$id)
                print("Learner: ")
                print(.self$learner)
                print("Preprocessing List: ")
                print(.self$preprocessing)
                print("Cross Validation: ")
                print(.self$cross.validation)
                print("Data: ")
                print(.self$data)
                print("Train Split: ")
                print(.self$train.split)

                print("Task: ")
                print(.self$mlr.task)
                print("Learner: ")
                print(.self$mlr.learner)
                print("Model: ")
                print(.self$mlr.model)
            }



        )
    )





plan <-
    MLPlan(type = "classification")
plan$addData(sample.data.iris)
plan$addTarget("Species")

plan$addPipe(pipe)
