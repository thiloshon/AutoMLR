library(tibble)
library(mlr)

#' MLPlan r6 class.
#'
#' @return MLPlan object with default settings
#'
#'
#' @export
MLPlan <-
    setRefClass(
        "MLPlan",
        fields = list(
            data = "data.frame",
            type = "character",
            target = "character",
            data.meta = "list",
            ml.pipelines = "list",
            evaluation = "character",
            results = "data.frame"
        ),
        methods = list(
            initialize = function(type = "classification") {
                .self$type <- type
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

                # add meta for report
                getMeta()
            },

            getMeta = function() {
                dataset <- .self$data
                predictor <- .self$target

                # get meta for classification data
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

                }

                # meta for general data
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
                pipeline$setData(.self$data)
                .self$ml.pipelines <-
                    c(.self$ml.pipelines, pipeline)
            },

            addEvaluation = function(eval) {
                .self$evaluation <- eval
            },

            # data resample based on data size
            split = function() {
                for (pipe in .self$ml.pipelines) {
                    if (length(pipe$train.split) == 0) {
                        pipe$addSplit(makeResampleDesc("Holdout", split = split_data(pipe$data)))
                    }
                }
            },

            # data preprocessing step
            preprocess = function() {
                for (pipe in .self$ml.pipelines) {
                    for (row in 1:nrow(pipe$preprocessing)) {
                        # call each preprocessing function with data as argumetn
                        dataTemp <-
                            do.call(pipe$preprocessing[row, 1],
                                    list(data = pipe$data, perform = T))
                        pipe$setData(dataTemp)
                    }
                }

                message("Data after preprocessing: ")
                print(head(.self$ml.pipelines[[1]]$data))
            },

            # Training and testing
            train = function() {
                configureMlr(on.learner.error = "warn")

                for (pipe in .self$ml.pipelines) {
                    if (length(pipe$mlr.task) == 0) {
                        # removing missing target records
                        dataTemp <-
                            subset(pipe$data, subset = !is.na(pipe$data[.self$target]))

                        # classification task
                        if (.self$type == "classification") {
                            classif.task = mlr::makeClassifTask(
                                id = pipe$id,
                                data = dataTemp,
                                target = .self$target
                            )
                            pipe$addMLRTask(classif.task)

                            # Classification lerner, set it up for predicting probabilities
                            classif.lrn = mlr::makeLearner(
                                pipe$learner,
                                predict.type = "response",
                                fix.factors.prediction = TRUE
                            )
                            pipe$addMLRLearner(classif.lrn)

                            # Train with resample and test
                            mod = mlr::resample(
                                classif.lrn,
                                classif.task,
                                pipe$train.split[[1]],
                                measures = list(ber, acc, timetrain)
                            )
                            pipe$addMLRModel(mod)

                        } else if (.self$type == "regression") {
                            # regression task
                            regr.task = mlr::makeRegrTask(
                                id = pipe$id,
                                data = dataTemp,
                                target = .self$target
                            )
                            pipe$addMLRTask(regr.task)

                            # regression learner
                            regr.lrn = mlr::makeLearner(pipe$learner)
                            pipe$addMLRLearner(regr.lrn)

                            # Train with resample and test
                            mod = mlr::resample(
                                regr.lrn,
                                regr.task,
                                pipe$train.split[[1]],
                                measures = list(mae, mse, timetrain)
                            )
                            pipe$addMLRModel(mod)
                        }
                    }
                }
            },

            # benchmarking trained models
            benchmark = function() {
                algorithms <-
                    read.csv("functions/algorithms scoring.csv")

                benchmark <- data.frame()

                for (pipe in .self$ml.pipelines) {
                    # get aggregated performance values
                    temp <- pipe$mlr.model[[1]]$aggr

                    temp$algo  <- pipe$mlr.model[[1]]$task.id
                    temp$totalTime <- pipe$mlr.model[[1]]$runtime
                    temp$name <-
                        algorithms[which(algorithms$algorithms_id == pipe$mlr.model[[1]]$task.id), 2]

                    benchmark <-
                        rbind(benchmark, temp, stringsAsFactors = F)
                }
                .self$results <- benchmark

                return(benchmark)
            },

            # tostring
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
            }

        )
    )


#' PipeLine r6 class.
#'
#' @return PipeLine object with default settings
#'
#'
#' @export
PipeLine <-
    setRefClass(
        "PipeLine",
        fields = list(
            id = "character",
            learner = "character",
            preprocessing = "data.frame",
            cross.validation = "character",
            data = "data.frame",
            train.split = "list",
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

            setData = function(data) {
                .self$data <- data
            },

            addSplit = function(resample) {
                .self$train.split <- list(resample)
            },

            addPreprocessing = function(preproc) {
                .self$preprocessing <- preproc
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

            printSelf = function() {
                cat("Machine Learning Pipeline Object")
                cat('\n')
                cat('\n')

                cat("ID: ")
                cat(.self$id)
                cat('\n')

                cat("Learning Algorithm: ")
                print(.self$learner)

                cat("Preprocessing List: ")
                print(.self$preprocessing)
                cat("")

                cat("Train, Test, Cross Validation Split: ")
                print(.self$train.split)
                cat("")

                cat("#####  MLR Task: ##### ")
                print(.self$mlr.task)
                cat("")

                cat("#####  MLR Learner: ##### ")
                print(.self$mlr.learner)
                cat("")

                cat("#####  MLR Model: ##### ")
                print(.self$mlr.model)
                cat("")
            }
        )
    )
