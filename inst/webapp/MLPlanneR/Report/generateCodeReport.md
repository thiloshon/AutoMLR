R Code of the Session, independent of automlr package
-----------------------------------------------------

    ## Loading Packages

    library(mlr)

    # Please pass your tabular data here

    data_original <- input_data_goes_here

    target <- " Species "

    ################# Model 1 ################# 

    data <- subset(data_original, subset = !is.na(data_original[target]))

    resample <- mlr::makeResampleDesc("Holdout", split = 0.6)


    # Task
    learning.task <- mlr::makeClassifTask(id = xgboost data = data, target = target)


    # Learner
    learner <- mlr::makeLearner( classif.xgboost predict.type = "response", fix.factors.prediction = TRUE)


    # Training and Testing
    mod = mlr::resample(learner, learning.task, resample, measures = list(mmce, acc, timetrain))
