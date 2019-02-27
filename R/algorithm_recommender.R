set.seed(1)

library(data.table)
library(rpart)
library(rpart.plot)


OpenMLRunEvaluationsData <- as.data.table(data01)
MLRRunEvaluationsData <-
    OpenMLRunEvaluationsData[grepl("mlr", OpenMLRunEvaluationsData$flow_name), ]

setnames(OpenMLRunEvaluationsData, "function.", "fun")

wideDataFormula = run_id + task_id + flow_name + setup_id + flow_id + data_name + upload_time ~ fun

OpenMLWideData = data.table::dcast(
    data = MLRRunEvaluationsData,
    formula = wideDataFormula,
    value.var = c("value"),
    fun.aggregate = mean
)

OpenMLMasterData <-
    merge(x = OpenMLWideData,
          y = tasks,
          by.x = "task_id",
          by.y = "task.id")



# Removing unwanted fields
OpenMLMasterData <-
    OpenMLMasterData[, c(-1, -2, -4,-6,-7,    -9,-24,-25,-30,-31,-32,-33)]

OpenMLMasterData <-
    OpenMLMasterData[, c(-1, -2, -4,-6,-7, -44, -45, -46, -47)]


colMeans(is.na(OpenMLMasterData))

# Removing mostly empty fields
OpenMLMasterData <-
    OpenMLMasterData[, c(1, 2, 3, 21 , 22, 31:40)]

OpenMLMasterData <-
    OpenMLMasterData[, c(1, 2, 3, 38, 41, 42, 50:59  )]


OpenMLMasterData$flow_id <- as.factor(OpenMLMasterData$flow_id)
OpenMLMasterData$flow_name <- as.factor(OpenMLMasterData$flow_name)
OpenMLMasterData$task.type <- as.factor(OpenMLMasterData$task.type)
OpenMLMasterData$estimation.procedure <-
    as.factor(OpenMLMasterData$estimation.procedure)
OpenMLMasterData$target.feature <-
    as.factor(OpenMLMasterData$target.feature)


# adding augmented columns
OpenMLMasterData$flow_name_fixed <-
    gsub("\\s*\\([^\\)]+\\)", "", OpenMLMasterData$flow_name)
OpenMLMasterData$flow_name_fixed <-
    as.factor(OpenMLMasterData$flow_name_fixed)

OpenMLMasterData$AreaUnderCurveRounded <-
    round(OpenMLMasterData$area_under_roc_curve * 10)

# Data Splitting
training <-
    sample(nrow(OpenMLMasterData), nrow(OpenMLMasterData) * 0.8)

OpenMLMasterTrainingData <- OpenMLMasterData[training, ]
OpenMLMasterTestingData <- OpenMLMasterData[-training, ]



# Training

predictorFormula <-
    flow_name_fixed ~
    AreaUnderCurveRounded +
    majority.class.size +
    max.nominal.att.distinct.values +
    minority.class.size +
    number.of.classes +
    number.of.features +
    number.of.instances +
    number.of.instances.with.missing.values +
    number.of.missing.values +
    number.of.numeric.features +
    number.of.symbolic.features

predictorFormulaNaive <-
    flow_name_fixed ~
    AreaUnderCurveRounded +
    majority.class.size +
    minority.class.size +
    number.of.classes +
    number.of.features +
    number.of.instances +
    number.of.instances.with.missing.values +
    number.of.missing.values +
    number.of.numeric.features +
    number.of.symbolic.features

predictorFormulaNaive <-
    area_under_roc_curve ~
    flow_name_fixed +
    majority.class.size +
    minority.class.size +
    number.of.classes +
    number.of.features +
    number.of.instances +
    number.of.instances.with.missing.values +
    number.of.missing.values +
    number.of.numeric.features +
    number.of.symbolic.features


fit <-
    rpart(
        predictorFormulaNaive,
        method = "anova",
        data = OpenMLMasterTrainingData
    )

fit2 <-
    rpart(
        predictorFormulaNaive,
        method = "anova",
        data = OpenMLMasterTrainingData,
        control = rpart.control(cp = 0.01)
    )

rpart.plot::rpart.plot(fit2, extra = 100)


Prediction <- predict(fit, OpenMLMasterTestingData, type = "anova")
submit <- data.frame(AUC.actual = OpenMLMasterTestingData$area_under_roc_curve, AUC.predicted = Prediction)



library(mlr)


OpenMLMasterTrainingDataNoNAs <- OpenMLMasterTrainingData[complete.cases(OpenMLMasterTrainingData[,c(-7)]), ]
OpenMLMasterTestingDataNoNAs <- OpenMLMasterTestingData[complete.cases(OpenMLMasterTestingData[,c(-7)]), ]

OpenMLMasterTrainingDataNoNAs$flow_name_fixed = droplevels(OpenMLMasterTrainingDataNoNAs$flow_name_fixed)
id <- which(!(OpenMLMasterTestingDataNoNAs$flow_name_fixed %in% levels (OpenMLMasterTrainingDataNoNAs$flow_name_fixed)))
OpenMLMasterTestingDataNoNAs$flow_name_fixed[id] <- NA

OpenMLMasterTestingDataNoNAs <- OpenMLMasterTestingDataNoNAs[complete.cases(OpenMLMasterTestingDataNoNAs[,c(-7)]), ]


trainTask <- makeRegrTask(data = OpenMLMasterTrainingDataNoNAs,target = "area_under_roc_curve")
testTask <- makeRegrTask(data = OpenMLMasterTestingDataNoNAs, target = "area_under_roc_curve")

trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

trainTask <- dropFeatures(task = trainTask,features = c("X","flow_name", "flow_id", "task.type"))
trainTask <- dropFeatures(task = trainTask,features = c("evaluation.measures"))


logistic.learner <- makeLearner("regr.nnet", predict.type = "response")
# nnet regr.fnn regr.gbm  regr.glm  kknn    regr.ranger  regr.xgboost  regr.ksvm
fmodel <- train(logistic.learner,trainTask)

fpmodel <- predict(fmodel, testTask)

submit <- data.frame(AUC.actual = OpenMLMasterTestingDataNoNAs$area_under_roc_curve, AUC.predicted = fpmodel$data$response)

d <- submit[order(submit$AUC.actual), ]

d$index <- 1:dim(d)[1]
ggplot(data=d, aes(index)) +
    geom_line(aes(y = AUC.actual, colour = "AUC.actual")) +
    geom_line(aes(y = AUC.predicted, colour = "AUC.predicted")) +
    ylim(0, 1)


