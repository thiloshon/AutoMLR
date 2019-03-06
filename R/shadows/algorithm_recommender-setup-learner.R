set.seed(1)
options(scipen = 999)

library(data.table)
library(rpart)
library(rpart.plot)
library(dplyr)
library(OpenML)
library(mlr)

data01 <- read.csv("./data/OpenMLRunEvaluationsSet01.csv")
data02 <- read.csv("./data/OpenMLRunEvaluationsSet02.csv")

data01 <- rbind.fill(data01, data02)

OpenMLRunEvaluationsData <- as.data.table(data01)
MLRRunEvaluationsData <-
    OpenMLRunEvaluationsData[grepl("mlr", OpenMLRunEvaluationsData$flow_name), ]

setnames(MLRRunEvaluationsData, "function.", "fun")

wideDataFormula = run_id + task_id + flow_name + setup_id + flow_id + data_name + upload_time ~ fun

OpenMLWideData = data.table::dcast(
    data = MLRRunEvaluationsData,
    formula = wideDataFormula,
    value.var = c("value"),
    fun.aggregate = mean
)

tasks <- listOMLTasks(limit = NULL)

OpenMLMasterData <-
    merge(x = OpenMLWideData,
          y = tasks,
          by.x = "task_id",
          by.y = "task.id")


# Removing unwanted fields
OpenMLMasterData <-
    OpenMLMasterData[, c(-1, -2, -4,-6,-7, -9,-24,-25,-30,-31,-32,-33)] # remove task type and target feature as well


colMeans(is.na(OpenMLMasterData))

# Removing mostly empty fields
OpenMLMasterData <-
    OpenMLMasterData[, c(1, 2, 3, 22, 31:40)]


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
    sample(nrow(OpenMLMasterTrainingDataNoNAs), nrow(OpenMLMasterTrainingDataNoNAs) * 0.5)

OpenMLMasterTrainingData <- OpenMLMasterData[training, ]
OpenMLMasterTestingData <- OpenMLMasterData[-training, ]

OpenMLMasterTrainingDataNoNAs <- OpenMLMasterTrainingDataNoNAs[training, ]



# Training

predictorFormulaClassification <-
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

predictorFormulaRegression <-
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


# fit <-
#     rpart(
#         predictorFormulaNaive,
#         method = "anova",
#         data = OpenMLMasterTrainingData
#     )
#
# fit2 <-
#     rpart(
#         predictorFormulaNaive,
#         method = "anova",
#         data = OpenMLMasterTrainingData,
#         control = rpart.control(cp = 0.01)
#     )
#
# rpart.plot::rpart.plot(fit2, extra = 100)
#
#
# Prediction <- predict(fit, OpenMLMasterTestingData, type = "anova")
# submit <- data.frame(AUC.actual = OpenMLMasterTestingData$area_under_roc_curve, AUC.predicted = Prediction)



library(mlr)


OpenMLMasterTrainingDataNoNAs <- OpenMLMasterTrainingData[complete.cases(OpenMLMasterTrainingData), ]
OpenMLMasterTestingDataNoNAs <- OpenMLMasterTestingData[complete.cases(OpenMLMasterTestingData[,c(-3)]), ]

OpenMLMasterTrainingDataNoNAs$flow_name_fixed = droplevels(OpenMLMasterTrainingDataNoNAs$flow_name_fixed)
id <- which(!(OpenMLMasterTestingDataNoNAs$flow_name_fixed %in% levels (OpenMLMasterTrainingDataNoNAs$flow_name_fixed)))
OpenMLMasterTestingDataNoNAs$flow_name_fixed[id] <- NA

OpenMLMasterTestingDataNoNAs <- OpenMLMasterTestingDataNoNAs[complete.cases(OpenMLMasterTestingDataNoNAs[,c(-7)]), ]


trainTask <- makeRegrTask(data = OpenMLMasterTrainingDataNoNAs,target = "AreaUnderCurveRounded")
testTask <- makeRegrTask(data = OpenMLMasterTestingDataNoNAs, target = "AreaUnderCurveRounded")

trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

trainTask <- dropFeatures(task = trainTask,features = c("flow_name", "flow_id"))
trainTask <- dropFeatures(task = trainTask,features = c("area_under_roc_curve"))
trainTask <- dropFeatures(task = trainTask,features = c("estimation.procedure"))


neural.learner <- makeLearner("regr.nnet", predict.type = "response")
# nnet regr.fnn regr.gbm  regr.glm  kknn    regr.ranger  regr.xgboost  regr.ksvm
nnet.model <- train(neural.learner,trainTask)


save(nnet.model, file = "inst/algorithm-recommender-model.RData")





fpmodel <- predict(fmodel, testTask)

submit <- data.frame(AUC.actual = OpenMLMasterTestingDataNoNAs$area_under_roc_curve, AUC.predicted = fpmodel$data$response)

d <- submit[order(submit$AUC.actual), ]

d$index <- 1:dim(d)[1]
ggplot(data=d, aes(index)) +
    geom_line(aes(y = AUC.actual, colour = "AUC.actual")) +
    geom_line(aes(y = AUC.predicted, colour = "AUC.predicted")) +
    ylim(0, 1)


