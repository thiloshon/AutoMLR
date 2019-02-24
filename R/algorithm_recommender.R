set.seed(1)

library(data.table)
library(rpart)
library(rpart.plot)


OpenMLRunEvaluationsData <- as.data.table(OpenMLRunEvaluations)
MLRRunEvaluationsData <-
    OpenMLRunEvaluationsData[grepl("mlr", OpenMLRunEvaluationsData$flow_name), ]

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
    OpenMLMasterData[, c(-1, -2, -4,-6,-7,-9,-24,-25,-30,-31,-32,-33)]


colMeans(is.na(OpenMLMasterData))

# Removing mostly empty fields
OpenMLMasterData <-
    OpenMLMasterData[, c(1, 2, 3, 21 , 22, 24, 31:40)]


OpenMLMasterData$flow_id <- as.factor(OpenMLMasterData$flow_id)
OpenMLMasterData$flow_name <- as.factor(OpenMLMasterData$flow_name)
OpenMLMasterData$task.type <- as.factor(OpenMLMasterData$task.type)
OpenMLMasterData$estimation.procedure <-
    as.factor(OpenMLMasterData$estimation.procedure)
OpenMLMasterData$target.feature <-
    as.factor(wideData$target.feature)


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



fit <-
    rpart(
        predictorFormulaNaive,
        method = "class",
        data = OpenMLMasterTrainingData,
        control = rpart.control(cp = 0.0025)
    )

rpart.plot::rpart.plot(fit, extra = 100)
