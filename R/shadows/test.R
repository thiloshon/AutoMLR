form = run_id + task_id + flow_name + setup_id + flow_id + data_name + upload_time ~ fun

evals = data.table::dcast(
    data = as.data.table(OpenMLRunEvaluations),
    formula = form,
    value.var = c("value"),
    fun.aggregate = mean
)


sampledata <- evals[sample(nrow(evals), nrow(evals) * 0.01), ]



wideData <-
    merge(x = sampledata,
          y = tasks,
          by.x = "task_id",
          by.y = "task.id")

wideDataMLR <- wideData[grepl("mlr", wideData$flow_name), ]


wideDataMLR$task_id <- as.factor(wideDataMLR$task_id)
wideDataMLR$run_id <- as.factor(wideDataMLR$run_id)
wideDataMLR$setup_id <- as.factor(wideDataMLR$setup_id)
wideDataMLR$flow_id <- as.factor(wideDataMLR$flow_id)
wideDataMLR$flow_name <- as.factor(wideDataMLR$flow_name)
wideDataMLR$upload_time <-
    as.POSIXct(wideDataMLR$upload_time, format = "%Y-%m-%d %H:%M:%OS")
wideDataMLR$data_name <- as.factor(wideDataMLR$data_name)
wideDataMLR$task.type <- as.factor(wideDataMLR$task.type)
wideDataMLR$data.id <- as.factor(wideDataMLR$data.id)
wideDataMLR$name <- as.factor(wideDataMLR$name)
wideDataMLR$estimation.procedure <-
    as.factor(wideDataMLR$estimation.procedure)
wideDataMLR$wideDataMLR <-
    as.factor(wideData$evaluation.measures)



# Regression Tree Example
library(rpart)

# grow tree
fit <-
    rpart(
        area_under_roc_curve ~ flow_name + build_cpu_time + build_memory + estimation.procedure + evaluation.measures + majority.class.size + minority.class.size + number.of.classes + number.of.features + number.of.instances,
        method = "anova",
        data = wideDataMLR
    )

fit <-
    rpart(
        flow_id ~ area_under_roc_curve + build_cpu_time + build_memory + estimation.procedure + evaluation.measures + majority.class.size + minority.class.size + number.of.classes + number.of.features + number.of.instances,
        method = "class",
        data = wideDataMLR
    )

fit <-
    rpart(
        area_under_roc_curve ~ flow_name,
        method = "class",
        data = wideDataMLR
    )

fancyRpartPlot(fit)



# plot tree
plot(fit, uniform = TRUE,
     main = "Algorithm Selection Tree for AUC")
text(fit,
     cex = .75)

fit <-
    rpart(
        area_under_roc_curve ~ majority.class.size + minority.class.size + number.of.classes + number.of.features + number.of.instances,
        method = "anova",
        data = wideDataMLR
    )

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# create additional plots
par(mfrow = c(1, 1)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results



# create attractive postcript plot of tree
post(fit, file = "c:/tree2.ps",
     title = "Regression Tree for Mileage ")

rpart.rules(fit)



# PARTY
library(party)
fit <-
    ctree(
        flow_id ~ area_under_roc_curve + build_cpu_time + build_memory + estimation.procedure + evaluation.measures + majority.class.size + minority.class.size + number.of.classes + number.of.features + number.of.instances + average_cost + kappa + f_measure + kb_relative_information_score + mean_absolute_error + mean_prior_absolute_error+ precision + recall + root_mean_squared_error,
        data = wideDataMLR
    )

fit <-
    ctree(
        flow_name  ~ area_under_roc_curve,
        data = wideDataMLR
    )

plot(fit)

fit <-
    cforest(
        area_under_roc_curve ~ flow_id + majority.class.size + minority.class.size + number.of.classes + number.of.features + number.of.instances,
        data = wideDataMLR
    )

fit <- ctree(area_under_roc_curve ~ ., data = wideDataMLR[, c(-7, -38, -39, -43, -44, -45, -46, -47, -48)])



library(randomForest)
fit <-
    randomForest(
        area_under_roc_curve ~ flow_id + build_cpu_time + build_memory + estimation.procedure + evaluation.measures + majority.class.size + minority.class.size + number.of.classes + number.of.features + number.of.instances,
        data = wideDataMLR
    )
print(fit) # view results
importance(fit) # importance of each predictor



# Make the plot
ggplot(data=d, aes(x=index, y=AUC.actual, ymin=0, ymax=1)) +
    geom_line() +
    geom_ribbon(alpha=0.5)

plot(d$AUC.actual,d$AUC.predicted,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

