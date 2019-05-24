
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"

df <- read.table(url)
dplyr::glimpse(df)

write.csv(df, "ecoli.csv", row.names=FALSE)






----------------------------------


summarizeColumns(test)

outliers package
scores(x, type="chisq", prob=0.9)  # beyond 90th %ile based on chi-sq
#> [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

factor


#impute missing values by mean and mode
> imp <- impute(train, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")
> imp1 <- impute(test, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")

> imp_train <- imp$data
> imp_test <- imp1$data

#for train data set
> cd <- capLargeValues(imp_train, target = "Loan_Status",cols = c("ApplicantIncome"),threshold = 40000)
> cd <- capLargeValues(cd, target = "Loan_Status",cols = c("CoapplicantIncome"),threshold = 21000)
> cd <- capLargeValues(cd, target = "Loan_Status",cols = c("LoanAmount"),threshold = 520)

#check correlation
> cor(xs)


---------------

    #create a task
    > trainTask <- makeClassifTask(data = cd_train,target = "Loan_Status")
> testTask <- makeClassifTask(data = cd_test, target = "Loan_Status")

> library(e1071)                    # load e1071
> duration = faithful$eruptions     # eruption durations
> skewness(duration)
normalize these variables

#normalize the variables
> trainTask <- normalizeFeatures(trainTask,method = "standardize")
> testTask <- normalizeFeatures(testTask,method = "standardize")

> trainTask <- dropFeatures(task = trainTask,features = c("Loan_ID","Married.dummy"))

#Feature importance
> im_feat <- generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
> plotFilterValues(im_feat,n.show = 20)


------------------------------

    #logistic regression
    > logistic.learner <- makeLearner("classif.logreg",predict.type = "response")

#cross validation (cv) accuracy
> cv.logistic <- crossval(learner = logistic.learner,task = trainTask,iters = 3,stratify = TRUE,measures = acc,show.info = F)

#train model
> fmodel <- train(logistic.learner,trainTask)
> getLearnerModel(fmodel)

#predict on test data
> fpmodel <- predict(fmodel, testTask)




-------------------------------

    #load svm
    > getParamSet("classif.ksvm") #do install kernlab package
> ksvm <- makeLearner("classif.ksvm", predict.type = "response")

#Set parameters
> pssvm <- makeParamSet(
    makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
    makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
)

#specify search function
> ctrl <- makeTuneControlGrid()

#tune model
> res <- tuneParams(ksvm, task = trainTask, resampling = set_cv, par.set = pssvm, control = ctrl,measures = acc)

#CV accuracy
> res$y
acc.test.mean
0.8062092

#set the model with best params
> t.svm <- setHyperPars(ksvm, par.vals = res$x)

#train
> par.svm <- train(ksvm, trainTask)

#test
> predict.svm <- predict(par.svm, testTask)



--------------------------------------

    #selecting top 6 important features
    > top_task <- filterFeatures(trainTask, method = "rf.importance", abs = 6)


# Define number of CPU cores to use when training models
parallelStartSocket(8)

tsk = createDummyFeatures(tsk)



--------------------------------------------------
    # Load data, create tsk and learner
    data(BostonHousing, package = "mlbench")
tsk = makeRegrTask("Xgboost with Boston Housing", data = BostonHousing, target = "medv")
tsk = createDummyFeatures(tsk)
# NOTE: You may want to decrease the threads here depending on your machine
lrn = makeLearner("regr.xgboost", nthread = 4)

# standard resampling and paramset
res = makeResampleDesc("CV", iters = 5)
par = makeParamSet(
    makeIntegerParam(id = "nrounds", lower = 1,upper = 80),
    makeIntegerParam(id = "max_depth", lower = 2, upper = 15),
    makeNumericParam(id = "eta", lower = .01, upper = .4)
)
# This is mostly ripped and modified from the mbo blog post
library(mlrMBO)
# In this simple example we construct the control object with the defaults:
mbo.ctrl = makeMBOControl()
# For this numeric optimization we are going to use the Expected Improvement as infill criterion:
mbo.ctrl = setMBOControlInfill(mbo.ctrl, crit = crit.ei)
# We will allow for exactly 25 evaluations of the objective function:
mbo.ctrl = setMBOControlTermination(mbo.ctrl, max.evals = 25L)

# Make a design matrix
design.mat = generateRandomDesign(n = 500, par.set = par)
# add the mbo control and design matrix to the mlrMBO tuning function
ctrl = makeTuneControlMBO(mbo.control = mbo.ctrl, mbo.design = design.mat)

# Everything else is pretty standard
tune.pars = tuneParams(learner = lrn, task = tsk, resampling = res,
                       measures = rmse, par.set = par, control = ctrl)



---------------------------------


    If we know that the distribution of values in the sample is Gaussian or Gaussian-like, we can use the standard deviation of the sample as a cut-off for identifying outliers.

The Gaussian distribution has the property that the standard deviation from the mean can be used to reliably summarize the percentage of values in the sample.

For example, within one standard deviation of the mean will cover 68% of the data.

So, if the mean is 50 and the standard deviation is 5, as in the test dataset above, then all data in the sample between 45 and 55 will account for about 68% of the data sample. We can cover more of the data sample if we expand the range as follows:

    1 Standard Deviation from the Mean: 68%
2 Standard Deviations from the Mean: 95%
3 Standard Deviations from the Mean: 99.7%


Not all data is normal or normal enough to treat it as being drawn from a Gaussian distribution.

A good statistic for summarizing a non-Gaussian distribution sample of data is the Interquartile Range, or IQR for short





--------------------------\


c(
    "Removing features with constant values,
                                Normalizing numerical features,
                                Merging small factors into one big factor level,
                                Cutting off large values like 'infinity',
                                Creating dummy features for factors,
                                Factoring features with encoding,
                                Binning continous variables to levels"
),
c(
    "Removing features with constant values,
                                Normalizing numerical features,
                                Merging small factors into one big factor level,
                                Cutting off large values like 'infinity',
                                Creating dummy features for factors,
                                Removing columns,
                                Factoring features with encoding,
                                Binning continous variables to levels,
                                Imputing missing values"
)



----------------------------


    algorithms_manual <-
    read.csv("functions/algorithms scoring.csv")
algorithms_manual[is.na(algorithms_manual)] <- 0

scoreboard <-
    algorithms_manual[, c(
        "algorithms_id",
        "algorithms_name",
        "meta_name",
        "meta_influenze",
        "type",
        requiredScores
    )]

print("sdf:")
print(scoreboard)
print(type)

# Cleanign scoreboard
scoreboard <-
    scoreboard[scoreboard$type == type |
                   scoreboard$type == "both", ]
print(scoreboard)
print("yu")
scoreboard2 <- getProperties(dataset, type, target)
print(scoreboard2)
mix <- scoreboard$algorithms_id

if(type == "classification"){
    clean <- sub("classif.", "", scoreboard2$class)
} else {
    clean <- sub("regr.", "", scoreboard2$class)
}


clean_board <- scoreboard[mix %in% clean,]



print("meee")
print(temp)

if(nrow(clean_board) < 10){
    temp <- scoreboard2[!(clean %in% clean_board$algorithms_id),]

    temp <- temp[sample(1:nrow(temp), 10 - nrow(clean_board)), ]

    for (row in 1:nrow(temp)) {
        clean_board[nrow(clean_board) + 1,] = list(temp$class, temp$name, "", 0, "")

    }

}

return(clean_board)
