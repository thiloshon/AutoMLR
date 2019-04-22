ML Plan
-------

Type: classification

Target Variable: Species

### Meta of Data

Number of classes: 3

Size of majority class: 50

Size of minority class: 50

Number of features: 5

Number of numeric features: 4

Number of symbolic features: 0

Number of records: 150

Number of records with missing values: 150

Number of total missing values: 0

Data Highlight:

    ## # A tibble: 150 x 1
    ##    `dataStore$mlPlan$data~ $Sepal.Width $Petal.Length $Petal.Width $Species
    ##                      <dbl>        <dbl>         <dbl>        <dbl> <chr>   
    ##  1                     5.1          3.5           1.4          0.2 setosa  
    ##  2                     4.9          3             1.4          0.2 setosa  
    ##  3                     4.7          3.2           1.3          0.2 setosa  
    ##  4                     4.6          3.1           1.5          0.2 setosa  
    ##  5                     5            3.6           1.4          0.2 setosa  
    ##  6                     5.4          3.9           1.7          0.4 setosa  
    ##  7                     4.6          3.4           1.4          0.3 setosa  
    ##  8                     5            3.4           1.5          0.2 setosa  
    ##  9                     4.4          2.9           1.4          0.2 setosa  
    ## 10                     4.9          3.1           1.5          0.1 setosa  
    ## # ... with 140 more rows

ML Pipes
--------

    ######################## nnet ########################  

    Machine Learning Pipeline Object

    ID: nnet
    Learning Algorithm: [1] "classif.nnet"
    Preprocessing List: character(0)
    Train, Test, Cross Validation Split: [[1]]
    Resample description: holdout with 0.60 split rate.
    Predict: test
    Stratification: FALSE

    #####  MLR Task: ##### [[1]]
    Supervised task: nnet
    Type: classif
    Target: Species
    Observations: 150
    Features:
       numerics     factors     ordered functionals 
              4           0           0           0 
    Missings: FALSE
    Has weights: FALSE
    Has blocking: FALSE
    Has coordinates: FALSE
    Classes: 3
        setosa versicolor  virginica 
            50         50         50 
    Positive class: NA

    #####  MLR Learner: ##### [[1]]
    Learner classif.nnet from package nnet
    Type: classif
    Name: Neural Network; Short name: nnet
    Class: classif.nnet
    Properties: twoclass,multiclass,numerics,factors,prob,weights
    Predict-Type: response
    Hyperparameters: size=3


    #####  MLR Model: ##### [[1]]
    Resample Result
    Task: nnet
    Learner: classif.nnet
    Aggr perf: mmce.test.mean=0.0000000,acc.test.mean=1.0000000,timetrain.test.mean=0.0000000
    Runtime: 0.017961

    ######################## ksvm ########################  

    Machine Learning Pipeline Object

    ID: ksvm
    Learning Algorithm: [1] "classif.ksvm"
    Preprocessing List: character(0)
    Train, Test, Cross Validation Split: [[1]]
    Resample description: holdout with 0.60 split rate.
    Predict: test
    Stratification: FALSE

    #####  MLR Task: ##### [[1]]
    Supervised task: ksvm
    Type: classif
    Target: Species
    Observations: 150
    Features:
       numerics     factors     ordered functionals 
              4           0           0           0 
    Missings: FALSE
    Has weights: FALSE
    Has blocking: FALSE
    Has coordinates: FALSE
    Classes: 3
        setosa versicolor  virginica 
            50         50         50 
    Positive class: NA

    #####  MLR Learner: ##### [[1]]
    Learner classif.ksvm from package kernlab
    Type: classif
    Name: Support Vector Machines; Short name: ksvm
    Class: classif.ksvm
    Properties: twoclass,multiclass,numerics,factors,prob,class.weights
    Predict-Type: response
    Hyperparameters: fit=FALSE


    #####  MLR Model: ##### [[1]]
    Resample Result
    Task: ksvm
    Learner: classif.ksvm
    Aggr perf: mmce.test.mean=0.0666667,acc.test.mean=0.9333333,timetrain.test.mean=0.0000000
    Runtime: 0.015955

References
----------

Thiloshon Nagarajah and Guhanathan Poravi (2019). automlr: Automated
Machine Learning in R. R package version 0.0.009.

R Core Team (2012). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria. ISBN
3-900051-07-0, URL <http://www.R-project.org/>
