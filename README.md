[![Build Status](https://travis-ci.org/thiloshon/AutoMLR.svg?branch=master)](https://travis-ci.org/thiloshon/AutoMLR)

[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![codecov](https://codecov.io/gh/thiloshon/AutoMLR/branch/master/graph/badge.svg)](https://codecov.io/gh/thiloshon/AutoMLR)

[![](https://img.shields.io/github/languages/code-size/thiloshon/AutoMLR.svg)](https://github.com/thiloshon/AutoMLR)

# AutoMLR
### Automated Machine Learning in R

`AutoMLR` works by making use of the knowledge of past trainings and several rules-based components. The promising combinations and settings of algorithms, preprocessing, evaluations and hyperparameters are recommended by the system. Several such settings are converted to machine learning models, trained, evaluated and finally, the models with best performance are presented to the user.

The system is developed to automate entire pipeline of machine learning, starting from preprocessing to evaluation of models. This is done without user having to type even a single line of code. No programming is required and not even any installation is required if you are using our publicly hosted server to train models. The entire UI process user follows is tracked, documented and provided back to user as PDF as to be transparent in our blackbox abstract workflow. Even the code required to manually perform all the model training is given back as the process must be reproducible.

This system is first of its kind in R language and no other existing systems offer as much functionality and automation as AutoMLR. It works with both regression and classification tasks and supports around 30 different algorithms. It is available as an R package and interactions with user is facilitated through web graphical interface that eliminates the need for the user to learn any new languages. 

The system is designed in a three-tiered architecture with data, logic and UI layers. R language was used to develop the core system. This is because, R is a statistical language and offers functionalities much required by the system as opposed to python. mlr package in R was used as machine learning library with main data used was from openml.org. Shiny framework was used to develop web IDE.



## Install


Development version, from GitHub

```r
library("devtools")
devtools::install_github("thiloshon/AutoMLR")
```

```r
library("automlr")
```


## Using automlr


```r
run_automlr()
```
