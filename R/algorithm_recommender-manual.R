# assign scores to each algorithm for number of characteristics from -10 to +10
# When suggesting, keep adding scores based on requirements
# select highest ranking algorithm for pipeline creation



# Decision tree method.
suggest_learner_manual <- function(dataset, type = "regression") {
    if (nrow(dataset)==0) {
        stop("Empty Dataset")
    }

    # Manual Rules from spread sheet
    # Thanks to scikit-learn


    # TODO: Get input type and decide (text / audio)

    number_of_records <- nrow(dataset)

    if (number_of_records < 50) {
        print("Not enough data")
        return(list("Collect more records"))
    }

    requiredScores <- vector()

    if (category_pred(type)) {
        print("Catgorical Data")

        requiredScores <- c(requiredScores, "categorical_data")

        print(requiredScores)

        if (label_data()) {
            # ------ Classification Algorithms ------
            print("Labelled Data")
            requiredScores <- c(requiredScores, "labelled_data")

            if (number_of_records > 100000) {
                print("Large dataset")
                requiredScores <- c(requiredScores, "large_data")

                if (text_data()) {
                    print("Text Data")
                    requiredScores <- c(requiredScores, "text_data")

                    # return(
                    #     list(
                    #         "Linear Support Vector Classification - classif.LiblineaRL1L2SVC",
                    #         "Naive Bayes - classif.naiveBayes"
                    #     )
                    # )
                } else {
                    print("Non textual data")
                    # return(
                    #     list(
                    #         "Linear SVC - classif.LiblineaRL1L2SVC",
                    #         "k-Nearest Neighbor - classif.kknn",
                    #         "Support Vector Classification - classif.svm"
                    #     )
                    # )
                }
            } else {
                print("Medium data")
                requiredScores <- c(requiredScores, "medium_data")
                # return(
                #     list(
                #         "Support Vector Machines with Kernel - classif.ksvm",
                #         "Stochastic Gradient Descent - sgd v1.1"
                #     )
                # )
            }

            # ------ END OF Classification Algorithms ------
        } else {
            # ------ Clustering Algorithms ------
            print(" Non labelled data")
            requiredScores <- c(requiredScores, "unlabelled_data")

            if (category_known()) {
                print("Known categories")
                requiredScores <- c(requiredScores, "known_clusters")

                if (number_of_records > 100000) {
                    print("Large dataset")
                    requiredScores <- c(requiredScores, "large_data")

                    # return(
                    #     list(
                    #         "K-Means - cluster.SimpleKMeans",
                    #         "Spectral Clustering - kernlab v0.9-27",
                    #         "Gaussian Mixture Model - mclust v5.4.2; ClusterR v1.1.8"
                    #     )
                    # )
                } else {
                    print("Medium data")
                    requiredScores <- c(requiredScores, "medium_data")
                    # return(list("MiniBatch KMeans - ClusterR v1.1.8"))
                }

            }
            else {
                print("Unknown categories")
                requiredScores <- c(requiredScores, "unknown_clusters")

                if (number_of_records > 100000) {
                    print("Large dataset")
                    requiredScores <- c(requiredScores, "large_data")

                    # return(
                    #     list(
                    #         "Mean shift - meanShiftR v0.53",
                    #         "Variational Bayesian Gaussian Mixture Model - TargetScore v1.10.0"
                    #     )
                    # )
                } else {
                    print("Medium data")
                    requiredScores <- c(requiredScores, "medium_data")
                    # return(list("Tough luck"))

                }

                # ------ END OF Clustering Algorithms ------
            }


        }
    }

    else {
        print("Continous data")
        requiredScores <- c(requiredScores, "continous_data")

        # ------ Regression Algorithms

        if (number_of_records > 100000) {
            print("Large dataset")
            requiredScores <- c(requiredScores, "large_data")

            # if(important_features()){
            #     print("few features should be important")
            #     return(list("GLM with Lasso or Elasticnet Regularization - regr.glmnet"))
            # }

            # return(
            #     list(
            #         "GLM with Lasso or Elasticnet Regularization - regr.glmnet",
            #         "Ridge Regression - glmnet v2.0-16 ; ridge v2.3",
            #         "Linear Kernel Support Vector Machines"
            #     )
            # )
        } else {
            print("Medium dataset")
            requiredScores <- c(requiredScores, "medium_data")
            # return(list("Stochastic Gradient Descent - sgd v1.1"))
        }



        # ------ END OF Regression Algorithms
    }

    algorithms_manual <- read.csv("inst/algorithms scoring.csv")
    algorithms_manual[is.na(algorithms_manual)] <- 0

    scoreboard <- algorithms_manual[, c("algorithms_id", "algorithms_name", requiredScores)]

    return(scoreboard)

}


category_pred <- function(type) {
    if (type != "regression") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

label_data <- function(type) {
    if (type != "classification") {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
