# assign scores to each algorithm for number of characteristics from -10 to +10
# When suggesting, keep adding scores based on requirements
# select highest ranking algorithm for pipeline creation



# Decision tree method. Deprecated
suggest_learner <- function(dataset){
    if (is.na(dataset)){
        stop("Empty Dataset")
    }

    # Manual Rules
    # Thanks to scikit-learn


    # TODO: Get input type and decide (text / audio)

    number_of_records = nrow(dataset)

    if(number_of_records < 50){
        print("Not enough data")
        return(list("Collect more records"))
    }

    if(category_pred()){

        print("Catgorical Data")


        if(label_data()){

            # ------ Classification Algorithms ------
            print("Labelled Data")
            if(number_of_records < 100000){
                print("Medium dataset")
                if(text_data()){
                    print("Text Data")
                    return(list("linear SVC", "naive bayes"))
                } else {
                    print("Non textual data")
                    return(list("linear SVC", "K neibours classifier", "SVC", "Ensemble Classifiers"))
                }
            } else {
                print("Large data")
                return(list("SGD Classifier", "Kernal approximation"))
            }

            # ------ END OF Classification Algorithms ------
        } else {

            # ------ Clustering Algorithms ------
            print(" Non labelled data")

            if(category_known()){
                print("Known categories")
                if(number_of_records < 100000){
                    print("Medium dataset")
                    return(list("Kmeans", "Spectral Clustering", "GMM"))
                }
                print("Large data")
                return(list("MiniBatch KMeans"))
            }

            print("Unknown categories")
            if(number_of_records < 100000){
                print("Medium dataset")
                return(list("Mean shift", "VBGMM"))
            }
            print("Large data")
            return(list("Tough luck"))

            # ------ END OF Clustering Algorithms ------


        }



    }

    else {
        print("Continous data")

        # ------ Regression Algorithms

        if(number_of_records < 100000){
            print("Medium dataset")

            if(important_features()){
                print("few features should be important")
                return(list("lasso", "elasticnet"))
            }

            return(list("RidgeRegression", "SVR(kernel = linear)", "SVR(kernal=rbf)", "Ensemble regressors"))
        }

        print("large dataset")
        return(list("SGD Regressor"))

        # ------ END OF Regression Algorithms
    }


}
