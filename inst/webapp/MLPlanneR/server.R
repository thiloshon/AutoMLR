library(shiny)
library(ggplot2)
library(mlr)

source("functions/reference_classes.R")
source("functions/algorithm_recommender-learner.R")
source("functions/algorithm_recommender-manual.R")
source("functions/algorithm_recommender.R")

options(scipen = 999)

shinyServer(function(input, output, session) {
    dataStore <-
        list(
            learning.type = "classification",
            mlPlan = NULL,
            inputReceived = FALSE
        )

    showModal(modalDialog(
        img(src = "82.png", align = "center")
        ))


    observeEvent(input$deploy.classification, {
        if (input$deploy.classification[[1]] > 0) {
            dataStore$learning.type <<- "classification"
            dataStore$mlPlan <<- MLPlan("classification")

        } else if (input$deploy.regression[[1]] > 0) {
            dataStore$learning.type <<- "regression"
            dataStore$mlPlan <<- MLPlan("regression")
        }

        updateTabsetPanel(session, "mlplan", selected = "select.data")
    })

    observeEvent(input$deploy.regression, {
        if (input$deploy.classification[[1]] > 0) {
            dataStore$learning.type <<- "classification"
            dataStore$mlPlan <<- MLPlan("classification")

        } else if (input$deploy.regression[[1]] > 0) {
            dataStore$learning.type <<- "regression"
            dataStore$mlPlan <<- MLPlan("regression")
        }

        updateTabsetPanel(session, "mlplan", selected = "select.data")
    })


    observeEvent(input$inputFile, {
        withProgress(message = paste("Reading", input$inputFile$name, "..."), {
            if (is.null(input$inputFile))
                return("No data to view")

            output$inputDataTable <-
                DT::renderDataTable(DT::datatable({
                    data.table::fread(input$inputFile$datapath)
                }))
        })
    })


    observeEvent(input$select.data.button, {
        dataStore$inputReceived <<- TRUE
        dataStore$mlPlan$addData(data.table::fread(input$inputFile$datapath))

        updateTabsetPanel(session, "mlplan", selected = "select.target.tab")
    })

    observeEvent(input$select.target.button, {
        dataStore$mlPlan$addTarget(as.numeric(input$selected.target))
        updateTabItems(session, "sideBar", "pipes")
        updateTabsetPanel(session, "mlpipes", selected = "data.split")
    })


    output$target.variables <- renderUI({
        components <- list(selectInput(
            "selected.target",
            label = h3("Select Target"),
            choices = setNames(
                1:ncol(dataStore$mlPlan$data),
                names(dataStore$mlPlan$data)
            ),
            selected = 1
        ))

        return(
            div(
                id = 'typeInput',
                class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
                column(width = 12,
                       components)
            )
        )
    })

    output$target.plots <- renderPlot({
        if (dataStore$learning.type == "classification") {
           suppressWarnings( ggplot(dataStore$mlPlan$data) +
                                 geom_bar(aes(dataStore$mlPlan$data[, as.numeric(input$selected.target)])) +
                                 labs(title = "Count by classes") +
                                 xlab("Classes") + theme_linedraw() + theme(
                                     plot.background = element_rect(fill = "#323232") ,
                                     panel.background = element_rect(fill = "#323232")
                                 ))
        } else {
            suppressWarnings(ggplot(dataStore$mlPlan$data, aes(
                x = 1:nrow(dataStore$mlPlan$data),
                y = dataStore$mlPlan$data[, as.numeric(input$selected.target)]
            )) +
                geom_point(shape = 1) +
                geom_smooth(method = lm ,
                            color = "red",
                            se = TRUE) +
                labs(title = "Target Scatter") +
                xlab("Record") + ylab("Value") + theme_linedraw() + theme(
                    plot.background = element_rect(fill = "#323232") ,
                    panel.background = element_rect(fill = "#323232")
                )
            )
        }

    })

    output$split.range <- renderPrint({
        train <-
            as.integer(input$slider2 * nrow(dataStore$mlPlan$data) / 100)
        test <- nrow(dataStore$mlPlan$data) - train

        # dataStore$mlPlan$addSplit(train)

        # print(dataStore$mlPlan$train.split)

        paste("Train Data : ", train,
              " and Test Data : ", test)
    })


    output$qualityChecks <- renderUI({
        components <- list()

        algorithms <-
            suggest_learner(dataStore$mlPlan$data,
                            dataStore$learning.type,
                            dataStore$mlPlan$target)

        #message(algorithms)

        split <- split_data(dataStore$mlPlan$data)


        for (i in 1:10) {

            preproc <-
                recommend_preprocessing(dataStore$mlPlan$data, algorithms$algorithms_id[1], F)
            preprocString <- list()

            for (pre in 1:nrow(preproc)) {
                preprocString[[length(preprocString) + 1]] <- tags$p(paste(preproc$label[1], "on", preproc[pre, 3]))



            }

            components[[i]] <- tagList(
                HTML(
                    paste(
                        "<input type=checkbox
                        name=algoSelect value=",
                        algorithms$algorithms_id[i],
                        ">"
                    )
                ),
                div(
                    class = "checksListContent",
                    h4(algorithms$algorithms_name[i]),

                    div(class = "checksListTopic col-sm-3", p("Preprocessing: ")),
                    div(class = "checksListTitle",
                        preprocString
                       ),
                    div(class = "checksListTopic col-sm-3", p("Train / Test Split: ")),
                    div(class = "checksListTitle",
                        p(paste(split * 100, "/", 100 - (split * 100)))),

                    div(class = "checksListTopic col-sm-3", p("Evaluation Metric")),
                    div(class = "checksListTitle",
                        p("Accuracy"))
                ),
                br(),
                br()
            )
        }

        return(
            div(
                id = "algoSelect",
                class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
                tags$br(),
                tags$br(),
                column(width = 12,
                       components)
            )
        )
    })



    observeEvent(input$breed.models, {
        updateTabItems(session, "sideBar", "breed")
    })


    output$breedModels <- renderUI({
        components <- list()

        for (i in 1:5) {
            components[[i]] <- tagList(
                HTML(
                    paste(
                        "<input type=checkbox
                        name=algoSelect value=",
                        i,
                        ">"
                    )
                ),
                div(
                    class = "checksListContent",
                    h4('Neural Net'),

                    div(class = "checksListTopic col-sm-3", p("Preprocessing: ")),
                    div(class = "checksListTitle",
                        p(ifelse(
                            i == 1,
                            c(
                                "None"
                            ),
                            ifelse(
                                i == 2,
                                c(
                                    "Removing features with constant values,
                                Normalizing numerical features,
                                Merging small factors into one big factor level,
                                Cutting off large values like 'infinity'"
                                ),
                                ifelse(
                                    i == 3,
                                    c(
                                        "Creating dummy features for factors,
                                Removing columns,
                                Factoring features with encoding,
                                Binning continous variables to levels,
                                Imputing missing values"
                                    ),
                                    ifelse(
                                        i == 4,
                                        c(
                                            "Normalizing numerical features"
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
                                    )
                                )
                            )

                        ))
                        ),

                    div(class = "checksListTopic col-sm-3", p("Train / Test Split: ")),
                    div(class = "checksListTitle",
                        p("80 / 20")),

                    div(class = "checksListTopic col-sm-3", p("Evaluation Metric")),
                    div(class = "checksListTitle",
                        p("Accuracy"))
                ),
                br(),
                br()
            )
        }

        return(
            div(
                id = "algoSelect",
                class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
                tags$br(),
                tags$br(),
                column(width = 12,
                       components)
            )
        )
    })

    observeEvent(input$dataToConfigure, {
        prefix <-
            ifelse(dataStore$learning.type == "classification",
                   "classif",
                   "regr")

        for (algo in input$algoSelect) {
            temp <- gsub("^\\s+|\\s+$", "", algo)
            pipe <- PipeLine(paste(prefix, temp, sep = "."), temp)
            dataStore$mlPlan$addPipe(pipe)
        }

        dataStore$mlPlan$split()

        updateTabItems(session, "sideBar", "play")

        # dataStore$mlPlan$train()
        #dataStore$mlPlan$test()

        #dataStore$mlPlan$printSelf()
    })

    observeEvent(input$train.models, {


        updateTabItems(session, "sideBar", "document")
        generate_detailed_report(dataStore)
        generate_code_report(dataStore)

    })

    generate_detailed_report <-
        function(dataStore) {
            withProgress(message = "Preparing Reports and Artifacts...", {
                try(rmarkdown::render(
                    file.path("functions/generateDetailedReport.Rmd"),
                    c("pdf_document", "md_document"),
                    quiet = T,
                    output_dir = 'Report'
                ))

                message(paste("Saved generated reports to '", tempdir(), sep = ""))
            })
        }

    generate_code_report <-
        function(dataStore) {
            withProgress(message = "Preparing Reports and Artifacts...", {
                try(rmarkdown::render(
                    file.path("functions/generateCodeReport.Rmd"),
                    c("pdf_document", "md_document"),
                    quiet = T,
                    output_dir = 'Report'
                ))
                message(paste("Saved generated reports to '", tempdir(), sep = ""))

            })
            }



    output$downloadInput <- downloadHandler(
        filename = function() {
            paste("inputData-", Sys.Date(), ".csv")
        },
        content = function(con) {
            write.csv(dataStore$mlPlan$data, con)
        }
    )



    output$evaluations <- renderUI({
        withProgress(message = "Training and testing models...", {
            dataStore$mlPlan$train()

        })


        data <- dataStore$mlPlan$benchmark()

        data$index <- c(1:nrow(data))

        data <- data[order(-data$acc.test.mean),]

        print(data)

        components <- list()

        for (i in 1:nrow(data)) {
            components[[i]] <- tagList(
                HTML(
                    paste(
                        "<input type=checkbox
                        name=trainSelect value=",
                        data$index[i],
                        ">"
                    )
                ),
                div(
                    class = "checksListContent",
                    h4(data$name[i]),

                    div(class = "checksListTopic col-sm-3", p("Out of Sample Accuracy: ")),
                    div(class = "checksListTitle",
                        p(as.numeric(data$acc.test.mean[i]) * 100)
                    ),

                    div(class = "checksListTopic col-sm-3", p("Training Duration")),
                    div(class = "checksListTitle",
                        p(paste(data$timetrain.test.mean[i], "Seconds")))
                ),
                br(),
                br()

                            )
        }

        return(
            div(
                id = "trainSelect",
                tags$br(),
                tags$br(),
                column(width = 12,
                       components)
            )
        )


    })



    output$documentContentUI <- renderUI({
        input$flagButton
        tagList(
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Data",
                    div(class = "secondaryHeaders", h3("Artifact 01: Input Data")),
                    downloadButton("downloadInput", "Download Input Data"),
                    br(),
                    br(),
                    DT::renderDataTable(dataStore$mlPlan$data, width = 300)
                ),
                tabPanel(
                    "Models",
                    div(class = "secondaryHeaders", h3(
                        "Artifact 02: Machine Learning Models"
                    )),

                    br(),

                    helpText("Models will be downloaded as .RData file. Load with command load('filename.RData') and a list object with the name `models` will be loaded to the calling environment with each models as elements of the list. Use mlr package to continue experiment."),
                    br(),
                    downloadButton("downloadModels", "Download Trained Models"),
                    br()
                ),

                tabPanel(
                    "Report",
                    div(class = "secondaryHeaders", h3(
                        "Artifact 03: Extensive Machine Learning Report"
                    )),

                    downloadButton("downloadShortReport", "Download Report in PDF"),
                    br(),
                    br(),
                    includeMarkdown("Report/generateDetailedReport.md")
                ),


                tabPanel(
                    "Source Code",
                    div(class = "secondaryHeaders", h3(
                        "Artifact 04: Workflow Source Code"
                    )),
                    br(),
                    br(),
                    includeMarkdown("Report/generateCodeReport.md")
                )
            )
        )
    })

    output$downloadModels <- downloadHandler(
        filename = function() {
            paste("models-", Sys.Date(), ".RData", sep = "")
        },
        content = function(con) {
            models <- c()
            for (i in 1:length(dataStore$mlPlan$ml.pipelines)) {
                models <- c(models, dataStore$mlPlan$ml.pipelines[[i]]$mlr.model)
            }
            save(models, file = con)
        }
    )

    output$downloadShortReport <- downloadHandler(
        filename = function() {
            paste("automated-training-report-", Sys.Date(), ".pdf", sep = "")
        },
        content = function(con) {
            file.copy('Report/generateDetailedReport.pdf',
            con)
        }
    )




    # ------------------REFERENCE POINT-----------------







    output$regre <- reactive({
        return(input$deploy.regression[[1]])
    })

    output$class <- reactive({
        return(input$deploy.classification[[1]])
    })



})
