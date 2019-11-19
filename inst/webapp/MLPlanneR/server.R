library(shiny)
library(ggplot2)
library(xgboost)
library(rpart)
library(e1071)
library(kernlab)
library(nnet)
library(kknn)
library(mlr)
library(fiery)
library(routr)

source("functions/reference_classes.R")
source("functions/algorithm_recommender-learner.R")
source("functions/algorithm_recommender-manual.R")
source("functions/algorithm_recommender.R")
source("functions/preprocessing.R")

options(scipen = 999)

shinyServer(function(input, output, session) {
    # main data storage
    dataStore <-
        list(
            learning.type = "classification",
            mlPlan = NULL,
            inputSource = "NA",
            con <- list(),
            service <- list()
        )

    showModal(modalDialog(img(src = "82.png", align = "center")))


    # Classifiaction is selected
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

    # regression is selected
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

    # input file is given
    observeEvent(input$inputFile, {
        withProgress(message = paste("Reading", input$inputFile$name, "..."), {
            if (is.null(input$inputFile))
                return("No data to view")

            dataStore$inputReceived <<- "FILE"

            output$inputDataTable <-
                DT::renderDataTable(DT::datatable({
                    data.table::fread(input$inputFile$datapath)
                }))
        })
    })

    # input file is selected
    observeEvent(input$select.data.button, {
        if (dataStore$inputReceived == "FILE") {
            dataStore$mlPlan$addData(data.table::fread(input$inputFile$datapath))
        } else {
            dataStore$mlPlan$addData(DBI::dbReadTable(dataStore$con, input$selected.table))
            DBI::dbDisconnect(dataStore$con)
        }


        print(summarizeColumns(dataStore$mlPlan$data))
        updateTabsetPanel(session, "mlplan", selected = "select.target.tab")
    })

    observeEvent(input$get.tables, {
        output$database.tables <- renderUI({
            input$get.tables

            dataStore$con <<- DBI::dbConnect(
                RMySQL::MySQL(),
                dbname = input$db_name,
                user    = input$db_user,
                password    = input$db_pass,
                host = input$db_server,
                port = input$db_port
            )
            tables <- DBI::dbListTables(dataStore$con)


            components <- list(
                selectInput(
                    "selected.table",
                    label = h3("Select Table"),
                    choices = setNames(tables,
                                       tables),
                    selected = tables[1]
                )
            )
            components <-
                c(components,
                  DT::dataTableOutput("dbTable"))

            dataStore$inputReceived <<- "DB"

            return(div(id = 'dbTableSelect',
                       column(width = 12,
                              components)))
        })
    })

    output$dbTable <- DT::renderDataTable({
        DBI::dbReadTable(dataStore$con, input$selected.table)
    }, width = 300)


    # target value is selected
    observeEvent(input$select.target.button, {
        dataStore$mlPlan$addTarget(as.numeric(input$selected.target))
        dataStore$mlPlan$addEvaluation(
            recommend_evaluation(
                dataStore$mlPlan$data,
                dataStore$learning.type,
                dataStore$mlPlan$target
            )
        )

        updateTabItems(session, "sideBar", "pipes")
        updateTabsetPanel(session, "mlpipes", selected = "data.split")
    })

    # possible targets are listed
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

    # Target graph is shown
    output$target.plots <- renderPlot({
        if (dataStore$learning.type == "classification") {
            suppressWarnings(
                ggplot(dataStore$mlPlan$data) +
                    geom_bar(aes(dataStore$mlPlan$data[, as.numeric(input$selected.target)])) +
                    labs(title = "Count by classes") +
                    xlab("Classes") + theme_linedraw() + theme(
                        plot.background = element_rect(fill = "#323232") ,
                        panel.background = element_rect(fill = "#323232")
                    )
            )
        } else {
            suppressWarnings(
                ggplot(
                    dataStore$mlPlan$data,
                    aes(
                        x = 1:nrow(dataStore$mlPlan$data),
                        y = dataStore$mlPlan$data[, as.numeric(input$selected.target)]
                    )
                ) +
                    geom_point(shape = 1) +
                    geom_smooth(
                        method = lm ,
                        color = "red",
                        se = TRUE
                    ) +
                    labs(title = "Target Scatter") +
                    xlab("Record") + ylab("Value") + theme_linedraw() + theme(
                        plot.background = element_rect(fill = "#323232") ,
                        panel.background = element_rect(fill = "#323232")
                    )
            )
        }
    })

    # Recommended algorithms are shown
    output$qualityChecks <- renderUI({
        components <- list()

        algorithms <-
            suggest_learner(dataStore$mlPlan$data,
                            dataStore$learning.type,
                            dataStore$mlPlan$target)

        split <- split_data(dataStore$mlPlan$data)

        for (i in 1:nrow(algorithms)) {
            preproc <-
                recommend_preprocessing(dataStore$mlPlan$data,
                                        algorithms$algorithms_id[1],
                                        F)
            preprocString <- list()

            for (pre in 1:nrow(preproc)) {
                preprocString[[length(preprocString) + 1]] <-
                    tags$p(paste(preproc[pre, 2], "on", preproc[pre, 3]))
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
                        preprocString),
                    div(class = "checksListTopic col-sm-3", p("Train / Test Split: ")),
                    div(class = "checksListTitle",
                        p(paste(
                            split * 100, "/", 100 - (split * 100)
                        ))),

                    div(class = "checksListTopic col-sm-3", p("Evaluation Metric")),
                    div(class = "checksListTitle",
                        p(
                            dataStore$mlPlan$evaluation
                        ))
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

    # Algorithms are selected
    observeEvent(input$dataToConfigure, {
        prefix <-
            ifelse(dataStore$learning.type == "classification",
                   "classif",
                   "regr")

        for (algo in input$algoSelect) {
            temp <- gsub("^\\s+|\\s+$", "", algo)
            pipe <- PipeLine(paste(prefix, temp, sep = "."), temp)

            preproc <-
                recommend_preprocessing(dataStore$mlPlan$data, algo, F)

            pipe$addPreprocessing(preproc)

            dataStore$mlPlan$addPipe(pipe)

        }

        dataStore$mlPlan$split()
        updateTabItems(session, "sideBar", "play")
    })

    # Algorithms are trained and evaluated
    output$evaluations <- renderUI({
        withProgress(message = "Training and testing models...", {
            dataStore$mlPlan$preprocess()
            dataStore$mlPlan$train()
        })

        data <- dataStore$mlPlan$benchmark()
        data$index <- c(1:nrow(data))

        isAccuracy <-
            dataStore$mlPlan$evaluation == "Accuracy"
        isBalanceError <-
            dataStore$mlPlan$evaluation == "Balanced Error Rate"

        if (isAccuracy) {
            data <- data[order(-data$acc.test.mean), ]
        } else if (isBalanceError) {
            data <- data[order(data$ber.test.mean), ]
        } else {
            data <- data[order(data$mae.test.mean), ]
        }

        components <- list()
        for (i in 1:nrow(data)) {
            value <-
                ifelse(
                    isAccuracy,
                    as.numeric(data$acc.test.mean[i]) * 100,
                    ifelse(
                        isBalanceError,
                        as.numeric(data$ber.test.mean[i]),
                        as.numeric(data$mae.test.mean[i])
                    )
                )
            label <-
                ifelse(
                    isAccuracy,
                    "Out of Sample Accuracy: ",
                    ifelse(
                        isBalanceError,
                        "Balanced Error Rate",
                        "Mean Absolute Error: "
                    )
                )
            components[[i]] <- tagList(
                HTML(
                    paste(
                        "<input type=checkbox
                        name=trainSelect value=",
                        data$algo[i],
                        ">"
                    )
                ),
                div(
                    class = "checksListContent",
                    h4(data$name[i]),

                    div(class = "checksListTopic col-sm-3", p(label)),
                    div(class = "checksListTitle",
                        p(value)),

                    div(class = "checksListTopic col-sm-3", p("Training Duration")),
                    div(class = "checksListTitle",
                        p(
                            paste(data$totalTime[i], "Seconds")
                        ))
                ),
                br(),
                br()
            )
        }

        return(
            div(
                id = "trainSelect",
                class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
                tags$br(),
                tags$br(),
                column(width = 12,
                       components)
            )
        )
    })

    # Breeding algorithms are shown
    observeEvent(input$breed.models, {
        components <- list()
        prefix <-
            ifelse(dataStore$learning.type == "classification",
                   "classif",
                   "regr")

        split <- split_data(dataStore$mlPlan$data)

        algorithms <-
            read.csv("functions/algorithms scoring.csv")

        for (algo in input$trainSelect) {
            temp <- algo
            preproc <-
                recommend_preprocessing(dataStore$mlPlan$data, temp, F)

            empty <- FALSE


            for (mod in 1:4) {
                preprocString <- list()
                count <- nrow(preproc)
                pre <-
                    preproc[sample(1:count, (count * 0.25 * mod)), ]

                for (d in 1:nrow(pre)) {
                    if (nrow(pre) == 0 & !empty) {
                        preprocString[[length(preprocString) + 1]] <-
                            tags$p("None")
                        empty <- T
                    } else {
                        preprocString[[length(preprocString) + 1]] <-
                            tags$p(paste(pre[d, 2], "on", pre[d, 3]))
                    }
                }

                components[[length(components) + 1]] <- tagList(
                    HTML(
                        paste(
                            "<input type=checkbox
                        name=breedSelect value=",
                            temp,
                            ">"
                        )
                    ),
                    div(
                        class = "checksListContent",
                        h4(algorithms[which(algorithms$algorithms_id == temp), 2]),

                        div(class = "checksListTopic col-sm-3", p("Preprocessing: ")),
                        div(class = "checksListTitle",
                            preprocString),
                        div(class = "checksListTopic col-sm-3", p("Train / Test Split: ")),
                        div(class = "checksListTitle",
                            p(
                                paste(split * 100, "/", 100 - (split * 100))
                            )),

                        div(class = "checksListTopic col-sm-3", p("Evaluation Metric")),
                        div(class = "checksListTitle",
                            p(
                                dataStore$mlPlan$evaluation
                            ))
                    ),
                    br(),
                    br()
                )
            }
        }

        output$breedModels <- renderUI({
            return(
                div(
                    id = "breedSelect",
                    class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
                    tags$br(),
                    tags$br(),
                    column(width = 12,
                           components)
                )
            )
        })

        # pipe <- PipeLine(paste(prefix, temp, sep = "."), paste(temp, "-", mod, sep = ""))
        # dataStore$mlPlan$addPipe(pipe)
        # dataStore$mlPlan$split()
        # pipe$addPreprocessing(preproc)
        #
        updateTabItems(session, "sideBar", "breed")
    })

    observeEvent(input$startService, {
        app <- fiery::Fire$new()
        app$host <- "127.0.0.1"
        app$port <- input$servicePort

        app$on("start", function(server, ...) {
            showNotification(paste0("Serving from ", app$host, ":" , app$port),
                             duration = 6)
        })

        # app$on('request', function(server, request, ...) {
        #     response <- request$respond()
        #     response$status <- 200L
        #     response$body <- paste0('')
        #     response$type <- 'html'
        # })

        router <- routr::RouteStack$new()
        route <- routr::Route$new()
        router$add_route(route, 'main')

        route$add_handler('get', '/predict', function(request,
                                                      response,
                                                      keys,
                                                      arg_list,
                                                      ...) {
            inputParams <- shiny::parseQueryString(request$querystring)

            predict <-
                dataStore$mlPlan$predict(as.data.frame(inputParams))
            response$body <-
                jsonlite::toJSON(predict,
                                 auto_unbox = TRUE,
                                 pretty = TRUE)
            response$status <- 200L
            TRUE
        })
        dataStore$service <<- app
        app$attach(router)
        app$ignite()
    })

    observeEvent(input$stopService, {
        dataStore$service[[1]]$extinguish()
    })

    #  artifacts are generated
    observeEvent(input$train.models, {
        updateTabItems(session, "sideBar", "document")
        generate_detailed_report(dataStore)
        generate_code_report(dataStore)
    })

    # Report is generated
    generate_detailed_report <-
        function(dataStore) {
            withProgress(message = "Preparing Reports and Artifacts...", {
                try(rmarkdown::render(
                    system.file("rmd/generateDetailedReport.Rmd", package = "automlr"),
                    c("pdf_document", "md_document"),
                    quiet = T,
                    output_dir = tempdir()
                ))

                message(paste("Saved generated reports to '", tempdir(), sep = ""))
            })
        }

    # Code is generated
    generate_code_report <-
        function(dataStore) {
            withProgress(message = "Preparing Reports and Artifacts...", {
                try(rmarkdown::render(
                    system.file("rmd/generateCodeReport.Rmd", package = "automlr"),
                    c("pdf_document", "md_document"),
                    quiet = T,
                    output_dir = tempdir()
                ))
                message(paste("Saved generated reports to '", tempdir(), sep = ""))
            })
        }

    # Artifacts are shown
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

                    helpText(
                        "Models will be downloaded as .RData file. Load with command load('filename.RData') and a list object with the name `models` will be loaded to the calling environment with each models as elements of the list. Use mlr package to continue experiment."
                    ),
                    br(),
                    downloadButton("downloadModels", "Download Trained Models"),
                    br()
                ),

                tabPanel(
                    "Report",
                    div(
                        class = "secondaryHeaders",
                        h3("Artifact 03: Extensive Machine Learning Report")
                    ),

                    downloadButton("downloadShortReport", "Download Report in PDF"),
                    br(),
                    br(),
                    includeMarkdown(paste0(tempdir(), "/generateDetailedReport.md"))
                ),

                tabPanel(
                    "Source Code",
                    div(class = "secondaryHeaders", h3(
                        "Artifact 04: Workflow Source Code"
                    )),
                    br(),
                    br(),
                    includeMarkdown(paste0(tempdir(), "/generateCodeReport.md"))
                ),

                tabPanel(
                    "Prediction Web Service",
                    div(class = "secondaryHeaders", h3(
                        "Artifact 05: Predictive RESTful Service"
                    )),
                    numericInput(
                        "servicePort",
                        label = h3("Port for Service"),
                        value = 8080
                    ),
                    actionButton("startService", "Start Service"),
                    actionButton("stopService", "Stop Service")
                )
            )
        )
    })


    # Data is downloaded
    output$downloadInput <- downloadHandler(
        filename = function() {
            paste("inputData-", Sys.Date(), ".csv")
        },
        content = function(con) {
            write.csv(dataStore$mlPlan$data, con)
        }
    )

    # models are downloaded
    output$downloadModels <- downloadHandler(
        filename = function() {
            paste("models-", Sys.Date(), ".RData", sep = "")
        },
        content = function(con) {
            models <- c()
            for (i in 1:length(dataStore$mlPlan$ml.pipelines)) {
                models <- c(models,
                            dataStore$mlPlan$ml.pipelines[[i]]$mlr.model)
            }
            save(models, file = con)
        }
    )

    # report is downloaded
    output$downloadShortReport <- downloadHandler(
        filename = function() {
            paste("automated-training-report-",
                  Sys.Date(),
                  ".pdf",
                  sep = "")
        },
        content = function(con) {
            file.copy(paste0(tempdir(), "/generateDetailedReport.md"),
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
