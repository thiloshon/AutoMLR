library(shiny)
library(ggplot2)
library(mlr)

shinyServer(function(input, output, session) {
    dataStore <-
        list(
            learning.type = "classification",
            mlPlan = NULL,
            inputReceived = FALSE
        )


    observeEvent(input$deploy.classification, {
        if (input$deploy.classification[[1]] > 0) {
            dataStore$learning.type <<- "classification"
            dataStore$mlPlan <<- MLPlan("classification")
        } else if (input$deploy.regression[[1]] > 0) {
            dataStore$learning.type <<- "regression"
            dataStore$mlPlan <<- MLPlan("regression")
        }

        showNotification("Recieved Type!", duration = 2)
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
        ggplot(dataStore$mlPlan$data) +
            geom_bar(aes(dataStore$mlPlan$data[, as.numeric(input$selected.target)])) +
            labs(title = "Count by classes") +
            xlab("Classes")


    })

    output$split.range <- renderPrint({
        train <- as.integer(input$slider2 * nrow(dataStore$mlPlan$data) / 100)
        test <- nrow(dataStore$mlPlan$data) - train

        # dataStore$mlPlan$addSplit(train)

        # print(dataStore$mlPlan$train.split)

        paste("Train Data : ", train,
              " and Test Data : ", test)
        })


    output$qualityChecks <- renderUI({
        components <- list()

        algorithms <- suggest_learner(dataStore$mlPlan$data, dataStore$learning.type, dataStore$mlPlan$target)

        for (i in 1:5) {
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
                    div(
                        class = "checksListTitle",
                        p("factoring, binning, na.imputing")
                    ),

                    div(class = "checksListTopic col-sm-3", p("Train / Test Split: ")),
                    div(
                        class = "checksListTitle",
                        p("70/30")
                    ),

                    div(class = "checksListTopic col-sm-3", p("Evaluation Metric")),
                    div(
                        class = "checksListTitle",
                        p("Area Under Curve, Accuracy")
                    )
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
        prefix <- ifelse(dataStore$learning.type == "classification", "classif", "regr")

        for (algo in input$algoSelect) {
            temp <- gsub("^\\s+|\\s+$", "", algo)
            pipe <- PipeLine(paste(prefix, temp, sep = "."), temp)
            dataStore$mlPlan$addPipe(pipe)
        }

        dataStore$mlPlan$split()
        dataStore$mlPlan$train()
        #dataStore$mlPlan$test()

        #dataStore$mlPlan$printSelf()
    })





    # ------------------REFERENCE POINT-----------------







    output$regre <- reactive({
        return(input$deploy.regression[[1]])
    })

    output$class <- reactive({
        return(input$deploy.classification[[1]])
    })



})
