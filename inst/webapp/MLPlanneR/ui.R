library(shiny)
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(leaflet))
library(DT)



shinyUI(dashboardPage(
    #Header Title
    dashboardHeader(
        title = tags$img(src = 'logo3.png'),
        dropdownMenuOutput("messageMenu")
    ),

    # ------------- Sidebar  -------------------
    dashboardSidebar(
        sidebarMenu(
            id = "sideBar",
            menuItem("Create Project",
                     tabName = "add",
                     icon = icon("cog")),

            menuItem(
                "Generate Models",
                tabName = "pipes",
                icon = icon("cubes")
            ),

            menuItem("Train and Test",
                     tabName = "play",
                     icon = icon("play")),

            menuItem("Experiment",
                     tabName = "breed",
                     icon = icon("vial")),

            menuItem(
                "Manage Artifacts",
                tabName = "document",
                icon = icon("file-download")
            )
        )
    ),

    # ------------- End of Sidebar  -------------------

    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "checkbox.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "override.css"),
            tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Merienda")
        ),
        useShinyjs(),



        tabItems(
            # ------------- Add Data Module -------------------
            tabItem("add",
                    fluidRow(column(
                        12,
                        h1("Create Machine Learning Project"),
                        column(
                            12,
                            tabsetPanel(
                                type = "tabs",
                                id = "mlplan",

                                # ------------- DB Module -------------------

                                tabPanel(
                                    "Select Learning Type",
                                    value = "select.type",

                                    div(class = "secondaryHeaders", h3("Select Learning Type of ML Project:")),
                                    div(
                                        # id = "queryDatabaseDiv",
                                        # class = "activeButton",
                                        # actionButton("queryDatabase", "Query Database", icon("download"))
                                        actionButton("deploy.classification", label = "Classification Learning"),
                                        actionButton("deploy.regression", label = "Regression Learning")
                                    ),

                                    div(class = "progressStep", taskItem(
                                        value = 15, color = "orange",
                                        "Step 1 of 6"
                                    ))
                                ),
                                tabPanel(
                                    "Select Data",
                                    value = "select.data",

                                    div(class = "secondaryHeaders", h3("ML Plan: Select Data")),
                                    div(
                                        tabsetPanel(
                                            type = "tabs",

                                            # ------------- File Module -------------------

                                            tabPanel(
                                                "Upload From File",
                                                div(
                                                    id = "inputFileDiv",
                                                    class = "activeButton",
                                                    fileInput(
                                                        "inputFile",
                                                        label = h3("CSV / TXT tabular data input"),
                                                        accept = c(
                                                            "text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv",
                                                            ".zip",
                                                            "application/zip"
                                                        )
                                                    )
                                                )
                                            ),

                                            tabPanel(
                                                "Read From Database",

                                                div(
                                                    id = "inputDBDiv",
                                                    class = "activeButton",
                                                    selectInput(
                                                        "dbType",
                                                        label = h3("Select ODBC Database Driver"),
                                                        choices = list(
                                                            "MySQL / MariaDB" = 1,
                                                            "MSSQL" = 2,
                                                            "Oracle" = 3,
                                                            "PostgreSQL" = 4,
                                                            "SQLite" = 5,
                                                            "Google BigQuery" = 6
                                                        ),
                                                        selected = 1
                                                    ),
                                                    textInput(
                                                        "db_name",
                                                        label = h3("Database Name"),
                                                        value = "machine_learning_datasets"
                                                    ),
                                                    textInput(
                                                        "db_server",
                                                        label = h3("Server Name"),
                                                        value = "localhost"
                                                    ),
                                                    numericInput("db_port", label = h3("Port"), value = "3306"),
                                                    textInput("db_user", label = h3("Username"), value = "root"),
                                                    passwordInput("db_pass", label = h3("Password"), value = ""),
                                                    br(),
                                                    actionButton("get.tables", label = "Connect To Database"),

                                                    br(),

                                                    uiOutput("database.tables")
                                                )
                                            )
                                        ),

                                        tags$br(),
                                        tags$br(),

                                        actionButton("select.data.button", label = "Mark this as data"),
                                        tags$br(),
                                        tags$br(),

                                        DT::dataTableOutput("inputDataTable")
                                    ),

                                    div(class = "progressStep", taskItem(
                                        value = 30, color = "orange",
                                        "Step 2 of 6"
                                    ))
                                ),

                                tabPanel(
                                    "Select Target Variable",
                                    value = "select.target.tab",

                                    div(class = "secondaryHeaders", h3("ML Plan: Select Target")),
                                    uiOutput("target.variables"),

                                    actionButton("select.target.button", label = "Mark this as target"),

                                    tags$br(),

                                    plotOutput("target.plots"),

                                    div(class = "progressStep", taskItem(
                                        value = 40, color = "orange",
                                        "Step 3 of 6"
                                    ))


                                )

                                # ------------- End of Local Disk Module -------------------


                            )
                        )
                    ))),




            # ------------- Add Data Module -------------------
            tabItem("pipes",
                    fluidRow(column(
                        12,
                        h1("Add Machine Learning Models"),
                        column(
                            12,
                            tabsetPanel(
                                type = "tabs",
                                id = "mlpipes",

                                # ------------- DB Module -------------------

                                tabPanel(
                                    "Select Pipes",
                                    value = "data.split",
                                    div(class = "secondaryHeaders", h3("Select Machine Learning Models")),

                                    # sliderInput(
                                    #     "slider2",
                                    #     label = h3("Train - Test percentages"),
                                    #     min = 0,
                                    #     max = 100,
                                    #     value = c(60),
                                    #     round = 10
                                    # ),
                                    #
                                    # verbatimTextOutput("split.range"),

                                    uiOutput("qualityChecks")

                                )

                            ),
                            div(id = "dataToConfigureDiv",
                                actionButton("dataToConfigure", "Next: Train Models")),

                            div(class = "progressStep", taskItem(
                                value = 60, color = "orange",
                                "Step 4 of 6"
                            ))
                        )

                        # ------------- End of Map/Table Module -------------------
                    ))),

            tabItem("play",
                    fluidRow(column(
                        12,
                        h1("Train Machine Learning Models"),
                        column(
                            12,

                            uiOutput("evaluations"),

                            div(
                                id = "dataToConfigureDiv",
                                actionButton("train.models", "Next: Download Artifacts"),
                                actionButton("breed.models", "Breed Selected Models ")
                            ),

                            div(class = "progressStep", taskItem(
                                value = 80, color = "orange",
                                "Step 5 of 6"
                            ))
                        )

                        # ------------- End of Map/Table Module -------------------
                    ))),

            tabItem("breed",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Breeded Models"),
                            br(),

                            uiOutput("breedModels"),

                            div(id = "dataToConfigureDiv",
                                actionButton("breedTrain", "Next: Train Breeds")),

                            div(class = "progressStep", taskItem(
                                value = 60, color = "orange",
                                "Step 6 of 7"
                            ))
                        )
                    ))),

            tabItem("document",
                    fluidRow(column(
                        12,
                        column(
                            12,
                            h1("Artifacts and Reports"),
                            br(),

                            uiOutput("documentContentUI")


                        )
                    )))
        )
    )

))
