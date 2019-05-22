library(shiny)
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(leaflet))
library(DT)
library(kernlab)
library(extraTrees)
library(FNN)


shinyUI(dashboardPage(
    #Header Title
    dashboardHeader(title = tags$img(src='logo3.png'), dropdownMenuOutput("messageMenu")),

    # ------------- Sidebar  -------------------
    dashboardSidebar(
        sidebarMenu(
            id = "sideBar",
            menuItem("Setup Plan",
                     tabName = "add",
                     icon = icon("cog")),

            menuItem("Generate Pipes",
                tabName = "pipes",
                icon = icon("cubes")
            ),

            menuItem("Play Plan",
                     tabName = "play",
                     icon = icon("play")),

            menuItem("Evaluate Plan",
                     tabName = "breed",
                     icon = icon("vial")),

            menuItem("Download Artifacts",
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



        tabItems(# ------------- Add Data Module -------------------
                 tabItem("add",
                         fluidRow(
                             column(12,
                                    h1("Create Machine Learning Plan"),
                                    column(
                                        12,
                                        tabsetPanel(
                                            type = "tabs",
                                            id = "mlplan",

                                            # ------------- DB Module -------------------

                                            tabPanel(
                                                "Select Learning Type",
                                                value = "select.type",

                                                div(class = "secondaryHeaders", h3("Select Learning Type of ML Plan:")),
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
                                                    ),



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
                                    ))
                         )),




                 # ------------- Add Data Module -------------------
                 tabItem("pipes",
                         fluidRow(
                             column(
                                 12,
                                 h1("Add Machine Learning Pipes"),
                                 column(
                                     12,
                                     tabsetPanel(
                                         type = "tabs",
                                         id = "mlpipes",

                                         # ------------- DB Module -------------------

                                         tabPanel(
                                             "Select Pipes",
                                             value = "data.split",
                                             div(class = "secondaryHeaders", h3("Select Machine Learning Pipes")),

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
                             )
                         )),

                 tabItem("play",
                         fluidRow(
                             column(
                                 12,
                                 h1("Train Machine Learning Pipes"),
                                 column(
                                     12,

                                     uiOutput("evaluations"),

                                     div(id = "dataToConfigureDiv",
                                         actionButton("train.models", "Next: Download Artifacts"),
                                         actionButton("breed.models", "Breed Selected Models ")),

                                     div(class = "progressStep", taskItem(
                                         value = 80, color = "orange",
                                         "Step 5 of 6"
                                     ))
                                 )

                                 # ------------- End of Map/Table Module -------------------
                             )
                         )),

                 tabItem("breed",
                         fluidRow(column(
                             12,
                             column(
                                 12,
                                 h1("Breeded Models"),
                                 br(),

                                 uiOutput("breedModels")


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
