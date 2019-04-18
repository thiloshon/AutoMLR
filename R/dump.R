,

# ------------- Map / Table Module -------------------
column(9,
       tabsetPanel(
           type = "tabs",
           tabPanel(
               "Map View",


               #--------------------


               conditionalPanel(
                   condition = "output.class === 0 && output.regre === 0",
                   actionButton("deploy.classification", label = "Classification Learning"),
                   actionButton("deploy.regression", label = "Regression Learning")

               ),

               conditionalPanel(condition = "output.class > 0",
                                p("Classi")),

               conditionalPanel(condition = "output.regre > 0",
                                p("Regres")),

               verbatimTextOutput("regre"),
               verbatimTextOutput("class"),

               #--------------------



               leafletOutput("mymap", height = "700"),
               absolutePanel(
                   top = 60,
                   right = 20,
                   selectInput(
                       "mapTexture",
                       "Map Texture",
                       choices = list(
                           "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
                           "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite"
                       ),
                       selected = "CartoDB.Positron"
                   ),
                   selectInput(
                       "mapColor",
                       "Points Color",
                       choices = list(
                           "Red" = 'red',
                           "Green" = "green",
                           "Blue" = "blue",
                           "Black" = "black"
                       )
                   )
               )
           ),
           tabPanel("Table View",
                    DT::dataTableOutput("asdf"))
       ))

# -------------  End of Add Data Module -------------------

# ------------- Cleaning Configuration Module -------------------

tabItem("configure",
        fluidRow(column(
            12,
            h1("Configure Cleaning"),
            column(
                12,
                tabsetPanel(
                    type = "tabs",
                    tabPanel(
                        "Option 01: Questionnaire ",
                        div(class = "secondaryHeaders", h3("Option 01: Questionnaire")),
                        helpText("Answer a few questions and let bdclean take care of the cleaning."),


                        # -------------------------------

                        uiOutput("questionnaire")



                        # -------------------------------
                    ),
                    tabPanel(
                        "Option 02: Customized Checks",
                        div(class = "secondaryHeaders", h3("Option 02: Customized Checks")),
                        helpText(
                            "Note: Select the quality checks you prefer and
                            continue cleaning with just those checks"
                        ),

                        # -------------------------------

                        uiOutput("qualityChecks")

                        # -------------------------------

                        ),
                    div(class = "progressStep", taskItem(
                        value = 30, color = "green",
                        "Step 2 of 6"
                    ))
                ),
                div(class = "completedButton", actionButton("configureToFlag", "Next: Flagging"))
            )
        ))),

# ------------- End of Cleaning Configuration Module -------------------


# ------------- Flagging Module -------------------

tabItem("flag",
        fluidRow(column(12,
                        column(
                            12,
                            p("lol")

                        )))),

# ------------- End of Flagging Module -------------------

# ------------- Documentation Module -------------------

tabItem("document",
        fluidRow(column(12,
                        column(
                            12,
                            p("lol")
                        ))))
)

# ------------- End of Documentation Module -------------------

)
))






output$qualityChecksw <- renderUI({
    components <- list()

    for (i in 1:5) {
        components[[i]] <- tagList(
            HTML(
                paste(
                    "<input type=checkbox
                    name=typeInput value=",
                    "test",
                    ">"
                )
            ),
            div(
                class = "checksListContent",
                h4(
                    "dataStore$qualityChecks[[i]]$nameOfQualityCheck"
                ),

                div(class = "checksListTopic col-sm-3", p("Description: ")),
                div(
                    class = "checksListTitle",
                    p("dataStore$qualityChecks[[i]]$description")
                ),

                div(class = "checksListTopic col-sm-3", p("Sample Passing Data: ")),
                div(
                    class = "checksListTitle",
                    p("dataStore$qualityChecks[[i]]$samplePassData")
                ),

                div(class = "checksListTopic col-sm-3", p("Sample Failing Data: ")),
                div(
                    class = "checksListTitle",
                    p("dataStore$qualityChecks[[i]]$sampleFailData")
                ),

                div(class = "checksListTopic col-sm-3", p("Category of Quality Check: ")),
                div(
                    class = "checksListTitle",
                    p("dataStore$qualityChecks[[i]]$checkCategory")
                ),

                div(class = "checksListTopic col-sm-3", p(
                    "DWC Field Targetted by Check: "
                )),
                div(
                    class = "checksListTitle",
                    p("dataStore$qualityChecks[[i]]$targetDWCField")
                )
            ),
            br(),
            br()
        )
    }

    return(
        div(
            id = 'typeInput',
            class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
            tags$br(),
            tags$br(),
            column(width = 12,
                   components)
        )
    )
})

output$domainClteaning <- renderUI({
    components <- list()

    components[[1]] <- tagList(
        HTML(
            paste("<input type=radio
                  name=domainInput value=",
                  "as",
                  ">")
        ),
        div(
            class = "checksListContent",
            h4("Marine Research"),

            div(class = "checksListTopic col-sm-3", p("Description: ")),
            div(
                class = "checksListTitle",
                p(
                    "Researches focused on marine species and marine occarance distribution"
                )
            ),

            div(class = "checksListTopic col-sm-3", p("Quality checks performed: ")),
            div(
                class = "checksListTitle",
                p(
                    "depth_out_of_range_flag, country_coordinate_mismatch_flag, precision_uncertainty_mismatch_flag
                        , center_of_the_country_coordinates_flag
                        , coordinate_negated_flag"
                )
            ),

            div(class = "checksListTopic col-sm-3", p("DWC Fields Targetted by Checks: ")),
            div(class = "checksListTitle", p("coordinates"))
        ),
        br(),
        br()
    )

    return(
        div(
            id = 'domainInput',
            class = "form-group shiny-input-radiogroup shiny-input-container shiny-bound-input",
            tags$br(),
            tags$br(),
            column(width = 12,
                   components)
        )
    )
})
