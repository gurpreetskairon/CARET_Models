shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Gurpreet Singh"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             helpText("The preprocessing steps and their order are important. ", 
                      "See:", a("Documentation", href="https://www.rdocumentation.org/packages/recipes/versions/0.1.13")),
             
             tabsetPanel(type = "pills",
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy"))
                          )
                        ),
                        column(width = 1, 
                               actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                               bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               )
               
               # maintenance point ------------------------------------------------------------------------------
               # add further tabs (with controls) here
               ,
               tabPanel("blasso Model",
                        verbatimTextOutput(outputId = "blassoModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "blassoPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "blassoGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "blassoGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "blassoMetrics"),
                        hr(),
                        plotOutput(outputId = "blassoModelPlots"),
                        #plotOutput(outputId = "blassoModelTree"),
                        verbatimTextOutput(outputId = "blassoRecipe"),
                        verbatimTextOutput(outputId = "blassoModelSummary2")
               ),
               tabPanel("Cubist Model",
                        verbatimTextOutput(outputId = "CubistModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "CubistPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("bagimpute", "date", "center", "scale", "pca", "dummy"))
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "CubistGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "CubistGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "CubistMetrics"),
                        hr(),
                        plotOutput(outputId = "CubistModelPlots"),
                        verbatimTextOutput(outputId = "CubistRecipe"),
                        verbatimTextOutput(outputId = "CubistModelSummary2")
               ),
               tabPanel("svmLinear3 Model",
                        verbatimTextOutput(outputId = "svmLinear3ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmLinear3Preprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svmLinear3Go", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "svmLinear3Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmLinear3Metrics"),
                        hr(),
                        plotOutput(outputId = "svmLinear3ModelPlots"),
                        #plotOutput(outputId = "svmLinear3ModelTree"),
                        verbatimTextOutput(outputId = "svmLinear3Recipe"),
                        verbatimTextOutput(outputId = "svmLinear3ModelSummary2")
               ),
               tabPanel("bridge Model",
                        verbatimTextOutput(outputId = "bridgeModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "bridgePreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "bridgeGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "bridgeGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bridgeMetrics"),
                        hr(),
                        plotOutput(outputId = "bridgeModelPlots"),
                        #plotOutput(outputId = "bridgeModelTree"),
                        verbatimTextOutput(outputId = "bridgeRecipe"),
                        verbatimTextOutput(outputId = "bridgeModelSummary2")
               ),
               tabPanel("krlsPoly Model",
                        verbatimTextOutput(outputId = "krlsPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "krlsPolyPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "krlsPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "krlsPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "krlsPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "krlsPolyModelPlots"),
                        plotOutput(outputId = "krlsPolyModelTree"),
                        verbatimTextOutput(outputId = "krlsPolyRecipe"),
                        verbatimTextOutput(outputId = "krlsPolyModelSummary2")
               ),
               tabPanel("qrf Model",
                        verbatimTextOutput(outputId = "qrfModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "qrfPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "qrfGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "qrfGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "qrfMetrics"),
                        hr(),
                        plotOutput(outputId = "qrfModelPlots"),
                        #plotOutput(outputId = "qrfModelTree"),
                        verbatimTextOutput(outputId = "qrfRecipe"),
                        verbatimTextOutput(outputId = "qrfModelSummary2")
               ),
               tabPanel("bstTree Model",
                        verbatimTextOutput(outputId = "bstTreeModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "bstTreePreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy"))
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "bstTreeGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "bstTreeGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bstTreeMetrics"),
                        hr(),
                        plotOutput(outputId = "bstTreeModelPlots"),
                        plotOutput(outputId = "bstTreeModelTree"),
                        verbatimTextOutput(outputId = "bstTreeRecipe"),
                        verbatimTextOutput(outputId = "bstTreeModelSummary2")
               ),
               tabPanel("M5 Model",
                        verbatimTextOutput(outputId = "M5ModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "M5Preprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("bagimpute", "date", "dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "M5Go", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "M5Go", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "M5Metrics"),
                        hr(),
                        plotOutput(outputId = "M5ModelPlots"),
                        #plotOutput(outputId = "M5ModelTree"),
                        verbatimTextOutput(outputId = "M5Recipe"),
                        verbatimTextOutput(outputId = "M5ModelSummary2")
               ),
               tabPanel("gbm Model",
                        verbatimTextOutput(outputId = "gbmModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gbmPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("bagimpute", "date", "dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gbmGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gbmGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gbmMetrics"),
                        hr(),
                        plotOutput(outputId = "gbmModelPlots"),
                        plotOutput(outputId = "gbmModelTree"),
                        verbatimTextOutput(outputId = "gbmRecipe"),
                        verbatimTextOutput(outputId = "gbmModelSummary2")
               ),
               tabPanel("treebag Model",
                        verbatimTextOutput(outputId = "treebagModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "treebagPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "treebagGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "treebagGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "treebagMetrics"),
                        hr(),
                        plotOutput(outputId = "treebagModelPlots"),
                        plotOutput(outputId = "treebagModelTree"),
                        verbatimTextOutput(outputId = "treebagRecipe"),
                        verbatimTextOutput(outputId = "treebagModelSummary2")
               ),
               tabPanel("gaussprRadial Model",
                        verbatimTextOutput(outputId = "gaussprRadialModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprRadialPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprRadialGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprRadialGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprRadialMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprRadialModelPlots"),
                        #plotOutput(outputId = "gaussprRadialModelTree"),
                        verbatimTextOutput(outputId = "gaussprRadialRecipe"),
                        verbatimTextOutput(outputId = "gaussprRadialModelSummary2")
               ),
               tabPanel("gaussprPoly Model",
                        verbatimTextOutput(outputId = "gaussprPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gaussprPolyPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("bagimpute", "date", "dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gaussprPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gaussprPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gaussprPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "gaussprPolyModelPlots"),
                        plotOutput(outputId = "gaussprPolyModelTree"),
                        verbatimTextOutput(outputId = "gaussprPolyRecipe"),
                        verbatimTextOutput(outputId = "gaussprPolyModelSummary2")
               )
               ,
               tabPanel("gamSpline Model",
                        verbatimTextOutput(outputId = "gamSplineModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "gamSplinePreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "gamSplineGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "gamSplineGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "gamSplineMetrics"),
                        hr(),
                        plotOutput(outputId = "gamSplineModelPlots"),
                        #plotOutput(outputId = "gamSplineModelTree"),
                        verbatimTextOutput(outputId = "gamSplineRecipe"),
                        verbatimTextOutput(outputId = "gamSplineModelSummary2")
               ),
               tabPanel("kknn Model",
                        verbatimTextOutput(outputId = "kknnModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "kknnPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "kknnGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "kknnGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "kknnMetrics"),
                        hr(),
                        plotOutput(outputId = "kknnModelPlots"),
                        #plotOutput(outputId = "kknnModelTree"),
                        verbatimTextOutput(outputId = "kknnRecipe"),
                        verbatimTextOutput(outputId = "kknneModelSummary2")
               ),
               tabPanel("brnn Model",
                        verbatimTextOutput(outputId = "brnnModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "brnnPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("bagimpute", "date", "center", "scale", "pca", "dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "brnnGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "brnnGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "brnnMetrics"),
                        hr(),
                        plotOutput(outputId = "brnnModelPlots"),
                        #plotOutput(outputId = "brnnModelTree"),
                        verbatimTextOutput(outputId = "brnnRecipe"),
                        verbatimTextOutput(outputId = "brnnModelSummary2")
               ),
               tabPanel("rbfDDA Model",
                        verbatimTextOutput(outputId = "rbfDDAModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "rbfDDAPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "rbfDDAGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "rbfDDAGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "rbfDDAMetrics"),
                        hr(),
                        plotOutput(outputId = "rbfDDAModelPlots"),
                        verbatimTextOutput(outputId = "rbfDDARecipe"),
                        verbatimTextOutput(outputId = "rbfDDAModelSummary2")
               )
               ,
               tabPanel("pcaNNet Model",
                        verbatimTextOutput(outputId = "pcaNNetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "pcaNNetPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "pcaNNetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "pcaNNetGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "pcaNNetMetrics"),
                        hr(),
                        plotOutput(outputId = "pcaNNetModelPlots"),
                        #plotOutput(outputId = "pcaNNetModelTree"),
                        verbatimTextOutput(outputId = "pcaNNetRecipe"),
                        verbatimTextOutput(outputId = "pcaNNetModelSummary2")
               ),
               tabPanel("svmPoly Model",
                        verbatimTextOutput(outputId = "svmPolyModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "svmPolyPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "svmPolyGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "svmPolyGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "svmPolyMetrics"),
                        hr(),
                        plotOutput(outputId = "svmPolyModelPlots"),
                        #plotOutput(outputId = "svmPolyModelTree"),
                        verbatimTextOutput(outputId = "svmPolyRecipe"),
                        verbatimTextOutput(outputId = "svmPolyModelSummary2")
               ),
               tabPanel("bagEarth Model",
                        verbatimTextOutput(outputId = "bagEarthModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "bagEarthPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "bagEarthGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "bagEarthGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "bagEarthMetrics"),
                        hr(),
                        plotOutput(outputId = "bagEarthModelPlots"),
                        plotOutput(outputId = "bagEarthModelTree"),
                        verbatimTextOutput(outputId = "bagEarthRecipe"),
                        verbatimTextOutput(outputId = "bagEarthModelSummary2")
               ),
               tabPanel("blackboost Model",
                        verbatimTextOutput(outputId = "blackboostModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "blackboostPreprocess", # name this control <Method>Preprocess
                                                label = "Pre-processing", 
                                                choices = ppchoices,  
                                                multiple = TRUE, 
                                                selected = c("knnimpute","date","dummy")) 
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "blackboostGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "blackboostGo", title = "This will train or retrain your model")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "blackboostMetrics"),
                        hr(),
                        plotOutput(outputId = "blackboostModelPlots"),
                        plotOutput(outputId = "blackboostModelTree"),
                        verbatimTextOutput(outputId = "blackboostRecipe"),
                        verbatimTextOutput(outputId = "blackboostModelSummary2")
               )
               
               
               
               ###
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width=4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width=2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width=2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
