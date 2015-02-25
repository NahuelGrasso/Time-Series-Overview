library(shiny)
library(dygraphs)
shinyUI(fluidPage(
  titlePanel("Time Series Analysis"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Select File",
                 fileInput('fileSelected', 'Choose file to upload',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv',
                             '.tsv',
                             '.xlsx',
                             '.xls'
                           )
                 ),
                 conditionalPanel(
                   condition = "!is.null('SheetSelector')",
                   uiOutput("SheetSelector", width = "100%")
                   
                 ),
                 tags$hr(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"'),
                 tags$hr()
                 
                 
        ),# END 1ST tabPanel
        tabPanel("Select Variables",
                 uiOutput("DateVariableSelector", width = "100%"),
                 uiOutput("variableSelector", width = "100%"),
                 selectInput("period", "Select period:",
                             c("daily","weekly","monthly","quarterly"))
                 
        )
        
        , id = "TABSETPANEL1", selected = "Select File"
      )#END TABSETPANEL
      
    )#END SIDEBARPANEL
    ,
    mainPanel(
      
      tabsetPanel(
        {
          tabPanel("Table Imported",dataTableOutput("ImportedTable"))
        },#Table Imported
        {
          tabPanel("Decomposition",plotOutput("decompositionPlot", width = "100%"))
        }#Decomposition
        ,
        {
          tabPanel("Winters",
                   fluidRow(
                     
                     column(4, 
                            h5(strong("Alpha:")),
                            textInput("alphaText", label = NULL, 
                                      value = "0.5"),
                            sliderInput("alphaSlider", label = NULL, 0.005, 1, 0.5,
                                        step = 0.02, animate=
                                          animationOptions(interval=300, loop=TRUE))
                     ),
                     column(4, 
                            h5(strong("Beta:")),
                            textInput("betaText", label = NULL, 
                                      value = "0.5"),
                            sliderInput("betaSlider", label = NULL, 0, 1, 0.5,
                                        step = 0.02, animate=
                                          animationOptions(interval=300, loop=TRUE))
                     ),
                     column(4, 
                            h5(strong("Gamma:")),
                            textInput("gammaText", label = NULL, 
                                      value = "0.5"),
                            sliderInput("gammaSlider", label = NULL, 0, 1, 0.5,
                                        step = 0.02, animate=
                                          animationOptions(interval=300, loop=TRUE))
                     )       
                   ),
                   # Create a new row for the table.
                   fluidRow(
                     tabsetPanel(
                        {
                          tabPanel("Model",plotOutput("wintersPlot", width = "100%"))
                        },#Table Imported
                        {
                          tabPanel("Prediction",
                                   textInput("predictionPeriodText", label = "Number of Periods:", 
                                             value = "5"),
                                   plotOutput("wintersPredictionPlot", width = "100%")
                                  )
                        }#Decomposition
                        , id = "TABSETPANEL_WINTERS", selected = "Model"
                     )
                     
                   )
                   
                   
                   
                   )
        }#Winters
        ,
        {
         tabPanel("Accuracy",br(),tableOutput("accuracy"))
#         tabPanel("Accuracy",dataTableOutput("accuracy"))
        }#Accuracy
        
      )
    )
  )
)

)
