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
                             c("daily","weekly","monthly","quarterly")),
                 textInput("startYear", label = "Suggested Start Year", 
                           value = "")
                 #                  ,
                 #                  textOutput("detectedYear")
                 
                 
        ),# END 2nd tabPanel
        tabPanel("Best Model",
                 selectInput("measurement2Optimize", "Select accuracy value:",
                             c("ME" = 1,"MRSE" = 2,"MAE/MAD"=3,"MPE"=4,"MAPE"=5,"ACF1"=6,"Absolute BIAS"=7,"TR"=8)),
                 textInput("numberIterationsOptimize", label = "Select Number of Iterations for Optimization:", 
                           value = "10")
                 
        )# END 3rd tabPanel
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
                                step = 0.02, round=FALSE, animate=
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
                    fluidRow(column(3,h5(strong("Gamma:"))), 
                             column(9,radioButtons("seasonalMode", label = NULL,c("Additive"="additive",
                                                                                  "Multiplicative"="multiplicative"),inline=TRUE))
                    ),
                    textInput("gammaText", label = NULL, 
                              value = "0.5"),
                    sliderInput("gammaSlider", label = NULL, 0, 1, 0.5,
                                step = 0.02, animate=
                                  animationOptions(interval=300, loop=TRUE))
             )       
           ),
           
           fluidRow(
             tabsetPanel(
               
               tabPanel("Model",plotOutput("wintersPlot", width = "100%"))
               ,
               tabPanel("Prediction",
                        textInput("predictionPeriodText", label = "Number of Periods:", 
                                  value = "5"),
                        plotOutput("wintersPredictionPlot", width = "100%")
               )
               , id = "TABSETPANEL_WINTERS", selected = "Model"
             )
             
           )
           
           ,fluidRow(tableOutput("accuracy"))
           
           
           
  )
}#Winters
# ,
# {
#   tabPanel("Accuracy",br(),tableOutput("accuracy")
#   )
# }#Accuracy
,
{
  tabPanel("Optimization",br()
           ,tableOutput("optimalValuesTable")
           ,br()
           ,tableOutput("accuracyOptimal")
           #            ,br()
           #            ,plotOutput("wintersPlotOptimal", width = "100%")
           ,
           fluidRow(
             tabsetPanel(
               
               tabPanel("Model",plotOutput("wintersPlotOptimal", width = "100%"))
               ,
               
               tabPanel("Prediction",
                        textInput("predictionPeriodTextOptimal", label = "Number of Periods:", 
                                  value = "5"),
                        plotOutput("wintersPredictionPlotOptimal", width = "100%")
               )
               
               , id = "TABSETPANEL_WINTERS_OPTIMAL", selected = "Model"
             )
             
           )
  )
}#Optimization

      )
    )
  )
)

)
