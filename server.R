library(shiny)
library(datasets)

distVariables <- function(input1,input2) {
  if (input == input2) {
    "Choose another variable to analyze time series"
  } else if (input == "") { 
    FALSE
  } else {
    NULL
  }
}

is.timeBased <- function (x) 
{
  if (!any(sapply(c("Date", "POSIXct","POSIXt", "chron", "dates", "times", 
                    "timeDate", "yearmon", "yearqtr", "xtime"), function(xx) inherits(x, xx)))) {
    FALSE
  }
  else TRUE
}

shinyServer(
  function(input, output, session) {
    library(XLConnect) 
    
    library("zoo", lib.loc="~/R/win-library/3.1")
    library("xts", lib.loc="~/R/win-library/3.1")
    library("TTR", lib.loc="~/R/win-library/3.1")
    library("timeDate", lib.loc="~/R/win-library/3.1")
    library("forecast", lib.loc="~/R/win-library/3.1")
    library("tseries", lib.loc="~/R/win-library/3.1")
    library(XLConnect) 
    library(dygraphs)
    
    frequency.daily <- "365"
    frequency.weekly <- "52"
    frequency.monthly <- "12"
    frequency.quarterly <- "4"
    
    workbook <- reactive({
      inFile <- input$fileSelected
      
      if (is.null(inFile))
        return(NULL)
      result = tryCatch({
        Workbook <- read.csv(inFile$datapath, header = input$header,
                             sep = input$sep, quote = input$quote)
      }, warning = function(w) {
        "error"
      }, error = function(e) {
        "error"
      })
      if(!is.object(result)){
        result  <-  tryCatch({
          Workbook <- loadWorkbook(inFile$datapath)
        }, error = function(e) {
          "error"
        })
        
      }
      if(!is.object(result)){
        return(NULL)
      }
      Workbook
    })
    
    sheets <- reactive({          
      if (is.null(workbook()))
        return(NULL)
      result = tryCatch({
        getSheets(workbook())
      }, warning = function(w) {
        "error"
      }, error = function(e) {
        "error"
      })
      
      if(length(result) == 1 && result=="error")
        return(NULL)
      
      return(result)
      
    })
    
    output$SheetSelector <- renderUI({
      if(is.null(sheets()))
        return(NULL)
      
      selectInput("Sheet", "Select Sheet:", as.list(c("",unique(sheets()))),selected = "") 
    })
    
    observe({
      if (!is.null(variables())) {
        updateTabsetPanel(session, "TABSETPANEL1", selected = "Select Variables")
      } 
      updateSliderInput(session, "alphaSlider",
                        value = input$alphaText)
      updateSliderInput(session, "betaSlider",
                        value = input$betaText)
      updateSliderInput(session, "gammaSlider",
                        value = input$gammaText)
      
    })
    
    observeEvent(input$alphaSlider, 
      updateNumericInput(session, "alphaText", value = input$alphaSlider)
    )
    observeEvent(input$betaSlider, 
      updateNumericInput(session, "betaText", value = input$betaSlider)
    )
    observeEvent(input$gammaSlider, 
      updateNumericInput(session, "gammaText", value = input$gammaSlider)
    )
    
    table <- reactive({
      if(is.null(workbook()) || is.null(input$Sheet)|| ''==input$Sheet){
        return(NULL)
      }else{
        if(!is.null(workbook()) && is.null(sheets()))
          return(workbook())
        return(readWorksheet(workbook(), sheet=input$Sheet))
      }
      #       return(readWorksheet(workbook(), sheet=input$Sheet))
      #       if(!is.null(workbook()) && is.null(sheets()))
      #         return(workbook())
    })
    
    #     table <- reactive({
    #       if(!is.null(workbook()) && is.null(sheets()))
    #         return(workbook())
    #     })
    
    output$ImportedTable <- renderDataTable(
      #       if(is.null(table()))
      #         return(NULL)
      table(),options = list(pageLength = 10)
    )
    
    
    variables <- reactive({
      if(is.null(table()))
        return(NULL)
      return(colnames(table())) 
    })
    
    output$DateVariableSelector <- renderUI({
      selectInput("DateVariable", "Select Date variable", as.list(c("",unique(variables()))),selected = "") 
    })
    
    
    
    output$variableSelector <- renderUI({
      
      selectInput("variable", "Select variable", as.list(c("",unique(variables()))),selected = "") 
    })  
    
    dataProcessed <- reactive({
      if(!is.null(input$DateVariable) && !is.null(input$variable)){
        if((input$DateVariable!="") && (input$variable!="")){
          #           if(!is.timeBased(input$DateVariable)){
          #             DateVariable <- as.Date(input$DateVariable)
          #           }else{DateVariable <- input$DateVariable}
          exec <- paste("df.",input$period,".",input$variable," <- apply.",input$period,"(xts(table()$",input$variable,", order.by=table()$",input$DateVariable,"), FUN=mean)",sep="")
          eval(parse(text=exec))
          
          freq <- get(paste("frequency.",input$period,sep=""))
          exec <- paste("dfts.",input$period,".",input$variable," <- ts(df.",input$period,".",input$variable,",frequency = ",freq,",start=c(2005,1))",sep="")
          eval(parse(text=exec))
          
        }else {
          return(NULL)
        }
      }else {
        return(NULL)
      }
    })
    
{
      output$decompositionPlot <- renderPlot({
        if(!is.null(dataProcessed())){
          #decompose
          exec <- paste("dfts.",input$period,".",input$variable,".components <-decompose(dataProcessed())",sep="")
          eval(parse(text=exec))
          
          decomposition <- get(paste("dfts.",input$period,".",input$variable,".components", sep=""))
          plot(decomposition)
        }else {
          return(NULL)
        }
      })
      
      alphavalue <- reactive({
        if(input$alphaSlider == 0){
          return(FALSE)
        }else{
          input$alphaSlider
        }
      })
      betavalue <- reactive({
        input$alphaSlider
      })
      gammavalue <- reactive({
        input$alphaSlider
      })
      
      wintersModel <- reactive({
        if(!is.null(dataProcessed())){
          #winters
          exec <- paste("dfts.",input$period,".",input$variable,".withtrend.withseasonal <- HoltWinters(dataProcessed(), alpha=",alphavalue(),",beta=",betavalue(),", gamma=",gammavalue(),")",sep="")
          eval(parse(text=exec))
        }else {
          return(NULL)
        }
      })
      
      wintersPrediction <- reactive({
        if(!is.null(dataProcessed())){
          #winters
          exec <- paste("predict(wintersModel(),n.ahead=",input$predictionPeriodText,",level=0.95)",sep="")
          eval(parse(text=exec))
          #           dfts.monthly.High.withtrend.withoutseasonal.prediction <- predict(dfts.monthly.High.withtrend.withoutseasonal,n.ahead=4,level=0.95)
          
        }else {
          return(NULL)
        }
      })
      
      output$wintersPlot <- renderPlot({
        if(!is.null(wintersModel())){
          #winters
          plot(wintersModel())
        }else {
          return(NULL)
        }
      })
      output$wintersPredictionPlot <- renderPlot({
        if(!is.null(wintersPrediction())){
          #winters
          plot(wintersModel(), wintersPrediction())
        }else {
          return(NULL)
        }
      })
      
    }#PLOTS
output$accuracy <- renderTable({
  if(!is.null(dataProcessed()) && !is.null(wintersModel())){
    RESIDUALS <- dataProcessed()-wintersModel()$fitted[,1]
    BIAS <- sum(RESIDUALS)
    TABLE <- accuracy(dataProcessed(),wintersModel()$fitted)
    MAD <- mean(abs(RESIDUALS))
    TR <- BIAS/TABLE[1,3]
    cbind(TABLE,BIAS,TR)
  }else {
    return(NULL)
  }
}) #ACCURACY TABLE

  })
