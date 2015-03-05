library(shiny)

library(datasets)
options(shiny.maxRequestSize=60*1024^2) 
options ( java.parameters = "-Xmx1024m" )
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
    library("zoo", lib.loc="~/R/win-library/3.1")
    library("xts", lib.loc="~/R/win-library/3.1")
    library("TTR", lib.loc="~/R/win-library/3.1")
    library("timeDate", lib.loc="~/R/win-library/3.1")
    library("forecast", lib.loc="~/R/win-library/3.1")
    library("tseries", lib.loc="~/R/win-library/3.1")
    library(XLConnect) 
    #     library(gdata)
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
          #           Workbook <- read.xls(inFile$datapath)
        }, error = function(e) {
          paste("error",e)
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
      #       updateSliderInput(session, "alphaSlider",
      #                         value = input$alphaText)
      #       updateSliderInput(session, "betaSlider",
      #                         value = input$betaText)
      #       updateSliderInput(session, "gammaSlider",
      #                         value = input$gammaText)
      
    })
    
    observeEvent(input$alphaText, 
                 updateNumericInput(session, "alphaSlider", value = input$alphaText)
    )
    observeEvent(input$betaText, 
                 updateNumericInput(session, "betaSlider", value = input$betaText)
    )
    observeEvent(input$gammaText, 
                 updateNumericInput(session, "gammaSlider", value = input$gammaText)
    )
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
        #         w <- readWorksheet(workbook(), sheet=input$Sheet)
        #         return(w[order(w$High),])
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
    
    suggestedYear <- reactive({
      if (!is.null(input$DateVariable) && input$DateVariable!= "")
        getYear(min(table()$Date))
      #       exec <- paste("getYear(min(table()$",input$DateVariable,"))",sep="")
      #       eval(parse(text=exec))
      
    })
    
    
    observeEvent(input$startYear, 
                 if(input$startYear == ""){
                   updateNumericInput(session, "startYear", value = suggestedYear())
                 }
    )
    
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
          #############I NEED START DATE###############
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
          #           exec <- paste("dfts.",input$period,".",input$variable,".withtrend.withseasonal <- HoltWinters(dataProcessed(), alpha=",alphavalue(),",beta=",betavalue(),", gamma=",gammavalue(),")",sep="")
          exec <- paste("HoltWinters(dataProcessed(), alpha=",alphavalue(),",beta=",betavalue(),", gamma=",gammavalue(),",seasonal=\"",input$seasonalMode,"\")",sep="")
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

optimalValue <- reactive({
# if(!is.null(dataProcessed()) && strtrim(input$numberIterationsOptimize)!=""){
if(!is.null(dataProcessed()) && input$numberIterationsOptimize!=""){
  optimizationFunction<- function(x) {   ## Optimization function for ALPHA, BETA and GAMMA for a specific accuracy value.
    winterSolution <- HoltWinters(dataProcessed(), alpha=x[1], beta=x[2], gamma=x[3],seasonal = input$seasonalMode)#,seasonal = "additive")
    #accuracy(dataProcessed(),winterSolution$fitted)[3]#[input$measurement2Optimize]
    index <- as.integer(input$measurement2Optimize)
    if (index<=6){
      accuracy(dataProcessed(),winterSolution$fitted)[as.integer(input$measurement2Optimize)]
    }else if (index==8) {
      RESIDUALS <- dataProcessed()-winterSolution$fitted[,1]
      BIAS <- sum(RESIDUALS)
      MAD <- mean(abs(RESIDUALS))
      abs(BIAS/MAD)#MINIMIZE THE ABS VALUE OF THE TR 
    }else {
      RESIDUALS <- dataProcessed()-winterSolution$fitted[,1]
      abs(sum(RESIDUALS))#MINIMIZE THE ABS VALUE OF THE BIAS-> CLOSER TO 0 BETTER
    }
  }
  
  optimal <-Inf 
  optimal_alpha <-Inf 
  optimal_beta <-Inf 
  optimal_gamma <-Inf 
  TOP <- as.integer(input$numberIterationsOptimize)
  for(i in 1:TOP){
    alpha <- runif(1, 0, 1)
    beta <- runif(1, 0, 1)
    gamma <- runif(1, 0, 1)
    optimalLocal <- suppressWarnings(optim(c(alpha,beta,gamma), optimizationFunction,lower = c(0.0000001,0,0), upper = c(1,1,1)))
    
    #       optimalLocal <- optim(c(alpha,beta,gamma), optimizationFunction,lower = c(0.0000001,0,0), upper = c(1,1,1))
    if(optimalLocal$value<optimal){
      optimal_alpha <-optimalLocal$par[1]
      optimal_beta <-optimalLocal$par[2] 
      optimal_gamma <-optimalLocal$par[3]
      optimal <- optimalLocal$value
    }
  }
  cbind(optimal_alpha,optimal_beta,optimal_gamma,optimal)
}else {
  return(NULL)
}
 })#optimalValue

output$optimalValuesTable <- renderTable({
    if(!is.null(optimalValue())){
       colnames(optimalValue()) <- c("optimal alpha","optimal beta","optimal gamma","optimal") 
      optimalValue()
    }else {
      return(NULL)
    }
}) #OPTIMAL TABLE

wintersOptimalModel <- reactive({
  if(!is.null(optimalValue())){
    HoltWinters(dataProcessed(), alpha=optimalValue()[1], beta=optimalValue()[2], gamma=optimalValue()[3],seasonal = input$seasonalMode)
  }else {
    return(NULL)
  }
})

output$accuracyOptimal <- renderTable({
  if(!is.null(optimalValue()) && !is.null(dataProcessed())){
    winterSolution <- HoltWinters(dataProcessed(), alpha=optimalValue()[1], beta=optimalValue()[2], gamma=optimalValue()[3],seasonal = input$seasonalMode)
    RESIDUALS <- dataProcessed()-wintersOptimalModel()$fitted[,1]
    BIAS <- sum(RESIDUALS)
    TABLE <- accuracy(dataProcessed(),wintersOptimalModel()$fitted)
    MAD <- mean(abs(RESIDUALS))
    TR <- BIAS/TABLE[1,3]
    cbind(TABLE,BIAS,TR)
  }else {
    return(NULL)
  }
}) #ACCURACY OPTIMAL TABLE

output$wintersPlotOptimal <- renderPlot({
  if(!is.null(optimalValue())){
    #winters
    plot(wintersOptimalModel())
  }else {
    return(NULL)
  }
})

wintersPredictionOptimal <- reactive({
  if(!is.null(wintersOptimalModel())){
    exec <- paste("predict(wintersOptimalModel(),n.ahead=",input$predictionPeriodTextOptimal,",level=0.95)",sep="")
    eval(parse(text=exec))   
  }else {
    return(NULL)
  }
})

output$wintersPredictionPlotOptimal <- renderPlot({
  if(!is.null(wintersPredictionOptimal())){
    plot(wintersOptimalModel(), wintersPredictionOptimal())
  }else {
    return(NULL)
  }
})

  })
