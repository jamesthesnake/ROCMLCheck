
library(shiny)
library(d3heatmap)
library(htmlwidgets)
library(tools)
library(edgeR)
library(GO.db)
library(goseq)
library(networkD3)
library(data.table)
library(dplyr)
library(png)
library(webshot)

# backend 
server <- shinyServer(function(input, output) {	
  
  output$text1<-renderText({ "input your files for stat analysis"})
  output$text2<-renderText({ "for the ROC tab, input for change and you can download the pic"})
  output$text3<-renderText({ "More features will be added later"})
  output$text5<-renderText({ "FDR is the best model in this case"})
  
  datasetInput <- reactive({
    validate(
      need(input$filename != 0, "To perform statistical analysis, please select a file for input") 
    )
    inFile <- input$filename
    if (is.null(inFile)) return(NULL)
    read.table(inFile$datapath, header= TRUE, sep=",", quote='"', row.names=1)
  })
  
  
  
#  datasetInputTwo <- reactive({
 #   validate(
  #    need(input$filename2 != 0, "To perform statistical analysis, please select second file for input") 
   # )
    #inFile <- input$filename2
    #if (is.null(inFile)) return(NULL)
    #read.table(inFile$datapath, header= TRUE, sep=",", quote='"', row.names=1)
  #})
  
  #datasetInputThree <- reactive({
   # validate(
    #  need(input$filename3 != 0, "To perform statistical analysis, please select a third file for input") 
    #)
    #inFile <- input$filename3
    #if (is.null(inFile)) return(NULL)
    #read.table(inFile$datapath, header= TRUE, sep=",", quote='"', row.names=1)
    
  #})
  
  slimStats <- reactive({
    results_df <- stats()
    datasetInputTwo <- datasetInput()
    
    if (input$pvFDRchoose == "Pvalue"){
      pNewResults_df <- results_df[which(results_df$PValue<input$statPV),]
      pNames <- row.names(pNewResults_df)
      pFinalResult <- datasetInputTwo[pNames,]
    }
    else if(input$pvFDRchoose == "FDR"){
      fNewResults_df <- results_df[which(results_df$FDR<input$statFDR),]
      fNames <- row.names(fNewResults_df)
      fFinalResult <- datasetInputTwo[fNames,]
    }
    else if(input$pvFDRchoose == "both"){
      bNewResults_df <- results_df[which(results_df$FDR<input$statFDR & results_df$PValue<input$statPV),]
      bNames <- row.names(bNewResults_df)
      bFinalResult <- datasetInputTwo[bNames,]
    }
  })
  
  # differential expression analysis table
  `%then%` <- shiny:::`%OR%`
  output$table <- renderTable({
    if (input$goButton == 0) {return(validate(
      need(input$filename != 0, "To run differential expression analysis, please first select a file for input") %then%
        need(input$goButton != 0, "To run differential expression analysis, please select your experimental samples under 'Specify Non-Control Samples' then click 'Run Statistics'")
    ))}        
    else {
      stats()
    }
  }, digits = -3)
  
  
  # statistical table download
  output$downloadtable <- downloadHandler(
    filename = function() {
      paste(basename(file_path_sans_ext(input$filename)), '_edgeR_stats_table', '.csv', sep='')
    },
    content = function(file) {
      if (input$goButton != 0) write.csv(stats(), file) else return()
    }
  )
  
  # rocGraph download
  output$downloadRoc <- downloadHandler(
    filename = function() {
      paste0(basename(file_path_sans_ext(input$filename)), '.html')
    },
    content = function(file) {
      heres<-plot()
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(heres, file="temp.html", selfcontained = F) 
      
      webshot("temp.html", file = file,
              cliprect = "viewport")
    }
    }
  )
  output$expcolumns <- renderUI({
    df <- datasetInput()
    if (is.null(df)) return(NULL)
    expcolumns <- names(df)
    selectInput('expcolumns', 'Specify Non-Control Samples:', choices = expcolumns, multiple = TRUE)
  })
  
  output$graph<-renderPlot({
    predictions<-stats()
    size<-length(predictions[,1])
    halfSize<-as.integer(size/2)
    labels<-predictions
    labels[,][1:halfSize,]<- -1
    labels[,][halfSize:size,]<- 1

    pred<-prediction(predictions,labels)
    perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
    plot(perf, col=rainbow(10))
  })
  stats <- reactive({
    if(!is.null(datasetInput()) & !is.null(input$expcolumns)){
      
      group <- as.numeric(names(datasetInput()) %in% input$expcolumns)
      y <- DGEList(counts = datasetInput(), group = group)
      y <- calcNormFactors(y)
      y <- estimateCommonDisp(y)
      y <- estimateTagwiseDisp(y)
      et <- exactTest(y)
      results <- topTags(et, n=50000)

      results_df <- as.data.frame(results)
      
      
    }
  })
})
