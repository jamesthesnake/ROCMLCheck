
library(shiny)
library(d3heatmap)
library(RColorBrewer)
library(edgeR)
library(GO.db)
library(goseq)
library(networkD3)
library(pROC)


# frontend
ui <- shinyUI(pageWithSidebar(
  
  headerPanel( "DataChoice"),
  sidebarPanel(
    downloadButton("downloadData", label = "Download Sample Input File"),
    fileInput("filename", "Choose File to Upload:", accept = c('.csv')),
    fileInput("filename2", "Choose File to Upload:", accept = c('.csv')),
    fileInput("filename3", "Choose File to Upload:", accept = c('.csv')),

    
    uiOutput("expcolumns"),
    actionButton("goButton", "Run Statistics"),
    downloadButton("downloadtable", "Download Stats Table"),
    downloadButton("downloadRoc", "Download ROC Graph")
    
    
    
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Instructions", textOutput("text1"), textOutput("text2"), textOutput("text3")),
      tabPanel("stat Analysis", tableOutput("table")),
      
      tabPanel("Graph Analysis",plotOutput("graph")),
      tabPanel("Decison",textOutput("text5"))
    ),
    
    width = 8,
    height = 10
    
  )
  
  
)
)
