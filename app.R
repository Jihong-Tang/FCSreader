library(shiny)
library(flowCore)
library(tidyverse)
library(openxlsx)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("FCSreader"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose FCS File",
                accept = ".fcs"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select input file type ----
      radioButtons("intype", "File Type",
                   choices = c(Index = "index",
                               Other = "other"),
                   selected = "index"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      tags$hr(),
      
      tags$br(),
      
      tags$br(),
      
      tags$br(),
      
      tags$hr(),
      
      # Input: Select output file format ----
      radioButtons("outtype", "Save Format",
                   choices = c(Excel = "excel",
                               Text = "txt",
                               CSV = "csv"),
                   selected = "excel"),
      tags$hr(),
      # Button
      downloadButton("downloadData", "Download")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(
        tabPanel("Table Show", 
                 dataTableOutput("tableshow")),
        tabPanel("Other fun", "contents2")
      )
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  datainput <- reactive({
    fcm <- read.FCS(input$file1$datapath)
    fcm <- as.data.frame((exprs(fcm)))
  })
  
  output$tableshow <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    if(input$disp == "head") {
      return(head(datainput()))
    }else if(input$disp == "all") {
      return(datainput())
    }
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$outtype == "excel"){
        sub("fcs", "xlsx", input$file1$name)
      }else if(input$outtype == "txt"){
        sub("fcs", "txt", input$file1$name)
      }else if(input$outtype == "csv"){
        sub("fcs", "csv", input$file1$name)
      }
    },
    content = function(file) {
      if(input$outtype == "excel"){
        write.xlsx(datainput(), file)
      }else if(input$outtype == "txt"){
        write_delim(datainput(), file)
      }else if(input$outtype == "csv"){
        write_csv(datainput(), file)
      }
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)