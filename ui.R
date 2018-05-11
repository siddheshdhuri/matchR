
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("xyz"),
  
    fluidRow(
      column(6, wellPanel(
       
        fluidRow(
          column(6,
                 fileInput('uploadedFile', 'Choose CSV File', 
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain',
                                    '.csv'))
          ),
          column(6,
                 selectInput("acNameCol", "Account Name Col", choices=NULL, multiple = TRUE),
                 selectInput("postcodeCol", "Postcode Col", choices=NULL, multiple = TRUE)
                 
          ),
          actionButton("shortName","Short Name")
          
        ),
        
        tags$hr(),
        
        shiny::tableOutput("tablePreview")
        
      )),
      
      column(6, wellPanel(
        
        fluidRow(
          column(6,
             fileInput('uploadedFile2', 'Choose CSV File', 
                       accept=c('text/csv', 
                                'text/comma-separated-values,text/plain',
                                '.csv'))
          ),
          column(6,
              selectInput("acNameCol2", "Account Name Col", choices=NULL, multiple = TRUE),
              selectInput("postcodeCol2", "Postcode Col", choices=NULL, multiple = TRUE)
          ),
          actionButton("shortName2","Short Name"),
          actionButton("getLiveCust", "Get Live Cust from DB")
        ),
        
        tags$hr(),
        
        shiny::tableOutput("tablePreview2")
        
      ))
    ),
    fluidRow(
      actionButton("match","Match"),
      downloadButton("downloadData", "Download"),
      shiny::tableOutput("tablePreview3")
      
    )
  
    
  )
)
