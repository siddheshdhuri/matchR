
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {
  
  options(shiny.maxRequestSize=60*1024^2)
  
  reactive.values <- reactiveValues()
  
  reactive.values$accounts.table <- NULL
  reactive.values$accounts.table2 <- NULL
  reactive.values$accounts.table3 <- NULL
  
  observeEvent(input$uploadedFile,{
    
    df <- NULL
    origina.colnames <- ""
    
    withProgress(message = 'Reading CSV into Table',{
      
      # Expensive stuff here
      
      df <- read.csv(input$uploadedFile$datapath, header=TRUE, encoding = 'UTF-8', fileEncoding = 'ISO8859-1')
      # df$Account.Name <- tolower(df$Account.Name)
      # original.colnames <- colnames(df)
    })
    
    updateSelectInput(session, "acNameCol", choices = colnames(df))
    updateSelectInput(session, "postcodeCol", choices = colnames(df))
    
    withProgress(message = 'Removing NAs',{
      
      df <- as.data.frame(lapply(df, function(x) gsub(x,pattern = na.patterns, replacement = "")))
      
    })
    
    updateSelectInput(session, "acNameCol", choices = colnames(df))
    
    reactive.values$accounts.table <- df
    
  })
  
  
  observeEvent(input$uploadedFile2,{
    
    df <- NULL
    origina.colnames <- ""
    
    withProgress(message = 'Reading CSV into Table',{
      
      # Expensive stuff here
      df <- read.csv(input$uploadedFile2$datapath, header=TRUE, encoding = 'UTF-8', fileEncoding = 'ISO8859-1')
      
    })
    
    updateSelectInput(session, "acNameCol2", choices = colnames(df))
    updateSelectInput(session, "postcodeCol2", choices = colnames(df))
    
    withProgress(message = 'Removing NAs',{
      
      df <- as.data.frame(lapply(df, function(x) gsub(x,pattern = na.patterns, replacement = "")))
      
    })
    
    updateSelectInput(session, "acNameCol2", choices = colnames(df), selected = 1)
    
    reactive.values$accounts.table2 <- df
    
  })
  
  
  
  observeEvent(input$getLiveCust, {
    
    live_cust_data <- RJDBC::dbGetQuery(jdbcConnection, live_cust_query)
    
    reactive.values$accounts.table2 <- live_cust_data
    
    updateSelectInput(session, "acNameCol2", choices = colnames(live_cust_data))
    updateSelectInput(session, "postcodeCol2", choices = colnames(live_cust_data))
    
  })
  
  

  output$tablePreview <- renderTable({
    
    acNameCol <- input$acNameCol
    
    display.table <- 
    
    if (is.null(reactive.values$accounts.table))
      return(" Load CSV File")
    
    return(head(isolate(reactive.values$accounts.table[,acNameCol]),10))
    
  })
  
  output$tablePreview2 <- renderTable({
    
    acNameCol <- input$acNameCol2
    
    if (is.null(reactive.values$accounts.table2))
      return(" Load CSV File")
    
    return(head(isolate(reactive.values$accounts.table2[,acNameCol]),10))
    
  })
  
  
  output$exportData <- downloadHandler(
    filename = function() { paste0("Unique_Name", '.csv') },
    content = function(file) {
      write.table(reactive.values$accounts.table, file, sep = ",", row.names = FALSE, fileEncoding="UTF-8")
    }
  )
  
  
  observeEvent(input$shortName,{
    
    df <- withProgress(message = 'computing short names',{
      computeShortName(reactive.values$accounts.table,input$acNameCol[1])
    })
    reactive.values$accounts.table <- df
    output$tablePreview <- renderTable({head(df[,c(input$acNameCol,"Account.Name.Short")],10)})
    
  })

  observeEvent(input$shortName2,{
    
    df <- withProgress(message = 'computing short names',{
      computeShortName(reactive.values$accounts.table2,input$acNameCol2[1])
    })
    reactive.values$accounts.table2 <- df
    output$tablePreview2 <- renderTable({head(df[,c("Account.Name.Short",input$acNameCol2)],10)})
    
  })
  
  observeEvent(input$match,{
    
    df <- withProgress(message = 'matching....',{
      matchAccountNames(reactive.values$accounts.table, reactive.values$accounts.table2)
    })
    
    df$REMOVE <- "KEEP"
    print("234234234  324234234 ")
    if(length(input$postcodeCol) > 0 & length(input$postcodeCol2) > 0) {
      
      postcode_df <- reactive.values$accounts.table2[,input$postcodeCol2,drop=FALSE]
      postcode_df$POSTCODE_MATCH = "POSTCODE_MATCH"
      df <- merge(df, postcode_df, 
                  by.x = input$postcodeCol, by.y = input$postcodeCol2, all.x=TRUE )
      
      to_remove_index <- (df$POSTCODE_MATCH == "POSTCODE_MATCH" & df$Levenshtein.Dist.Norm < 0.16)
      
      df$REMOVE[to_remove_index] <- "REMOVE"
    }
    print("234234234  324234234 ")
    
    reactive.values$accounts.table3 <- df
    print("234234234  324234234 ")
    output$tablePreview3 <- renderTable({head(df[,c("Account.Name.Short",
                                                    "Levenshtein.Match","Levenshtein.Dist", "Levenshtein.Dist.Norm",
                                                    #"Hamming.Match","Hamming.Dist",
                                                    "Levenshtein.Damerau.Match","Levenshtein.Damerau.Dist")])})
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste0("Matched_Data", '.csv') },
    content = function(file) {
      write.table(reactive.values$accounts.table3, file, sep = ",", row.names = FALSE, fileEncoding="UTF-8")
    }
  )
  
})
