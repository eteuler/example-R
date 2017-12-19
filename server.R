setwd("~/Desktop/PEO_ACWA")
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(data.table)
library(DT)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(formattable)
library(rhandsontable)
library(knitr)
library(rmarkdown)
source('./data_prep.R')

server <- function(input, output, session) {
  
  
  ifHQ <-  reactive({
    
    h3(
      fileInput("file0", "Upload HQ .csv",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    )
    
  })
  
  ifPueblo <- reactive({
    
    h3(
      fileInput("file3", "Upload Pueblo .csv",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")), 
      fileInput("file3feedback", "Upload Pueblo Feedback Form .csv",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    )
    
  })
  
  ifBlueGrass <- reactive({
    
    h3(
      fileInput("file6", "Upload Blue Grass .csv",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("file6feedback", "Upload Blue Grass Feedback Form .csv",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    )
    
  })
  
  
  output$survey <- renderUI({
    
    allcontents <- switch(
      input$site,
      "Headquarters" = ifHQ() ,
      "Pueblo" = ifPueblo(),
      "Blue Grass" = ifBlueGrass())
    
    
    
    box(width = 12,
        allcontents
    )
  })
  
  
  ##################HEADQUARTERS############################  
  ##################HEADQUARTERS############################
  ##################HEADQUARTERS############################ 
  
  # Initiate a reactive values list to store the reactive dataframes so that user can interact with them after rendering
  # i.e. delete rows and update entries
  values <- reactiveValues()
  ##########HQII CLEANING AND DISPLAYING#############
 
  #function that converts hands on table to r object 
  testHandsonFunc <- function(inputtrigger){
    hot_to_r(inputtrigger)
  }
 #reading in or rawdata and creation (by cleaning script) of the HQ Individual Interaction data 
  observe({
    inFile <- input$file0 
    
    if (is.null(inFile))
      return(NULL)
    
    HQIIfinal <- cleanHQII(inFile$datapath)
    values$HQIIworking <- HQIIfinal
    
  })
  
  #running of function that changes the HQ individual interaction hand on table to r object table 
  RobjectHQII <- reactive({
    testHandsonFunc(input$HQIIrawdata)
  })
  
  # observeEvent(input$deleteRows0,{
  #   
  #   if (!is.null(input$HQIIrawdata_rows_selected)) {
  #     
  #     values$HQIIworking <- values$HQIIworking[-as.numeric(input$HQIIrawdata_rows_selected),]
  #   }
  # }) 
  
  # observeEvent(input$cancel0, {
  #   if(!is.null(isolate(values$HQIIworking))) values$HQIIworking <- isolate(values$HQIIworking)
  # })

# displaying of HQII data, and attempt to add search option(not working)
  output$HQIIrawdata <- renderRHandsontable({
    rhandsontable(values$HQIIworking, useTypes = FALSE) %>%
      hot_context_menu(
        customOpts = list(
          search = list(name = "Search",
                        callback = htmlwidgets::JS(
                          "function (key, options) {
                          var srch = prompt('Search criteria');
                          
                          this.search.query(srch);
                          this.render();
  }")))) 
  })
  
  
  ##########HQED CLEANING AND DISPLAYING#############
  #calling of cleaning script from data_prep.R
  HQEDdata <- observe({
    inFile <- input$file0 
    
    if (is.null(inFile))
      return(NULL)
    
    HQEDfinal <- cleanHQED(inFile$datapath)
    values$HQEDworking <- HQEDfinal
    
  })
  
  #running of function that changes the HQ event or dist. hands on table to r object table   
  RobjectHQED <- reactive({
    testHandsonFunc(input$HQEDrawdata)
  })
  
  #row deleting code that doesnt work with hands on table 
  # observeEvent(input$deleteRows1,{
  #   
  #   if (!is.null(input$HQEDrawdata_rows_selected)) {
  #     
  #     values$HQEDworking <- values$HQEDworking[-as.numeric(input$HQEDrawdata_rows_selected),]
  #   }
  # })
  
  #observe(print(input$HQEDrawdata))
  

#displaying ot HQ event or dist data   
  output$HQEDrawdata <- renderRHandsontable(
rhandsontable(values$HQEDworking, useTypes = FALSE)
  )
  
  ##########HQSMS CLEANING AND DISPLAYING#############
  
 #calling of cleaning script from data_prep.R
   HQSMSdata <- observe({
    inFile <- input$file0 
    
    if (is.null(inFile))
      return(NULL)
    HQSMSfinal <- cleanHQSMS(inFile$datapath)
    
    values$HQSMSworking <- HQSMSfinal
    
  })
  
#running of function that changes hands on table to R object
  RobjectHQSMS <- reactive({
    testHandsonFunc(input$HQSMSrawdata)
  })
  
  #   #row deleting code that doesnt work with hands on table 
  # observeEvent(input$deleteRows2,{
  #   
  #   if (!is.null(input$HQSMSrawdata_rows_selected)) {
  #     
  #     values$HQSMSworking <- values$HQSMSworking[-as.numeric(input$HQSMSrawdata_rows_selected),]
  #   }
  # })
  
# displaying of HQ social media submission raw data   
  output$HQSMSrawdata <- renderRHandsontable(
    rhandsontable(values$HQSMSworking, useTypes = FALSE)
  )
  
 
#save button will read the raw data and create the dashboard with potentially edited cells    
  observeEvent(input$saveHQ, {
  
    HQEDdisplay <- RobjectHQED() %>%
      ungroup() %>%
      group_by(Quarter, Event.or.distribution.type.) %>%
      summarise(countname=n()) %>%
      spread(key = Quarter, value = countname, fill = 0)
    
    HQEDdisplay$Q1.Metric <- c(1,0,0,12,12)
    HQEDdisplay$Q2.Metric <- c(1,0,1,13,0)
    HQEDdisplay$Q3.Metric <- c(1,0,1,13,0)
    HQEDdisplay$Q4.Metric <- c(1,0,0,13,0)
    HQEDdisplay$FY.Metric <- (HQEDdisplay$Q1.Metric + HQEDdisplay$Q2.Metric +
                                HQEDdisplay$Q3.Metric + HQEDdisplay$Q4.Metric)
    HQEDdisplay$FY.Total <- (HQEDdisplay$Q1 + HQEDdisplay$Q2 + HQEDdisplay$Q3 + HQEDdisplay$Q4)
    values$HQtable1test <- HQEDdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
                          "Q3.Metric","Q3","Q4.Metric", "Q4", "FY.Metric", "FY.Total")]
      
    HQSMSdisplay <- RobjectHQSMS() %>%
      ungroup() %>% 
      gather(key = "Event.or.distribution.type.", 
             value = "Count", 
             Facebook.Posts, Tweets, Instagram.Posts, Full.Length.Videos, Digital.Shorts, 
             Digital.Communications.Distributions) %>% 
      group_by(Quarter,Event.or.distribution.type.) %>% 
      summarise(counts=sum(as.numeric(Count))) %>% 
      spread(key=Quarter, value=counts)
    
    HQSMSdisplay$Q1.Metric <- c(51,12,160,2,75,250)
    HQSMSdisplay$Q2.Metric <- c(51,13,160,2,75,250)
    HQSMSdisplay$Q3.Metric <- c(51,12,160,2,75,250)
    HQSMSdisplay$Q4.Metric <- c(51,13,160,2,75,250)
    HQSMSdisplay$FY.Metric <- (HQSMSdisplay$Q1.Metric + HQSMSdisplay$Q2.Metric +
                                 HQSMSdisplay$Q3.Metric + HQSMSdisplay$Q4.Metric)
    HQSMSdisplay$FY.Total <- (HQSMSdisplay$Q1 + HQSMSdisplay$Q2 + HQSMSdisplay$Q3 + HQSMSdisplay$Q4)
    values$HQtable1test1 <- HQSMSdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
                           "Q3.Metric","Q3","Q4.Metric", "Q4", "FY.Metric", "FY.Total")]
    
    values$HQfinal <- rbind(values$HQtable1test, values$HQtable1test1)
    
  output$HQtable1 <- renderDataTable(as.datatable(formattable(values$HQfinal, list(
    Q1 =  formatter("span",
                    style = ~ style(color = ifelse(Q1 < Q1.Metric, "red" , NA))),
    Q2 =  formatter("span",
                    style = ~ style(color = ifelse(Q2 < Q2.Metric, "red" , NA))),
    Q3 =  formatter("span",
                    style = ~ style(color = ifelse(Q3 < Q3.Metric, "red" , NA))),
    Q4 =  formatter("span",
                    style = ~ style(color = ifelse(Q4 < Q4.Metric, "red" , NA))),
    FY.Total = formatter("span",
                    style = ~ style(color = ifelse(FY.Total < FY.Metric, "red" , NA)))
  )),options = list("pageLength" = 15, columnDefs = list(list(className = 'dt-center', targets = 1:10))),
                    rownames = FALSE))
  })
  
  
  
  ###############DOWNLOADING HQ DATA#######################  
  ###############DOWNLOADING HQ DATA#######################
  ###############DOWNLOADING HQ DATA#######################  
  
  output$downloadHQII <- downloadHandler(
    filename = function() {
      paste("HQIndividualInteractionsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RobjectHQII(), file, row.names = FALSE)
    }
  )
  output$downloadHQED <- downloadHandler(
    filename = function() {
      paste("HQEventOrDistData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RobjectHQED(), file, row.names = FALSE)
    }
  ) 
  output$downloadHQSMS <- downloadHandler(
    filename = function() {
      paste("HQSocialMediaData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RobjectHQSMS(), file, row.names = FALSE)
    }
  )
  
  HQrawdata <- reactive({
    merge(values$HQIIworking, values$HQEDworking, by = "Response.ID")
  })
  
  output$downloadHQRAW <- downloadHandler(
    filename = function() {
      paste("HQrawdata", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(HQrawdata(), file, row.names = FALSE)
    }
  )
  
  
  ##################PUEBLO############################  
  ##################PUEBLO############################
  ##################PUEBLO############################
  
  PBIIdata <- observe({  
    inFile <- input$file3 
    
    if (is.null(inFile))
      return(NULL)
    PBIIfinal <- cleanPBII(inFile$datapath) 
    
    values$PBIIworking <- PBIIfinal 
  })
  
  # observeEvent(input$deleteRows3,{
  #   
  #   if (!is.null(input$PBIIrawdata_rows_selected)) {
  #     
  #     values$PBIIworking <- values$PBIIworking[-as.numeric(input$PBIIrawdata_rows_selected),]
  #   }
  # })
  
  
  output$PBIIrawdata <- renderRHandsontable({
    rhandsontable(values$PBIIworking, useTypes = FALSE)
  })
  RobjectPBII <- reactive({
    testHandsonFunc(input$PBIIrawdata)
  })
  
  PBEDdata <- observe({
    inFile <- input$file3 
    
    if (is.null(inFile))
      return(NULL)
    PBEDfinal <- cleanPBED(inFile$datapath) 
    
    values$PBEDworking <- PBEDfinal
    
  })
  
  # observeEvent(input$deleteRows4,{
  #   
  #   if (!is.null(input$PBEDrawdata_rows_selected)) {
  #     
  #     values$PBEDworking <- values$PBEDworking[-as.numeric(input$PBEDrawdata_rows_selected),]
  #   }
  # })
  
  
  output$PBEDrawdata <- renderRHandsontable(
    rhandsontable(values$PBEDworking, useTypes = FALSE)
  )
  RobjectPBED <- reactive({
    testHandsonFunc(input$PBEDrawdata)
  })
  
  
  PBSMSdata <- observe({
    inFile <- input$file3 
    
    if (is.null(inFile))
      return(NULL)
    PBSMSfinal <- cleanPBSMS(inFile$datapath)
    
    values$PBSMSworking <- PBSMSfinal
    
  })
  
  # observeEvent(input$deleteRows5,{
  #   
  #   if (!is.null(input$PBSMSrawdata_rows_selected)) {
  #     
  #     values$PBSMSworking <- values$PBSMSworking[-as.numeric(input$PBSMSrawdata_rows_selected),]
  #   }
  # })
  
  
  
  output$PBSMSrawdata <- renderRHandsontable(
    rhandsontable(values$PBSMSworking, useTypes = FALSE)
  )
  
  RobjectPBSMS <- reactive({
    testHandsonFunc(input$PBSMSrawdata)
  })
  
  # 
  # PBtable1test <- reactive({ 
  #   PBEDdisplay <- values$PBEDworking %>%
  #     ungroup() %>%
  #     group_by(Quarter, Event.or.distribution.type.) %>%
  #     summarize(countname=n()) %>%
  #     spread(key = Quarter, value = countname, fill = 0) 
  #   
  #   PBEDdisplay$Q1.Metric <- c(5,6,0,30,5,1,2,0,12,12,0,1,2,50)
  #   PBEDdisplay$Q2.Metric <- c(4,6,0,30,5,1,0,1,12,12,0,1,2,50)
  #   PBEDdisplay$Q3.Metric <- c(2,6,1,30,5,2,1,0,6,12,0,1,2,50)
  #   PBEDdisplay$Q4.Metric <- c(2,6,0,30,5,2,0,1,6,12,0,1,2,50)
  #   PBEDdisplay$FY.Metric <- (PBEDdisplay$Q1.Metric + PBEDdisplay$Q2.Metric +
  #                               PBEDdisplay$Q3.Metric + PBEDdisplay$Q4.Metric )
  #   PBEDdisplay$FY.Total <- (PBEDdisplay$Q1 + PBEDdisplay$Q2 + PBEDdisplay$Q3 + PBEDdisplay$Q4)
  #   return(PBEDdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
  #                         "Q3.Metric","Q3","Q4.Metric", "Q4","FY.Metric", "FY.Total")])
  # })
  # 
  
  
  
  # PBtable1test1 <- reactive({ 
  #   PBSMSdisplay <- values$PBSMSworking %>%
  #     ungroup() %>% 
  #     gather(key = "Event.or.distribution.type.", 
  #            value = "Count", 
  #            Facebook.Posts, Tweets,
  #            Full.Length.Videos, Digital.Shorts, Livestreaming) %>% 
  #     group_by(Quarter,Event.or.distribution.type.) %>% 
  #     summarize(counts=sum(as.numeric(Count))) %>% 
  #     spread(key=Quarter, value=counts) 
  #   
  #   PBSMSdisplay$Q1.Metric <- c(3,80,1,2,60)
  #   PBSMSdisplay$Q2.Metric <- c(3,80,1,2,60)
  #   PBSMSdisplay$Q3.Metric <- c(3,80,1,2,60)
  #   PBSMSdisplay$Q4.Metric <- c(3,80,1,2,60)
  #   PBSMSdisplay$FY.Metric <- (PBSMSdisplay$Q1.Metric + PBSMSdisplay$Q2.Metric +
  #                                PBSMSdisplay$Q3.Metric + PBSMSdisplay$Q4.Metric)
  #   PBSMSdisplay$FY.Total <- (PBSMSdisplay$Q1 + PBSMSdisplay$Q2 + PBSMSdisplay$Q3 + PBSMSdisplay$Q4)
  #   return(PBSMSdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
  #                          "Q3.Metric","Q3","Q4.Metric", "Q4","FY.Metric", "FY.Total")])
  # })
  
  observeEvent(input$savePB, {
 
    PBEDdisplay <- RobjectPBED() %>%
      ungroup() %>%
      group_by(Quarter, Event.or.distribution.type.) %>%
      summarize(countname=n()) %>%
      spread(key = Quarter, value = countname, fill = 0) 
    
    PBEDdisplay$Q1.Metric <- c(5,6,30,50,5,1,2,0,12,12,0,1,2,0)
    PBEDdisplay$Q2.Metric <- c(4,6,30,50,5,1,0,1,12,12,0,1,2,0)
    PBEDdisplay$Q3.Metric <- c(2,6,30,50,5,2,1,0,6,12,0,1,2,1)
    PBEDdisplay$Q4.Metric <- c(2,6,30,50,5,2,0,1,6,12,0,1,2,0)
    PBEDdisplay$FY.Metric <- (PBEDdisplay$Q1.Metric + PBEDdisplay$Q2.Metric +
                                PBEDdisplay$Q3.Metric + PBEDdisplay$Q4.Metric )
    PBEDdisplay$FY.Total <- (PBEDdisplay$Q1 + PBEDdisplay$Q2 + PBEDdisplay$Q3 + PBEDdisplay$Q4)
    values$PBtable1test <- PBEDdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
                          "Q3.Metric","Q3","Q4.Metric", "Q4","FY.Metric", "FY.Total")]
     
    PBSMSdisplay <- RobjectPBSMS() %>%
      ungroup() %>% 
      gather(key = "Event.or.distribution.type.", 
             value = "Count", 
             Facebook.Posts, Tweets,
             Full.Length.Videos, Digital.Shorts, Livestreaming) %>% 
      group_by(Quarter,Event.or.distribution.type.) %>% 
      summarize(counts=sum(as.numeric(Count))) %>% 
      spread(key=Quarter, value=counts) 
    
    PBSMSdisplay$Q1.Metric <- c(3,80,1,2,60)
    PBSMSdisplay$Q2.Metric <- c(3,80,1,2,60)
    PBSMSdisplay$Q3.Metric <- c(3,80,1,2,60)
    PBSMSdisplay$Q4.Metric <- c(3,80,1,2,60)
    PBSMSdisplay$FY.Metric <- (PBSMSdisplay$Q1.Metric + PBSMSdisplay$Q2.Metric +
                                 PBSMSdisplay$Q3.Metric + PBSMSdisplay$Q4.Metric)
    PBSMSdisplay$FY.Total <- (PBSMSdisplay$Q1 + PBSMSdisplay$Q2 + PBSMSdisplay$Q3 + PBSMSdisplay$Q4)
    values$PBtable1test1 <- PBSMSdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
                           "Q3.Metric","Q3","Q4.Metric", "Q4","FY.Metric", "FY.Total")]
    
  values$PBfinal <- rbind(values$PBtable1test, values$PBtable1test1)
  
  output$PBtable1 <- renderDataTable(as.datatable(formattable(values$PBfinal, list(
    Q1 =  formatter("span",
                    style = ~ style(color = ifelse(Q1 < Q1.Metric, "red" , NA))),
    Q2 =  formatter("span",
                    style = ~ style(color = ifelse(Q2 < Q2.Metric, "red" , NA))),
    Q3 =  formatter("span",
                    style = ~ style(color = ifelse(Q3 < Q3.Metric, "red" , NA))),
    Q4 =  formatter("span",
                    style = ~ style(color = ifelse(Q4 < Q4.Metric, "red" , NA))), 
    FY.Total = formatter("span",
                    style = ~ style(color = ifelse(FY.Total < FY.Metric, "red" , NA)))
  )),options = list("pageLength" = 25, columnDefs = list(list(className = 'dt-center', targets = 1:10))),
                    rownames = FALSE))
  })
  
  
  ###############DOWNLOADING PUEBLO DATA#######################  
  ###############DOWNLOADING PUEBLO DATA#######################
  ###############DOWNLOADING PUEBLO DATA#######################  
  
  output$downloadPBII <- downloadHandler(
    filename = function() {
      paste("PBIndividualInteractionsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RobjectPBII(), file, row.names = FALSE)
    }
  )
  output$downloadPBED <- downloadHandler(
    filename = function() {
      paste("PBEventOrDistData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RobjectPBED(), file, row.names = FALSE)
    }
  ) 
  output$downloadPBSMS <- downloadHandler(
    filename = function() {
      paste("PBSocialMediaData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(RobjectPBSMS(), file, row.names = FALSE)
    }
  )
  
  
  ################# BLUE GRASSSSS################  
  ################# BLUE GRASSSSS################
  ################# BLUE GRASSSSS################
  
  BGIIdata <- observe({
    inFile <- input$file6 
    
    if (is.null(inFile))
      return(NULL)
    BGIIfinal <- cleanBGII(inFile$datapath)
    
    values$BGIIworking <- BGIIfinal 
  })
  
  # observeEvent(input$deleteRows6,{
  #   
  #   if (!is.null(input$BGIIrawdata_rows_selected)) {
  #     
  #     values$BGIIworking <- values$BGIIworking[-as.numeric(input$BGIIrawdata_rows_selected),]
  #   }
  # })
  
  output$BGIIrawdata <- renderRHandsontable({
    rhandsontable(values$BGIIworking, useTypes = FALSE)
  })
  RobjectBGII <- reactive({
    testHandsonFunc(input$BGIIrawdata)
  })
  
  BGEDdata <- observe({
    inFile <- input$file6 
    
    if (is.null(inFile))
      return(NULL)
    BGEDfinal <- cleanBGED(inFile$datapath)
    
    values$BGEDworking <- BGEDfinal
    
  })
  # 
  # observeEvent(input$deleteRows7,{
  #   
  #   if (!is.null(input$BGEDrawdata_rows_selected)) {
  #     
  #     values$BGEDworking <- values$BGEDworking[-as.numeric(input$BGEDrawdata_rows_selected),]
  #   }
  # })
  
  output$BGEDrawdata <- renderRHandsontable(
    rhandsontable(values$BGEDworking, useTypes = FALSE)
  )
  RobjectBGED <- reactive({
    testHandsonFunc(input$BGEDrawdata)
  })
  
  BGSMSdata <- observe({
    inFile <- input$file6 
    
    if (is.null(inFile))
      return(NULL)
    BGSMSfinal <- cleanBGSMS(inFile$datapath)
    
    values$BGSMSworking <- BGSMSfinal
    
  })
  
  # observeEvent(input$deleteRows8,{
  #   
  #   if (!is.null(input$BGSMSrawdata_rows_selected)) {
  #     
  #     values$BGSMSworking <- values$BGSMSworking[-as.numeric(input$BGSMSrawdata_rows_selected),]
  #   }
  # })
  # 
  output$BGSMSrawdata <- renderRHandsontable(
    rhandsontable(values$BGSMSworking, useTypes = FALSE)
  )
  
  RobjectBGSMS <- reactive({
    testHandsonFunc(input$BGSMSrawdata)
  })
  
  # BGtable1test <- reactive({ 
  #   BGEDdisplay <- values$BGEDworking %>%
  #     ungroup() %>%
  #     group_by(Quarter, Event.or.distribution.type.) %>%
  #     summarize(countname=n()) %>%
  #     spread(key = Quarter, value = countname, fill = 0) 
  #   
  #   BGEDdisplay$Q1.Metric <- c(4,3,1,0,26,12,1,1,1,4,12,1,0,4,6)
  #   BGEDdisplay$Q2.Metric <- c(4,4,1,0,35,12,1,1,1,4,12,1,0,4,6)
  #   BGEDdisplay$Q3.Metric <- c(4,2,1,0,35,12,0,1,1,4,12,1,0,4,6)
  #   BGEDdisplay$Q4.Metric <- c(4,4,1,2,35,0,2,1,1,4,12,1,0,4,6)
  #   BGEDdisplay$FY.Metric <- (BGEDdisplay$Q1.Metric + BGEDdisplay$Q2.Metric +
  #                               BGEDdisplay$Q3.Metric + BGEDdisplay$Q4.Metric )
  #   BGEDdisplay$FY.Total <- (BGEDdisplay$Q1+ BGEDdisplay$Q2+ BGEDdisplay$Q3+ BGEDdisplay$Q4)
  #   return(BGEDdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
  #                         "Q3.Metric","Q3","Q4.Metric", "Q4",  "FY.Metric", "FY.Total")])
  # })
  
  # BGtable1test1 <- reactive({ 
  #   BGSMSdisplay  <- values$BGSMSworking %>%
  #     ungroup() %>% 
  #     gather(key = "Event.or.distribution.type.", 
  #            value = "Count", 
  #            Facebook.Posts, Tweets,
  #            Full.Length.Videos, Digital.Shorts, Livestreaming) %>% 
  #     group_by(Quarter,Event.or.distribution.type.) %>% 
  #     summarize(counts=sum(as.numeric(Count))) %>% 
  #     spread(key=Quarter, value=counts) 
  #   
  #   BGSMSdisplay$Q1.Metric <- c(3,70,1,1,50)
  #   BGSMSdisplay$Q2.Metric <- c(3,70,1,1,50)
  #   BGSMSdisplay$Q3.Metric <- c(3,70,1,1,50)
  #   BGSMSdisplay$Q4.Metric <- c(3,70,1,1,50)
  #   BGSMSdisplay$FY.Metric <- (BGSMSdisplay$Q1.Metric + BGSMSdisplay$Q2.Metric +
  #                                BGSMSdisplay$Q3.Metric + BGSMSdisplay$Q4.Metric)
  #   BGSMSdisplay$FY.Total <- (BGSMSdisplay$Q1 + BGSMSdisplay$Q2 + BGSMSdisplay$Q3+ BGSMSdisplay$Q4)
  #   return(BGSMSdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
  #                          "Q3.Metric","Q3","Q4.Metric", "Q4",  "FY.Metric", "FY.Total")])
  # }) 
observeEvent(input$saveBG, {
  BGEDdisplay <- RobjectBGED() %>%
    ungroup() %>%
    group_by(Quarter, Event.or.distribution.type.) %>%
    summarize(countname=n()) %>%
    spread(key = Quarter, value = countname, fill = 0) 
  
  BGEDdisplay$Q1.Metric <- c(4,3,1,0,26,12,1,1,1,4,12,1,0,4,6)
  BGEDdisplay$Q2.Metric <- c(4,4,1,0,35,12,1,1,1,4,12,1,0,4,6)
  BGEDdisplay$Q3.Metric <- c(4,2,1,0,35,12,0,1,1,4,12,1,0,4,6)
  BGEDdisplay$Q4.Metric <- c(4,4,1,2,35,0,2,1,1,4,12,1,0,4,6)
  BGEDdisplay$FY.Metric <- (BGEDdisplay$Q1.Metric + BGEDdisplay$Q2.Metric +
                              BGEDdisplay$Q3.Metric + BGEDdisplay$Q4.Metric )
  BGEDdisplay$FY.Total <- (BGEDdisplay$Q1+ BGEDdisplay$Q2+ BGEDdisplay$Q3+ BGEDdisplay$Q4)
  values$BGtable1test <- BGEDdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
                        "Q3.Metric","Q3","Q4.Metric", "Q4",  "FY.Metric", "FY.Total")]
  
    
  BGSMSdisplay  <- RobjectBGSMS() %>%
    ungroup() %>% 
    gather(key = "Event.or.distribution.type.", 
           value = "Count", 
           Facebook.Posts, Tweets,
           Full.Length.Videos, Digital.Shorts, Livestreaming) %>% 
    group_by(Quarter,Event.or.distribution.type.) %>% 
    summarize(counts=sum(as.numeric(Count))) %>% 
    spread(key=Quarter, value=counts) 
  
  BGSMSdisplay$Q1.Metric <- c(3,70,1,1,50)
  BGSMSdisplay$Q2.Metric <- c(3,70,1,1,50)
  BGSMSdisplay$Q3.Metric <- c(3,70,1,1,50)
  BGSMSdisplay$Q4.Metric <- c(3,70,1,1,50)
  BGSMSdisplay$FY.Metric <- (BGSMSdisplay$Q1.Metric + BGSMSdisplay$Q2.Metric +
                               BGSMSdisplay$Q3.Metric + BGSMSdisplay$Q4.Metric)
  BGSMSdisplay$FY.Total <- (BGSMSdisplay$Q1 + BGSMSdisplay$Q2 + BGSMSdisplay$Q3+ BGSMSdisplay$Q4)
  values$BGtable1test1 <- BGSMSdisplay[,c("Event.or.distribution.type.","Q1.Metric","Q1","Q2.Metric", "Q2",
                         "Q3.Metric","Q3","Q4.Metric", "Q4",  "FY.Metric", "FY.Total")]
    
  values$BGfinal <- rbind(values$BGtable1test, values$BGtable1test1)
  
  output$BGtable1 <- renderDataTable(as.datatable(formattable(values$BGfinal, list(
    Q1 =  formatter("span",
                    style = ~ style(color = ifelse(Q1 < Q1.Metric, "red" , NA))),
    Q2 =  formatter("span",
                    style = ~ style(color = ifelse(Q2 < Q2.Metric, "red" , NA))),
    Q3 =  formatter("span",
                    style = ~ style(color = ifelse(Q3 < Q3.Metric, "red" , NA))),
    Q4 =  formatter("span",
                    style = ~ style(color = ifelse(Q4 < Q4.Metric, "red" , NA))), 
    FY.Total = formatter("span",
                    style = ~ style(color = ifelse(FY.Total < FY.Metric, "red" , NA)))
  )),options = list("pageLength" = 25, columnDefs = list(list(className = 'dt-center', targets = 1:10))),
                    rownames = FALSE))
})
  
  ###############DOWNLOADING BLUEGRASS DATA#######################  
  ###############DOWNLOADING BLUEGRASS DATA#######################
  ###############DOWNLOADING BLUEGRASS DATA#######################  
  
  output$downloadBGII <- downloadHandler(
    filename = function() {
      paste("BGIndividualInteractionsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(testthing6(), file, row.names = FALSE)
    }
  )
  output$downloadBGED <- downloadHandler(
    filename = function() {
      paste("BGEventOrDistData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(testthing7(), file, row.names = FALSE)
    }
  ) 
  output$downloadBGSMS <- downloadHandler(
    filename = function() {
      paste("BGSocialMediaData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(testthing8(), file, row.names = FALSE)
    }
  )
  
  ############PUEBLO FEEDBACK#####################  
  ############PUEBLO FEEDBACK#####################
  ############PUEBLO FEEDBACK#####################
  
  Feedbacktable <- reactive({
    inFile <- input$file3feedback 
    
    if (is.null(inFile))
      return(NULL)
    
    PBfeedback <- read.csv(inFile$datapath, 
                           header = TRUE, na.strings = c("          -", "          -", "          - ")) 
    PBfeedback["Month"] <- month(as.Date(PBfeedback$Date,format="%m/%d/%Y"))
    PBfeedback["Quarter"] <- ifelse(
      PBfeedback$Month >= 10, "Q1",
      ifelse(PBfeedback$Month <= 3, "Q2", 
             ifelse(PBfeedback$Month >= 4 & PBfeedback$Month < 7, "Q3", 
                    ifelse(PBfeedback$Month >=7 & PBfeedback$Month < 10, "Q4", 0))))
    PBfeedback[is.na(PBfeedback)] <- 6
    
    PBfeedbackQ1 <- PBfeedback[PBfeedback$Quarter == "Q1",]
    PBQ1formscollected <- nrow(PBfeedbackQ1)
    PBQ1percent <- ((
      round((sum(PBfeedbackQ1$Addressed.Questions..Concerns <= 2) / sum(PBfeedbackQ1$Addressed.Questions..Concerns <= 5)), digits = 2) +
        round((sum(PBfeedbackQ1$Activities.Increased.Awareness <= 2) / sum(PBfeedbackQ1$Activities.Increased.Awareness <= 5)), digits = 2) +
        round((sum(PBfeedbackQ1$Satisfied.with.Program <= 2) / sum(PBfeedbackQ1$Satisfied.with.Program <= 5)), digits = 2) +
        round((sum(as.numeric(PBfeedbackQ1$Accurate.Information) <= 2)) / sum(as.numeric(PBfeedbackQ1$Accurate.Information) <= 5), digits = 2) +
        round((sum(PBfeedbackQ1$ACWA.is.open.shares.with.public <= 2) / sum(PBfeedbackQ1$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
        round((sum(PBfeedbackQ1$Aware.of.current.developments.progress <= 2) / sum(PBfeedbackQ1$Aware.of.current.developments.progress <= 5)), digits = 2) +
        round((sum(PBfeedbackQ1$Information.easy.to.understand <= 2) / sum(PBfeedbackQ1$Information.easy.to.understand <= 5)), digits = 2) +
        round((sum(as.numeric(PBfeedbackQ1$Information.easily.accessible) <= 2) / sum(as.numeric(PBfeedbackQ1$Information.easily.accessible) <= 5)), digits = 2) +
        round((sum(PBfeedbackQ1$Offers.Ops..For.Involvement <= 2) / sum(PBfeedbackQ1$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    
    PBfeedbackQ2 <- PBfeedback[PBfeedback$Quarter == "Q2",]
    PBQ2formscollected <- nrow(PBfeedbackQ2)
    PBQ2percent <- ((
      round((sum(PBfeedbackQ2$Addressed.Questions..Concerns <= 2) / sum(PBfeedbackQ2$Addressed.Questions..Concerns <= 5)), digits = 2) +
        round((sum(PBfeedbackQ2$Activities.Increased.Awareness <= 2) / sum(PBfeedbackQ2$Activities.Increased.Awareness <= 5)), digits = 2) +
        round((sum(PBfeedbackQ2$Satisfied.with.Program <= 2) / sum(PBfeedbackQ2$Satisfied.with.Program <= 5)), digits = 2) +
        round((sum(as.numeric(PBfeedbackQ2$Accurate.Information) <= 2)) / sum(as.numeric(PBfeedbackQ2$Accurate.Information) <= 5), digits = 2) +
        round((sum(PBfeedbackQ2$ACWA.is.open.shares.with.public <= 2) / sum(PBfeedbackQ2$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
        round((sum(PBfeedbackQ2$Aware.of.current.developments.progress <= 2) / sum(PBfeedbackQ2$Aware.of.current.developments.progress <= 5)), digits = 2) +
        round((sum(PBfeedbackQ2$Information.easy.to.understand <= 2) / sum(PBfeedbackQ2$Information.easy.to.understand <= 5)), digits = 2) +
        round((sum(as.numeric(PBfeedbackQ2$Information.easily.accessible) <= 2) / sum(as.numeric(PBfeedbackQ2$Information.easily.accessible) <= 5)), digits = 2) +
        round((sum(PBfeedbackQ2$Offers.Ops..For.Involvement <= 2) / sum(PBfeedbackQ2$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    
    PBfeedbackQ3 <- PBfeedback[PBfeedback$Quarter == "Q3",]
    PBQ3formscollected <- nrow(PBfeedbackQ3)
    PBQ3percent <- ((
      round((sum(PBfeedbackQ3$Addressed.Questions..Concerns <= 2) / sum(PBfeedbackQ3$Addressed.Questions..Concerns <= 5)), digits = 2) +
        round((sum(PBfeedbackQ3$Activities.Increased.Awareness <= 2) / sum(PBfeedbackQ3$Activities.Increased.Awareness <= 5)), digits = 2) +
        round((sum(PBfeedbackQ3$Satisfied.with.Program <= 2) / sum(PBfeedbackQ3$Satisfied.with.Program <= 5)), digits = 2) +
        round((sum(as.numeric(PBfeedbackQ3$Accurate.Information) <= 2)) / sum(as.numeric(PBfeedbackQ3$Accurate.Information) <= 5), digits = 2) +
        round((sum(PBfeedbackQ3$ACWA.is.open.shares.with.public <= 2) / sum(PBfeedbackQ3$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
        round((sum(PBfeedbackQ3$Aware.of.current.developments.progress <= 2) / sum(PBfeedbackQ3$Aware.of.current.developments.progress <= 5)), digits = 2) +
        round((sum(PBfeedbackQ3$Information.easy.to.understand <= 2) / sum(PBfeedbackQ3$Information.easy.to.understand <= 5)), digits = 2) +
        round((sum(as.numeric(PBfeedbackQ3$Information.easily.accessible) <= 2) / sum(as.numeric(PBfeedbackQ3$Information.easily.accessible) <= 5)), digits = 2) +
        round((sum(PBfeedbackQ3$Offers.Ops..For.Involvement <= 2) / sum(PBfeedbackQ3$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    
    PBfeedbackQ4 <- PBfeedback[PBfeedback$Quarter == "Q4",]
    PBQ4formscollected <- nrow(PBfeedbackQ4)
    PBQ4percent <- ((
      round((sum(PBfeedbackQ4$Addressed.Questions..Concerns <= 2) / sum(PBfeedbackQ4$Addressed.Questions..Concerns <= 5)), digits = 2) +
        round((sum(PBfeedbackQ4$Activities.Increased.Awareness <= 2) / sum(PBfeedbackQ4$Activities.Increased.Awareness <= 5)), digits = 2) +
        round((sum(PBfeedbackQ4$Satisfied.with.Program <= 2) / sum(PBfeedbackQ4$Satisfied.with.Program <= 5)), digits = 2) +
        round((sum(as.numeric(PBfeedbackQ4$Accurate.Information) <= 2)) / sum(as.numeric(PBfeedbackQ4$Accurate.Information) <= 5), digits = 2) +
        round((sum(PBfeedbackQ4$ACWA.is.open.shares.with.public <= 2) / sum(PBfeedbackQ4$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
        round((sum(PBfeedbackQ4$Aware.of.current.developments.progress <= 2) / sum(PBfeedbackQ4$Aware.of.current.developments.progress <= 5)), digits = 2) +
        round((sum(PBfeedbackQ4$Information.easy.to.understand <= 2) / sum(PBfeedbackQ4$Information.easy.to.understand <= 5)), digits = 2) +
        round((sum(as.numeric(PBfeedbackQ4$Information.easily.accessible) <= 2) / sum(as.numeric(PBfeedbackQ4$Information.easily.accessible) <= 5)), digits = 2) +
        round((sum(PBfeedbackQ4$Offers.Ops..For.Involvement <= 2) / sum(PBfeedbackQ4$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    
    PBQ4percent1 <- ifelse(PBQ4formscollected == 0, PBQ3percent, PBQ4percent) 
    PBtotalFormscollected <- sum(PBQ1formscollected,PBQ2formscollected,PBQ3formscollected,PBQ4formscollected)
    PBFYsatisfaction <- (sum(PBQ1percent,PBQ2percent,PBQ3percent,PBQ4percent1) /4)
    
    
    PuebloFormsCollected <- c(PBQ1formscollected,PBQ2formscollected,PBQ3formscollected,PBQ4formscollected,PBtotalFormscollected)
    PuebloPercentSatisfied <- c(paste0(round(PBQ1percent*100,digits = 1),"%"),paste0(round(PBQ2percent*100,digits = 1),"%"),
                                paste0(round(PBQ3percent*100,digits = 1),"%"),paste0(round(PBQ4percent1*100,digits = 1),"%"),
                                paste0(round(PBFYsatisfaction*100,digits = 1),"%"))
    
    
    
    ############### BLUE GRASS FEEDBACK##################
    ############### BLUE GRASS FEEDBACK##################
    ############### BLUE GRASS FEEDBACK##################
    
    inFile <- input$file6feedback 
    
    if (is.null(inFile))
      return(NULL)
    
    BGfeedback <- read.csv(inFile$datapath, 
                           header = TRUE, na.strings = c("           -","            -","           -","-","","           -    ")) 
    BGfeedback["Month"] <- month(as.Date(BGfeedback$Date,format="%m/%d/%Y"))
    BGfeedback["Quarter"] <- ifelse(
      BGfeedback$Month >= 10, "Q1",
      ifelse(BGfeedback$Month <= 3, "Q2", 
             ifelse(BGfeedback$Month >= 4 & BGfeedback$Month < 7, "Q3", 
                    ifelse(BGfeedback$Month >=7 & BGfeedback$Month < 10, "Q4", 0))))
    BGfeedback[is.na(BGfeedback)] <- 6
    
    BGfeedbackQ1 <- BGfeedback[BGfeedback$Quarter == "Q1",]
    BGQ1formscollected <- nrow(BGfeedbackQ1)
    BGQ1percent <- ((
      round((sum(BGfeedbackQ1$Addressed.Questions..Concerns <= 2) / sum(BGfeedbackQ1$Addressed.Questions..Concerns <= 5)), digits = 2) +
        round((sum(BGfeedbackQ1$Activities.Increased.Awareness <= 2) / sum(BGfeedbackQ1$Activities.Increased.Awareness <= 5)), digits = 2) +
        round((sum(BGfeedbackQ1$Satisfied.with.Program <= 2) / sum(BGfeedbackQ1$Satisfied.with.Program <= 5)), digits = 2) +
        round((sum(as.numeric(BGfeedbackQ1$Accurate.Information) <= 2)) / sum(as.numeric(BGfeedbackQ1$Accurate.Information) <= 5), digits = 2) +
        round((sum(BGfeedbackQ1$ACWA.is.open.shares.with.public <= 2) / sum(BGfeedbackQ1$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
        round((sum(BGfeedbackQ1$Aware.of.current.developments.progress <= 2) / sum(BGfeedbackQ1$Aware.of.current.developments.progress <= 5)), digits = 2) +
        round((sum(BGfeedbackQ1$Information.easy.to.understand <= 2) / sum(BGfeedbackQ1$Information.easy.to.understand <= 5)), digits = 2) +
        round((sum(as.numeric(BGfeedbackQ1$Information.easily.accessible) <= 2) / sum(as.numeric(BGfeedbackQ1$Information.easily.accessible) <= 5)), digits = 2) +
        round((sum(BGfeedbackQ1$Offers.Ops..For.Involvement <= 2) / sum(BGfeedbackQ1$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    
    BGfeedbackQ2 <- BGfeedback[BGfeedback$Quarter == "Q2",]
    BGQ2formscollected <- nrow(BGfeedbackQ2)
    BGQ2percent <- ((
      round((sum(BGfeedbackQ2$Addressed.Questions..Concerns <= 2) / sum(BGfeedbackQ2$Addressed.Questions..Concerns <= 5)), digits = 2) +
        round((sum(BGfeedbackQ2$Activities.Increased.Awareness <= 2) / sum(BGfeedbackQ2$Activities.Increased.Awareness <= 5)), digits = 2) +
        round((sum(BGfeedbackQ2$Satisfied.with.Program <= 2) / sum(BGfeedbackQ2$Satisfied.with.Program <= 5)), digits = 2) +
        round((sum(as.numeric(BGfeedbackQ2$Accurate.Information) <= 2)) / sum(as.numeric(BGfeedbackQ2$Accurate.Information) <= 5), digits = 2) +
        round((sum(BGfeedbackQ2$ACWA.is.open.shares.with.public <= 2) / sum(BGfeedbackQ2$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
        round((sum(BGfeedbackQ2$Aware.of.current.developments.progress <= 2) / sum(BGfeedbackQ2$Aware.of.current.developments.progress <= 5)), digits = 2) +
        round((sum(BGfeedbackQ2$Information.easy.to.understand <= 2) / sum(BGfeedbackQ2$Information.easy.to.understand <= 5)), digits = 2) +
        round((sum(as.numeric(BGfeedbackQ2$Information.easily.accessible) <= 2) / sum(as.numeric(BGfeedbackQ2$Information.easily.accessible) <= 5)), digits = 2) +
        round((sum(BGfeedbackQ2$Offers.Ops..For.Involvement <= 2) / sum(BGfeedbackQ2$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    
    BGfeedbackQ3 <- BGfeedback[BGfeedback$Quarter == "Q3",]
    BGQ3formscollected <- nrow(BGfeedbackQ3)
    BGQ3percent <- ((
      round((sum(BGfeedbackQ3$Addressed.Questions..Concerns <= 2) / sum(BGfeedbackQ3$Addressed.Questions..Concerns <= 5)), digits = 2) +
        round((sum(BGfeedbackQ3$Activities.Increased.Awareness <= 2) / sum(BGfeedbackQ3$Activities.Increased.Awareness <= 5)), digits = 2) +
        round((sum(BGfeedbackQ3$Satisfied.with.Program <= 2) / sum(BGfeedbackQ3$Satisfied.with.Program <= 5)), digits = 2) +
        round((sum(as.numeric(BGfeedbackQ3$Accurate.Information) <= 2)) / sum(as.numeric(BGfeedbackQ3$Accurate.Information) <= 5), digits = 2) +
        round((sum(BGfeedbackQ3$ACWA.is.open.shares.with.public <= 2) / sum(BGfeedbackQ3$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
        round((sum(BGfeedbackQ3$Aware.of.current.developments.progress <= 2) / sum(BGfeedbackQ3$Aware.of.current.developments.progress <= 5)), digits = 2) +
        round((sum(BGfeedbackQ3$Information.easy.to.understand <= 2) / sum(BGfeedbackQ3$Information.easy.to.understand <= 5)), digits = 2) +
        round((sum(as.numeric(BGfeedbackQ3$Information.easily.accessible) <= 2) / sum(as.numeric(BGfeedbackQ3$Information.easily.accessible) <= 5)), digits = 2) +
        round((sum(BGfeedbackQ3$Offers.Ops..For.Involvement <= 2) / sum(BGfeedbackQ3$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    
    BGfeedbackQ4 <- BGfeedback[BGfeedback$Quarter == "Q4",]
    BGQ4formscollected <- nrow(BGfeedbackQ4)
    BGQ4percent <- ((
      round((sum(BGfeedbackQ4$Addressed.Questions..Concerns <= 2) / sum(BGfeedbackQ4$Addressed.Questions..Concerns <= 5)), digits = 2) +
        round((sum(BGfeedbackQ4$Activities.Increased.Awareness <= 2) / sum(BGfeedbackQ4$Activities.Increased.Awareness <= 5)), digits = 2) +
        round((sum(BGfeedbackQ4$Satisfied.with.Program <= 2) / sum(BGfeedbackQ4$Satisfied.with.Program <= 5)), digits = 2) +
        round((sum(as.numeric(BGfeedbackQ4$Accurate.Information) <= 2)) / sum(as.numeric(BGfeedbackQ4$Accurate.Information) <= 5), digits = 2) +
        round((sum(BGfeedbackQ4$ACWA.is.open.shares.with.public <= 2) / sum(BGfeedbackQ4$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
        round((sum(BGfeedbackQ4$Aware.of.current.developments.progress <= 2) / sum(BGfeedbackQ4$Aware.of.current.developments.progress <= 5)), digits = 2) +
        round((sum(BGfeedbackQ4$Information.easy.to.understand <= 2) / sum(BGfeedbackQ4$Information.easy.to.understand <= 5)), digits = 2) +
        round((sum(as.numeric(BGfeedbackQ4$Information.easily.accessible) <= 2) / sum(as.numeric(BGfeedbackQ4$Information.easily.accessible) <= 5)), digits = 2) +
        round((sum(BGfeedbackQ4$Offers.Ops..For.Involvement <= 2) / sum(BGfeedbackQ4$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    
    BGQ4percent1 <- ifelse(BGQ4formscollected == 0, BGQ3percent, BGQ4percent)
    BGtotalFormscollected <- sum(BGQ1formscollected,BGQ2formscollected,BGQ3formscollected,BGQ4formscollected)
    BGFYsatisfaction <- (sum(BGQ1percent,BGQ2percent,BGQ3percent,BGQ4percent1) /4)
    
    
    BlueGrassFormsCollected <- c(BGQ1formscollected,BGQ2formscollected,BGQ3formscollected,BGQ4formscollected,BGtotalFormscollected)
    BlueGrassPercentSatisfied <- c(paste0(round(BGQ1percent*100,digits = 1),"%"),paste0(round(BGQ2percent*100,digits = 1),"%"), 
                                   paste0(round(BGQ3percent*100,digits = 1),"%"),paste0(round(BGQ4percent*100,digits = 1),"%"), 
                                   paste0(round(BGFYsatisfaction*100,digits = 1),"%"))
    
    ################TOTAL FEEDBACK###################  
    ################TOTAL FEEDBACK###################
    ################TOTAL FEEDBACK###################
    
    qters <- c("Q1", "Q2", "Q3", "Q4", "FYTD")
    PEOACWAtotalforms <- c(sum(sum(BGQ1formscollected,PBQ1formscollected),sum(BGQ2formscollected,PBQ2formscollected),
                               sum(BGQ3formscollected,PBQ3formscollected),sum(BGQ4formscollected,PBQ4formscollected)))
    PEOACWAFormsCollected <- c(sum(BGQ1formscollected,PBQ1formscollected),sum(BGQ2formscollected,PBQ2formscollected),
                               sum(BGQ3formscollected,PBQ3formscollected),sum(BGQ4formscollected,PBQ4formscollected), PEOACWAtotalforms)
    PEOACWAsatisfactionTotal <- c(sum((sum(BGQ1percent,PBQ1percent)/2),(sum(BGQ2percent,PBQ2percent)/2),
                                      (sum(BGQ3percent,PBQ3percent)/2),(sum(BGQ4percent1,PBQ4percent1)/2))/4)
    Q1percent <- (sum(BGQ1percent,PBQ1percent)/2)
    Q2percent <- (sum(BGQ2percent,PBQ2percent)/2)
    Q3percent <- (sum(BGQ3percent,PBQ3percent)/2)
    Q4percent <- (sum(BGQ4percent1,PBQ4percent1)/2)
    PEOACWAPercentSatisfied <- c(paste0(round(Q1percent*100,digits = 1),"%"),paste0(round(Q2percent*100,digits = 1),"%"),
                                 paste0(round(Q3percent*100,digits = 1),"%"),paste0(round(Q4percent*100,digits = 1),"%"),
                                 paste0(round(PEOACWAsatisfactionTotal*100,digits = 1),"%"))
    
    feedbackPEOACWA <- data.frame(qters, PEOACWAFormsCollected,PEOACWAPercentSatisfied,BlueGrassFormsCollected,
                                  BlueGrassPercentSatisfied, PuebloFormsCollected, PuebloPercentSatisfied) %>%
      gather(key = " ", 
             value = "Count", 
             PEOACWAFormsCollected, PEOACWAPercentSatisfied, BlueGrassFormsCollected,
             BlueGrassPercentSatisfied, PuebloFormsCollected, PuebloPercentSatisfied) %>%
      spread(key=qters, value=Count) 
    
    
  })
  output$Feedbacktable1 <- renderDataTable({
    Feedbacktable()
  })
  output$report <- downloadHandler(
    filename = "dashboard.pdf", 
    content = function(file) {
      tempReport <- file.path(tempdir(), "PEOACWA.Rmd")
      file.copy("PEOACWA.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(tempReport, output_file = file) 
                       # envir = new.env(parent = globalenv))
    }
  )
  
}