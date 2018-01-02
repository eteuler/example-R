

server <- function(input, output, session) {
  outputDir <- "./outfiles"
  #will use this later
  all.qcols <- c("Q1","Q2","Q3","Q4")
  
  ## Read in HQ data from previous session
  HQ.path.df <- read.csv('./data/HQ_paths.csv',stringsAsFactors = FALSE)
  last.max.hq <- HQ.path.df$MaxId[nrow(HQ.path.df)]
  path.to.HQinitial <- HQ.path.df$Path[nrow(HQ.path.df)]
  HQInitialList <- readRDS(path.to.HQinitial)
  
  ## Read in Pueblo data from previous session
  PB.path.df <- read.csv('./data/PB_paths.csv',stringsAsFactors = FALSE)
  last.max.pb <- PB.path.df$MaxId[nrow(PB.path.df)]
  path.to.PBinitial <- PB.path.df$Path[nrow(PB.path.df)]
  PBInitialList <- readRDS(path.to.PBinitial)
  
  ## Read in Blue Grass data from previous session
  BG.path.df <- read.csv('./data/BG_paths.csv',stringsAsFactors = FALSE)
  last.max.bg <- BG.path.df$MaxId[nrow(BG.path.df)]
  path.to.BGinitial <- BG.path.df$Path[nrow(BG.path.df)]
  BGInitialList <- readRDS(path.to.BGinitial)
  
  ifHQ <-  reactive({
    
    h3(
      fileInput("file0", "Upload HQ .csv",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    )
    
  })
  
  ifPueblo <- reactive({
    
    h3(
      fileInput("file3", "Upload Pueblo .csv",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")), 
      fileInput("file3feedback", "Upload Pueblo Feedback Form .csv",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    )
    
  })
  
  ifBlueGrass <- reactive({
    
    h3(
      fileInput("file6", "Upload Blue Grass .csv",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput("file6feedback", "Upload Blue Grass Feedback Form .csv",
                multiple = FALSE,
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
  values <- reactiveValues(
    HQII = HQInitialList$II,
    HQED = HQInitialList$ED,
    HQSMS = HQInitialList$SMS,
    PBII = PBInitialList$II,
    PBED = PBInitialList$ED,
    PBSMS = PBInitialList$SMS,
    BGII = BGInitialList$II,
    BGED = BGInitialList$ED,
    BGSMS = BGInitialList$SMS,
    feedback = NULL)
  ## Initiate the reactives tables with data from previous sessions
  # values$HQII <- HQInitialList$II
  # values$HQED <- HQInitialList$ED
  # values$HQSMS <- HQInitialList$SMS
  
  ##########HQII CLEANING AND DISPLAYING#############
 
  #function that converts hands on table to r object 
  testHandsonFunc <- function(inputtrigger){
    hot_to_r(inputtrigger) %>% filter(!(is.na(Response.ID)| Response.ID == "" | is.null(Response.ID) ))
  }
 #reading in or rawdata and creation (by cleaning script) of the HQ Individual Interaction data 
  observe({
    inFile <- input$file0 
    
    if (is.null(inFile))
      return(NULL)
    
    HQII.new <- cleanHQII(inFile$datapath, prev.resp.id=last.max.hq)
    isolate(values$HQII <- rbind(values$HQII,HQII.new))
    
  })
  
  #running of function that changes the HQ individual interaction hand on table to r object table 
  HQIIfromHandsOn <- reactive({
    testHandsonFunc(input$HQIIrawdata)
  })
  

# displaying of HQII data
  output$HQIIrawdata <- renderRHandsontable({
    print("I'm inside")
    print(tail(values$HQII))
    rhandsontable(values$HQII, useTypes = FALSE, rowHeaders = FALSE)
    
    #rhandsontable(HQinitialII, useTypes = FALSE, rowHeaders = FALSE)
  })
  
  
  ##########HQED CLEANING AND DISPLAYING#############
  #calling of cleaning script from data_prep.R
  HQEDdata <- observe({
    inFile <- input$file0 
    
    if (is.null(inFile))
      return(NULL)
    
    HQED.new <- cleanHQED(inFile$datapath, prev.resp.id=last.max.hq)
    isolate(values$HQED <- rbind(values$HQED,HQED.new))
    #values$HQEDworking <- HQEDfinal
    
  })
  
  #running of function that changes the HQ event or dist. hands on table to r object table   
  HQEDfromHandsOn <- reactive({
    testHandsonFunc(input$HQEDrawdata)
  })
  



#displaying of HQ event or dist data   
  output$HQEDrawdata <- renderRHandsontable(
rhandsontable(values$HQED, useTypes = FALSE, rowHeaders = FALSE)
  )
  
  ##########HQSMS CLEANING AND DISPLAYING#############
  
 #calling of cleaning script from data_prep.R
   HQSMSdata <- observe({
    inFile <- input$file0 
    
    if (is.null(inFile))
      return(NULL)
    HQSMS.new <- cleanHQSMS(inFile$datapath, prev.resp.id=last.max.hq)
    isolate(values$HQSMS <- rbind(values$HQSMS,HQSMS.new))
    # values$HQSMSworking <- HQSMSfinal
    
  })
  
#running of function that changes hands on table to R object
  HQSMSfromHandsOn <- reactive({
    testHandsonFunc(input$HQSMSrawdata)
  })

  
# displaying of HQ social media submission raw data   
  output$HQSMSrawdata <- renderRHandsontable(
    rhandsontable(values$HQSMS, useTypes = FALSE, rowHeaders = FALSE)
  )
  
 
#save button will read the raw data and create the dashboard with potentially edited cells    
  observeEvent(input$saveHQall, {
  
    HQEDdisplay <- HQEDfromHandsOn() %>%
      ungroup() %>%
      group_by(Quarter, Event.or.distribution.type.) %>%
      summarise(countname=n()) %>%
      spread(key = Quarter, value = countname, fill = 0)
    
    
    HQEDMetrics <- data.frame(
      Event.or.distribution.type. = c("Public Interaction Report","News Articles","PEO ACWA Information Booth",
                                     "Weekly News Update","Weekly News Summary"),
      Q1.Metric = c(12,1,1,2,2),
      Q2.Metric = c(13,1,1,2,2),
      Q3.Metric = c(13,1,1,2,2),
      Q4.Metric = c(13,1,1,2,2)
    ) 
    HQEDMetrics$FY.Metric <- sum(HQEDMetrics$Q1.Metric, HQEDMetrics$Q2.Metric, 
                                HQEDMetrics$Q3.Metric, HQEDMetrics$Q4.Metric,na.rm=TRUE)
    

    qcols <- colnames(HQEDdisplay)[grepl("Q[0-9]$",colnames(HQEDdisplay))]
    
    gdata::unknownToNA(x=HQEDdisplay[,qcols], unknown = NA)
    
    HQEDdisplay[,qcols][is.null(HQEDdisplay[,qcols])] <- 0
    therest <- all.qcols[!all.qcols %in% qcols]
   HQEDdisplay[,therest] <- 0 

    
    HQEDdisplay$FY.Total <- sum(HQEDdisplay$Q1,HQEDdisplay$Q2, HQEDdisplay$Q3, HQEDdisplay$Q4, na.rm = TRUE)
    
    HQtableWithED <- merge(HQEDdisplay, HQEDMetrics, by = "Event.or.distribution.type.", all.y = TRUE)
    HQtableWithED[is.na(HQtableWithED)] <- 0

      
    HQSMSdisplay <- HQSMSfromHandsOn() %>%
      ungroup() %>% 
      gather(key = "Event.or.distribution.type.", 
             value = "Count", 
             Facebook.Posts, Tweets, Instagram.Posts, Full.Length.Videos, Digital.Shorts, 
             Digital.Communications.Distributions) %>% 
      group_by(Quarter,Event.or.distribution.type.) %>% 
      summarise(counts=sum(as.numeric(Count))) %>% 
      spread(key=Quarter, value=counts)

    HQSMSMetrics <- data.frame(
      Event.or.distribution.type. = c("Facebook.Posts","Instagram.Posts","Tweets",
                                     "Full.Length.Videos","Digital.Shorts"),
      Q1.Metric = c(160,105,300,1,10),
      Q2.Metric = c(160,105,300,1,13),
      Q3.Metric = c(160,105,300,1,13),
      Q4.Metric = c(160,105,300,1,14)
    ) 
    HQSMSMetrics$FY.Metric <- sum(HQSMSMetrics$Q1.Metric, HQSMSMetrics$Q2.Metric,
                                HQSMSMetrics$Q3.Metric,HQSMSMetrics$Q4.Metric,na.rm=TRUE)  
    
    qcols <- colnames(HQSMSdisplay)[grepl("Q[0-9]$",colnames(HQSMSdisplay))]
    gdata::unknownToNA(x=HQSMSdisplay[,qcols], unknown = NA)
    
    HQSMSdisplay[,qcols][is.null(HQSMSdisplay[,qcols])] <- 0
    therest <- all.qcols[!all.qcols %in% qcols]
    HQSMSdisplay[,therest] <- 0
    
    # gdata::unknownToNA(x=HQSMSdisplay$Q1, unknown = NA)
    # gdata::unknownToNA(x=HQSMSdisplay$Q2, unknown = NA)
    # gdata::unknownToNA(x=HQSMSdisplay$Q3, unknown = NA)
    # gdata::unknownToNA(x=HQSMSdisplay$Q4, unknown = NA)
    # HQSMSdisplay$Q1[is.null(HQSMSdisplay$Q1)] <- 0
    # HQSMSdisplay$Q2[is.null(HQSMSdisplay$Q2)] <- 0
    # HQSMSdisplay$Q3[is.null(HQSMSdisplay$Q3)] <- 0
    # HQSMSdisplay$Q4[is.null(HQSMSdisplay$Q4)] <- 0
    
    HQSMSdisplay$FY.Total <- sum(HQSMSdisplay$Q1, HQSMSdisplay$Q2,HQSMSdisplay$Q3,HQSMSdisplay$Q4,na.rm=TRUE)

    HQtableWithSMS <- merge(HQSMSdisplay, HQSMSMetrics, by = "Event.or.distribution.type.", all.y = TRUE)
    HQtableWithSMS[is.na(HQtableWithSMS)] <- 0
    
    values$HQfinal <- rbind(HQtableWithED, HQtableWithSMS) %>%
      select(Event.or.distribution.type.,Q1.Metric,Q1,Q2.Metric,Q2,Q3.Metric,Q3,Q4.Metric,Q4,FY.Metric, FY.Total)


  output$HQtable1 <- renderDataTable({
    values$HQfinal %>%
      mutate(Event.or.distribution.type.= recodeEvents(Event.or.distribution.type.)) %>%
    formattable(., list(
    Q1 =  formatter("span",
                    style = ~ style(color = ifelse(Q1 < Q1.Metric, "red" , "green"))),
    Q2 =  formatter("span",
                    style = ~ style(color = ifelse(Q2 < Q2.Metric, "red" , "green"))),
    Q3 =  formatter("span",
                    style = ~ style(color = ifelse(Q3 < Q3.Metric, "red" , "green"))),
    Q4 =  formatter("span",
                    style = ~ style(color = ifelse(Q4 < Q4.Metric, "red" , "green"))),
    FY.Total = formatter("span",
                    style = ~ style(color = ifelse(FY.Total < FY.Metric, "red" , "green")))
  )) %>% as.datatable(.,options = list("pageLength" = 15, columnDefs = list(list(className = 'dt-center', targets = 1:10))),
                    rownames = FALSE)
    })
  })
  
  
  
  ###############DOWNLOADING HQ DATA#######################  
  ###############DOWNLOADING HQ DATA#######################
  ###############DOWNLOADING HQ DATA#######################  
  
  output$downloadHQII <- downloadHandler(
    filename = function() {
      paste("HQIndividualInteractionsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(HQIIfromHandsOn(), file, row.names = FALSE)
    }
  )
  output$downloadHQED <- downloadHandler(
    filename = function() {
      paste("HQEventOrDistData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(HQEDfromHandsOn(), file, row.names = FALSE)
    }
  ) 
  output$downloadHQSMS <- downloadHandler(
    filename = function() {
      paste("HQSocialMediaData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(HQSMSfromHandsOn(), file, row.names = FALSE)
    }
  )

#downloading a excel sheet with all data seperated by contact type and one sheet with full raw data
#merging of datasets   
  HQrawdata <- reactive({
    Reduce(function(x,y) merge(x,y, all = TRUE), 
           list(HQIIfromHandsOn(), HQEDfromHandsOn(), HQSMSfromHandsOn()))
  })
#defining individual datasets
HQdatasets <- reactive({
  list("Individual Interactions" = HQIIfromHandsOn(), "Event  or Distributions" = HQEDfromHandsOn(), 
                   "Social Media Submissions" = HQSMSfromHandsOn(), "All HQ Data" = HQrawdata())
})

#writing of excel 
HQexcel <- reactive({
  write.xlsx(HQdatasets(), file = "Headquarters_Data.xlsx", append = TRUE)
})

# action button to write the execl/ download button did not work 
observeEvent(input$downloadHQRAW, {
  HQexcel()
})
  



# saveHQIIdata <- function(data,namestring="HQII") {
# 
#   fileName <- paste0("TTest_",namestring,".csv")
#     #sprintf("TTest.csv", as.integer(Sys.time()), digest::digest(data))
#   write.csv(
#     x = data,
#     file = file.path(outputDir, fileName),
#     row.names = FALSE, quote = TRUE, append = FALSE
#   )
# }
observeEvent(input$saveHQall, {
  HQdfList <- list()
  HQdfList$II <- HQIIfromHandsOn()
  HQdfList$ED <- HQEDfromHandsOn()
  HQdfList$SMS <- HQSMSfromHandsOn()
  max.id <- max(HQdfList$II$Response.ID,HQdfList$ED$Response.ID,HQdfList$SMS$Response.ID,na.rm = TRUE)
  old.max <- max(HQInitialList$II$Response.ID,HQInitialList$ED$Response.ID,HQInitialList$SMS$Response.ID,na.rm = TRUE)
  if (max.id > old.max | 
      !all(identical(HQInitialList$II, HQdfList$II),identical(HQInitialList$ED,HQdfList$ED), identical(HQInitialList$SMS,HQdfList$SMS))){
  systime = Sys.time()
  
  HQpath <- paste0("./data/intermediate/HQ_FY2018", format(systime, "%Y_%m_%d_%H_%M_%S"), ".rds")
  write.table(x = data.frame(Date=c(systime),Path=c(HQpath),MaxId=c(max.id)),
            file = "./data/HQ_paths.csv",append=TRUE,row.names=FALSE,sep=",",col.names = FALSE)
  
  saveRDS(HQdfList,file=HQpath)
}
})



# loadHQIIdata <- function(Robjects) {
#   files <- list.files(outputDir, full.names = TRUE)
#   print(files)
#   data <- lapply(files, read.csv, stringsAsFactors = FALSE)
#   print(head(data))
#   data <- bind_rows(data, Robjects())
#   data
# }
# 
# observeEvent(input$loadHQII, {
#   print("I am inside observeEvent")
#   olddata <- read.csv(file.path(outputDir, "TTest_HQII.csv"),stringsAsFactors = FALSE,header=TRUE)
#   values$HQIIworking <- rbind(olddata,values$HQIIworking)
#   # loadHQIIdata(HQIIfromHandsOn)
# })
  
  
  ##################PUEBLO############################  
  ##################PUEBLO############################
  ##################PUEBLO############################
 #reading in and celaning raw data for only pueblo INdividual interaction data 
  PBIIdata <- observe({  
    inFile <- input$file3 
    
    if (is.null(inFile))
      return(NULL)
    
    PBII.new <- cleanPBII(inFile$datapath, prev.resp.id=last.max.pb)
    isolate(values$PBII <- rbind(values$PBII,PBII.new))

  })


 #call and display raw II pueblo  data  
  output$PBIIrawdata <- renderRHandsontable({
    rhandsontable(values$PBII, useTypes = FALSE, rowHeaders = FALSE)
  })
#function to turn it into an R object
  PBIIfromHandsOn <- reactive({
    testHandsonFunc(input$PBIIrawdata)
  })
  
#reading in and celaning raw data for only INdividual interaction data   
  PBEDdata <- observe({
    inFile <- input$file3 
    
    if (is.null(inFile))
      return(NULL)
    
    PBED.new <- cleanPBED(inFile$datapath, prev.resp.id=last.max.pb)
    isolate(values$PBED <- rbind(values$PBED,PBED.new))

    
  })
  
  
#call and display raw Event and Dist, pueblo data  
  output$PBEDrawdata <- renderRHandsontable(
    rhandsontable(values$PBED, useTypes = FALSE, rowHeaders = FALSE)
  )
#function that turns data into R object
  PBEDfromHandsOn <- reactive({
    testHandsonFunc(input$PBEDrawdata)
  })
  
#readind in and cleaning of the social media submission peublo data   
  PBSMSdata <- observe({
    inFile <- input$file3 
    
    if (is.null(inFile))
      return(NULL)
    
    PBSMS.new <- cleanPBSMS(inFile$datapath, prev.resp.id=last.max.pb)
    isolate(values$PBSMS <- rbind(values$PBSMS,PBSMS.new))
    
  })
  
  
  
#displaying of SMS raw pueblo data  
  output$PBSMSrawdata <- renderRHandsontable(
    rhandsontable(values$PBSMS, useTypes = FALSE, rowHeaders = FALSE)
  )

#function that creates  R object     
  PBSMSfromHandsOn <- reactive({
    testHandsonFunc(input$PBSMSrawdata)
  })
  
#when user presses save button the dashboard table is made with the code below  
  observeEvent(input$savePBall, {
#event and dist table creation 
    PBEDdisplay <- PBEDfromHandsOn() %>%
      ungroup() %>%
      group_by(Quarter, Event.or.distribution.type.) %>%
      summarize(countname=n()) %>%
      spread(key = Quarter, value = countname, fill = 0) 
#creating of Metrics,  
    
    PBEDMetrics <- data.frame(
      Event.or.distribution.type. = c("CAC (or CAC subcommittee)/advisory board meeting/Public meeting",
                                      "News article","Information booth/exhibit",
                                      "Digital communications distribution","Briefing/update",
                                      "Workforce event/distribution","Networking event",
                                      "Education outreach event",
                                      "Tour","Media event", "Other (please specify)"),
      Q1.Metric = c(4,12,2,25,2,50,12,5,1,0,0),
      Q2.Metric = c(4,12,1,25,2,50,12,5,1,1,0),
      Q3.Metric = c(4,12,1,25,2,50,12,5,1,1,0),
      Q4.Metric = c(4,12,1,25,2,50,12,5,1,0,0)
    )
    PBEDMetrics$FY.Metric <- (PBEDMetrics$Q1.Metric + PBEDMetrics$Q2.Metric +
                                PBEDMetrics$Q3.Metric + PBEDMetrics$Q4.Metric)
    

#code that makes sure all columns will be displayed even if all data is fron one quarter
    qcols <- colnames(PBEDdisplay)[grepl("Q[0-9]$",colnames(PBEDdisplay))]
    gdata::unknownToNA(x=PBEDdisplay[,qcols], unknown = NA)
    
    PBEDdisplay[,qcols][is.null(PBEDdisplay[,qcols])] <- 0
    therest <- all.qcols[!all.qcols %in% qcols]
    PBEDdisplay[,therest] <- 0
    
    # gdata::unknownToNA(x=PBEDdisplay$Q1, unknown = NA)
    # gdata::unknownToNA(x=PBEDdisplay$Q2, unknown = NA)
    # gdata::unknownToNA(x=PBEDdisplay$Q3, unknown = NA)
    # gdata::unknownToNA(x=PBEDdisplay$Q4, unknown = NA)
    # PBEDdisplay$Q1[is.null(PBEDdisplay$Q1)] <- 0
    # PBEDdisplay$Q2[is.null(PBEDdisplay$Q2)] <- 0
    # PBEDdisplay$Q3[is.null(PBEDdisplay$Q3)] <- 0
    # PBEDdisplay$Q4[is.null(PBEDdisplay$Q4)] <- 0
    
    PBEDdisplay$FY.Total <- sum(PBEDdisplay$Q1, PBEDdisplay$Q2, PBEDdisplay$Q3, PBEDdisplay$Q4, na.rm=TRUE)
    PBtableWithED <- merge(PBEDdisplay, PBEDMetrics, by = "Event.or.distribution.type.", all.y = TRUE)
    PBtableWithED[is.na(PBtableWithED)] <- 0
#social media submission data creation     
    PBSMSdisplay <- PBSMSfromHandsOn() %>%
      ungroup() %>% 
      gather(key = "Event.or.distribution.type.", 
             value = "Count", 
             Facebook.Posts, Tweets, Instagram.Posts,
             Full.Length.Videos, Digital.Shorts, Livestreaming) %>% 
      group_by(Quarter,Event.or.distribution.type.) %>% 
      summarize(counts=sum(as.numeric(Count))) %>% 
      spread(key=Quarter, value=counts) 
#Metrics  
    
    PBSMSMetrics <- data.frame(
      Event.or.distribution.type. = c("Digital.Shorts",
                                      "Facebook.Posts","Full.Length.Videos",
                                      "Instagram.Posts","Livestreaming",
                                      "Tweets"),
      Q1.Metric = c(3,70,1,48,2,70),
      Q2.Metric = c(4,70,0,48,2,70),
      Q3.Metric = c(4,70,0,48,3,70),
      Q4.Metric = c(4,70,0,48,2,70)
    )
    PBSMSMetrics$FY.Metric <- (PBSMSMetrics$Q1.Metric + PBSMSMetrics$Q2.Metric +
                                PBSMSMetrics$Q3.Metric + PBSMSMetrics$Q4.Metric)
    
#code that makes sure all columns will be displayed even if all data is fron one quarter
    qcols <- colnames(PBSMSdisplay)[grepl("Q[0-9]$",colnames(PBSMSdisplay))]
    gdata::unknownToNA(x=PBSMSdisplay[,qcols], unknown = NA)
    
    PBSMSdisplay[,qcols][is.null(PBSMSdisplay[,qcols])] <- 0
    therest <- all.qcols[!all.qcols %in% qcols]
    PBSMSdisplay[,therest] <- 0
    
    # gdata::unknownToNA(x=PBSMSdisplay$Q1, unknown = NA)
    # gdata::unknownToNA(x=PBSMSdisplay$Q2, unknown = NA)
    # gdata::unknownToNA(x=PBSMSdisplay$Q3, unknown = NA)
    # gdata::unknownToNA(x=PBSMSdisplay$Q4, unknown = NA)
    # PBSMSdisplay$Q1[is.null(PBSMSdisplay$Q1)] <- 0
    # PBSMSdisplay$Q2[is.null(PBSMSdisplay$Q2)] <- 0
    # PBSMSdisplay$Q3[is.null(PBSMSdisplay$Q3)] <- 0
    # PBSMSdisplay$Q4[is.null(PBSMSdisplay$Q4)] <- 0
    
    PBSMSdisplay$FY.Total <- sum(PBSMSdisplay$Q1, PBSMSdisplay$Q2, PBSMSdisplay$Q3, PBSMSdisplay$Q4, na.rm=TRUE)
    PBtableWithSMS <- merge(PBSMSdisplay, PBSMSMetrics, by = "Event.or.distribution.type.", all.y = TRUE)
    PBtableWithSMS[is.na(PBtableWithSMS)] <- 0
#combining of SMS and ED tables  
  values$PBfinal <- rbind(PBtableWithED, PBtableWithSMS) %>%
    select(Event.or.distribution.type.,Q1.Metric,Q1,Q2.Metric,Q2,Q3.Metric,Q3,Q4.Metric,Q4,FY.Metric, FY.Total) 
  
  output$PBtable1 <- renderDataTable({
    
    values$PBfinal %>%
      mutate(Event.or.distribution.type.= recodeEvents(Event.or.distribution.type.)) %>%
    formattable(., list(
    Q1 =  formatter("span",
                    style = ~ style(color = ifelse(Q1 < Q1.Metric, "red" , "green"))),
    Q2 =  formatter("span",
                    style = ~ style(color = ifelse(Q2 < Q2.Metric, "red" , "green"))),
    Q3 =  formatter("span",
                    style = ~ style(color = ifelse(Q3 < Q3.Metric, "red" , "green"))),
    Q4 =  formatter("span",
                    style = ~ style(color = ifelse(Q4 < Q4.Metric, "red" , "green"))), 
    FY.Total = formatter("span",
                    style = ~ style(color = ifelse(FY.Total < FY.Metric, "red" , "green")))
  )) %>%
    as.datatable(.,options = list("pageLength" = 25, columnDefs = list(list(className = 'dt-center', targets = 1:10))),
                    rownames = FALSE)
  })
  })
  
  
  ###############DOWNLOADING PUEBLO DATA#######################  
  ###############DOWNLOADING PUEBLO DATA#######################
  ###############DOWNLOADING PUEBLO DATA#######################  
  
  output$downloadPBII <- downloadHandler(
    filename = function() {
      paste("PBIndividualInteractionsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(PBIIfromHandsOn(), file, row.names = FALSE)
    }
  )
  output$downloadPBED <- downloadHandler(
    filename = function() {
      paste("PBEventOrDistData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(PBEDfromHandsOn(), file, row.names = FALSE)
    }
  ) 
  output$downloadPBSMS <- downloadHandler(
    filename = function() {
      paste("PBSocialMediaData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(PBSMSfromHandsOn(), file, row.names = FALSE)
    }
  )
#downloading a excel sheet with all data seperated by contact type and one sheet with full raw data
#merging of datasets 
  PBrawdata <- reactive({
    Reduce(function(x,y) merge(x,y, all = TRUE), 
           list(PBIIfromHandsOn(), PBEDfromHandsOn(), PBSMSfromHandsOn()))
  })
#defining individual datasets 
PBdatasets <- reactive({
  list("Individual Interactions" = PBIIfromHandsOn(), "Event or Distributions" =PBQEDfromHandsOn(), 
       "Social Media Submissions" = PBSMSfromHandsOn(), "All Pueblo Data" = PBrawdata())
})
#writing of excel 
PBexcel <- reactive({
  write.xlsx(PBdatasets(), file = "Pueblo_Data.xlsx", append = TRUE)
})

#action button to write excel and save it 
observeEvent(input$downloadPBRAW, {
 PBexcel() 
}) 

observeEvent(input$savePBall, {
  PBdfList <- list()
  PBdfList$II <- PBIIfromHandsOn()
  PBdfList$ED <-PBEDfromHandsOn()
  PBdfList$SMS <- PBSMSfromHandsOn()
  max.id <- max(PBdfList$II$Response.ID,PBdfList$ED$Response.ID,PBdfList$SMS$Response.ID,na.rm = TRUE)
  old.max <- max(PBInitialList$II$Response.ID,PBInitialList$ED$Response.ID,PBInitialList$SMS$Response.ID,na.rm = TRUE)
  if (max.id > old.max | !all(identical(PBInitialList$II,PBdfList$II),identical(PBInitialList$ED, PBdfList$ED), identical(PBInitialList$SMS,PBdfList$SMS))){
  
  systime = Sys.time()
  
  PBpath <- paste0("./data/intermediate/PB_FY2018_", format(systime, "%Y_%m_%d_%H_%M_%S"), ".rds")
  write.table(x = data.frame(Date=c(systime),Path=c(PBpath),MaxId=c(max.id)),
            file = "./data/PB_paths.csv",append=TRUE,row.names=FALSE,sep=",",col.names = FALSE)
  
  saveRDS(PBdfList,file=PBpath)
  }
})
  
  
  ################# BLUE GRASSSSS################  
  ################# BLUE GRASSSSS################
  ################# BLUE GRASSSSS################
#reading in and cleaning or individual interactions raw data blue grass  
  BGIIdata <- observe({
    inFile <- input$file6 
    
    if (is.null(inFile))
      return(NULL)
    
    BGII.new <- cleanBGII(inFile$datapath, prev.resp.id=last.max.bg)
    isolate(values$BGII <- rbind(values$BGII,BGII.new))
  })
  

#displaying of raw II blue grass data  
  output$BGIIrawdata <- renderRHandsontable({
    rhandsontable(values$BGII, useTypes = FALSE, rowHeaders = FALSE)
  })
#turns raw data into an R object   
  BGIIfromHandsOn <- reactive({
    testHandsonFunc(input$BGIIrawdata)
  })

#reading and cleaning of blue grass Event or Dist. rawdata     
  BGEDdata <- observe({
    inFile <- input$file6 
    
    if (is.null(inFile))
      return(NULL)
    
    BGED.new <- cleanBGED(inFile$datapath, prev.resp.id=last.max.bg)
    isolate(values$BGED <- rbind(values$BGED,BGED.new))

  })

#displying of blue grass event or dist raw data 
  output$BGEDrawdata <- renderRHandsontable(
    rhandsontable(values$BGED, useTypes = FALSE, rowHeaders = FALSE)
  )
#turns raw data table into an R object 
  BGEDfromHandsOn <- reactive({
    testHandsonFunc(input$BGEDrawdata)
  })
#reacing and cleaning of blue grass Social media data
  BGSMSdata <- observe({
    inFile <- input$file6 
    
    if (is.null(inFile))
      return(NULL)
    BGSMS.new <- cleanBGSMS(inFile$datapath, prev.resp.id=last.max.bg)
    isolate(values$BGSMS <- rbind(values$BGSMS,BGSMS.new))
    
  })
  

#displaying of SMS raw data  
  output$BGSMSrawdata <- renderRHandsontable(
    rhandsontable(values$BGSMS, useTypes = FALSE, rowHeaders = FALSE)
  )
#turns table into r object  
  BGSMSfromHandsOn <- reactive({
    testHandsonFunc(input$BGSMSrawdata)
  })
  
#after users press save the dashboard table for BG is created 
  
observeEvent(input$saveBGall, {
#creating dasboard table for ED  
  BGEDdisplay <- BGEDfromHandsOn() %>%
    ungroup() %>%
    group_by(Quarter, Event.or.distribution.type.) %>%
    summarize(countname=n()) %>%
    spread(key = Quarter, value = countname, fill = 0) 
#metrics  
  
  BGEDMetrics <- data.frame(
    Event.or.distribution.type. = c("Weekly News Summary","CAC/CDCAB (or subcommittee)/advisory board meeting",
                                  "News Article","Information booth/exhibit","Digital communications distribution",
                                  "Networking event","Briefing/presentation", "Tour", "Media event",
                                  "Workforce event", "Education Outreach Event"),
    Q1.Metric = c(12,1,12,1,22,4,3,4,1,6,5), 
    Q2.Metric = c(13,1,12,1,22,4,4,4,1,6,5), 
    Q3.Metric = c(13,1,12,1,22,4,2,4,1,6,5),
    Q4.Metric = c(13,1,12,3,22,4,4,4,1,6,5) 
  )
    BGEDMetrics$FY.Metric <- sum(BGEDMetrics$Q1.Metric, BGEDMetrics$Q2.Metric,
                                BGEDMetrics$Q3.Metric, BGEDMetrics$Q4.Metric,na.rm=TRUE )
    
    qcols <- colnames(BGEDdisplay)[grepl("Q[0-9]$",colnames(BGEDdisplay))]
    gdata::unknownToNA(x=BGEDdisplay[,qcols], unknown = NA)
    
    BGEDdisplay[,qcols][is.null(BGEDdisplay[,qcols])] <- 0
    therest <- all.qcols[!all.qcols %in% qcols]
    BGEDdisplay[,therest] <- 0
  
    
  # gdata::unknownToNA(x=BGEDdisplay$Q1, unknown = NA)
  # gdata::unknownToNA(x=BGEDdisplay$Q2, unknown = NA)
  # gdata::unknownToNA(x=BGEDdisplay$Q3, unknown = NA)
  # gdata::unknownToNA(x=BGEDdisplay$Q4, unknown = NA)
  # BGEDdisplay$Q1[is.null(BGEDdisplay$Q1)] <- 0
  # BGEDdisplay$Q2[is.null(BGEDdisplay$Q2)] <- 0
  # BGEDdisplay$Q3[is.null(BGEDdisplay$Q3)] <- 0
  # BGEDdisplay$Q4[is.null(BGEDdisplay$Q4)] <- 0

  BGEDdisplay$FY.Total <- sum(BGEDdisplay$Q1, BGEDdisplay$Q2, BGEDdisplay$Q3, BGEDdisplay$Q4,na.rm=TRUE)
  BGtableWithED <- merge(BGEDdisplay, BGEDMetrics, by = "Event.or.distribution.type.",all.y = TRUE)
  BGtableWithED[is.na(BGtableWithED)] <- 0
  
#creating of dashboard table for SMS    
  BGSMSdisplay  <- BGSMSfromHandsOn() %>%
    ungroup() %>% 
    gather(key = "Event.or.distribution.type.", 
           value = "Count", 
           Facebook.Posts, Tweets, Instagram.Posts,
           Full.Length.Videos, Digital.Shorts, Livestreaming, Flickr) %>%
    group_by(Quarter,Event.or.distribution.type.) %>% 
    summarize(counts=sum(as.numeric(Count))) %>% 
    spread(key=Quarter, value=counts) 
#metrics

  BGSMSMetrics <- data.frame(
    Event.or.distribution.type. = c("Facebook.Posts","Tweets",
                                    "Instagram.Posts","Flickr","Full.Length.Videos",
                                    "Digital.Shorts","Livestreaming"),
    Q1.Metric = c(70,70,48,60,1,3,1), 
    Q2.Metric = c(70,70,48,60,0,4,1), 
    Q3.Metric = c(70,70,48,60,0,4,1),
    Q4.Metric = c(70,70,48,60,0,4,1) 
  )
  BGSMSMetrics$FY.Metric <- sum(BGSMSMetrics$Q1.Metric, BGSMSMetrics$Q2.Metric,
                              BGSMSMetrics$Q3.Metric, BGSMSMetrics$Q4.Metric,na.rm=TRUE )
  
  qcols <- colnames(BGSMSdisplay)[grepl("Q[0-9]$",colnames(BGSMSdisplay))]
  gdata::unknownToNA(x=BGSMSdisplay[,qcols], unknown = NA)
  
  BGSMSdisplay[,qcols][is.null(BGSMSdisplay[,qcols])] <- 0
  therest <- all.qcols[!all.qcols %in% qcols]
  BGSMSdisplay[,therest] <- 0
  
  # gdata::unknownToNA(x=BGSMSdisplay$Q1, unknown = NA)
  # gdata::unknownToNA(x=BGSMSdisplay$Q2, unknown = NA)
  # gdata::unknownToNA(x=BGSMSdisplay$Q3, unknown = NA)
  # gdata::unknownToNA(x=BGSMSdisplay$Q4, unknown = NA)
  # BGSMSdisplay$Q1[is.null(BGSMSdisplay$Q1)] <- 0
  # BGSMSdisplay$Q2[is.null(BGSMSdisplay$Q2)] <- 0
  # BGSMSdisplay$Q3[is.null(BGSMSdisplay$Q3)] <- 0
  # BGSMSdisplay$Q4[is.null(BGSMSdisplay$Q4)] <- 0
  
  BGSMSdisplay$FY.Total <- sum(BGSMSdisplay$Q1, BGSMSdisplay$Q2, BGSMSdisplay$Q3, BGSMSdisplay$Q4,na.rm=TRUE)
  BGtableWithSMS <- merge(BGSMSdisplay, BGSMSMetrics, by = "Event.or.distribution.type.",all.y = TRUE)
  BGtableWithSMS[is.na(BGtableWithSMS)] <- 0
    
  values$BGfinal <- rbind(BGtableWithED, BGtableWithSMS) %>%
    select(Event.or.distribution.type.,Q1.Metric,Q1,Q2.Metric,Q2,Q3.Metric,Q3,Q4.Metric,Q4,FY.Metric, FY.Total)
  
 #displaying if table with code to turn certain number red  
  output$BGtable1 <- renderDataTable({
    
    values$BGfinal %>%
      mutate(Event.or.distribution.type.= recodeEvents(Event.or.distribution.type.)) %>%
      formattable(., list(
        Q1 =  formatter("span",
                        style = ~ style(color = ifelse(Q1 < Q1.Metric, "red" , "green"))),
        Q2 =  formatter("span",
                        style = ~ style(color = ifelse(Q2 < Q2.Metric, "red" , "green"))),
        Q3 =  formatter("span",
                        style = ~ style(color = ifelse(Q3 < Q3.Metric, "red" , "green"))),
        Q4 =  formatter("span",
                        style = ~ style(color = ifelse(Q4 < Q4.Metric, "red" , "green"))), 
        FY.Total = formatter("span",
                             style = ~ style(color = ifelse(FY.Total < FY.Metric, "red" , "green")))
      )) %>% as.datatable(.,options = list("pageLength" = 25, columnDefs = list(list(className = 'dt-center', targets = 1:10))),
                          rownames = FALSE)
  })
})
  
  ###############DOWNLOADING BLUEGRASS DATA#######################  
  ###############DOWNLOADING BLUEGRASS DATA#######################
  ###############DOWNLOADING BLUEGRASS DATA#######################  
  
  output$downloadBGII <- downloadHandler(
    filename = function() {
      paste("BGIndividualInteractionsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(BGIIfromHandsOn(), file, row.names = FALSE)
    }
  )
  output$downloadBGED <- downloadHandler(
    filename = function() {
      paste("BGEventOrDistData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(BGEDfromHandsOn(), file, row.names = FALSE)
    }
  ) 
  output$downloadBGSMS <- downloadHandler(
    filename = function() {
      paste("BGSocialMediaData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(BGSMSfromHandsOn(), file, row.names = FALSE)
    }
  )
  
#downloading a excel sheet with all data seperated by contact type and one sheet with full raw data
#merging of datasets 
  BGrawdata <- reactive({
    Reduce(function(x,y) merge(x,y, all = TRUE),
           list(BGIIfromHandsOn(), BGEDfromHandsOn(), BGSMSfromHandsOn()))
  })
#defining individual datasets 
  BGdatasets <- reactive({
    list("Individual Interactions" = BGIIfromHandsOn(), "Event or Distributions" = BGEDfromHandsOn(), 
         "Social Media Submissions" = BGSMSfromHandsOn(), "All Blue Grass Data" = BGrawdata())
  })
  
#writing of excel 
  BGexcel <- reactive({
    write.xlsx(BGdatasets(), file = "Blue_Grass_Data.xlsx", append = TRUE)
  })
  
  observeEvent(input$downloadBGRAW, {
    BGexcel()
  })
   
  observeEvent(input$saveBGall, {
    BGdfList <- list()
    BGdfList$II <- BGIIfromHandsOn()
    BGdfList$ED <-BGEDfromHandsOn()
    BGdfList$SMS <- BGSMSfromHandsOn()
    
    max.id <- max(BGdfList$II$Response.ID,BGdfList$ED$Response.ID,BGdfList$SMS$Response.ID,na.rm = TRUE)
    old.max <- max(BGInitialList$II$Response.ID,BGInitialList$ED$Response.ID,BGInitialList$SMS$Response.ID,na.rm = TRUE)
    if (max.id > old.max | !all(identical(BGInitialList$II, BGdfList$II),identical(BGInitialList$ED,BGdfList$ED),identical(BGInitialList$SMS,BGdfList$SMS))){

    systime = Sys.time()
    
    BGpath <- paste0("./data/intermediate/BG_FY2018_", format(systime, "%Y_%m_%d_%H_%M_%S"), ".rds")
    write.table(x = data.frame(Date=c(systime),Path=c(BGpath),MaxId=c(max.id)),
              file = "./data/BG_paths.csv",append=TRUE,row.names=FALSE,sep=",",col.names = FALSE)
    
    saveRDS(BGdfList,file=BGpath)
    }
  }) 
  
  ############PUEBLO FEEDBACK#####################  
  ############PUEBLO FEEDBACK#####################
  ############PUEBLO FEEDBACK#####################
#function to change Nan% to a 0, may need to be revised because the 0% messes with the FY average %
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
 
  #reactive that reads in feedback data, and transforms it into the feedback table    
  pb.feedback <- reactive({
    inFile <- input$file3feedback 
    if (is.null(inFile))
      return(NULL)
 #read in 
    PBfeedback <- read.csv(inFile$datapath,header=TRUE,strip.white = TRUE,blank.lines.skip = TRUE,stringsAsFactors = FALSE) %>%
      filter(Stand.alone.Card.or.Feedback.Form. != "") %>% cleanFeedback(.) %>%
      mutate(Site = "PUEBLO")
    PBfeedback
  })
      
#     PBfeedback$Month <- month(as.Date(PBfeedback$Date,format="%m/%d/%Y"))
#     PBfeedback$Quarter <- ifelse(
#       PBfeedback$Month >= 10, "Q1",
#       ifelse(PBfeedback$Month <= 3, "Q2", 
#              ifelse(PBfeedback$Month >= 4 & PBfeedback$Month < 7, "Q3", 
#                     ifelse(PBfeedback$Month >=7 & PBfeedback$Month < 10, "Q4", 0))))
# #makes na values 6 so they are not counted in the percent calculations 
#     PBfeedback[is.na(PBfeedback)] <- 6
# 
# #create Q1 data frame, counts rows as forms collected, and calculates percent satified by dividing all 1's and 2's by total    
#     PBfeedbackQ1 <- PBfeedback[PBfeedback$Quarter == "Q1",]
#     PBQ1formscollected <- nrow(PBfeedbackQ1)
#     PBQ1percent <- ((
#       round((sum(PBfeedbackQ1$Addressed.Questions..Concerns <= 2) / sum(PBfeedbackQ1$Addressed.Questions..Concerns <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ1$Activities.Increased.Awareness <= 2) / sum(PBfeedbackQ1$Activities.Increased.Awareness <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ1$Satisfied.with.Program <= 2) / sum(PBfeedbackQ1$Satisfied.with.Program <= 5)), digits = 2) +
#         round((sum(as.numeric(PBfeedbackQ1$Accurate.Information) <= 2)) / sum(as.numeric(PBfeedbackQ1$Accurate.Information) <= 5), digits = 2) +
#         round((sum(PBfeedbackQ1$ACWA.is.open.shares.with.public <= 2) / sum(PBfeedbackQ1$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ1$Aware.of.current.developments.progress <= 2) / sum(PBfeedbackQ1$Aware.of.current.developments.progress <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ1$Information.easy.to.understand <= 2) / sum(PBfeedbackQ1$Information.easy.to.understand <= 5)), digits = 2) +
#         round((sum(as.numeric(PBfeedbackQ1$Information.easily.accessible) <= 2) / sum(as.numeric(PBfeedbackQ1$Information.easily.accessible) <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ1$Offers.Ops..For.Involvement <= 2) / sum(PBfeedbackQ1$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
#     PBQ1percent[is.nan.data.frame(PBQ1percent)] <- 0
#     
# #create Q2 data frame, counts rows as forms collected, and calculates percent satified by dividing all 1's and 2's by total        
#     PBfeedbackQ2 <- PBfeedback[PBfeedback$Quarter == "Q2",]
#     PBQ2formscollected <- nrow(PBfeedbackQ2)
#     PBQ2percent <- ((
#       round((sum(PBfeedbackQ2$Addressed.Questions..Concerns <= 2) / sum(PBfeedbackQ2$Addressed.Questions..Concerns <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ2$Activities.Increased.Awareness <= 2) / sum(PBfeedbackQ2$Activities.Increased.Awareness <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ2$Satisfied.with.Program <= 2) / sum(PBfeedbackQ2$Satisfied.with.Program <= 5)), digits = 2) +
#         round((sum(as.numeric(PBfeedbackQ2$Accurate.Information) <= 2)) / sum(as.numeric(PBfeedbackQ2$Accurate.Information) <= 5), digits = 2) +
#         round((sum(PBfeedbackQ2$ACWA.is.open.shares.with.public <= 2) / sum(PBfeedbackQ2$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ2$Aware.of.current.developments.progress <= 2) / sum(PBfeedbackQ2$Aware.of.current.developments.progress <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ2$Information.easy.to.understand <= 2) / sum(PBfeedbackQ2$Information.easy.to.understand <= 5)), digits = 2) +
#         round((sum(as.numeric(PBfeedbackQ2$Information.easily.accessible) <= 2) / sum(as.numeric(PBfeedbackQ2$Information.easily.accessible) <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ2$Offers.Ops..For.Involvement <= 2) / sum(PBfeedbackQ2$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
#     PBQ2percent[is.nan.data.frame(PBQ2percent)] <- 0
# 
# #create Q3 data frame, counts rows as forms collected, and calculates percent satified by dividing all 1's and 2's by total            
#     PBfeedbackQ3 <- PBfeedback[PBfeedback$Quarter == "Q3",]
#     PBQ3formscollected <- nrow(PBfeedbackQ3)
#     PBQ3percent <- ((
#       round((sum(PBfeedbackQ3$Addressed.Questions..Concerns <= 2) / sum(PBfeedbackQ3$Addressed.Questions..Concerns <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ3$Activities.Increased.Awareness <= 2) / sum(PBfeedbackQ3$Activities.Increased.Awareness <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ3$Satisfied.with.Program <= 2) / sum(PBfeedbackQ3$Satisfied.with.Program <= 5)), digits = 2) +
#         round((sum(as.numeric(PBfeedbackQ3$Accurate.Information) <= 2)) / sum(as.numeric(PBfeedbackQ3$Accurate.Information) <= 5), digits = 2) +
#         round((sum(PBfeedbackQ3$ACWA.is.open.shares.with.public <= 2) / sum(PBfeedbackQ3$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ3$Aware.of.current.developments.progress <= 2) / sum(PBfeedbackQ3$Aware.of.current.developments.progress <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ3$Information.easy.to.understand <= 2) / sum(PBfeedbackQ3$Information.easy.to.understand <= 5)), digits = 2) +
#         round((sum(as.numeric(PBfeedbackQ3$Information.easily.accessible) <= 2) / sum(as.numeric(PBfeedbackQ3$Information.easily.accessible) <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ3$Offers.Ops..For.Involvement <= 2) / sum(PBfeedbackQ3$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
#     PBQ3percent[is.nan.data.frame(PBQ3percent)] <- 0
# 
# #create Q4 data frame, counts rows as forms collected, and calculates percent satified by dividing all 1's and 2's by total            
#     PBfeedbackQ4 <- PBfeedback[PBfeedback$Quarter == "Q4",]
#     PBQ4formscollected <- nrow(PBfeedbackQ4)
#     PBQ4percent <- ((
#       round((sum(PBfeedbackQ4$Addressed.Questions..Concerns <= 2) / sum(PBfeedbackQ4$Addressed.Questions..Concerns <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ4$Activities.Increased.Awareness <= 2) / sum(PBfeedbackQ4$Activities.Increased.Awareness <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ4$Satisfied.with.Program <= 2) / sum(PBfeedbackQ4$Satisfied.with.Program <= 5)), digits = 2) +
#         round((sum(as.numeric(PBfeedbackQ4$Accurate.Information) <= 2)) / sum(as.numeric(PBfeedbackQ4$Accurate.Information) <= 5), digits = 2) +
#         round((sum(PBfeedbackQ4$ACWA.is.open.shares.with.public <= 2) / sum(PBfeedbackQ4$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ4$Aware.of.current.developments.progress <= 2) / sum(PBfeedbackQ4$Aware.of.current.developments.progress <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ4$Information.easy.to.understand <= 2) / sum(PBfeedbackQ4$Information.easy.to.understand <= 5)), digits = 2) +
#         round((sum(as.numeric(PBfeedbackQ4$Information.easily.accessible) <= 2) / sum(as.numeric(PBfeedbackQ4$Information.easily.accessible) <= 5)), digits = 2) +
#         round((sum(PBfeedbackQ4$Offers.Ops..For.Involvement <= 2) / sum(PBfeedbackQ4$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
#     PBQ4percent[is.nan.data.frame(PBQ4percent)] <- 0
#     
#     PBQ4percent1 <- ifelse(PBQ4formscollected == 0, PBQ3percent, PBQ4percent) 
#     #total forms collected for pueblo 
#     PBtotalFormscollected <- sum(PBQ1formscollected,PBQ2formscollected,PBQ3formscollected,PBQ4formscollected)
#     #total satisfaction for Pueblo 
#     PBFYsatisfaction <- (sum(PBQ1percent,PBQ2percent,PBQ3percent,PBQ4percent1) /4)
#     
#     
#     Pueblo.Forms.Collected <- c(PBQ1formscollected,PBQ2formscollected,PBQ3formscollected,PBQ4formscollected,PBtotalFormscollected)
#     Pueblo.Percent.Satisfied <- c(paste0(round(PBQ1percent*100,digits = 1),"%"),paste0(round(PBQ2percent*100,digits = 1),"%"),
#                                 paste0(round(PBQ3percent*100,digits = 1),"%"),paste0(round(PBQ4percent1*100,digits = 1),"%"),
#                                 paste0(round(PBFYsatisfaction*100,digits = 1),"%"))
#     
    
    
    ############### BLUE GRASS FEEDBACK##################
    ############### BLUE GRASS FEEDBACK##################
    ############### BLUE GRASS FEEDBACK##################
    bg.feedback <- reactive({
    inFile <- input$file6feedback 
    
    if (is.null(inFile))
      return(NULL)
  BGfeedback <- read.csv(inFile$datapath,header=TRUE,strip.white = TRUE,blank.lines.skip = TRUE,stringsAsFactors = FALSE) %>%
    filter(Stand.alone.Card.or.Feedback.Form. != "") %>% cleanFeedback(.) %>%
    mutate(Site="BLUE GRASS")
  BGfeedback
    })
    # BGfeedback <- read.csv(inFile$datapath,header=TRUE,strip.white = TRUE,blank.lines.skip = TRUE) %>%
    #   filter(Stand.alone.Card.or.Feedback.Form. != "")
    # 
    # BGfeedback$Month <- month(as.Date(BGfeedback$Date,format="%m/%d/%Y"))
    # BGfeedback$Quarter <- ifelse(
    #   BGfeedback$Month >= 10, "Q1",
    #   ifelse(BGfeedback$Month <= 3, "Q2", 
    #          ifelse(BGfeedback$Month >= 4 & BGfeedback$Month < 7, "Q3", 
    #                 ifelse(BGfeedback$Month >=7 & BGfeedback$Month < 10, "Q4", 0))))
    # BGfeedback[is.na(BGfeedback)] <- 6
    # 
    # BGfeedbackQ1 <- BGfeedback[BGfeedback$Quarter == "Q1",]
    # BGQ1formscollected <- nrow(BGfeedbackQ1)
    # BGQ1percent <- ((
    #   round((sum(BGfeedbackQ1$Addressed.Questions..Concerns <= 2) / sum(BGfeedbackQ1$Addressed.Questions..Concerns <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ1$Activities.Increased.Awareness <= 2) / sum(BGfeedbackQ1$Activities.Increased.Awareness <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ1$Satisfied.with.Program <= 2) / sum(BGfeedbackQ1$Satisfied.with.Program <= 5)), digits = 2) +
    #     round((sum(as.numeric(BGfeedbackQ1$Accurate.Information) <= 2)) / sum(as.numeric(BGfeedbackQ1$Accurate.Information) <= 5), digits = 2) +
    #     round((sum(BGfeedbackQ1$ACWA.is.open.shares.with.public <= 2) / sum(BGfeedbackQ1$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ1$Aware.of.current.developments.progress <= 2) / sum(BGfeedbackQ1$Aware.of.current.developments.progress <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ1$Information.easy.to.understand <= 2) / sum(BGfeedbackQ1$Information.easy.to.understand <= 5)), digits = 2) +
    #     round((sum(as.numeric(BGfeedbackQ1$Information.easily.accessible) <= 2) / sum(as.numeric(BGfeedbackQ1$Information.easily.accessible) <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ1$Offers.Ops..For.Involvement <= 2) / sum(BGfeedbackQ1$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    # BGQ1percent[is.nan.data.frame(BGQ1percent)] <- 0
    #  
    # BGfeedbackQ2 <- BGfeedback[BGfeedback$Quarter == "Q2",]
    # BGQ2formscollected <- nrow(BGfeedbackQ2)
    # BGQ2percent <- ((
    #   round((sum(BGfeedbackQ2$Addressed.Questions..Concerns <= 2) / sum(BGfeedbackQ2$Addressed.Questions..Concerns <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ2$Activities.Increased.Awareness <= 2) / sum(BGfeedbackQ2$Activities.Increased.Awareness <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ2$Satisfied.with.Program <= 2) / sum(BGfeedbackQ2$Satisfied.with.Program <= 5)), digits = 2) +
    #     round((sum(as.numeric(BGfeedbackQ2$Accurate.Information) <= 2)) / sum(as.numeric(BGfeedbackQ2$Accurate.Information) <= 5), digits = 2) +
    #     round((sum(BGfeedbackQ2$ACWA.is.open.shares.with.public <= 2) / sum(BGfeedbackQ2$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ2$Aware.of.current.developments.progress <= 2) / sum(BGfeedbackQ2$Aware.of.current.developments.progress <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ2$Information.easy.to.understand <= 2) / sum(BGfeedbackQ2$Information.easy.to.understand <= 5)), digits = 2) +
    #     round((sum(as.numeric(BGfeedbackQ2$Information.easily.accessible) <= 2) / sum(as.numeric(BGfeedbackQ2$Information.easily.accessible) <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ2$Offers.Ops..For.Involvement <= 2) / sum(BGfeedbackQ2$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    # BGQ2percent[is.nan.data.frame(BGQ2percent)] <- 0
    #  
    # BGfeedbackQ3 <- BGfeedback[BGfeedback$Quarter == "Q3",]
    # BGQ3formscollected <- nrow(BGfeedbackQ3)
    # BGQ3percent <- ((
    #   round((sum(BGfeedbackQ3$Addressed.Questions..Concerns <= 2) / sum(BGfeedbackQ3$Addressed.Questions..Concerns <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ3$Activities.Increased.Awareness <= 2) / sum(BGfeedbackQ3$Activities.Increased.Awareness <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ3$Satisfied.with.Program <= 2) / sum(BGfeedbackQ3$Satisfied.with.Program <= 5)), digits = 2) +
    #     round((sum(as.numeric(BGfeedbackQ3$Accurate.Information) <= 2)) / sum(as.numeric(BGfeedbackQ3$Accurate.Information) <= 5), digits = 2) +
    #     round((sum(BGfeedbackQ3$ACWA.is.open.shares.with.public <= 2) / sum(BGfeedbackQ3$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ3$Aware.of.current.developments.progress <= 2) / sum(BGfeedbackQ3$Aware.of.current.developments.progress <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ3$Information.easy.to.understand <= 2) / sum(BGfeedbackQ3$Information.easy.to.understand <= 5)), digits = 2) +
    #     round((sum(as.numeric(BGfeedbackQ3$Information.easily.accessible) <= 2) / sum(as.numeric(BGfeedbackQ3$Information.easily.accessible) <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ3$Offers.Ops..For.Involvement <= 2) / sum(BGfeedbackQ3$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    # BGQ3percent[is.nan.data.frame(BGQ3percent)] <- 0
    # 
    # BGfeedbackQ4 <- BGfeedback[BGfeedback$Quarter == "Q4",]
    # BGQ4formscollected <- nrow(BGfeedbackQ4)
    # BGQ4percent <- ((
    #   round((sum(BGfeedbackQ4$Addressed.Questions..Concerns <= 2) / sum(BGfeedbackQ4$Addressed.Questions..Concerns <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ4$Activities.Increased.Awareness <= 2) / sum(BGfeedbackQ4$Activities.Increased.Awareness <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ4$Satisfied.with.Program <= 2) / sum(BGfeedbackQ4$Satisfied.with.Program <= 5)), digits = 2) +
    #     round((sum(as.numeric(BGfeedbackQ4$Accurate.Information) <= 2)) / sum(as.numeric(BGfeedbackQ4$Accurate.Information) <= 5), digits = 2) +
    #     round((sum(BGfeedbackQ4$ACWA.is.open.shares.with.public <= 2) / sum(BGfeedbackQ4$ACWA.is.open.shares.with.public <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ4$Aware.of.current.developments.progress <= 2) / sum(BGfeedbackQ4$Aware.of.current.developments.progress <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ4$Information.easy.to.understand <= 2) / sum(BGfeedbackQ4$Information.easy.to.understand <= 5)), digits = 2) +
    #     round((sum(as.numeric(BGfeedbackQ4$Information.easily.accessible) <= 2) / sum(as.numeric(BGfeedbackQ4$Information.easily.accessible) <= 5)), digits = 2) +
    #     round((sum(BGfeedbackQ4$Offers.Ops..For.Involvement <= 2) / sum(BGfeedbackQ4$Offers.Ops..For.Involvement <= 5)), digits = 2)) /9)
    # BGQ4percent[is.nan.data.frame(BGQ4percent)] <- 0
    # 
    # BGQ4percent1 <- ifelse(BGQ4formscollected == 0, BGQ3percent, BGQ4percent)
    # BGtotalFormscollected <- sum(BGQ1formscollected,BGQ2formscollected,BGQ3formscollected,BGQ4formscollected)
    # BGFYsatisfaction <- (sum(BGQ1percent,BGQ2percent,BGQ3percent,BGQ4percent1) /4)
    # 
    # 
    # Blue_Grass.Forms.Collected <- c(BGQ1formscollected,BGQ2formscollected,BGQ3formscollected,BGQ4formscollected,BGtotalFormscollected)
    # Blue_Grass.Percent.Satisfied <- c(paste0(round(BGQ1percent*100,digits = 1),"%"),paste0(round(BGQ2percent*100,digits = 1),"%"), 
    #                                paste0(round(BGQ3percent*100,digits = 1),"%"),paste0(round(BGQ4percent*100,digits = 1),"%"), 
    #                                paste0(round(BGFYsatisfaction*100,digits = 1),"%"))
    
    ################TOTAL FEEDBACK###################  
    ################TOTAL FEEDBACK###################
    ################TOTAL FEEDBACK###################
  
  observe({
    if(!(is.null(pb.feedback()) & is.null(bg.feedback()))){
    tmp <-
      bind_rows(pb.feedback(),bg.feedback()) %>%
      # cleanFeedback(.) %>%
        prepareFeedback(.)
    isolate(values$feedback <- tmp)
    }
    })
    # qters <- c("Q1", "Q2", "Q3", "Q4", "FYTD")
    # PEOACWAtotalforms <- c(sum(sum(BGQ1formscollected,PBQ1formscollected),sum(BGQ2formscollected,PBQ2formscollected),
    #                            sum(BGQ3formscollected,PBQ3formscollected),sum(BGQ4formscollected,PBQ4formscollected)))
    # PEO_ACWA.Forms.Collected <- c(sum(BGQ1formscollected,PBQ1formscollected),sum(BGQ2formscollected,PBQ2formscollected),
    #                            sum(BGQ3formscollected,PBQ3formscollected),sum(BGQ4formscollected,PBQ4formscollected), PEOACWAtotalforms)
    # 
    # Q1formscollected <- (sum(BGQ1formscollected, PBQ1formscollected))
    # Q2formscollected <- (sum(BGQ2formscollected, PBQ2formscollected))
    # Q3formscollected <- (sum(BGQ3formscollected, PBQ3formscollected))
    # Q4formscollected <- (sum(BGQ4formscollected, PBQ4formscollected))
    # 
    # Q1percent <- (sum(BGQ1percent,PBQ1percent)/Q1formscollected)
    # Q2percent <- (sum(BGQ2percent,PBQ2percent)/Q2formscollected)
    # Q3percent <- (sum(BGQ3percent,PBQ3percent)/Q3formscollected)
    # Q4percent <- (sum(BGQ4percent1,PBQ4percent1)/Q4formscollected)
    # 
    # PEOACWAsatisfactionTotal <- c(sum((sum(BGQ1percent,PBQ1percent)/2),(sum(BGQ2percent,PBQ2percent)/2),
    #                                   (sum(BGQ3percent,PBQ3percent)/2),(sum(BGQ4percent1,PBQ4percent1)/2))/4)
    # PEO_ACWA.Percent.Satisfied <- c(paste0(round(Q1percent*100,digits = 1),"%"),paste0(round(Q2percent*100,digits = 1),"%"),
    #                              paste0(round(Q3percent*100,digits = 1),"%"),paste0(round(Q4percent*100,digits = 1),"%"),
    #                              paste0(round(PEOACWAsatisfactionTotal*100,digits = 1),"%"))
    # 
    # # PEO_ACWA.Percent.Satisfied <- ifelse(
    # #   Q2percent == 0, Q1percent, 
    # #   ifelse(Q3percent == 0, (paste0(((round(Q1percent + Q2percent)/2), digits = 1),"%"),
    # #          ifelse(Q4percent == 0, (paste0(round(Q1percent + Q2percent + Q3percent)/3), digits = 1),"%",
    # #                 ifelse(,
    # #   ))))
    # 
    # feedbackPEOACWA <- data.frame(qters, PEO_ACWA.Forms.Collected,PEO_ACWA.Percent.Satisfied,Blue_Grass.Forms.Collected,
    #                               Blue_Grass.Percent.Satisfied, Pueblo.Forms.Collected, Pueblo.Percent.Satisfied) %>%
    #   gather(key = "Titles",
    #          value = "Count",
    #          PEO_ACWA.Forms.Collected, PEO_ACWA.Percent.Satisfied, Blue_Grass.Forms.Collected,
    #          Blue_Grass.Percent.Satisfied, Pueblo.Forms.Collected, Pueblo.Percent.Satisfied) %>%
    #   spread(key=qters, value=Count) %>% 
    #   select(Titles, Q1, Q2, Q3, Q4, FYTD) 
    # 
    # title.order <- data.frame(Titles=c("PEO_ACWA.Forms.Collected","PEO_ACWA.Percent.Satisfied",
    #                                    "Blue_Grass.Forms.Collected","Blue_Grass.Percent.Satisfied",
    #                                    "Pueblo.Forms.Collected","Pueblo.Percent.Satisfied"),rnum=1:6)
    # 
    # feedbackPEOACWA <- merge(title.order,feedbackPEOACWA,all=T,by="Titles") %>%
    #   arrange(rnum) %>% select(-rnum)
  #   
  #   
  # })
  output$Feedbacktable1 <- renderDataTable({
    values$feedback %>% DT::datatable(rownames = FALSE)
  })
  
  output$report <- downloadHandler(
    filename="PEO_Report.pdf",
    content = function(file){
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "REO_Report.Rmd")
      file.copy("PEO_Report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        HQ_FY = values$HQfinal,
        PB_FY = values$PBfinal,#Event.or.distribution.type.= recodeEvents(Event.or.distribution.type.)),
        BG_FY = values$BGfinal,
        feedback = values$feedback,
        cwd = getwd())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    })
  
}