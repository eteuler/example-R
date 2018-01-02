library(tidyr)
library(dplyr)
library(shiny)
#library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
#library(plotly)
#library(data.table)
library(DT)
#library(zoo)
#library(ggplot2)
library(formattable)
library(rhandsontable)
library(rmarkdown)
library(gdata)
library(knitr)
library(rmarkdown)
library(openxlsx)
library(kableExtra)
require(lubridate)
library(reshape2)
source('./R/data_prep.R')

#HQ empty files 
#HQinitialII <- read.csv("./outfiles/Headquarters_DataII.csv")
#HQinitialED <- read.csv("./outfiles/Headquarters_DataED.csv")
#HQinitialSMS <- read.csv("./outfiles/Headquarters_DataSMS.csv")
# HQ.path.df <- read.csv('./data/HQ_paths.csv',stringsAsFactors = FALSE)
# 
# 
# last.max.hq <- HQ.path.df$MaxId[nrow(HQ.path.df)]
# path.to.HQinitial <- HQ.path.df$Path[nrow(HQ.path.df)]
# HQInitialList <- readRDS(path.to.HQinitial)


#PUEBLO empty files 
#PBinitialII <- read.csv("./outfiles/Pueblo_DataII.csv",stringsAsFactors = FALSE)
#PBinitialED <- read.csv("./outfiles/Pueblo_DataED.csv",stringsAsFactors = FALSE)
#PBinitialSMS <- read.csv("./outfiles/Pueblo_DataSMS.csv",stringsAsFactors = FALSE) 

# PB.path.df <- read.csv('./data/PB_paths.csv',stringsAsFactors = FALSE)
# 
# last.max.pb <- PB.path.df$MaxId[nrow(PB.path.df)]
# path.to.PBinitial <- PB.path.df$Path[nrow(PB.path.df)]
# PBInitialList <- readRDS(path.to.PBinitial)
# 

#bluegrass empty files 
#BGinitialII <- read.csv("./outfiles/Blue_Grass_DataII.csv",stringsAsFactors = FALSE)
#BGinitialED <- read.csv("./outfiles/Blue_Grass_DataED.csv",stringsAsFactors = FALSE)
#BGinitialSMS <- read.csv("./outfiles/Blue_Grass_DataSMS.csv",stringsAsFactors = FALSE)
# 
# 
# BG.path.df <- read.csv('./data/BG_paths.csv',stringsAsFactors = FALSE)
# last.max.bg <- BG.path.df$MaxId[nrow(BG.path.df)]
# path.to.BGinitial <- BG.path.df$Path[nrow(BG.path.df)]
# BGInitialList <- readRDS(path.to.BGinitial)

