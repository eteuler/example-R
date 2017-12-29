

###HQ empty files 
HQinitialII <- read.csv("./data/initial/Headquarters_DataII.csv")
HQinitialED <- read.csv("./data/initial/Headquarters_DataED.csv")
HQinitialSMS <- read.csv("./data/initial/Headquarters_DataSMS.csv") 

HQ.df.list <- list()
HQ.df.list$II <- HQinitialII
HQ.df.list$ED <- HQinitialED
HQ.df.list$SMS <- HQinitialSMS

HQpath <- "./data/initial/HQ_initial_FY2018.rds"
  #paste0("./data/FY2018_HQ_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".rds")
write.table(x = data.frame(Date=c(Sys.time()),Path=c(HQpath),MaxId=c(0)),
          file = "./data/HQ_paths.csv",append=FALSE,row.names=FALSE, sep=",")
saveRDS(HQ.df.list,file=HQpath)

###PUEBLO empty files 

PBinitialII <- read.csv("./data/initial/Pueblo_DataII.csv",stringsAsFactors = FALSE)
PBinitialED <- read.csv("./data/initial/Pueblo_DataED.csv",stringsAsFactors = FALSE)
PBinitialSMS <- read.csv("./data/initial/Pueblo_DataSMS.csv",stringsAsFactors = FALSE) 


PB.df.list <- list()
PB.df.list$II <- PBinitialII
PB.df.list$ED <- PBinitialED
PB.df.list$SMS <- PBinitialSMS

systime = Sys.time()
PBpath <- "./data/initial/PB_initial_FY2018.rds"
  #paste0("./data/FY2018_PB_", format(systime, "%Y_%m_%d_%H_%M_%S"), ".rds")
write.table(x = data.frame(Date=c(systime),Path=c(PBpath),MaxId=c(0)),
          file = "./data/PB_paths.csv",append=FALSE,row.names=FALSE, sep=",")
saveRDS(PB.df.list,file=PBpath)


#bluegrass empty files 
BGinitialII <- read.csv("./data/initial/Blue_Grass_DataII.csv",stringsAsFactors = FALSE)
BGinitialED <- read.csv("./data/initial/Blue_Grass_DataED.csv",stringsAsFactors = FALSE)
BGinitialSMS <- read.csv("./data/initial/Blue_Grass_DataSMS.csv",stringsAsFactors = FALSE)


BG.df.list <- list()
BG.df.list$II <- BGinitialII
BG.df.list$ED <- BGinitialED
BG.df.list$SMS <- BGinitialSMS

systime = Sys.time()
BGpath <- "./data/initial/BG_initial_FY2018.rds"
write.table(x = data.frame(Date=c(systime),Path=c(BGpath),MaxId=c(0)),
          file = "./data/BG_paths.csv",append=FALSE,row.names=FALSE, sep=",")
saveRDS(BG.df.list,file=BGpath)

