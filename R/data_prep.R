require(dplyr)
require(tidyr)
recodeEvents <- function(x){

  tmp <- recode(x,
    "CAC (or CAC subcommittee)/advisory board meeting/Public meeting"="CAC (or Sub)/Advisory Board Mtng",
    "Other (please specify)" = "Other",
    "CAC/CDCAB (or subcommittee)/advisory board meeting" = "CAC/CDCAB (or Sub)/Advisory Board Mtng"
    )
  tmp <- gsub('\\.'," ",tmp)
  #capitalize all words
  tmp <- gsub("\\b(\\w)",perl=TRUE,replacement="\\U\\1",tmp)
  #well, maybe not ALL words
  tmp <- gsub("Or","or",tmp)
  return(tmp)
}


cleanHQII <- function(filepath,prev.resp.id=0){
  
  HQIIread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""), stringsAsFactors = FALSE)
  HQII <- HQIIread[HQIIread$Select.which.type.of.data.to.enter. == "Individual contact",]
  emptycols <- sapply(HQII, function(x) all(is.na(x)))
  HQIIfinal <- HQII[,!emptycols] %>% filter(Response.ID > prev.resp.id)
  return(HQIIfinal)
}

cleanHQED <- function(filepath,prev.resp.id=0){
  HQEDread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""), stringsAsFactors = FALSE)
  HQED <- HQEDread[HQEDread$Select.which.type.of.data.to.enter. == "Event or information distribution",]
  
  HQED$Month <- month(as.Date(HQED$Date.of.event.or.distribution.,format="%m/%d/%Y"))
  HQED$Quarter <- ifelse(
    HQED$Month >= 10, "Q1",
    ifelse(HQED$Month <= 3, "Q2", 
           ifelse(HQED$Month >= 4 & HQED$Month < 7, "Q3", 
                  ifelse(HQED$Month >=7 & HQED$Month < 10, "Q4", 0))))
  HQED[is.na(HQED)] <- 0
  HQED <- HQED[!(HQED$Response.ID==0),]
  #HQED[HQED$Event.or.distribution.type == "peo acwa information booth",]$Event.or.distribution.type <- "PEO ACWA information booth" 
  HQEDkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Outreach.team.member..1", 
                 "Name.of.event.or.distribution.", "Date.of.event.or.distribution.", "Event.or.distribution.type.", 
                 #"Other..please.specify...Event.or.distribution.type.",
                 "Army.DoD.partners..includes.CMA..AASC..Depot.commanders..Number.of.participants.", 
                 "Citizens..Advisory.Commission.members.Number.of.participants.",	"Congress.and.congressional.staffers.Number.of.participants.",
                 "Emergency.Management..e.g..CSEPP..FEMA..etc...Number.of.participants.",	"International.community.members.Number.of.participants.",
                 "Local.community.members.Number.of.participants.",	"Local.elected.officials.Number.of.participants.",
                 "Media.Number.of.participants.",	"Oversight.agency.members.Number.of.participants.",
                 "PEO.ACWA.Workforce.members.Number.of.participants.", "Regulatory.agency.members.Number.of.participants.",
                 "Other.Number.of.participants.", "Month",	"Quarter")
  
  HQEDfinal <- HQED[,HQEDkeeps] %>% filter(Response.ID > prev.resp.id)
  return(HQEDfinal)
}




cleanHQSMS <- function(filepath,prev.resp.id=0){
  
  HQSMSread <- read.csv(filepath, header = TRUE, stringsAsFactors = FALSE) %>%
    filter(Response.ID > prev.resp.id)
  
  HQSMS <- HQSMSread[HQSMSread$Select.which.type.of.data.to.enter. == "Social media or web",]
  
  HQSMS$Month <- month(as.Date(HQSMS$Date.range.of.posting.s.,format="%m/%d/%Y"))
  HQSMS$Quarter <- ifelse(
    HQSMS$Month >= 10, "Q1",
    ifelse(HQSMS$Month <= 3, "Q2", 
           ifelse(HQSMS$Month >= 4 & HQSMS$Month < 7, "Q3", 
                  ifelse(HQSMS$Month >=7 & HQSMS$Month < 10, "Q4", 0))))
  HQSMS[is.na(HQSMS)] <- 0
  HQSMS$Facebook.Posts <- HQSMS$Facebook.posts.Number.of.social.media.posting.s..
  HQSMS$Tweets <- HQSMS$Tweets.Number.of.social.media.posting.s..
  HQSMS$Instagram.Posts <- HQSMS$Instagram.posts.Number.of.social.media.posting.s..
  #HQSMS["Full.Length.Videos"] <- HQSMS$HQ.Video.Number.of.social.media.posting.s..
  HQSMS$Full.Length.Videos <- HQSMS$Full.length.video.Number.of.social.media.posting.s..
  HQSMS$Digital.Shorts <- HQSMS$Digital.shorts.Number.of.social.media.posting.s..
  #HQSMS["Digital.Communications.Distributions"] <- HQSMS$Digital.communications..Flickr..Website.updates.and.YouTube.Uploads..Number.of.social.media.posting.s..
  HQSMS$Digital.Communications.Distributions <- HQSMS$Digital.Communications.Update..Website.updates.and.YouTube.uploads..Number.of.social.media.posting.s..
  HQSMSkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Date.range.of.posting.s..", 
                  "Facebook.Posts", "Tweets", "Instagram.Posts", "Full.Length.Videos", "Digital.Shorts", 
                  "Digital.Communications.Distributions", "Month", "Quarter")
  
  HQSMSfinal <- HQSMS[,HQSMSkeeps] %>% filter(Response.ID > prev.resp.id)
  return(HQSMSfinal)
  
}




######################PUEBLO##################

cleanPBII <- function(filepath, prev.resp.id=0){
  
  PBIIread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""), stringsAsFactors = FALSE) %>%
    filter(Response.ID > prev.resp.id)
  PBII <- PBIIread[PBIIread$Select.which.type.of.data.to.enter. == "Individual contact",]
  
  emptycols <- sapply(PBII, function(x) all(is.na(x)))
  PBIIfinal <- PBII[!emptycols] %>% filter(Response.ID > prev.resp.id)
  return(PBIIfinal)
  
}

cleanPBED <- function(filepath, prev.resp.id=0){
  
  PBEDread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""), stringsAsFactors = FALSE) %>%
    filter(Response.ID > prev.resp.id)
  PBED <- PBEDread[PBEDread$Select.which.type.of.data.to.enter. == "Event or distribution",]
  
  PBED$Month <- month(as.Date(PBED$Date.of.event.or.distribution.,format="%m/%d/%Y"))
  PBED$Quarter <- ifelse(
    PBED$Month >= 10, "Q1",
    ifelse(PBED$Month <= 3, "Q2", 
           ifelse(PBED$Month >= 4 & PBED$Month < 7, "Q3", 
                  ifelse(PBED$Month >=7 & PBED$Month < 10, "Q4", 0))))
  PBED[is.na(PBED)] <- 0
  PBED <- PBED[!(PBED$Response.ID==0),]
  #Below changed for 2018 data!!!!
  PBEDkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Outreach.team.member..1", 
                 "Name.of.event.or.distribution.", "Date.of.event.or.distribution.", "Event.or.distribution.type.", 
                 "Other..please.specify..Event.or.distribution.type.", 
                 #"Business...economic.development.group.members.Number.of.participants.",
                 "CAC.Working.Group.members.Number.of.participants.",
                 "Education.community.Number.of.participants.",
                 "Elected.and.Government.appointed.officials.Number.of.participants.",
                 "Emergency.management.Number.of.participants.",
                 "International.community.Number.of.participants.", 
                 "Local.national.community.members.Number.of.participants.",
                 #"Citizen.board.commission.members.Number.of.participants.", 
                 #"Community.advocacy.Non.profit.Number.of.participants.",
                 "Media.Number.of.participants.",
                 "Oversight.regulatory.agency.members.Number.of.participants.",
                 "Workforce.Number.of.participants.",	"Other.Number.of.participants.",
              	 "Business.Number.of.participants.",
                 "Advisory.Board.Number.of.participants.",
                 "Civic.Organization.Number.of.participants.",
                 "Number.Information.packets.distributed.Complete.the.following.",	
                 "Number.Feedback.forms.distributed.Complete.the.following.",	
                 "Number.Feedback.forms.collected.Complete.the.following.",	"Month", "Quarter")
  PBEDfinal <- PBED[,PBEDkeeps] %>% filter(Response.ID > prev.resp.id)
  return(PBEDfinal)
  
}

cleanPBSMS <- function(filepath,prev.resp.id=0){
  
  PBSMSread <- read.csv(filepath, header = TRUE, stringsAsFactors = FALSE) %>%
    filter(Response.ID > prev.resp.id)
  PBSMS <- PBSMSread[PBSMSread$Select.which.type.of.data.to.enter. == "Social media posting submission",]
  
  PBSMS$Month <- month(as.Date(PBSMS$Date.range.of.submission.s..,format="%m/%d/%Y"))
  PBSMS$Quarter <- ifelse(
    PBSMS$Month >= 10, "Q1",
    ifelse(PBSMS$Month <= 3, "Q2", 
           ifelse(PBSMS$Month >= 4 & PBSMS$Month < 7, "Q3", 
                  ifelse(PBSMS$Month >=7 & PBSMS$Month < 10, "Q4", 0))))
  PBSMS[is.na(PBSMS)] <- 0
  PBSMS$Facebook.Posts <- PBSMS$Facebook.posts.Number.of.social.media.postings.submitted.
  PBSMS$Tweets <- PBSMS$Tweets.Number.of.social.media.postings.submitted.
  PBSMS$Instagram.Posts <- PBSMS$Instagram.Number.of.social.media.postings.submitted.
  PBSMS$Full.Length.Videos <- PBSMS$Full.length.videos.Number.of.social.media.postings.submitted.
  PBSMS$Digital.Shorts <- PBSMS$Digital.shorts.Number.of.social.media.postings.submitted.
  PBSMS$Livestreaming <- PBSMS$Livestreaming.Number.of.social.media.postings.submitted. 
  
  PBSMSkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Date.range.of.submission.s..",
                  "Facebook.Posts", "Tweets", "Instagram.Posts", "Full.Length.Videos", "Digital.Shorts", "Livestreaming",
                  "Name.of.video..if.applicable..", "Month", "Quarter")
  PBSMSfinal <- PBSMS[,PBSMSkeeps] %>% filter(Response.ID > prev.resp.id)
  return(PBSMSfinal)
  
}

cleanBGII <- function(filepath, prev.resp.id=0){
  
  BGIIread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""), stringsAsFactors = FALSE) %>%
    filter(Response.ID > prev.resp.id)
  BGII <- BGIIread[BGIIread$Select.which.type.of.data.to.enter. == "Individual contact",]
  
  emptycols <- sapply(BGII, function(x) all(is.na(x)))
  BGIIfinal <- BGII[,!emptycols] %>% filter(Response.ID > prev.resp.id)
  return(BGIIfinal)
  
}

cleanBGED <- function(filepath, prev.resp.id=0){
  
  BGEDread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""), stringsAsFactors = FALSE)  %>%
    filter(Response.ID > prev.resp.id)
  BGED <- BGEDread[BGEDread$Select.which.type.of.data.to.enter. == "Event or information distribution",]
  
  BGED$Month <- month(as.Date(BGED$Date.of.event.or.distribution.,format="%m/%d/%Y"))
  BGED$Quarter <- ifelse(
    BGED$Month >= 10, "Q1",
    ifelse(BGED$Month <= 3, "Q2", 
           ifelse(BGED$Month >= 4 & BGED$Month < 7, "Q3", 
                  ifelse(BGED$Month >=7 & BGED$Month < 10, "Q4", 0))))
  BGED[is.na(BGED)] <- 0
  BGED <- BGED[!(BGED$Response.ID==0),] 
  BGEDkeeps <- c("Response.ID",	"Select.which.type.of.data.to.enter.",
                 "Outreach.team.member..1",	"Name.of.event.or.distribution.", 
                 "Date.of.event.or.distribution.",	"Event.or.distribution.type.",
                 "Other..please.specify..Event.or.distribution.type.",
                 "BGCAPP.Workforce.Number.of.participants.",
                 "Business.Number.of.participants.", 
                 "CAC.CDCAB.members.Number.of.participants.",	"Civic.church.group.members.Number.of.participants.", 
                 "CSEPP.Immediate.Reaction.Zone.residents.Number.of.participants.",
                 "CSEPP.Protective.Action.Zone.residents.Number.of.participants.",
                 "Elected.officials.Number.of.participants.",	"Emergency.management.Number.of.participants.",
                 "Local.schools.Number.of.participants.",	"Media.Number.of.participants.",	
                 "Oversight.regulatory.agency.members.Number.of.participants.",
                 "Other.Number.of.participants.",	"Number.Information.packets.distributed.Complete.the.following.", 
                 "Number.Feedback.forms.distributed.Complete.the.following.",
                 "Number.Feedback.forms.collected.Complete.the.following.",	"Month",	"Quarter")
  
  BGEDfinal <- BGED[,BGEDkeeps] %>% filter(Response.ID > prev.resp.id)
  return(BGEDfinal)
  
}

cleanBGSMS <- function(filepath, prev.resp.id=0){
  
  BGSMSread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""), stringsAsFactors = FALSE)  %>%
    filter(Response.ID > prev.resp.id)
  
  BGSMS <- BGSMSread[BGSMSread$Select.which.type.of.data.to.enter. == "Social media submission",]
  
  BGSMS$Month <- month(as.Date(BGSMS$Date.range.of.submission.s..,format="%m/%d/%Y"))
  BGSMS$Quarter <- ifelse(
    BGSMS$Month >= 10, "Q1",
    ifelse(BGSMS$Month <= 3, "Q2", 
           ifelse(BGSMS$Month >= 4 & BGSMS$Month < 7, "Q3", 
                  ifelse(BGSMS$Month >=7 & BGSMS$Month < 10, "Q4", 0))))
  BGSMS[is.na(BGSMS)] <- 0
  BGSMS <- BGSMS[!(BGSMS$Response.ID==0),]
  BGSMS$Facebook.Posts <- BGSMS$Facebook.posts.Number.of.social.media.postings.submitted.
  BGSMS$Tweets <- BGSMS$Tweets.Number.of.social.media.postings.submitted.
  BGSMS$Instagram.Posts <- BGSMS$Instagram.Number.of.social.media.postings.submitted.
  BGSMS$Full.Length.Videos <- BGSMS$Full.length.videos.Number.of.social.media.postings.submitted.
  BGSMS$Digital.Shorts <- BGSMS$Digital.shorts.Number.of.social.media.postings.submitted.
  BGSMS$Livestreaming <- BGSMS$Livestreaming.Number.of.social.media.postings.submitted. 
  BGSMS$Flickr <- BGSMS$Flickr.updates.Number.of.social.media.postings.submitted.
  BGSMSkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Date.range.of.submission.s..",
                  "Facebook.Posts", "Tweets", "Instagram.Posts", "Full.Length.Videos", "Digital.Shorts", "Livestreaming",
                  "Flickr","Name.of.video..if.applicable..", "Month", "Quarter")
  BGSMSfinal <- BGSMS[,BGSMSkeeps] %>% filter(Response.ID > prev.resp.id)
  return(BGSMSfinal)
  
}

cleanFeedback <- function(fb_df){

  if (is.null(fb_df))
    return(NULL)
  tmp <- fb_df %>% select(-contains("list",ignore.case = TRUE),
         -contains("Comment",ignore.case=TRUE),-contains("Name",ignore.case=TRUE),
         -contains("Stand",ignore.case=TRUE)) %>%
    mutate(Month = month(as.Date(Date,format="%m/%d/%Y")),
           Quarter = ifelse(
             Month >= 10, "Q1",ifelse(Month <= 3, "Q2", 
                                      ifelse(Month >= 4 & Month < 7, "Q3", 
                                             ifelse(Month >=7 & Month < 10, "Q4", 0))))) %>%
    select(-Date,-Month,-Location)
  return(tmp)
}

prepareFeedback <- function(fb_df){

  goodjob <- function(x){
    return(ifelse(is.na(x),x,ifelse(x <=2,1,0)))
    
  }
  
  feedbacksite <- fb_df %>%
    mutate_at(vars(-Site,-Quarter),.funs = goodjob) %>%
    group_by(Site,Quarter) %>%
    mutate(`Form Count`=n()) %>% 
    gather(value="Pos.Question",key="Question",-Quarter,-Site,-`Form Count`) %>% 
    group_by(Site,Quarter,`Form Count`) %>% 
    summarise(qcount=n(),pos.vals=sum(Pos.Question,na.rm=TRUE))
  
  sitefy <- feedbacksite %>%
    ungroup() %>%
    group_by(Site) %>%
    summarize_at(vars(pos.vals,qcount,`Form Count`),function(x){sum(x,na.rm=TRUE)}) %>%
    mutate(Quarter="FYTD")
  
  feedbackall <- bind_rows(feedbacksite,sitefy)
  
  feedbackpeo <- feedbacksite %>%
    ungroup() %>% 
    group_by(Quarter) %>%
    summarise_at(.vars = vars(pos.vals,qcount,`Form Count`),function(x){sum(x,na.rm=TRUE)}) %>%
    mutate(Site="PEO ACWA")
  
  peofy <- feedbackpeo %>%
    ungroup() %>% group_by(Site) %>%
    summarize_at(vars(pos.vals,qcount,`Form Count`),function(x){sum(x,na.rm=TRUE)}) %>%
    mutate(Quarter="FYTD")
  print(peofy)
  
  peoall <- bind_rows(feedbackpeo,peofy)
  print(peoall)
  finalfeedback <- bind_rows(feedbackall,peoall) %>%
    ungroup() %>%
    mutate(`Percent Satisfied`=(pos.vals/qcount)*100,
           Quarter = factor(Quarter,levels = c("Q1","Q2","Q3","Q4","FYTD")),
           Site = factor(Site,levels = c("PEO ACWA","BLUE GRASS","PUEBLO"))) %>%
    select(-qcount,-pos.vals) %>%
    reshape2::melt(value.name = "Metric") %>%
    spread(Quarter,Metric,fill = 0,drop=FALSE) %>%
    #merge(.,data.frame(Site=c("PEO ACWA","BLUE GRASS","PUEBLO"),ordering=1:3),all.x=T) %>%
    #arrange(ordering) %>%
    #select(-ordering) %>%
    filter(!is.na(Site))

  return(finalfeedback)
}