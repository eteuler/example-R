
cleanHQII <- function(filepath){
  
  HQIIread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""))
  HQII <- HQIIread[HQIIread$Select.which.type.of.data.to.enter. == "Individual contact",]
  emptycols <- sapply(HQII, function(x) all(is.na(x)))
  HQIIfinal <- HQII[,!emptycols]
  
}

cleanHQED <- function(filepath){
  HQEDread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""))
  HQED <- HQEDread[HQEDread$Select.which.type.of.data.to.enter. == "Event or information distribution",]
  
  HQED["Month"] <- month(as.Date(HQED$Date.of.event.or.distribution.,format="%m/%d/%Y"))
  HQED["Quarter"] <- ifelse(
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
  
  HQEDfinal <- HQED[,HQEDkeeps]
  return(HQEDfinal)
}




cleanHQSMS <- function(filepath){
  
  HQSMSread <- read.csv(filepath, header = TRUE) 
  
  HQSMS <- HQSMSread[HQSMSread$Select.which.type.of.data.to.enter. == "Social media or web",]
  
  HQSMS["Month"] <- month(as.Date(HQSMS$Date.range.of.posting.s.,format="%m/%d/%Y"))
  HQSMS["Quarter"] <- ifelse(
    HQSMS$Month >= 10, "Q1",
    ifelse(HQSMS$Month <= 3, "Q2", 
           ifelse(HQSMS$Month >= 4 & HQSMS$Month < 7, "Q3", 
                  ifelse(HQSMS$Month >=7 & HQSMS$Month < 10, "Q4", 0))))
  HQSMS[is.na(HQSMS)] <- 0
  HQSMS["Facebook.Posts"] <- HQSMS$Facebook.posts.Number.of.social.media.posting.s..
  HQSMS["Tweets"] <- HQSMS$Tweets.Number.of.social.media.posting.s..
  HQSMS["Instagram.Posts"] <- HQSMS$Instagram.posts.Number.of.social.media.posting.s..
  #HQSMS["Full.Length.Videos"] <- HQSMS$HQ.Video.Number.of.social.media.posting.s..
  HQSMS["Full.Length.Videos"] <- HQSMS$Full.length.video.Number.of.social.media.posting.s..
  HQSMS["Digital.Shorts"] <- HQSMS$Digital.shorts.Number.of.social.media.posting.s..
  #HQSMS["Digital.Communications.Distributions"] <- HQSMS$Digital.communications..Flickr..Website.updates.and.YouTube.Uploads..Number.of.social.media.posting.s..
  HQSMS["Digital.Communications.Distributions"] <- HQSMS$Digital.Communications.Update..Website.updates.and.YouTube.uploads..Number.of.social.media.posting.s..
  HQSMSkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Date.range.of.posting.s..", 
                  "Facebook.Posts", "Tweets", "Instagram.Posts", "Full.Length.Videos", "Digital.Shorts", 
                  "Digital.Communications.Distributions", "Month", "Quarter")
  
  HQSMSfinal <- HQSMS[,HQSMSkeeps]
  return(HQSMSfinal)
  
}




######################PUEBLO##################

cleanPBII <- function(filepath){
  
  PBIIread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""))
  PBII <- PBIIread[PBIIread$Select.which.type.of.data.to.enter. == "Individual contact",]
  
  emptycols <- sapply(PBII, function(x) all(is.na(x)))
  PBIIfinal <- PBII[!emptycols] 
  return(PBIIfinal)
  
}

cleanPBED <- function(filepath){
  
  PBEDread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""))
  PBED <- PBEDread[PBEDread$Select.which.type.of.data.to.enter. == "Event or distribution",]
  
  PBED["Month"] <- month(as.Date(PBED$Date.of.event.or.distribution.,format="%m/%d/%Y"))
  PBED["Quarter"] <- ifelse(
    PBED$Month >= 10, "Q1",
    ifelse(PBED$Month <= 3, "Q2", 
           ifelse(PBED$Month >= 4 & PBED$Month < 7, "Q3", 
                  ifelse(PBED$Month >=7 & PBED$Month < 10, "Q4", 0))))
  PBED[is.na(PBED)] <- 0
  PBED <- PBED[!(PBED$Response.ID==0),]
  PBEDkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Outreach.team.member..1", 
                 "Name.of.event.or.distribution.", "Date.of.event.or.distribution.", "Event.or.distribution.type.", 
                 "Other..please.specify..Event.or.distribution.type.", "Business...economic.development.group.members.Number.of.participants.",
                 "CAC.Working.Group.members.Number.of.participants.",	"Citizen.board.commission.members.Number.of.participants.", 
                 "Community.advocacy.Non.profit.Number.of.participants.",	"Elected.and.Government.appointed.officials.Number.of.participants.",
                 "Emergency.management.Number.of.participants.",	"Community.members.Number.of.participants.",	
                 "Education.community.Number.of.participants.",
                 "Media.Number.of.participants.",	"Oversight.regulatory.agency.members.Number.of.participants.",
                 "Workforce.Number.of.participants.",	"Other.Number.of.participants.",	
                 "Number.Information.packets.distributed.Complete.the.following.",	
                 "Number.Feedback.forms.distributed.Complete.the.following.",	
                 "Number.Feedback.forms.collected.Complete.the.following.",	"Month", "Quarter")
  PBEDfinal <- PBED[,PBEDkeeps]
  return(PBEDfinal)
  
}

cleanPBSMS <- function(filepath){
  
  PBSMSread <- read.csv(filepath, header = TRUE)
  PBSMS <- PBSMSread[PBSMSread$Select.which.type.of.data.to.enter. == "Social media posting submission",]
  
  PBSMS["Month"] <- month(as.Date(PBSMS$Date.range.of.submission.s..,format="%m/%d/%Y"))
  PBSMS["Quarter"] <- ifelse(
    PBSMS$Month >= 10, "Q1",
    ifelse(PBSMS$Month <= 3, "Q2", 
           ifelse(PBSMS$Month >= 4 & PBSMS$Month < 7, "Q3", 
                  ifelse(PBSMS$Month >=7 & PBSMS$Month < 10, "Q4", 0))))
  PBSMS[is.na(PBSMS)] <- 0
  PBSMS["Facebook.Posts"] <- PBSMS$Facebook.posts.Number.of.social.media.postings.submitted.
  PBSMS["Tweets"] <- PBSMS$Tweets.Number.of.social.media.postings.submitted.
  PBSMS["Full.Length.Videos"] <- PBSMS$Full.length.videos.Number.of.social.media.postings.submitted.
  PBSMS["Digital.Shorts"] <- PBSMS$Digital.shorts.Number.of.social.media.postings.submitted.
  PBSMS["Livestreaming"] <- PBSMS$Livestreaming.Number.of.social.media.postings.submitted. 
  
  PBSMSkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Date.range.of.submission.s..",
                  "Facebook.Posts", "Tweets", "Full.Length.Videos", "Digital.Shorts", "Livestreaming",
                  "Name.of.video..if.applicable..", "Month", "Quarter")
  PBSMSfinal <- PBSMS[,PBSMSkeeps]
  return(PBSMSfinal)
  
}

cleanBGII <- function(filepath){
  
  BGIIread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""))
  BGII <- BGIIread[BGIIread$Select.which.type.of.data.to.enter. == "Individual contact",]
  
  emptycols <- sapply(BGII, function(x) all(is.na(x)))
  BGIIfinal <- (BGII[!emptycols])
  return(BGIIfinal)
  
}

cleanBGED <- function(filepath){
  
  BGEDread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""))
  BGED <- BGEDread[BGEDread$Select.which.type.of.data.to.enter. == "Event or information distribution",]
  
  BGED["Month"] <- month(as.Date(BGED$Date.of.event.or.distribution.,format="%m/%d/%Y"))
  BGED["Quarter"] <- ifelse(
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
                 "Workforce.Number.of.participants.",	"Business.Number.of.participants.", 
                 "CAC.CDCAB.members.Number.of.participants.",	"Civic.church.group.members.Number.of.participants.", 
                 "CSEPP.Immediate.Reaction.Zone.residents.Number.of.participants.",
                 "Elected.officials.Number.of.participants.",	"Emergency.management.Number.of.participants.",
                 "Local.schools.Number.of.participants.",	"Media.Number.of.participants.",	
                 "Oversight.regulatory.agency.members.Number.of.participants.",
                 "CSEPP.Protective.Action.Zone.residents.Number.of.participants.",
                 "Other.Number.of.participants.",	"Number.Information.packets.distributed.Complete.the.following.", 
                 "Number.Feedback.forms.distributed.Complete.the.following.",
                 "Number.Feedback.forms.collected.Complete.the.following.",	"Month",	"Quarter")
  
  BGEDfinal <- BGED[,BGEDkeeps]
  return(BGEDfinal)
  
}

cleanBGSMS <- function(filepath){
  
  BGSMSread <- read.csv(filepath, header = TRUE, na.strings = c("NA", ""))
  
  BGSMS <- BGSMSread[BGSMSread$Select.which.type.of.data.to.enter. == "Social media submission",]
  
  BGSMS["Month"] <- month(as.Date(BGSMS$Date.range.of.submission.s..,format="%m/%d/%Y"))
  BGSMS["Quarter"] <- ifelse(
    BGSMS$Month >= 10, "Q1",
    ifelse(BGSMS$Month <= 3, "Q2", 
           ifelse(BGSMS$Month >= 4 & BGSMS$Month < 7, "Q3", 
                  ifelse(BGSMS$Month >=7 & BGSMS$Month < 10, "Q4", 0))))
  BGSMS[is.na(BGSMS)] <- 0
  BGSMS <- BGSMS[!(BGSMS$Response.ID==0),]
  BGSMS["Facebook.Posts"] <- BGSMS$Facebook.posts.Number.of.social.media.postings.submitted.
  BGSMS["Tweets"] <- BGSMS$Tweets.Number.of.social.media.postings.submitted.
  BGSMS["Full.Length.Videos"] <- BGSMS$Full.length.videos.Number.of.social.media.postings.submitted.
  BGSMS["Digital.Shorts"] <- BGSMS$Digital.shorts.Number.of.social.media.postings.submitted.
  BGSMS["Livestreaming"] <- BGSMS$Livestreaming.Number.of.social.media.postings.submitted. 
  
  BGSMSkeeps <- c("Response.ID", "Select.which.type.of.data.to.enter.", "Date.range.of.submission.s..",
                  "Facebook.Posts", "Tweets", "Full.Length.Videos", "Digital.Shorts", "Livestreaming",
                  "Name.of.video..if.applicable..", "Month", "Quarter")
  BGSMSfinal <- BGSMS[,BGSMSkeeps]
  return(BGSMSfinal)
  
}