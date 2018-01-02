
BGfeedback <- read.csv("./data/FY2018 Blue Grass Feedback Data.csv",header=TRUE,strip.white = TRUE,blank.lines.skip = TRUE, stringsAsFactors = FALSE) %>%
  filter(Stand.alone.Card.or.Feedback.Form. != "")

PBfeedback <- read.csv("./data/FY2018 Pueblo Feedback Data.csv",header=TRUE,strip.white = TRUE,blank.lines.skip = TRUE,stringsAsFactors = FALSE) %>%
  filter(Stand.alone.Card.or.Feedback.Form. != "")

BGfeedback$Site = "BLUE GRASS"
PBfeedback$Site = "PUEBLO"


goodjob <- function(x){
  return(ifelse(is.na(x),x,ifelse(x <=2,1,0)))
    
}

allFeedback <- rbind(PBfeedback, BGfeedback) %>%
  select(-contains("list",ignore.case = TRUE),
         -contains("Comment",ignore.case=TRUE),-contains("Name",ignore.case=TRUE),
         -contains("Stand",ignore.case=TRUE)) %>%
  mutate(Month = month(as.Date(Date,format="%m/%d/%Y")),
         Quarter = ifelse(
           Month >= 10, "Q1",ifelse(Month <= 3, "Q2", 
         ifelse(Month >= 4 & Month < 7, "Q3", 
                ifelse(Month >=7 & Month < 10, "Q4", 0))))) %>%
  select(-Date,-Month,-Location) %>%
  mutate_at(vars(-Site,-Quarter),.funs = goodjob) %>%
  group_by(Site,Quarter) %>%
  mutate(form.count=n())
  


feedbacksite <-
  allFeedback %>% 
  group_by(Site,Quarter) %>%
  gather(value="Pos.Question",key="Question",-Quarter,-Site,-form.count) %>% 
  group_by(Site,Quarter,form.count) %>% 
  summarise(qcount=n(),pos.vals=sum(Pos.Question,na.rm=TRUE))
sitefy <- feedbacksite %>%
  ungroup() %>%
  group_by(Site) %>%
  summarize_at(vars(pos.vals,qcount,form.count),function(x){sum(x,na.rm=TRUE)})%>%
  mutate(Quarter="FYTD")
feedbackall <- bind_rows(feedbacksite,sitefy)


feedbackpeo <- feedbackall %>%
  ungroup() %>% 
  group_by(Quarter) %>%
  # select(-pct.value) %>%
  summarise_at(.vars = vars(pos.vals,qcount,form.count),funs(sum)) %>%
  mutate(Site="PEO ACWA")

peofy <- feedbackpeo %>%
  ungroup() %>% group_by(Site) %>%
  summarize_at(vars(pos.vals,qcount,form.count),funs(sum)) %>%
  mutate(Quarter="FYTD")

peoall <- bind_rows(feedbackpeo,peofy)

finalfeedback <- bind_rows(feedbackall,peoall) %>%
  ungroup() %>%
  mutate(`Percent Satisfied`=(pos.vals/qcount)*100,
         Quarter = factor(Quarter,levels = c("Q1","Q2","Q3","Q4","FYTD")),
         Site = factor(Site,levels = c("PEO ACWA","BLUE GRASS","PUEBLO"))) %>%
  select(-qcount,-pos.vals) %>%
  reshape2::melt(value.name = "Metric") %>%
  spread(Quarter,Metric,fill = 0,drop=FALSE) %>%
  merge(.,data.frame(Site=c("PEO ACWA","BLUE GRASS","PUEBLO"),ordering=1:3),all.x=T) %>%
  arrange(ordering) %>%
  mutate(Site = toupper(Site)) %>%
  filter(!is.na(Site)) %>%
  select(-ordering)
  
ungroup() %>%
mutate(pct.value=(pos.vals/qcount)*100, Quarter = factor(Quarter,levels = c("Q1","Q2","Q3","Q4"))) %>%
  #select(-qcount,-pos.vals) %>%
  reshape2::melt(value.name = "Count") %>%
  spread(Quarter,Count) %>%
  rowwise() %>%
  mutate(FYTD = sum(Q1,Q2,Q3,Q4,na.rm=TRUE)) %>% rename(Metric=variable) %>%
  reshape2::melt(value.name="test") %>%
  spread(Quarter,value) %>%
  merge(.,data.frame(Site=c("PEO ACWA","Blue Grass","Pueblo"),ordering=1:3),all.x=T) %>%
  arrange(ordering) %>% select(-ordering)
              
# #summarise(value=as.numeric(pos.vals)/as.numeric(qcount) 
# BGfeedbackforms <- feedbackall %>% 
#   # gather(key = "Site",
#   #         value = "form.count",
#   #        "Blue Grass", "Pueblo") %>%
#   spread(key=Quarter, value=form.count) #%>% 
# 
#   #aggregate(by = list("Site"), FUN=sum)
# BGfeedbackpercent <- feedbackall %>% 
#   spread(key=Quarter, value=pct.value)
# 
# 
# allFeedback %>% group_by(Quarter,Site) %>%
#   summarize(Count=)
# 
