#######################################read questionnaire   ############################################################################################
questionnaire <- read.csv("T:\\live\\2160 SWW PCC Sept 2016/02 Delivery/R input files/questionnaire_from_SodwacOct2016.csv")

#format data
summary(questionnaire)
str(questionnaire)

colnames(questionnaire)[3] <- "pRef"
questionnaire$pRef <- as.numeric(as.character(questionnaire$pRef))

library(lubridate)
questionnaire$customerEndMoveIn=as.POSIXct(parse_date_time(questionnaire$customerEndMoveIn, c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM", "dmY", "Ymd")),
                format="%Y-%m-%d %H:%M:%S",tz="UTC")

questionnaire<- questionnaire[order(questionnaire$pRef),]
