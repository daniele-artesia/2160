#######################################  load and format questionnaire   ############################################################################################
questionnaire <- read.csv("T:\\live\\2160 SWW PCC Sept 2016/02 Delivery/R input files/questionnaire_from_SodwacOct2016.csv")

###############format data#############
summary(questionnaire)
str(questionnaire)

#rename column
colnames(questionnaire)[3] <- "pRef"
questionnaire$pRef <- as.numeric(as.character(questionnaire$pRef))
questionnaire<- questionnaire[order(questionnaire$pRef),]


#set date
library(lubridate)
questionnaire$customerEndMoveIn=as.POSIXct(parse_date_time(questionnaire$customerEndMoveIn, c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM", "dmY", "Ymd")),
                format="%Y-%m-%d %H:%M:%S",tz="UTC")

#set categorical as factors
categorical <- c(13, 29,30,31,34,42,49,50,56,59,63,68,69,72,78)
questionnaire[categorical] <- lapply(questionnaire[categorical], factor)
questionnaire<- questionnaire[order(questionnaire$pRef),]

#check customer in / customer out
questionnaire$sameCustomer <- questionnaire$customerStart == questionnaire$customerEnd

diff.customer <- subset(questionnaire, questionnaire$sameCustomer == FALSE)
diff.customer<- diff.customer[order(diff.customer$pRef),]

#select properties were customer has not moved since the begininng of the year
same.customer <- subset(questionnaire,questionnaire$sameCustomer == TRUE) 
same.customer<- same.customer[order(same.customer$pRef),]



#######################################  meter reads   ############################################################################################
meter.reads <- read.csv("T:\\live\\2160 SWW PCC Sept 2016/02 Delivery/R input files/meterreads_from_SodwacOct2016.csv")

#prepare data
meter.reads$pRef <- as.numeric(as.character(meter.reads$pRef))
meter.reads <- meter.reads[order(meter.reads$pRef),]

mr. <- as.character(unique(meter.reads$pRef)) 
mr. <- mr.[order(mr.)]

meter.reads$Date <- as.Date(meter.reads$Date, format="%d/%m/%Y")
meter.reads$Reading <- as.numeric(as.character(meter.reads$Reading))
meter.reads$year <- as.POSIXlt(meter.reads$Date)$year+1900
meter.reads$month <- as.POSIXlt(meter.reads$Date)$mon+1           

#remove flagged readings [ excluded ]
meter.reads <- meter.reads[order(meter.reads$pRef, meter.reads$Date),]
colnames(meter.reads)[7] <- "Flag"  ######### changed column name to use Sarah's code
table(meter.reads$Flag)
meter.reads$flag <- ifelse(meter.reads$Flag %in% c("cerr","err","err ","err - Meter Services agent problems","errr","excl"),"Excl",NA)
meter.reads$flag <- ifelse(meter.reads$Flag %in% c("c"),"Changed",meter.reads$flag)
meter.reads$flag <- ifelse(meter.reads$Flag %in% c("leak","Leak","leak - retro","leak now fixed?","leak?","leak?  "),"Leak",meter.reads$flag)
table(meter.reads$flag,meter.reads$Flag)

meter.reads$excl <- ifelse(meter.reads$flag=="Excl",1,0)
meter.reads$excl <- ifelse(is.na(meter.reads$flag),0,meter.reads$excl)

meter.reads.raw <- meter.reads

#remove exclusions
meter.reads <- meter.reads[meter.reads$excl==0,]
meter.reads <- meter.reads[!is.na(meter.reads$excl),]
meter.reads <- meter.reads[order(meter.reads$pRef),]

#remove reading prior 2005 (questionnaire dates back to 2015 only) & meter read from 2015 as they are wrong according to previous model
meter.reads <- subset(meter.reads, meter.reads$Date >= "2005-01-01" & meter.reads$Date < "2015-01-01")


##################### merge with properties and assign Sodwac or Bind ######################################################

survey.properties <- read.csv("T:\\live\\2160 SWW PCC Sept 2016/02 Delivery/R input files/property_from_SodwacOct2016.csv")
survey.properties <- unique(survey.properties)
survey.properties$pRef <- as.numeric(as.character(survey.properties$pRef))

total.s <- merge(survey.properties, meter.reads[meter.reads$watStatOnReadDate == "u",], by="pRef")

# calculate difference per reading
total.s <- total.s[order(total.s$pRef, total.s$Date),]
t. <- as.numeric(as.character(unique(meter.reads$pRef)))

############### for total property without discrimination by zone######
for (j in t.[1])
{
  i <- total.s[total.s$pRef==j,]
  i <- i[!is.na(i$pRef),]
  i <- i[order(i$Date),]
  i$date.dif <- diff(c(NA,i$Date))
  i$reading.dif <- diff(c(NA,i$Reading))
  assign("total.PHC",i)
}

for (j in t.[2:length(t.)])
{
  i <- total.s[total.s$pRef==j,]
  i <- i[!is.na(i$pRef),]
  i <- i[order(i$Date),]
  i$date.dif <- diff(c(NA,i$Date))
  i$reading.dif <- diff(c(NA,i$Reading))
  total.PHC <- rbind(total.PHC,i)
}

#Create new dif reading which replaces negative diffs (likely where meter has been replaced) with meter read (likely the value acrued since new meter installed)
total.PHC$reading.dif2 <- ifelse(total.PHC$reading.dif<0,total.PHC$Reading,total.PHC$reading.dif)

## calculate PHC with method 2  fro each reading 
total.PHC$PHC2 <- total.PHC$reading.dif2/total.PHC$date.dif 
View(total.PHC[,c(1, 3,13, 23, 25)])

########################### create time series ############################################################################################
require("xts")
new <- xts(total.PHC, as.POSIXct(total.PHC[,4], format="'%Y-%m-%d"))
new.2 <- new[,c(1,18)]

new.3 <- subset(x = new.2, !is.na(new.3[,2]== F))

#apply.daily(new.2[,2], mean) # to check










#sarah's method
# New <- as.data.frame(seq.Date(as.Date(min(total.PHC$Date)), as.Date(max(total.PHC$Date)), by="days"))
# colnames(New)[1] <- "Date"
# 
# PHC.list <- as.character(unique(total.PHC$pRef))
# 
# for (i in PHC.list)
# {
#   j <- i
#   i <- total.PHC[total.PHC$pRef==i,]
#   i <- i[,c("Date","PHC2")]
#   colnames(i)[2] <- j
#   New<-merge(New,i,by="Date", all.x=T)
# }
# 
# New <- New[order(rev(New$Date)),]
# for (i in PHC.list)
# { 
#   for (j in 1:nrow(New))
#   {
#     
#     New[j,i] <- ifelse(is.na(New[j,i]),New[(j-1),i],New[j,i])
#   }
# }
# New <- New[order(New$Date),]
# 
