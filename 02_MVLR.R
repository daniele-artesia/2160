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

meter.reads.raw <- meter.reads # backup

#remove exclusions
meter.reads <- meter.reads[meter.reads$excl==0,]
meter.reads <- meter.reads[!is.na(meter.reads$excl),]
meter.reads <- meter.reads[order(meter.reads$pRef),]

meter.reads$pRef <- as.numeric(as.character(meter.reads$pRef))
meter.reads <- meter.reads[order(meter.reads$pRef),]

##################### merge with properties and assign Sodwac or Bind ######################################################
require(plyr)
require(dplyr)

survey.properties <- read.csv("T:\\live\\2160 SWW PCC Sept 2016/02 Delivery/R input files/property_from_SodwacOct2016.csv")
survey.properties <- unique(survey.properties)
survey.properties$pRef <- as.numeric(as.character(survey.properties$pRef))

total.s <- unique(merge(survey.properties, meter.reads[meter.reads$watStatOnReadDate == "u",], by="pRef"))

total.s <- total.s[order(total.s$pRef, total.s$Date),]

t. <- as.numeric(as.character(unique(total.s$pRef)))

############### # calculate difference per reading ###################################

total.PHC<- total.s %>% 
  arrange(pRef, Date)%>%
  group_by(pRef)%>%
  mutate(date.dif=c(NA,diff(Date)))%>%
  mutate(reading.dif=c(NA,diff(Reading)))


# ######## Sarah's loop#####
# for (j in t.[1])
# {
#   i <- total.s[total.s$pRef==j,]
#   i <- i[!is.na(i$pRef),]
#   i <- i[order(i$Date),]
#   i$date.dif <- diff(c(NA,i$Date))
#   i$reading.dif <- diff(c(NA,i$Reading))
#   assign("total.PHC.2",i)
# }
# 
# for (j in t.[2:length(t.)])
# {
#   i <- total.s[total.s$pRef==j,]
#   i <- i[!is.na(i$pRef),]
#   i <- i[order(i$Date),]
#   i$date.dif <- diff(c(NA,i$Date))
#   i$reading.dif <- diff(c(NA,i$Reading))
#   total.PHC.2 <- rbind(total.PHC.2,i)
# }
#
# #checlk if my method is equal
#
# all.equal(total.PHC,total.PHC.2) # TRUE


######################################## calculate PHC with method 2 ###############################################################

#Create new dif reading which replaces negative diffs (likely where meter has been replaced) with meter read (likely the value acrued since new meter installed) # from previous models
total.PHC$reading.dif2 <- ifelse(total.PHC$reading.dif<0,total.PHC$Reading,total.PHC$reading.dif)

total.PHC$PHC2 <- total.PHC$reading.dif2/total.PHC$date.dif ## calculate PHC with method 2  for each reading 
total.PHC$PHC2[is.na(total.PHC$PHC2) & is.na(total.PHC$date.dif)] <- "first_reading" #assign first reading to NAs
first.readings <- total.PHC[total.PHC$PHC2 %in% "first_reading",] # select just row with first_reading note

#View(table(first.readings$pRef))
#View(total.PHC[,c(1,3,11,13, 22:25)])

#remove first reading note from dataframe in order to check for outliers
total.PHC <- total.PHC[!total.PHC$PHC2 %in% "first_reading",]
total.PHC$PHC2 <- as.numeric(total.PHC$PHC2)


########################################check outliers#####################################################
require(reshape2)
require(ggplot2)

ggplot(melt(total.PHC[,c(3,25)]), aes(surveyType, value)) + geom_boxplot()

### split into 2 dataset
blind <- subset(total.PHC, total.PHC$surveyType == "Blind")
sodwac <- subset(total.PHC, total.PHC$surveyType == "Sodwac")

#blind.outliers <- boxplot.stats(blind.PHC$PHC2)$out
#sodwac.outliers <- boxplot.stats(sodwac.PHC$PHC2)$out

### outliers detector  function ###### remove outliers with Tukey's method (outliers ranged above and below the 1.5*IQR)
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

######remove outliers for each surveyType 

blind.PHC <- blind # backups
sodwac.PHC <- sodwac # backups

outlierKD(blind.PHC, PHC2) # need to write yes to both - will put outliers to NA
outlierKD(sodwac.PHC, PHC2)

total.PHC <- unique(rbind(blind.PHC, sodwac.PHC))
total.PHC$pRef <- as.numeric(as.character(total.PHC$pRef))
total.PHC <- total.PHC[order(total.PHC$pRef, total.PHC$Date),]

t.nooutliers <- as.numeric(as.character(unique(total.PHC$pRef)))

########################## create time series and fill with PHC value for timeperiods between readings ############################################################################################
require(tidyr)
require(reshape2)
require(zoo)

t.PHC<- as.data.frame(total.PHC[,c(1,11,25)])
t.PHC <- t.PHC[complete.cases(t.PHC), ] # remove non value derived from division and property with NAs for the whole period
t.PHC <- dcast(t.PHC, Date~pRef)
t.PHC <- t.PHC[order(t.PHC$Date),]

ts<- as.data.frame(seq.Date(as.Date(min(total.PHC$Date)), as.Date(max(total.PHC$Date)), by="days"))
colnames(ts)[1] <- "Date"


ts.PHC <- left_join(ts, t.PHC) # obtain full time series
ts.PHC <- ts.PHC[order(ts.PHC$Date),]
ts.PHC[ts.PHC==0] <-NA # remove 0 reading (initial - also following Sarah's method she removed all value < 57)
ts.PHC <- (na.locf(ts.PHC, fromLast = T)) # fill with value below
ts.PHC[,-1] <- sapply(ts.PHC[,-1], as.numeric)
ts.PHC$Date <- as.POSIXct(parse_date_time(ts.PHC$Date, c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM", "dmY", "Ymd")),
                                                          format="%Y-%m-%d %H:%M:%S",tz="UTC")

#########means combined without correcting for SODWAC factor################
#write.csv(ts.PHC, "t:/live/2160 SWW PCC Sept 2016/02 Delivery/R output files/all properties averages.csv")

#split into blind and SODWAC and correct by factor###################
blind.list <- blind.PHC[,1]
blind.list$pRef <- as.character(blind.list$pRef)
blind.list <- unlist(blind.list$pRef)
blind.list <- unique(blind.list)

ts.blind.PHC <- ts.PHC[,colnames(ts.PHC) %in% blind.list]  # by matching column names
ts.sodwac.PHC <- ts.PHC[,!colnames(ts.PHC) %in% blind.list]

ts.sodwac.PHC[,-1] <- ts.sodwac.PHC[,-1]*1.015 # multiply by sodwac factor
no.bias.PHC <- cbind(ts.sodwac.PHC,ts.blind.PHC)

##########means combined with correcting for SODWAC factor###
#write.csv(no.bias.PHC, "t:/live/2160 SWW PCC Sept 2016/02 Delivery/R output files/all properties averages with correcting factor.csv")





#






















# #for PHC ################### this will not backdate everything but will put a first reading string#############################################
# t.PHC<- as.data.frame(total.PHC[,c(1,11,25)])
# t.PHC <- t.PHC[complete.cases(t.PHC), ] # remove non value derived from division and property with NAs for the whole period
# t.PHC <- dcast(t.PHC, Date~pRef)
# t.PHC <- t.PHC[order(t.PHC$Date),]
# 
# first.readings <- unique(first.readings[,c(1,11,25)])
# f.readings <- dcast(first.readings, Date~pRef)
# f.readings <- f.readings[order(f.readings$Date),]
# 
# 
#  
# # TS.PHC <- rbind.fill(t.PHC,f.readings) # bind dataframes with different length
# # TS.PHC <- TS.PHC[order(TS.PHC$Date),]
# 
# 
# ts<- as.data.frame(seq.Date(as.Date(min(total.PHC$Date)), as.Date(max(total.PHC$Date)), by="days"))
# colnames(ts)[1] <- "Date"
# 
# 
# ts.PHC <- left_join(ts, TS.PHC) # obtain full time series
# 
# ##################### fill time series with mean value (na.locf works)#########################################################
# require(zoo)
# 
# ts.PHC <- na.locf(ts.PHC, fromLast = T) # fill with value below
# ts.PHC[ts.PHC== "first_reading"] <- NA #replace first reading with NAs
# 
# write.csv(ts.PHC, "t:/live/2160 SWW PCC Sept 2016/02 Delivery/R output files/all properties averages method2.csv")


##########################sarah's method  ################################################
# New <- as.data.frame(seq.Date(as.Date(min(total.PHC$Date)), as.Date(max(total.PHC$Date)), by="days"))
# colnames(New)[1] <- "Date"
# 
# PHC.list <- as.character(unique(total.PHC$pRef))
# 
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
# 
# for (i in PHC.list)
# {
#   for (j in 1:nrow(New))
#   {
# 
#     New[j,i] <- ifelse(is.na(New[j,i]),New[(j-1),i],New[j,i])
#   }
# }
# New <- New[order(New$Date),]
# PHC2 <- New
# 
# write.csv(PHC2, "t:/live/2160 SWW PCC Sept 2016/02 Delivery/R output files/timeseries_s_method.csv")
###################################################################################################################

