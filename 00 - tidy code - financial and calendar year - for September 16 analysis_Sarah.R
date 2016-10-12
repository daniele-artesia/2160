##########
#new data PCC exclusion
rm(list=ls())

###NEW
survey.properties <- read.csv("T:\\live\\2140 SWW PCC Apr 2016\\02 Delivery\\Data in\\surveyProperties - updated.csv")
meter.reads <- read.csv("T:\\live\\2140 SWW PCC Apr 2016\\02 Delivery\\Data in\\meter read - updated.csv")
#questionnaire.occupants <- read.csv("T:\\live\\2133 SWW PCC Jan 2016\\02 Delivery\\Data in\\questionnaireOccupants.csv")

#prep survey data; 
#format
survey.properties$pRef <- as.numeric(as.character(survey.properties$pRef))
#sort
survey.properties <- survey.properties[order(survey.properties$pRef),]
survey.properties2 <-survey.properties[!duplicated(survey.properties),]
#prep meter reads; 
#format
meter.reads$pRef <- as.numeric(as.character(meter.reads$pRef))
#sort
meter.reads <- meter.reads[order(meter.reads$pRef),]

##Not needed - as of Apr 2016
# 
# #prep number of occupants; 
# #format,  
# questionnaire.occupants$pRef <- as.numeric(as.character(questionnaire.occupants$pRef))
# #remove missing, 
# questionnaire.occupants <- questionnaire.occupants[!is.na(questionnaire.occupants$pRef),]
# questionnaire.occupants <- questionnaire.occupants[questionnaire.occupants$num.all_persons!="NULL" & questionnaire.occupants$num.all_persons!=0,]
# #sort by date, 
# questionnaire.occupants <- questionnaire.occupants[order(questionnaire.occupants$pRef,-(questionnaire.occupants$Year)),]
# #and then dedupe.
# questionnaire.occupants <- questionnaire.occupants[!duplicated(questionnaire.occupants$pRef,questionnaire.occupants$Year),] 


# str(meter.reads$pRef)
# str(survey.properties$pRef)
# str(questionnaire.occupants$pRef)

#meter.reads$pRef <- as.numeric(meter.reads$pRef)
#survey.properties$pRef <- as.numeric(survey.properties$pRef)
#questionnaire.occupants$pRef <- as.numeric(as.character(questionnaire.occupants$pRef))

mr. <- as.character(unique(meter.reads$pRef))
sp.sodwac <- as.character(unique(survey.properties$pRef[survey.properties$surveyType=="Sodwac" & survey.properties$watStatEndApril15=="u"]))
sp.blind <- as.character(unique(survey.properties$pRef[survey.properties$surveyType=="Blind" & survey.properties$watStatEndApril15=="u"]))

mr. <- mr.[order(mr.)]

#Format
meter.reads$Date <- as.Date(meter.reads$Date, format="%d/%m/%Y")
meter.reads$Reading <- as.numeric(as.character(meter.reads$Reading))
meter.reads$year <- as.POSIXlt(meter.reads$Date)$year+1900
meter.reads$month <- as.POSIXlt(meter.reads$Date)$mon+1

meter.reads <- meter.reads[order(meter.reads$pRef, meter.reads$Date),]
table(meter.reads$Flag)
meter.reads$flag <- ifelse(meter.reads$Flag %in% c("cerr","err","err ","err - Meter Services agent problems","errr","excl"),"Excl",NA)
meter.reads$flag <- ifelse(meter.reads$Flag %in% c("c"),"Changed",meter.reads$flag)
meter.reads$flag <- ifelse(meter.reads$Flag %in% c("leak","Leak","leak - retro","leak now fixed?","leak?","leak?  "),"Leak",meter.reads$flag)
table(meter.reads$flag,meter.reads$Flag)

meter.reads$excl <- ifelse(meter.reads$flag=="Excl",1,0)
meter.reads$excl <- ifelse(is.na(meter.reads$flag),0,meter.reads$excl)

meter.reads.raw <- meter.reads

meter.reads <- meter.reads[meter.reads$excl==0,]
meter.reads <- meter.reads[!is.na(meter.reads$excl),]

blind <- merge(survey.properties2[survey.properties2$watStat=="u"&survey.properties2$surveyCode=="Blind",],meter.reads, by="pRef")
sodwac <- merge(survey.properties2[survey.properties2$watStat=="u"&survey.properties2$surveyCode=="Sodwac",],meter.reads, by="pRef")

b. <- as.numeric(as.character(unique(blind$pRef))) # 435
s. <- as.numeric(as.character(unique(sodwac$pRef))) # 286

blind <- blind[order(blind$pRef, blind$Date),]
sodwac <- sodwac[order(sodwac$pRef, sodwac$Date),]


str(meter.reads$excl)
# calculate difference in date and reading between meter reads
#BLIND
for (j in b.[1])
{
  i <- meter.reads[meter.reads$pRef==j,]
  i <- i[!is.na(i$pRef),]
  i <- i[order(i$Date),]
  i$date.dif <- diff(c(NA,i$Date))
  i$reading.dif <- diff(c(NA,i$Reading))
  assign("blind.PHC",i)
}


for (j in b.[2:length(b.)])
{
  i <- meter.reads[meter.reads$pRef==j,]
  i <- i[!is.na(i$pRef),]
  i <- i[order(i$Date),]
  i$date.dif <- diff(c(NA,i$Date))
  i$reading.dif <- diff(c(NA,i$Reading))
  blind.PHC <- rbind(blind.PHC,i)
}
#SODWAC
for (j in s.[1])
{
  i <- meter.reads[meter.reads$pRef==j,]
  i <- i[!is.na(i$pRef),]
  i <- i[order(i$Date),]
  i$date.dif <- diff(c(NA,i$Date))
  i$reading.dif <- diff(c(NA,i$Reading))
  assign("sodwac.PHC",i)
}


for (j in s.[2:length(s.)])
{
  i <- meter.reads[meter.reads$pRef==j,]
  i <- i[!is.na(i$pRef),]
  i <- i[order(i$Date),]
  i$date.dif <- diff(c(NA,i$Date))
  i$reading.dif <- diff(c(NA,i$Reading))
  sodwac.PHC <- rbind(sodwac.PHC,i)
}
# Create new dif reading which replaces negative diffs (likely where meter has been replaced) with meter read (likely the value acrued since new meter installed)
blind.PHC$reading.dif2 <- ifelse(blind.PHC$reading.dif<0,blind.PHC$Reading,blind.PHC$reading.dif)
sodwac.PHC$reading.dif2 <- ifelse(sodwac.PHC$reading.dif<0,sodwac.PHC$Reading,sodwac.PHC$reading.dif)

# Calc PHC using standard(PHC1) or replacement of negative(PHC2) per meter read for SODWAC and BLIND
blind.PHC$PHC1 <- blind.PHC$reading.dif/blind.PHC$date.dif
blind.PHC$PHC2 <- blind.PHC$reading.dif2/blind.PHC$date.dif 
sodwac.PHC$PHC1 <- sodwac.PHC$reading.dif/sodwac.PHC$date.dif
sodwac.PHC$PHC2 <- sodwac.PHC$reading.dif2/sodwac.PHC$date.dif 

#Add flag to discriminate when combined
sodwac.PHC$survey <- "sodwac"
blind.PHC$survey <- "blind"

#Create PCC for SODWAC by merge on and dividing by number of persons
occs <- survey.properties2[,c("pRef","occsInOccYear","occYear")]
occs <- occs[!duplicated(c(occs$pRef,occs$occYear)),]

sodwac.PCC <- merge(sodwac.PHC,occs[,c("pRef","occsInOccYear")], by="pRef",all.x=T)

sodwac.PCC$occsInOccYear <- as.numeric(as.character(sodwac.PCC$occsInOccYear))
# questionnaire.occupants <- questionnaire.occupants[questionnaire.occupants$num.all_persons!="NULL" & questionnaire.occupants$num.all_persons!=0,]
sodwac.PCC <- sodwac.PCC[sodwac.PCC$occsInOccYear!=0,]
sodwac.PCC <- sodwac.PCC[!is.na(sodwac.PCC$occsInOccYear),]

sodwac.PCC$PCC1 <- sodwac.PCC$PHC1/sodwac.PCC$occsInOccYear
sodwac.PCC$PCC2 <- sodwac.PCC$PHC2/sodwac.PCC$occsInOccYear


comb.PHC <- rbind(blind.PHC,sodwac.PHC)

PHC.excl <- 57
length(comb.PHC$PHC1[comb.PHC$PHC1<=PHC.excl& comb.PHC$PHC1>boxplot(comb.PHC$PHC1)$stats[1,1]]) #797
length(comb.PHC$PHC1[comb.PHC$PHC1<=boxplot(comb.PHC$PHC1)$stats[5,1]& comb.PHC$PHC1>812]) #1518
comb.PHC$outlier.PHC1 <- ifelse(comb.PHC$PHC1<=812 &comb.PHC$PHC1>PHC.excl ,0,1)
sodwac.PCC$outlier.PHC1 <- ifelse(sodwac.PCC$PHC1<=812 &sodwac.PCC$PHC1>PHC.excl ,0,1)

length(comb.PHC$PHC2[comb.PHC$PHC2<PHC.excl]) #823
length(comb.PHC$PHC2[comb.PHC$PHC2<boxplot(comb.PHC$PHC2)$stats[5,1] & comb.PHC$PHC2>764]) #823
comb.PHC$outlier.PHC2 <- ifelse(comb.PHC$PHC2<=765 &comb.PHC$PHC2>PHC.excl ,0,1)
sodwac.PCC$outlier.PHC2 <- ifelse(sodwac.PCC$PHC2<=765 &sodwac.PCC$PHC2>PHC.excl ,0,1)



###PCC
PCC.excl <- 20
length(sodwac.PCC$PCC1[sodwac.PCC$PCC1<=PCC.excl & sodwac.PCC$PCC1>=boxplot(sodwac.PCC$PCC1)$stats[1,1]]) #307
length(sodwac.PCC$PCC1[sodwac.PCC$PCC1<=boxplot(sodwac.PCC$PCC1)$stats[5,1] & sodwac.PCC$PCC1>=302]) #307
sodwac.PCC$outlier.PCC1<- ifelse(sodwac.PCC$PCC1<338 & sodwac.PCC$PCC1>PCC.excl ,0,1)


length(sodwac.PCC$PCC2[sodwac.PCC$PCC2<PCC.excl]) #319
length(sodwac.PCC$PCC2[sodwac.PCC$PCC2<=boxplot(sodwac.PCC$PCC2)$stats[5,1] & sodwac.PCC$PCC2>280]) # 318
sodwac.PCC$outlier.PCC2<- ifelse(sodwac.PCC$PCC2<=334 & sodwac.PCC$PCC2>PCC.excl ,0,1)

###########################

test <- sodwac.PCC

###########################
comb.PHC$outlier.PHC1.PCC <- ifelse(comb.PHC$PHC1<=max(sodwac.PCC$PHC1[sodwac.PCC$outlier.PCC1==0],na.rm=T) &comb.PHC$PHC1>min(sodwac.PCC$PHC1[sodwac.PCC$outlier.PCC1==0],na.rm=T) ,0,1)
comb.PHC$outlier.PHC2.PCC <- ifelse(comb.PHC$PHC2<=max(sodwac.PCC$PHC2[sodwac.PCC$outlier.PCC2==0],na.rm=T) &comb.PHC$PHC2>min(sodwac.PCC$PHC2[sodwac.PCC$outlier.PCC2==0],na.rm=T) ,0,1)

# hist(sodwac.occ$num.all_persons,breaks=20)
# hist(sodwac.occ$num.all_persons,breaks=20)

PHC.list <- as.character(unique(comb.PHC$pRef))
PCC.list <- as.character(unique(sodwac.PCC$pRef))

as.Date(max(comb.PHC$Date))

#create full time series from start date to end-2015 with PHC/PCC applied with method 1 and method 2
New <- as.data.frame(seq.Date(as.Date(min(comb.PHC$Date)), as.Date(max(comb.PHC$Date)), by="days"))
colnames(New)[1] <- "Date"

for (i in PHC.list)
{
  j <- i
  i <- comb.PHC[comb.PHC$pRef==i & comb.PHC$outlier.PHC1.PCC==0,]
  i <- i[,c("Date","PHC1")]
  colnames(i)[2] <- j
  New<-merge(New,i,by="Date", all.x=T)
}

New <- New[order(rev(New$Date)),]
for (i in PHC.list)
{ 
  for (j in 1:nrow(New))
  {
    
    New[j,i] <- ifelse(is.na(New[j,i]),New[(j-1),i],New[j,i])
  }
}
New <- New[order(New$Date),]
PHC1 <- New

New <- as.data.frame(seq.Date(as.Date(min(comb.PHC$Date)), as.Date(max(comb.PHC$Date)), by="days"))
colnames(New)[1] <- "Date"

for (i in PHC.list)
{
  j <- i
  i <- comb.PHC[comb.PHC$pRef==i & comb.PHC$outlier.PHC2.PCC==0,]
  i <- i[,c("Date","PHC2")]
  colnames(i)[2] <- j
  New<-merge(New,i,by="Date", all.x=T)
}

New <- New[order(rev(New$Date)),]
for (i in PHC.list)
{ 
  for (j in 1:nrow(New))
  {
    
    New[j,i] <- ifelse(is.na(New[j,i]),New[(j-1),i],New[j,i])
  }
}
New <- New[order(New$Date),]
PHC2 <- New




New <- as.data.frame(seq.Date(as.Date(min(sodwac.PCC$Date)),as.Date(max(comb.PHC$Date)), by="days"))
colnames(New)[1] <- "Date"

for (i in PCC.list)
{
  j <- i
  i <- sodwac.PCC[sodwac.PCC$pRef==i & sodwac.PCC$outlier.PCC1==0,]
  i <- i[,c("Date","PCC1")]
  colnames(i)[2] <- j
  New<-merge(New,i,by="Date", all.x=T)
}

New <- New[order(rev(New$Date)),]
for (i in PCC.list)
{ 
  for (j in 1:nrow(New))
  {
    
    New[j,i] <- ifelse(is.na(New[j,i]),New[(j-1),i],New[j,i])
  }
}
New <- New[order(New$Date),]
PCC1 <- New

New <- as.data.frame(seq.Date(as.Date(min(sodwac.PCC$Date)), as.Date(max(comb.PHC$Date)), by="days"))
colnames(New)[1] <- "Date"

for (i in PCC.list)
{
  j <- i
  i <- sodwac.PCC[sodwac.PCC$pRef==i & sodwac.PCC$outlier.PCC2==0,]
  i <- i[,c("Date","PCC2")]
  colnames(i)[2] <- j
  New<-merge(New,i,by="Date", all.x=T)
}

New <- New[order(rev(New$Date)),]
for (i in PCC.list)
{ 
  for (j in 1:nrow(New))
  {
    
    New[j,i] <- ifelse(is.na(New[j,i]),New[(j-1),i],New[j,i])
  }
}
New <- New[order(New$Date),]
PCC2 <- New

# create year to date version and 365days version
# multiply sodwac by 0.15 (sodwac factor)
sodwac.list <- PHC.list[PHC.list %in% PCC.list ]
blind.list <- PHC.list[!PHC.list %in% PCC.list ]

PHC1_old <- PHC1
PHC2_old <- PHC2
PCC1_old <- PCC1
PCC2_old <- PCC2
for (i in sodwac.list){
  PHC1[,i] <- PHC1[,i]*1.015
  PHC2[,i] <- PHC2[,i]*1.015
  PCC1[,i] <- PCC1[,i]*1.015
  PCC2[,i] <- PCC2[,i]*1.015
}
##keep two years here - end date changes. 
##Calendar Year
PHC1.calendar <- PHC1[PHC1$Date>="2014-01-01"&PHC1$Date<"2016-01-01",]
PHC2.calendar <- PHC2[PHC2$Date>="2014-01-01"&PHC2$Date<"2016-01-01",]
PCC1.calendar <- PCC1[PCC1$Date>="2014-01-01"&PCC1$Date<"2016-01-01",]
PCC2.calendar <- PCC2[PCC2$Date>="2014-01-01"&PCC2$Date<"2016-01-01",]

##Financial Year
PHC1.financial <- PHC1[PHC1$Date>="2014-04-01"&PHC1$Date<"2016-04-01",]
PHC2.financial <- PHC2[PHC2$Date>="2014-04-01"&PHC2$Date<"2016-04-01",]
PCC1.financial <- PCC1[PCC1$Date>="2014-04-01"&PCC1$Date<"2016-04-01",]
PCC2.financial <- PCC2[PCC2$Date>="2014-04-01"&PCC2$Date<"2016-04-01",]



for (i in PHC.list)
{ 
  for (j in 1:nrow(PHC1.calendar))
  {
    PHC1.calendar[j,i] <- ifelse(is.na(PHC1.calendar[j,i]),PHC1.calendar[(j-365),i],PHC1.calendar[j,i])
  }
}

for (i in PHC.list)
{ 
  for (j in 1:nrow(PHC2.calendar))
  {
    PHC2.calendar[j,i] <- ifelse(is.na(PHC2.calendar[j,i]),PHC2.calendar[(j-365),i],PHC2.calendar[j,i])
  }
}

for (i in PCC.list)
{ 
  for (j in 1:nrow(PCC1.calendar))
  {
    PCC1.calendar[j,i] <- ifelse(is.na(PCC1.calendar[j,i]),PCC1.calendar[(j-365),i],PCC1.calendar[j,i])
  }
}

for (i in PCC.list)
{ 
  for (j in 1:nrow(PCC2.calendar))
  {
    PCC2.calendar[j,i] <- ifelse(is.na(PCC2.calendar[j,i]),PCC2.calendar[(j-365),i],PCC2.calendar[j,i])
  }
}

for (i in PHC.list)
{ 
  for (j in 1:nrow(PHC1.financial))
  {
    PHC1.financial[j,i] <- ifelse(is.na(PHC1.financial[j,i]),PHC1.financial[(j-365),i],PHC1.financial[j,i])
  }
}

for (i in PHC.list)
{ 
  for (j in 1:nrow(PHC2.financial))
  {
    PHC2.financial[j,i] <- ifelse(is.na(PHC2.financial[j,i]),PHC2.financial[(j-365),i],PHC2.financial[j,i])
  }
}

for (i in PCC.list)
{ 
  for (j in 1:nrow(PCC1.financial))
  {
    PCC1.financial[j,i] <- ifelse(is.na(PCC1.financial[j,i]),PCC1.financial[(j-365),i],PCC1.financial[j,i])
  }
}

for (i in PCC.list)
{ 
  for (j in 1:nrow(PCC2.financial))
  {
    PCC2.financial[j,i] <- ifelse(is.na(PCC2.financial[j,i]),PCC2.financial[(j-365),i],PCC2.financial[j,i])
  }
}


PHC1.calendar <- PHC1.calendar[PHC1.calendar$Date>="2015-04-01",]
PHC2.calendar <- PHC2.calendar[PHC2.calendar$Date>="2015-04-01",]
PCC1.calendar <- PCC1.calendar[PCC1.calendar$Date>="2015-04-01",]
PCC2.calendar <- PCC2.calendar[PCC2.calendar$Date>="2015-04-01",]

PHC1.financial <- PHC1.financial[PHC1.financial$Date>="2015-04-01",]
PHC2.financial <- PHC2.financial[PHC2.financial$Date>="2015-04-01",]
PCC1.financial <- PCC1.financial[PCC1.financial$Date>="2015-04-01",]
PCC2.financial <- PCC2.financial[PCC2.financial$Date>="2015-04-01",]



sodwac.occ <- survey.properties2[survey.properties2$pRef %in% PCC.list,]
sodwac.occ$num.all_persons <- as.numeric(as.character(sodwac.occ$occsInOccYear))
sodwac.list <- PHC.list[PHC.list %in% PCC.list ]
blind.list <- PHC.list[!PHC.list %in% PCC.list ]
## extract blind and sodwac from 
final.blind.PHC1.calendar <- mean(colMeans(PHC1.calendar[,colnames(PHC1.calendar) %in%blind.list],na.rm=T),na.rm=T)
final.blind.PHC1.financial <- mean(colMeans(PHC1.financial[,colnames(PHC1.financial) %in%blind.list],na.rm=T),na.rm=T)
final.sodwac.PHC1.calendar <-mean(colMeans(PHC1.calendar[,colnames(PHC1.calendar) %in%sodwac.list],na.rm=T),na.rm=T)
final.sodwac.PHC1.financial <-mean(colMeans(PHC1.financial[,colnames(PHC1.financial) %in%sodwac.list],na.rm=T),na.rm=T)
final.combined.PHC1.calendar <- mean(colMeans(PHC1.calendar[,c(2:ncol(PHC1.calendar))],na.rm=T),na.rm=T)
final.combined.PHC1.financial <-mean(colMeans(PHC1.financial[,c(2:ncol(PHC1.financial))],na.rm=T),na.rm=T)

final.blind.PHC2.calendar <- mean(colMeans(PHC2.calendar[,colnames(PHC2.calendar) %in%blind.list],na.rm=T),na.rm=T)
final.blind.PHC2.financial <- mean(colMeans(PHC2.financial[,colnames(PHC2.financial) %in%blind.list],na.rm=T),na.rm=T)
final.sodwac.PHC2.calendar <-mean(colMeans(PHC2.calendar[,colnames(PHC2.calendar) %in%sodwac.list],na.rm=T),na.rm=T)
final.sodwac.PHC2.financial <-mean(colMeans(PHC2.financial[,colnames(PHC2.financial) %in%sodwac.list],na.rm=T),na.rm=T)
final.combined.PHC2.calendar <- mean(colMeans(PHC2.calendar[,c(2:ncol(PHC2.calendar))],na.rm=T),na.rm=T)
final.combined.PHC2.financial <-mean(colMeans(PHC2.financial[,c(2:ncol(PHC2.financial))],na.rm=T),na.rm=T)

final.sodwac.PCC1.calendar <-mean(colMeans(PCC1.calendar[,colnames(PCC1.calendar) %in%sodwac.list],na.rm=T),na.rm=T)
final.sodwac.PCC1.financial <-mean(colMeans(PCC1.financial[,colnames(PCC1.financial) %in%sodwac.list],na.rm=T),na.rm=T)
final.sodwac.PCC2.calendar <-mean(colMeans(PCC2.calendar[,colnames(PCC2.calendar) %in%sodwac.list],na.rm=T),na.rm=T)
final.sodwac.PCC2.financial <-mean(colMeans(PCC2.financial[,colnames(PCC2.financial) %in%sodwac.list],na.rm=T),na.rm=T)

final.sodwac.PHC1.calendar.PCC <- final.sodwac.PHC1.calendar/mean(sodwac.occ$num.all_persons)
final.sodwac.PHC1.financial.PCC <- final.sodwac.PHC1.financial/mean(sodwac.occ$num.all_persons)
final.sodwac.PHC2.calendar.PCC <- final.sodwac.PHC2.calendar/mean(sodwac.occ$num.all_persons)
final.sodwac.PHC2.financial.PCC <- final.sodwac.PHC2.financial/mean(sodwac.occ$num.all_persons)
sterr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
final.sterr.PHC.blind.calendar.1 <- sterr(colMeans(PHC1.calendar[,colnames(PHC1.calendar) %in%blind.list],na.rm=T))
final.sterr.PHC.sodwac.calendar.1 <- sterr(colMeans(PHC1.calendar[,colnames(PHC1.calendar) %in%sodwac.list],na.rm=T))
final.sterr.PHC.comb.calendar.1 <- sterr(colMeans(PHC1.calendar[,c(2:ncol(PHC1.calendar))],na.rm=T))
final.sterr.PCC.sodwac.calendar.1<- sterr(colMeans(PCC1.calendar[,colnames(PCC1.calendar) %in%sodwac.list],na.rm=T))

final.sterr.PHC.blind.calendar.2 <- sterr(colMeans(PHC2.calendar[,colnames(PHC2.calendar) %in%blind.list],na.rm=T))
final.sterr.PHC.sodwac.calendar.2 <- sterr(colMeans(PHC2.calendar[,colnames(PHC2.calendar) %in%sodwac.list],na.rm=T))
final.sterr.PHC.comb.calendar.2 <- sterr(colMeans(PHC2.calendar[,c(2:ncol(PHC2.calendar))],na.rm=T))
final.sterr.PCC.sodwac.calendar.2<- sterr(colMeans(PCC2.calendar[,colnames(PCC2.calendar) %in%sodwac.list],na.rm=T))

final.sterr.PHC.blind.financial.1 <- sterr(colMeans(PHC1.financial[,colnames(PHC1.financial) %in%blind.list],na.rm=T))
final.sterr.PHC.sodwac.financial.1 <- sterr(colMeans(PHC1.financial[,colnames(PHC1.financial) %in%sodwac.list],na.rm=T))
final.sterr.PHC.comb.financial.1 <- sterr(colMeans(PHC1.financial[,c(2:ncol(PHC1.financial))],na.rm=T))
final.sterr.PCC.sodwac.financial.1<- sterr(colMeans(PCC1.financial[,colnames(PCC1.financial) %in%sodwac.list],na.rm=T))

final.sterr.PHC.blind.financial.2 <- sterr(colMeans(PHC2.financial[,colnames(PHC2.financial) %in%blind.list],na.rm=T))
final.sterr.PHC.sodwac.financial.2 <- sterr(colMeans(PHC2.financial[,colnames(PHC2.financial) %in%sodwac.list],na.rm=T))
final.sterr.PHC.comb.financial.2 <- sterr(colMeans(PHC2.financial[,c(2:ncol(PHC2.financial))],na.rm=T))
final.sterr.PCC.sodwac.financial.2<- sterr(colMeans(PCC2.financial[,colnames(PCC2.financial) %in%sodwac.list],na.rm=T))


final.PCC.new <- as.data.frame(rbind(

                                     final.blind.PHC2.calendar,
                                     final.sodwac.PHC2.calendar,
                                     final.combined.PHC2.calendar,
                                     final.sodwac.PCC2.calendar,
                                     final.sodwac.PHC2.calendar.PCC,
                                     final.sterr.PHC.blind.calendar.2,
                                     final.sterr.PHC.sodwac.calendar.2,
                                     final.sterr.PHC.comb.calendar.2,
                                     final.sterr.PCC.sodwac.calendar.2,
                                     
                                     final.blind.PHC2.financial,
                                     final.sodwac.PHC2.financial,
                                     final.combined.PHC2.financial,
                                     final.sodwac.PCC2.financial,
                                     final.sodwac.PHC2.financial.PCC,
                                     final.sterr.PHC.blind.financial.2,
                                     final.sterr.PHC.sodwac.financial.2,
                                     final.sterr.PHC.comb.financial.2,
                                     final.sterr.PCC.sodwac.financial.2                                      
))

write.csv(final.PCC.new,"T:\\live\\xxxx SWW PCC Apr 2016\\02 Delivery\\R output\\updated - all stats - calendar and financial.csv")
