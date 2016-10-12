#######################################read files in  and remove dates   ############################################################################################
survey.properties <- read.csv("T:\\live\\2160 SWW PCC Sept 2016/02 Delivery/R input files/propertystatus_from_SodwacOct2016.csv")
meter.reads <- read.csv("T:\\live\\2160 SWW PCC Sept 2016/02 Delivery/R input files/meterreads_from_SodwacOct2016.csv")

############# load sarah's file for comparison
#sarahs.properties  <- read.csv("T:\\live\\2140 SWW PCC Apr 2016\\02 Delivery\\Data in\\surveyProperties - updated.csv")
#sarahs.mr<- read.csv("T:\\live\\2140 SWW PCC Apr 2016\\02 Delivery\\Data in\\meter read - updated.csv")

#prep survey data
survey.properties <- survey.properties[,-c(14,15,16)] 
survey.properties$pRef <- as.numeric(as.character(survey.properties$pRef))
survey.properties <- survey.properties[order(survey.properties$pRef),] 
survey.properties2 <- unique(survey.properties)

#prep meter reads; 
meter.reads$pRef <- as.numeric(as.character(meter.reads$pRef))
meter.reads <- meter.reads[order(meter.reads$pRef),]

mr. <- as.character(unique(meter.reads$pRef)) 
mr. <- mr.[order(mr.)]

meter.reads$Date <- as.Date(meter.reads$Date, format="%d/%m/%Y")
meter.reads$Reading <- as.numeric(as.character(meter.reads$Reading))
meter.reads$year <- as.POSIXlt(meter.reads$Date)$year+1900
meter.reads$month <- as.POSIXlt(meter.reads$Date)$mon+1           

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

meter.reads <- meter.reads[meter.reads$excl==0,]
meter.reads <- meter.reads[!is.na(meter.reads$excl),]

#what dates we need? / do we need unique or duplicates? / meaning of watstat values

#split by area
sp.sodwac <- as.character(unique(survey.properties$pRef[survey.properties$surveyType=="Sodwac" & survey.properties$watStat=="u"]))
sp.blind <- as.character(unique(survey.properties$pRef[survey.properties$surveyType=="Blind" & survey.properties$watStat=="u"]))

blind <- merge(survey.properties2[survey.properties2$watStat=="u"&survey.properties2$surveyType=="Blind",],meter.reads, by="pRef") # changed to match new column name
sodwac <- merge(survey.properties2[survey.properties2$watStat=="u"&survey.properties2$surveyType=="Sodwac",],meter.reads, by="pRef")

b. <- as.numeric(as.character(unique(blind$pRef))) # 435
s. <- as.numeric(as.character(unique(sodwac$pRef))) # 286

blind <- blind[order(blind$pRef, blind$Date),]
sodwac <- sodwac[order(sodwac$pRef, sodwac$Date),]


