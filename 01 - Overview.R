################### read files in  and remove dates   ############################################################################################
survey.properties <- read.csv("T:\\live\\2160 SWW PCC Sept 2016/02 Delivery/R input files/propertystatus_from_SodwacOct2016.csv")


############# load sarah's file for comparison
#sarahs.properties  <- read.csv("T:\\live\\2140 SWW PCC Apr 2016\\02 Delivery\\Data in\\surveyProperties - updated.csv")
#sarahs-mr<- read.csv("T:\\live\\2140 SWW PCC Apr 2016\\02 Delivery\\Data in\\meter read - updated.csv")

#prep survey data
survey.properties <- survey.properties[,-c(14,15,16)] 
survey.properties$pRef <- as.numeric(as.character(survey.properties$pRef))
survey.properties <- survey.properties[order(survey.properties$pRef),] 
survey.properties2 <- unique(survey.properties)

#what dates we need? / do we need unique or duplicates? / meaning of watstat values

#split by area
sp.sodwac <- as.character(unique(survey.properties$pRef[survey.properties$surveyType=="Sodwac" & survey.properties$watStat=="u"]))
sp.blind <- as.character(unique(survey.properties$pRef[survey.properties$surveyType=="Blind" & survey.properties$watStat=="u"]))
