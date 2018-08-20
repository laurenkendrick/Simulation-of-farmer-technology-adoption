#Lauren Kendrick January 31, 2018
library(FinCal)
set.seed(1234)

########################
#Start here
######################
#Import hog farm data
setwd('C:/Users/kendrick/Documents/Dissertation Workshop/Data/Facility listings')
#hogAFOsbackup<-read.csv("completehogsimulationdata.csv",header=TRUE,skipNul=TRUE)  #From Iowa DNR AFO listing
#hogAFOsbackup<-read.csv("completehogsimulationdataLOW_May_4_2018.csv",header=TRUE,skipNul=TRUE)  #From Iowa DNR AFO listing
hogAFOsbackup<-read.csv("completehogsimulationdataHIGH_May_4_2018.csv",header=TRUE,skipNul=TRUE)  #From Iowa DNR AFO listing
#tech<-"LOW"
tech<-"HIGH"
hogAFOsdecisionsmontecarlo<-0

for (p in 1:5){
x<-sample(1:10000,1)
hogAFOsdecisions<-hogAFOsbackup[,c(1:8)]
scenarioconditions<-data.frame(matrix(ncol=6,nrow=0))
scenariovars<-c("scenario","wastevalue","consumerWTP","wage","base_awareness","cost_share")
colnames(scenarioconditions)<-scenariovars
n=1

source('Farmer decision simulation reset.R')
#Scenario 32
scenario<-'S32'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-0 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.5 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.06
base_awareness<-0.3 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.75
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 33
scenario<-'S33'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-3 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.5 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.06
base_awareness<-0.3 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.75
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 34
scenario<-'S34'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-3 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.5 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.1
base_awareness<-0.5 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 35
scenario<-'S35'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-3 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.06
base_awareness<-0.3 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 36
scenario<-'S36'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-3 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-37.28 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.06
base_awareness<-0.3 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.75
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 37
scenario<-'S37'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-3 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.5 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-37.28 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.06
base_awareness<-0.3 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 38
scenario<-'S38'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-3 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.5 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-37.28 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.1
base_awareness<-0.5 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.75
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 39
scenario<-'S39'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-3 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.1
base_awareness<-0.5 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.75
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 40
scenario<-'S40'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-0 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.5 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.06
base_awareness<-0.3 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 41
scenario<-'S41'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-0 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.5 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.1
base_awareness<-0.5 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.75
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 42
scenario<-'S42'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-0 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.5 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-37.28 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.06
base_awareness<-0.3 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.75
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 43
scenario<-'S43'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-0 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.06
base_awareness<-0.3 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.75
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')

source('Farmer decision simulation reset.R')
#Scenario 44
scenario<-'S44'
#Want to vary wastevalue zero to 15 (because that's a bit LOWer than average quotes)
wastevalue<-3 #$ per N in fertilizer from one pig annually under base conditions #Change this to manipulate the value of the waste stream
#Want to vary consumerWTP zero to 7, because that's 10% of hog price
consumerWTP<-0.75 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
#Want to vary wage 13.12 (farm hand) to 22.06 (farm technician) to 37.28 (farmer or farm manager) https://www.bls.gov/oes/current/oes_ia.htm
wage<-13.12 #Change this to change sensitivity PARAMETER
#Could vary awareness increment from 0.05 to 0.15 AND/OR Vary level of awareness for certain technologies for certain individuals
awarenessincrement<-0.04
base_awareness<-0.2 #Vary this one
#Vary cost-share from 0 to 75% of costs for each technology to match EQIP https://www.benefits.gov/benefits/benefit-details/375
cost_share<-0.9
hogAFOs$costshare_segregation<-abs(hogAFOs$cost_segregation_wo_manure*cost_share)
hogAFOs$costshare_acid<-abs(hogAFOs$cost_acid_wo_manure*cost_share)
hogAFOs$costshare_impcover<-abs(hogAFOs$cost_impcover_wo_manure*cost_share)
hogAFOs$costshare_diet<-abs(hogAFOs$cost_diet_wo_manure*cost_share)
hogAFOs$costshare_percover<-abs(hogAFOs$cost_percover_wo_manure*cost_share)
hogAFOs$costshare_inject<-abs(hogAFOs$cost_inject_wo_manure*cost_share)
hogAFOs$costshare_scrubbers<-abs(hogAFOs$cost_scrubbers_wo_manure*cost_share)
hogAFOs$costshare_biofilter<-abs(hogAFOs$cost_biofilter_wo_manure*cost_share)
hogAFOs$costshare_trees<-abs(hogAFOs$cost_trees_wo_manure*cost_share)
scenarioconditions[n,]<-c(scenario,wastevalue,consumerWTP,wage,base_awareness,cost_share)
n<-n+1
source('Simulation farmer decisions.R')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS18Feb6LOW.csv')


hogAFOsdecisionsmontecarlo<-rbind(hogAFOsdecisionsmontecarlo,hogAFOsdecisions)
} #end p loop
hogAFOsdecisionsmontecarlo<-hogAFOsdecisionsmontecarlo[-1,]
summary(hogAFOsdecisionsmontecarlo)
scenarioconditions

#write.csv(hogAFOsdecisionsmontecarlo,file='farmerdecisionsHIGH_May42018_inflation.csv')

