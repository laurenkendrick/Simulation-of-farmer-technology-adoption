#Lauren Kendrick January 31, 2018
##########
#Read in scenario values
################

hogAFOs$cost_impcover_wo_manure[hogAFOs$cost_impcover_wo_manure==0]<-100000000
hogAFOs$cost_percover_wo_manure[hogAFOs$cost_percover_wo_manure==0]<-100000000
hogAFOs$cost_impcover[hogAFOs$cost_impcover==0]<-100000000
hogAFOs$cost_percover[hogAFOs$cost_percover==0]<-100000000
###############
#Run simulation
################
#wastevalue<-11.43 #$ per N in fertilizer from one pig annually under base conditions PARAMETER #Change this to manipulate the value of the waste stream
hogAFOs$manure_valuebase<-wastevalue*hogAFOs$Swine.Animal.Units*4  #$ per farm
hogAFOs$manure_valuesegregation<-wastevalue*hogAFOs$remainingNsegregation*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase #$ per farm more than baseline emissions with broadcast
hogAFOs$manure_valueacid<-wastevalue*hogAFOs$remainingNacid*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
hogAFOs$manure_valueimpcover<-wastevalue*hogAFOs$remainingNimpcover*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
hogAFOs$manure_valuediet<-wastevalue*hogAFOs$remainingNdiet*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
hogAFOs$manure_valuepercover<-wastevalue*hogAFOs$remainingNpercover*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
hogAFOs$manure_valueinject<-wastevalue*hogAFOs$remainingNinject*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
#(value added to manure by retaining N)
#Use cost wo manure estimates for cost
#Could also manipulate profit by consumer WTP per pound N not emitted
#summary(hogAFOs$baseemission) #mean 11.76 
#additional consumer WTP for each mitigation method
#consumerWTP<-1 #dollars per lb/year N not emitted #Change this to manipulate the value of the product #PARAMETER
hogAFOs$consumerWTPbase<-0
hogAFOs$consumerWTPsegregation<- -(hogAFOs$totalsegregation-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units
hogAFOs$consumerWTPacid<--(hogAFOs$totalacid-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units
hogAFOs$consumerWTPimpcover<--(hogAFOs$totalimpcover-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units
hogAFOs$consumerWTPdiet<--(hogAFOs$totaldiet-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units
hogAFOs$consumerWTPpercover<--(hogAFOs$totalpercover-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units
hogAFOs$consumerWTPinject<--(hogAFOs$totalinject-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units
hogAFOs$consumerWTPscrubbers<--(hogAFOs$totalscrubbers-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units
hogAFOs$consumerWTPbiofilter<--(hogAFOs$totalbiofilter-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units
hogAFOs$consumerWTPtrees<--(hogAFOs$totaltrees-hogAFOs$baseemission)*consumerWTP*4*hogAFOs$Swine.Animal.Units

#Administrative burden #Although maybe I should think of this also in terms of financial risk
#Multiply burden level by farm worker salary for the time needed to manage
#Average wage $13.17/hour http://www.thegazette.com/subject/news/business/iowa-farm-workers-paid-more-than-us-average-20160526
#Level 1: Days: $13.17*8*3
#Level 2: Weeks: $13.17*8*15
#Level 3: Months: $13.17*8*120
#Level 4: Year: $13.17*8*365
#wage<-13.17 #Change this to change sensitivity PARAMETER
hogAFOs$burden_base<-0
hogAFOs$burden_segregation<-wage*8*120 #3
hogAFOs$burden_segregation[hogAFOs$Below.Buildings.pits.Deep=="No"]<-wage*8*365#4
hogAFOs$burden_acid<-wage*8*15 #2
hogAFOs$burden_impcover<-wage*8*3 #1
hogAFOs$burden_diet<-wage*8*15 #2
hogAFOs$burden_percover<-wage*8*3 #1
hogAFOs$burden_inject<-wage*8*15 #2
hogAFOs$burden_scrubbers<-wage*8*120 #3
hogAFOs$burden_biofilter<-wage*8*120 #3
hogAFOs$burden_trees<-wage*8*15 #2
#https://www.reuters.com/article/usa-livestock-barns/building-binge-by-iowa-pig-farmers-may-lead-to-even-lower-prices-idUSL2N14Z2ZA
#Quote is for profit per head rather than income, but since I'm already nullifying normal operational costs I think it's probably okay
#https://www.ams.usda.gov/mnreports/lsddhps.pdf
#$62.00-$71.75, weighted average $69.50
hogAFOs$burden_fraction_base<-0
hogAFOs$burden_fraction_segregation<-hogAFOs$cost_segregation_wo_manure/(hogAFOs$Swine.Animal.Units*4*69.5)
hogAFOs$burden_fraction_acid<-hogAFOs$cost_acid_wo_manure/(hogAFOs$Swine.Animal.Units*4*69.5)
hogAFOs$burden_fraction_impcover<-hogAFOs$cost_impcover_wo_manure/(hogAFOs$Swine.Animal.Units*4*69.5)
hogAFOs$burden_fraction_diet<-hogAFOs$cost_diet_wo_manure/(hogAFOs$Swine.Animal.Units*4*69.5)
hogAFOs$burden_fraction_percover<-hogAFOs$cost_percover_wo_manure/(hogAFOs$Swine.Animal.Units*4*69.5)
hogAFOs$burden_fraction_inject<-hogAFOs$cost_inject_wo_manure/(hogAFOs$Swine.Animal.Units*4*69.5)
hogAFOs$burden_fraction_scrubbers<-hogAFOs$cost_scrubbers/(hogAFOs$Swine.Animal.Units*4*69.5)
hogAFOs$burden_fraction_biofilter<-hogAFOs$cost_biofilter/(hogAFOs$Swine.Animal.Units*4*69.5)
hogAFOs$burden_fraction_trees<-hogAFOs$cost_trees/(hogAFOs$Swine.Animal.Units*4*69.5)

#Farmer risk aversion level, willingness to accept (WTA) wager of x percent of farm income. Menapace, 2012
hogAFOs$risk_aversion_step1<-runif(6596,0,100)
hogAFOs$risk_WTA<-0
hogAFOs$risk_WTA[hogAFOs$risk_aversion_step1>42.8]<-0.15
hogAFOs$risk_WTA[hogAFOs$risk_aversion_step1>66.4]<-0.3
hogAFOs$risk_WTA[hogAFOs$risk_aversion_step1>87.5]<-0.45
hogAFOs$risk_WTA[hogAFOs$risk_aversion_step1>92.6]<-0.60
hogAFOs$risk_WTA[hogAFOs$risk_aversion_step1>95.5]<-0.75
hogAFOs$risk_WTA[hogAFOs$risk_aversion_step1>98.7]<-1.05
hogAFOs$risk_WTA[hogAFOs$risk_aversion_step1>99.7]<-1.5
hogAFOsdecisions$risk_WTA<-hogAFOs$risk_WTA
#Awareness risk starts at 0.5 and moves down to 0.1 as more farmers in each county adopt a technology
#Change increment to change sensitivity PARAMETER
#awarenessincrement<-0.1
hogAFOs$awarenessrisk_base<-0
hogAFOs$awarenessrisk_segregation<-base_awareness
hogAFOs$awarenessrisk_acid<-base_awareness
hogAFOs$awarenessrisk_impcover<-base_awareness
hogAFOs$awarenessrisk_diet<-base_awareness
hogAFOs$awarenessrisk_percover<-base_awareness
hogAFOs$awarenessrisk_inject<-base_awareness
hogAFOs$awarenessrisk_scrubbers<-base_awareness
hogAFOs$awarenessrisk_biofilter<-base_awareness
hogAFOs$awarenessrisk_trees<-base_awareness
#Profits
hogAFOs$profits_base<-0
hogAFOs$profits_segregation<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPsegregation+hogAFOs$manure_valuesegregation),type=0)-hogAFOs$cost_segregation_wo_manure+hogAFOs$costshare_segregation
hogAFOs$profits_acid<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPacid+hogAFOs$manure_valueacid),type=0)-hogAFOs$cost_acid_wo_manure+hogAFOs$costshare_acid
hogAFOs$profits_impcover<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPimpcover+hogAFOs$manure_valueimpcover),type=0)-hogAFOs$cost_impcover_wo_manure+hogAFOs$costshare_impcover
hogAFOs$profits_diet<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPdiet+hogAFOs$manure_valuediet),type=0)-hogAFOs$cost_diet_wo_manure+hogAFOs$costshare_diet
hogAFOs$profits_percover<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPpercover+hogAFOs$manure_valuepercover),type=0)-hogAFOs$cost_percover_wo_manure+hogAFOs$costshare_percover
hogAFOs$profits_inject<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPinject+hogAFOs$manure_valueinject),type=0)-hogAFOs$cost_inject_wo_manure+hogAFOs$costshare_inject
hogAFOs$profits_scrubbers<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPscrubbers),type=0)-hogAFOs$cost_scrubbers+hogAFOs$costshare_scrubbers
hogAFOs$profits_biofilter<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPbiofilter),type=0)-hogAFOs$cost_biofilter+hogAFOs$costshare_biofilter
hogAFOs$profits_trees<-pv.annuity(hogAFOs$discount_rate,5,-(hogAFOs$consumerWTPtrees),type=0)-hogAFOs$cost_trees+hogAFOs$costshare_trees

#Loop to go over multiple periods
#hogAFOsdecisions<-hogAFOs[,c(1:34,44:45,48,53,58,62,65,68,70,74,78,94:122)]
#hogAFOsdecisions<-hogAFOs[,c(1:8,185)]
period<-c(1:5)
for (m in 1:length(period)){
#Now do the farmer calculation
hogAFOs$decision_part2_base<-0
hogAFOs$decision_part1_segregation<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_segregation-hogAFOs$burden_fraction_segregation
hogAFOs$decision_part2_segregation<-0
hogAFOs$decision_part2_segregation[hogAFOs$decision_part1_segregation>0]<-hogAFOs$profits_segregation[hogAFOs$decision_part1_segregation>0]-hogAFOs$burden_segregation[hogAFOs$decision_part1_segregation>0] 
hogAFOs$decision_part1_acid<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_acid-hogAFOs$burden_fraction_acid
hogAFOs$decision_part2_acid<-0
hogAFOs$decision_part2_acid[hogAFOs$decision_part1_acid>0]<-hogAFOs$profits_acid[hogAFOs$decision_part1_acid>0]-hogAFOs$burden_acid[hogAFOs$decision_part1_acid>0]
hogAFOs$decision_part1_impcover<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_impcover-hogAFOs$burden_fraction_impcover
hogAFOs$decision_part2_impcover<-0
hogAFOs$decision_part2_impcover[hogAFOs$decision_part1_impcover>0]<-hogAFOs$profits_impcover[hogAFOs$decision_part1_impcover>0]-hogAFOs$burden_impcover[hogAFOs$decision_part1_impcover>0]
hogAFOs$decision_part1_diet<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_diet-hogAFOs$burden_fraction_diet
hogAFOs$decision_part2_diet<-0
hogAFOs$decision_part2_diet[hogAFOs$decision_part1_diet>0]<-hogAFOs$profits_diet[hogAFOs$decision_part1_diet>0]-hogAFOs$burden_diet[hogAFOs$decision_part1_diet>0]
hogAFOs$decision_part1_percover<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_percover-hogAFOs$burden_fraction_percover
hogAFOs$decision_part2_percover<-0
hogAFOs$decision_part2_percover[hogAFOs$decision_part1_percover>0]<-hogAFOs$profits_percover[hogAFOs$decision_part1_percover>0]-hogAFOs$burden_percover[hogAFOs$decision_part1_percover>0]
hogAFOs$decision_part1_inject<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_inject-hogAFOs$burden_fraction_inject
hogAFOs$decision_part2_inject<-0
hogAFOs$decision_part2_inject[hogAFOs$decision_part1_inject>0]<-hogAFOs$profits_inject[hogAFOs$decision_part1_inject>0]-hogAFOs$burden_inject[hogAFOs$decision_part1_inject>0]
hogAFOs$decision_part1_scrubbers<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_scrubbers-hogAFOs$burden_fraction_scrubbers
hogAFOs$decision_part2_scrubbers<-0
hogAFOs$decision_part2_scrubbers[hogAFOs$decision_part1_scrubbers>0]<-hogAFOs$profits_scrubbers[hogAFOs$decision_part1_scrubbers>0]-hogAFOs$burden_scrubbers[hogAFOs$decision_part1_scrubbers>0]
hogAFOs$decision_part1_biofilter<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_biofilter-hogAFOs$burden_fraction_biofilter
hogAFOs$decision_part2_biofilter<-0
hogAFOs$decision_part2_biofilter[hogAFOs$decision_part1_biofilter>0]<-hogAFOs$profits_biofilter[hogAFOs$decision_part1_biofilter>0]-hogAFOs$burden_biofilter[hogAFOs$decision_part1_biofilter>0]
hogAFOs$decision_part1_trees<-hogAFOs$risk_WTA-hogAFOs$awarenessrisk_trees-hogAFOs$burden_fraction_trees
hogAFOs$decision_part2_trees<-0
hogAFOs$decision_part2_trees[hogAFOs$decision_part1_trees>0]<-hogAFOs$profits_trees[hogAFOs$decision_part1_trees>0]-wage*hogAFOs$burden_trees[hogAFOs$decision_part1_trees>0]
#hogAFOs$decision_part3<-colnames(hogAFOs[,c(194,196,198,200,202,204,206,208,210,212)])[max.col(hogAFOs[,c(194,196,198,200,202,204,206,208,210,212)],ties.method="first")]
#hogAFOs$decision_part3<-colnames(hogAFOs[,c(203,205,207,209,211,213,215,217,219,221)])[max.col(hogAFOs[,c(203,205,207,209,211,213,215,217,219,221)],ties.method="first")]
#hogAFOs$decision_part3<-colnames(hogAFOs[,c(206,208,210,212,214,216,218,220,222,224)])[max.col(hogAFOs[,c(206,208,210,212,214,216,218,220,222,224)],ties.method="first")]
hogAFOs$decision_part3<-colnames(hogAFOs[,c(207,209,211,213,215,217,219,221,223,225)])[max.col(hogAFOs[,c(207,209,211,213,215,217,219,221,223,225)],ties.method="first")]
hogAFOs$decision<-sub('decision_part2_','',hogAFOs$decision_part3,16)
hogAFOs$decision<-as.factor(hogAFOs$decision)
hogAFOs$decision_period1<-hogAFOs$decision
hogAFOsdecisions[,paste(scenario,'_decision_',period[m],sep="")]<-hogAFOs$decision

#Now need to update awarenessrisk
Countycounts<-unique(hogAFOs$County)
Countycounts<-data.frame(Countycounts)
colnames(Countycounts)<-'County'
county<-unique(as.character(hogAFOs$County))
for (h in 1:99){
Countycounts$base_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='base'&hogAFOs$County==county[h]))
Countycounts$segregation_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='segregation'&hogAFOs$County==county[h]))
Countycounts$acid_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='acid'&hogAFOs$County==county[h]))
Countycounts$impcover_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='impcover'&hogAFOs$County==county[h]))
Countycounts$diet_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='diet'&hogAFOs$County==county[h]))
Countycounts$percover_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='percover'&hogAFOs$County==county[h]))
Countycounts$inject_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='inject'&hogAFOs$County==county[h]))
Countycounts$scrubbers_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='scrubbers'&hogAFOs$County==county[h]))
Countycounts$biofilter_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='biofilter'&hogAFOs$County==county[h]))
Countycounts$trees_period1[Countycounts$County==county[h]]<-length(which(hogAFOs$decision=='trees'&hogAFOs$County==county[h]))
Countycounts$AFOs[Countycounts$County==county[h]]<-length(which(hogAFOs$County==county[h]))
} #close h loop

#Continue working here. January 24, 2018.  Need to use county counts to update awareness risk level by county, 
#then repeat decision rows, only changing last line from decision_period1 to decision_period2

hogAFOs$awarenessrisk_segregation[hogAFOs$County==Countycounts$County[Countycounts$segregation_period1>0]]<-max(hogAFOs$awarenessrisk_segregation[hogAFOs$County==Countycounts$County[Countycounts$segregation_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_segregation[hogAFOs$County==Countycounts$County[Countycounts$segregation_period1>4]]<-max(hogAFOs$awarenessrisk_segregation[hogAFOs$County==Countycounts$County[Countycounts$segregation_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_segregation[hogAFOs$County==Countycounts$County[Countycounts$segregation_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_segregation[hogAFOs$County==Countycounts$County[Countycounts$segregation_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_segregation[hogAFOs$County==Countycounts$County[Countycounts$segregation_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_segregation[hogAFOs$County==Countycounts$County[Countycounts$segregation_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)
hogAFOs$awarenessrisk_acid[hogAFOs$County==Countycounts$County[Countycounts$acid_period1>0]]<-max(hogAFOs$awarenessrisk_acid[hogAFOs$County==Countycounts$County[Countycounts$acid_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_acid[hogAFOs$County==Countycounts$County[Countycounts$acid_period1>4]]<-max(hogAFOs$awarenessrisk_acid[hogAFOs$County==Countycounts$County[Countycounts$acid_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_acid[hogAFOs$County==Countycounts$County[Countycounts$acid_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_acid[hogAFOs$County==Countycounts$County[Countycounts$acid_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_acid[hogAFOs$County==Countycounts$County[Countycounts$acid_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_acid[hogAFOs$County==Countycounts$County[Countycounts$acid_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)
hogAFOs$awarenessrisk_impcover[hogAFOs$County==Countycounts$County[Countycounts$impcover_period1>0]]<-max(hogAFOs$awarenessrisk_impcover[hogAFOs$County==Countycounts$County[Countycounts$impcover_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_impcover[hogAFOs$County==Countycounts$County[Countycounts$impcover_period1>4]]<-max(hogAFOs$awarenessrisk_impcover[hogAFOs$County==Countycounts$County[Countycounts$impcover_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_impcover[hogAFOs$County==Countycounts$County[Countycounts$impcover_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_impcover[hogAFOs$County==Countycounts$County[Countycounts$impcover_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_impcover[hogAFOs$County==Countycounts$County[Countycounts$impcover_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_impcover[hogAFOs$County==Countycounts$County[Countycounts$impcover_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)
hogAFOs$awarenessrisk_diet[hogAFOs$County==Countycounts$County[Countycounts$diet_period1>0]]<-max(hogAFOs$awarenessrisk_diet[hogAFOs$County==Countycounts$County[Countycounts$diet_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_diet[hogAFOs$County==Countycounts$County[Countycounts$diet_period1>4]]<-max(hogAFOs$awarenessrisk_diet[hogAFOs$County==Countycounts$County[Countycounts$diet_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_diet[hogAFOs$County==Countycounts$County[Countycounts$diet_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_diet[hogAFOs$County==Countycounts$County[Countycounts$diet_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_diet[hogAFOs$County==Countycounts$County[Countycounts$diet_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_diet[hogAFOs$County==Countycounts$County[Countycounts$diet_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)
hogAFOs$awarenessrisk_percover[hogAFOs$County==Countycounts$County[Countycounts$percover_period1>0]]<-max(hogAFOs$awarenessrisk_percover[hogAFOs$County==Countycounts$County[Countycounts$percover_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_percover[hogAFOs$County==Countycounts$County[Countycounts$percover_period1>4]]<-max(hogAFOs$awarenessrisk_percover[hogAFOs$County==Countycounts$County[Countycounts$percover_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_percover[hogAFOs$County==Countycounts$County[Countycounts$percover_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_percover[hogAFOs$County==Countycounts$County[Countycounts$percover_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_percover[hogAFOs$County==Countycounts$County[Countycounts$percover_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_percover[hogAFOs$County==Countycounts$County[Countycounts$percover_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)
hogAFOs$awarenessrisk_inject[hogAFOs$County==Countycounts$County[Countycounts$inject_period1>0]]<-max(hogAFOs$awarenessrisk_inject[hogAFOs$County==Countycounts$County[Countycounts$inject_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_inject[hogAFOs$County==Countycounts$County[Countycounts$inject_period1>4]]<-max(hogAFOs$awarenessrisk_inject[hogAFOs$County==Countycounts$County[Countycounts$inject_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_inject[hogAFOs$County==Countycounts$County[Countycounts$inject_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_inject[hogAFOs$County==Countycounts$County[Countycounts$inject_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_inject[hogAFOs$County==Countycounts$County[Countycounts$inject_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_inject[hogAFOs$County==Countycounts$County[Countycounts$inject_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)
hogAFOs$awarenessrisk_scrubbers[hogAFOs$County==Countycounts$County[Countycounts$scrubbers_period1>0]]<-max(hogAFOs$awarenessrisk_scrubbers[hogAFOs$County==Countycounts$County[Countycounts$scrubbers_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_scrubbers[hogAFOs$County==Countycounts$County[Countycounts$scrubbers_period1>4]]<-max(hogAFOs$awarenessrisk_scrubbers[hogAFOs$County==Countycounts$County[Countycounts$scrubbers_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_scrubbers[hogAFOs$County==Countycounts$County[Countycounts$scrubbers_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_scrubbers[hogAFOs$County==Countycounts$County[Countycounts$scrubbers_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_scrubbers[hogAFOs$County==Countycounts$County[Countycounts$scrubbers_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_scrubbers[hogAFOs$County==Countycounts$County[Countycounts$scrubbers_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)
hogAFOs$awarenessrisk_biofilter[hogAFOs$County==Countycounts$County[Countycounts$biofilter_period1>0]]<-max(hogAFOs$awarenessrisk_biofilter[hogAFOs$County==Countycounts$County[Countycounts$biofilter_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_biofilter[hogAFOs$County==Countycounts$County[Countycounts$biofilter_period1>4]]<-max(hogAFOs$awarenessrisk_biofilter[hogAFOs$County==Countycounts$County[Countycounts$biofilter_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_biofilter[hogAFOs$County==Countycounts$County[Countycounts$biofilter_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_biofilter[hogAFOs$County==Countycounts$County[Countycounts$biofilter_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_biofilter[hogAFOs$County==Countycounts$County[Countycounts$biofilter_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_biofilter[hogAFOs$County==Countycounts$County[Countycounts$biofilter_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)
hogAFOs$awarenessrisk_trees[hogAFOs$County==Countycounts$County[Countycounts$trees_period1>0]]<-max(hogAFOs$awarenessrisk_trees[hogAFOs$County==Countycounts$County[Countycounts$trees_period1>0]]-awarenessincrement,0)
hogAFOs$awarenessrisk_trees[hogAFOs$County==Countycounts$County[Countycounts$trees_period1>4]]<-max(hogAFOs$awarenessrisk_trees[hogAFOs$County==Countycounts$County[Countycounts$trees_period1>4]]-2*awarenessincrement,0)
hogAFOs$awarenessrisk_trees[hogAFOs$County==Countycounts$County[Countycounts$trees_period1>0.05*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_trees[hogAFOs$County==Countycounts$County[Countycounts$trees_period1>0.05*Countycounts$AFOs]]-3*awarenessincrement,0)
hogAFOs$awarenessrisk_trees[hogAFOs$County==Countycounts$County[Countycounts$trees_period1>0.1*Countycounts$AFOs]]<-max(hogAFOs$awarenessrisk_trees[hogAFOs$County==Countycounts$County[Countycounts$trees_period1>0.1*Countycounts$AFOs]]-4*awarenessincrement,0)

##Change costs to reflect the technology that has already been adopted (installation costs already paid for in previous period) 
#LOW
if (tech=="LOW") {
hogAFOs$cost_segregation_wo_manure[hogAFOs$decision=='segregation']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='segregation'],5,-1.28*4*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='segregation'],type=0)+pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='segregation'],5,-hogAFOs$cost_broadcastsegregation[hogAFOs$decision=='segregation'],type=1)
hogAFOs$cost_impcover_wo_manure[hogAFOs$decision=='impcover']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='impcover'],5,-hogAFOs$cost_broadcastimpcover[hogAFOs$decision=='impcover'],type=1) 
hogAFOs$cost_diet_wo_manure[hogAFOs$decision=='diet']<--pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='diet'],5,-1.42*10*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='diet'],type=0)+pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='diet'],5,-hogAFOs$cost_broadcastdiet[hogAFOs$decision=='diet'],type=1) 
hogAFOs$cost_percover_wo_manure[hogAFOs$decision=='percover']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='percover'],5,-hogAFOs$cost_broadcastpercover[hogAFOs$decision=='percover'],type=1) 
hogAFOs$cost_scrubbers[hogAFOs$decision=='scrubbers']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='scrubbers'],5,-15*4*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='scrubbers'],type=1)
hogAFOs$cost_biofilter[hogAFOs$decision=='biofilter']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='biofilter'],5,-0.175*4*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='biofilter'],type=1)
hogAFOs$cost_trees[hogAFOs$decision=='trees']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='trees'],20,-0.05*4*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='trees'],type=1)
}
#High
if (tech=="HIGH") {
hogAFOs$cost_segregation_wo_manure[hogAFOs$decision=='segregation']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='segregation'],5,-1.5*4*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='segregation'],type=0)+pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='segregation'],5,-hogAFOs$cost_broadcastsegregation[hogAFOs$decision=='segregation'],type=1)
hogAFOs$cost_impcover_wo_manure[hogAFOs$decision=='impcover']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='impcover'],5,-hogAFOs$cost_broadcastimpcover[hogAFOs$decision=='impcover'],type=1) 
hogAFOs$cost_diet_wo_manure[hogAFOs$decision=='diet']<--pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='diet'],5,-1.79*10*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='diet'],type=0)+pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='diet'],5,-hogAFOs$cost_broadcastdiet[hogAFOs$decision=='diet'],type=1) #feed cost is reduced, labor costs increase
hogAFOs$cost_percover_wo_manure[hogAFOs$decision=='percover']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='percover'],5,-hogAFOs$cost_broadcastpercover[hogAFOs$decision=='percover'],type=1) 
hogAFOs$cost_scrubbers[hogAFOs$decision=='scrubbers']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='scrubbers'],5,-20*4*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='scrubbers'],type=1)
hogAFOs$cost_biofilter[hogAFOs$decision=='biofilter']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='biofilter'],5,-0.525*4*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='biofilter'],type=1)
hogAFOs$cost_trees[hogAFOs$decision=='trees']<-pv.annuity(hogAFOs$discount_rate[hogAFOs$decision=='trees'],20,-0.05*4*hogAFOs$Swine.Animal.Units[hogAFOs$decision=='trees'],type=1)
}
}
 #close m loop
head(hogAFOsdecisions)
#summary(hogAFOsdecisions)
#write.csv(hogAFOsdecisions,file='farmerdecisionsS1.csv')
#write.csv(hogAFOsdecisions,file='farmerdecisionsS19.csv')
