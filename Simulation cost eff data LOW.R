#Should I add reverse osmosis NH3 filtration to the mix?

############################
#Calculating the emissions from the baseline (no additional mitigation scenario)
############################
#Import hog farm data
setwd('C:/Users/kendrick/Documents/Dissertation Workshop/Data/Facility listings')
hogAFOs<-read.csv("Hogs_only_StorageStructAnimalUnit_for_R.csv",header=TRUE,skipNul=TRUE)  #From Iowa DNR AFO listing
hogAFOs$Storage_type<-'outdoor'
hogAFOs$Storage_type[hogAFOs$Lagoon.anaerobic=="Yes"]<-'lagoon' #anaerobic lagoons only
hogAFOs$Storage_type[hogAFOs$Below.Buildings.pits.Deep=="Yes"]<-'pit'
hogAFOs$Storage_type[hogAFOs$Storage_type=='outdoor'&hogAFOs$Below.Buildings.pits=="Yes"]<-'pit'
hogAFOs$Storage_type<-as.factor(hogAFOs$Storage_type)

set.seed(19)
#Calculate emissions for each farm (baseline and mitigated)
#(farm inputs are number of animals, housing type, storage type, application type)
excretion<-21.1*17/14 #lbs NH3/head/yr (17/14 conversion factor to ammonia weight)
#excretion will be different for diet manipulation
hogAFOs$housingbaseline<-7.3 #lbs NH3/head/yr (below.Building.pits is default for those with missing data)
hogAFOs$housingbaseline[!(hogAFOs$Storage_type=='pit')]<-6.0 #lbs NH3/head/yr
hogAFOs$storagebaseline<-0  #lbs NH3/head/yr (below.Building.pits is default for those with missing data)

#Different possible sizes of basin/concrete/slurry store: 8ft, 12ft, 16ft depth
#1 mature hog equivalent to 0.25 animal units
hogAFOs$manurevolume<-hogAFOs$Swine.Animal.Units*1000*.001066666*350 #AU*weight of 1 AU*manure volume per pound of pig per day in cubic feet*350 days
#http://agrienvarchive.ca/bioenergy/facts.html#Volumes_&_Amounts
hogAFOs$manureSA8<-hogAFOs$manurevolume/8 #Manure surface area in square feet if pit can be filled to 8 ft depth #All assume that manure is stored such that it fills all storage tanks.  Other way to calculate would be to divide the total by the volume of the average storage tank and shape to see how many tanks would be used
hogAFOs$manureSA12<-hogAFOs$manurevolume/12 #Manure surface area in square feet if pit can be filled to 12 ft depth
hogAFOs$manureSA16<-hogAFOs$manurevolume/16 #Manure surface area in square feet if pit can be filled to 16 ft depth
#slurry storage emission rate is 0.8 to 2.5 g NH3/meter squared/day (Balsari, et al., 2007) Italian data
##slurry storage emission rate is 0.8*10.7639*.00220462 to 2.5*10.7639*.00220462 lb NH3/square foot/day (Balsari, et al., 2007) Italian data
#Toggle between SA8,12,16 based on animal units
#Above should be relevant but gives emission rates that are WAY larger than the actual nutrient content
hogAFOs$landappbaselinerand<-0.2
hogAFOs$landappbaselinerand[hogAFOs$Swine.Animal.Units<500]<-0.23
hogAFOs$storagebaselinerand<-0.2
hogAFOs$storagebaselinerand[hogAFOs$Storage_type=='lagoon']<-0.71
hogAFOs$storagebaselinerand[hogAFOs$Storage_type=='pit']<-0
hogAFOs$storagebaseline<-(excretion-hogAFOs$housingbaseline)*hogAFOs$storagebaselinerand
hogAFOs$landappbaseline<- (excretion-hogAFOs$housingbaseline-hogAFOs$storagebaseline)*hogAFOs$landappbaselinerand
#The above formula will be incorrect for scrubbers, biofilter, and landscaping
hogAFOs$baseemission<-hogAFOs$housingbaseline+hogAFOs$storagebaseline+hogAFOs$landappbaseline
#Check average against published data of 6-7 kg NH3"N/yr"animal place (Arogo, 2003)
#summary(hogAFOs) #lbs NH3/yr/  #Median 11.23, mean 11.81, max 21.45
6*17/14*2.2 #~16 lower avg
7*17/14*2.2 #~19 upper avg
#Close enough to reported emission factors...

#Total baseline emissions for each farm
hogAFOs$totalbaseemission<-hogAFOs$baseemission*hogAFOs$Swine.Animal.Units*4
#################333
#Calculate efficiency of strategies
######################
#Scrubbers
hogAFOs$scrubbersrand<-0.7
hogAFOs$housingscrubbers<-(1-hogAFOs$scrubbersrand)*hogAFOs$housingbaseline
hogAFOs$totalscrubbers<-hogAFOs$housingscrubbers+hogAFOs$storagebaseline+hogAFOs$landappbaseline

#Urine/feces segregation
hogAFOs$segregationrand<-0.4
hogAFOs$housingsegregation<-(1-hogAFOs$segregationrand)*hogAFOs$housingbaseline
hogAFOs$storagesegregation<-(1-hogAFOs$segregationrand)*(excretion-hogAFOs$housingsegregation)*hogAFOs$storagebaselinerand
hogAFOs$landappsegregation<-(1-hogAFOs$segregationrand)*(excretion-hogAFOs$housingsegregation-hogAFOs$storagesegregation)*hogAFOs$landappbaselinerand
hogAFOs$totalsegregation<-hogAFOs$housingsegregation+hogAFOs$storagesegregation+hogAFOs$landappsegregation
#Check this against 40%-80% overall reduction, which might be more accurate

#acidification
hogAFOs$acidrand<-0.5
hogAFOs$housingacid<-(1-hogAFOs$acidrand)*hogAFOs$housingbaseline
hogAFOs$housingacid[!(hogAFOs$Storage_type=='pit')]<-hogAFOs$housingbaseline[!(hogAFOs$Storage_type=='pit')]
hogAFOs$storageacid<-(1-hogAFOs$acidrand)*(excretion-hogAFOs$housingacid)*hogAFOs$storagebaselinerand
#Unsure if acid can be used in deep pit operations like above lines suggest
hogAFOs$landappacid<-(1-hogAFOs$acidrand)*(excretion-hogAFOs$housingacid-hogAFOs$storageacid)*hogAFOs$landappbaselinerand
hogAFOs$totalacid<-hogAFOs$housingacid+hogAFOs$storageacid+hogAFOs$landappacid

#impermeable cover
hogAFOs$impcoverrand<-0.85
hogAFOs$storageimpcover<-(1-hogAFOs$impcoverrand)*hogAFOs$storagebaseline
hogAFOs$landappimpcover<-(excretion-hogAFOs$housingbaseline-hogAFOs$storageimpcover)*hogAFOs$landappbaselinerand
hogAFOs$totalimpcover<-hogAFOs$housingbaseline+hogAFOs$storageimpcover+hogAFOs$landappimpcover
hogAFOs$totalimpcover[hogAFOs$Storage_type=='pit']<-0

#direct injection
hogAFOs$injectrand<-0.2
hogAFOs$landappinject<-(excretion-hogAFOs$housingbaseline-hogAFOs$storagebaseline)*hogAFOs$landappbaselinerand*(1-hogAFOs$injectrand)
hogAFOs$totalinject<-hogAFOs$housingbaseline+hogAFOs$storagebaseline+hogAFOs$landappinject

#biofilter
hogAFOs$biofilterrand<-0.45
hogAFOs$housingbiofilter<-(1-hogAFOs$biofilterrand)*hogAFOs$housingbaseline
hogAFOs$totalbiofilter<-hogAFOs$housingbiofilter+hogAFOs$storagebaseline+hogAFOs$landappbaseline

#diet manipulation
hogAFOs$dietrand<-0.2
hogAFOs$totaldiet<-(1-hogAFOs$dietrand)*hogAFOs$baseemission

#landscaping
hogAFOs$treesrand<-0
hogAFOs$housingtrees<-(1-hogAFOs$treesrand)*hogAFOs$housingbaseline
hogAFOs$storagetrees<-(1-hogAFOs$treesrand)*hogAFOs$storagebaseline
hogAFOs$totaltrees<-hogAFOs$housingtrees+hogAFOs$storagetrees+hogAFOs$landappbaseline

#permeable cover
hogAFOs$percoverrand<-0.3
hogAFOs$storagepercover<-(1-hogAFOs$percoverrand)*hogAFOs$storagebaseline
hogAFOs$landapppercover<-(excretion-hogAFOs$housingbaseline-hogAFOs$storagepercover)*hogAFOs$landappbaselinerand
hogAFOs$totalpercover<-hogAFOs$housingbaseline+hogAFOs$storagepercover+hogAFOs$landapppercover
hogAFOs$totalpercover[hogAFOs$Storage_type=='pit']<-0

#reverse osmosis


summary(hogAFOs)
unique(hogAFOs$totalpercover) #5
unique(hogAFOs$totaltrees) #6
unique(hogAFOs$totaldiet)  #6
unique(hogAFOs$totalbiofilter) #6
unique(hogAFOs$totalinject) #6
unique(hogAFOs$totalimpcover) #5
unique(hogAFOs$totalacid) #4 #6
unique(hogAFOs$totalsegregation) #6 
unique(hogAFOs$totalscrubbers) #6  
unique(hogAFOs$baseemission) #6
unique(hogAFOs$baseemission[hogAFOs$Storage_type=='pit']) #2
unique(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon']) #2
unique(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor']) #2
length(hogAFOs$baseemission[hogAFOs$Storage_type=='pit']) #2
length(hogAFOs$baseemission[hogAFOs$Storage_type=='pit']) #2
length(hogAFOs$baseemission[hogAFOs$Storage_type=='pit']) #2


####Effectiveness 
#Average value over all operations
#scrubber 6.223
#urine segregation 5.805
#acidification 8.461
#imperm cover 11.010
#direct injection 9.676
#biofilter 7.614
#diet manipulation 8.820
#landscaping/trees 9.765
#permeable cover 11.27

summary(hogAFOs[hogAFOs$Below.Buildings.pits=="No"& hogAFOs$Below.Buildings.pits.Deep=="No",])
####Average Effectiveness for non-deep/below pit structures
#base 12.92                   
#scrubber 8.112            37.21% reduction
#urine segregation 5.490   57.51% reduction
#acidification 11.10       14.09% reduction
#imperm cover 10.433       19.25% reduction
#direct injection 11.000   14.86% reduction
#biofilter 9.327           27.81% reduction
#diet manipulation 9.680   25.08% reduction
#landscaping/trees 10.564  18.24% reduction
#permeable cover 11.31     12.46% reduction

summary(hogAFOs[hogAFOs$Below.Buildings.pits=="No"& hogAFOs$Below.Buildings.pits.Deep=="No" & hogAFOs$Swine.Animal.Units<500,])
####Average Effectiveness for small operations with non-deep/below pit structures
#base 12.24                   
#scrubber 7.413            39.44% reduction
#urine segregation 5.561   54.57% reduction
#acidification 10.88       11.11% reduction
#imperm cover 10.376       15.23% reduction
#direct injection 10.182   16.81% reduction
#biofilter 8.650           29.33% reduction
#diet manipulation 9.163   25.14% reduction
#landscaping/trees 9.914   19.00% reduction
#permeable cover 11.05     9.72$ reduction

summary(hogAFOs[!(hogAFOs$Below.Buildings.pits=="No"& hogAFOs$Below.Buildings.pits.Deep=="No"),])
####Average Effectiveness for deep/below pit structures
#base 11.29
#scrubber 5.457            51.67% reduction
#urine segregation 5.932   47.46% reduction
#acidification 7.392       34.53% reduction
#imperm cover 11.24        0% reduction
#direct injection 9.140    19.04% reduction
#biofilter 6.920           38.71% reduction
#diet manipulation 8.472   24.96% reduction
#landscaping/trees 9.441   16.38% reduction
#permeable cover 11.26     0% reduction

##########################
#Import mitigation decisions or algorithm for determining which mitigations each farm adopts
############################
#Decision rules

#Cost basis for research question 1

#https://www.pioneer.com/home/site/us/agronomy/library/optimum-nitrogen-rates/
#Figure out how much area would be needed for land application
hogAFOs$remainingN<-excretion-hogAFOs$baseemission  #per head in lbs NH3
hogAFOs$remainingNsegregation<-excretion-hogAFOs$totalsegregation
hogAFOs$remainingNacid<-excretion-hogAFOs$totalacid
hogAFOs$remainingNimpcover<-excretion-hogAFOs$totalimpcover
hogAFOs$remainingNinject<-excretion-hogAFOs$totalinject
hogAFOs$remainingNdiet<-(excretion-hogAFOs$baseemission)*(1-hogAFOs$dietrand)
hogAFOs$remainingNpercover<-excretion-hogAFOs$totalpercover
hogAFOs$spreadarearand<-124 #units lbs N/acre
hogAFOs$spreadarea<-4*hogAFOs$Swine.Animal.Units*hogAFOs$remainingN*(14/17)/hogAFOs$spreadarearand #lb N divided by lb N/acre
hogAFOs$spreadareasegregation<-4*hogAFOs$Swine.Animal.Units*hogAFOs$remainingNsegregation*(14/17)/hogAFOs$spreadarearand #lb N divided by lb N/acre
hogAFOs$spreadareaacid<-4*hogAFOs$Swine.Animal.Units*hogAFOs$remainingNacid*(14/17)/hogAFOs$spreadarearand #lb N divided by lb N/acre
hogAFOs$spreadareaimpcover<-4*hogAFOs$Swine.Animal.Units*hogAFOs$remainingNimpcover*(14/17)/hogAFOs$spreadarearand #lb N divided by lb N/acre
hogAFOs$spreadareadiet<-4*hogAFOs$Swine.Animal.Units*hogAFOs$remainingNdiet*(14/17)/hogAFOs$spreadarearand #lb N divided by lb N/acre
hogAFOs$spreadareapercover<-4*hogAFOs$Swine.Animal.Units*hogAFOs$remainingNpercover*(14/17)/hogAFOs$spreadarearand #lb N divided by lb N/acre
hogAFOs$spreadareainject<-4*hogAFOs$Swine.Animal.Units*hogAFOs$remainingNinject*(14/17)/hogAFOs$spreadarearand #lb N divided by lb N/acre
#$65.58 ($ 2013) from Gray et al. 2014.  Converts to 1.03*65.58 ($ 2016)
hogAFOs$cost_broadcast<-1.03*65.58*hogAFOs$spreadarea
hogAFOs$cost_inject_wo_manure<-1.66*1.03*65.58*hogAFOs$spreadareainject-hogAFOs$cost_broadcast #$ per farm more than baseline emissions with broadcast
hogAFOs$cost_broadcastsegregation<-1.03*65.58*hogAFOs$spreadareasegregation-hogAFOs$cost_broadcast
hogAFOs$cost_broadcastacid<-1.03*65.58*hogAFOs$spreadareaacid-hogAFOs$cost_broadcast
hogAFOs$cost_broadcastimpcover<-1.03*65.58*hogAFOs$spreadareaimpcover-hogAFOs$cost_broadcast
hogAFOs$cost_broadcastdiet<-1.03*65.58*hogAFOs$spreadareadiet-hogAFOs$cost_broadcast
hogAFOs$cost_broadcastpercover<-1.03*65.58*hogAFOs$spreadareapercover-hogAFOs$cost_broadcast
wastevalue<-11.43 #($ 2016) Hora, 2016
hogAFOs$manure_valuebase<-wastevalue*hogAFOs$Swine.Animal.Units*4  #$ per farm
hogAFOs$manure_valuesegregation<-wastevalue*hogAFOs$remainingNsegregation*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase #$ per farm more than baseline emissions with broadcast
hogAFOs$manure_valueacid<-wastevalue*hogAFOs$remainingNacid*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
hogAFOs$manure_valueimpcover<-wastevalue*hogAFOs$remainingNimpcover*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
hogAFOs$manure_valuediet<-wastevalue*hogAFOs$remainingNdiet*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
hogAFOs$manure_valuepercover<-wastevalue*hogAFOs$remainingNpercover*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
hogAFOs$manure_valueinject<-wastevalue*hogAFOs$remainingNinject*hogAFOs$Swine.Animal.Units*4/hogAFOs$remainingN-hogAFOs$manure_valuebase
#(value added to manure by retaining N)

#Calculate waste stream costs and benefits separately from technology equipment

#Figure out how to do present cost of investment and yearly expenses
library(FinCal)
#Farmer discount rate is a function of yes/no contract, age of operation, and farmer's savings/wealth level
#https://academic.oup.com/ajae/article-abstract/94/2/451/56604/Farmer-Discount-Rates-Experimental-Evidence?redirectedFrom=fulltext
#hogAFOs$contract<-sample(c("Yes","No"),1)
#Think about how runif commands should be run for costs.  Are costs random across farms or do they change over model periods?
#What is planning horizon?
#Cost PER OPERATION

hogAFOs$discount_rate<-0.28   #Is this appropriate??? #https://books.google.com/books?id=d57o8ajm6tEC&pg=PA41&lpg=PA41&dq=farmer+discount+rate&source=bl&ots=lgsVrjbeR2&sig=dWj2G3ykAFfd5THEMUnBShMjpZM&hl=en&sa=X&ved=0ahUKEwiv06GQ3YnVAhUJcj4KHVSKDucQ6AEIXTAI#v=onepage&q=farmer%20discount%20rate&f=false
#https://pubs.aeaweb.org/doi/pdf/10.1257/000282802762024674
#Duquette, 2011

#$8,268-9,109 to install belt system for 4,320 head feeder-finish barn
#$1047-1,289 to operate belt system for 4,320 head feeder-finish barn
#https://projects.ncsu.edu/cals/waste_mgt/smithfield_projects/phase2report05/cd,web%20files/B1g.pdf

#5 year horizon
hogAFOs$cost_inject<-pv.annuity(hogAFOs$discount_rate,5,-(1.66*1.03*65.58*hogAFOs$spreadareainject-hogAFOs$cost_broadcast),type=0)-pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$manure_valueinject,type=0)
hogAFOs$cost_scrubbers<-1.09*45*4*hogAFOs$Swine.Animal.Units+1.09*pv.annuity(hogAFOs$discount_rate,5,-15*4*hogAFOs$Swine.Animal.Units,type=1)
hogAFOs$cost_biofilter<-1.47*2.17*4*hogAFOs$Swine.Animal.Units+1.47*pv.annuity(hogAFOs$discount_rate,5,-0.175*4*hogAFOs$Swine.Animal.Units,type=1)
hogAFOs$cost_segregation<-1.27*9.4*4*hogAFOs$Swine.Animal.Units+1.27*pv.annuity(hogAFOs$discount_rate,5,-1.2*4*hogAFOs$Swine.Animal.Units,type=0)+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastsegregation,type=1)-pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$manure_valuesegregation,type=0)
hogAFOs$cost_acid<-1.01*pv.annuity(hogAFOs$discount_rate,5,-6*4*hogAFOs$Swine.Animal.Units,type=1)+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastacid,type=1)-pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$manure_valueacid,type=0)
hogAFOs$cost_impcover<-1.27*1/9*3*hogAFOs$manureSA8+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastimpcover,type=1)-pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$manure_valueimpcover,type=0) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_impcover[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]<-1.27*1/9*3*hogAFOs$manureSA12[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]+pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],5,-hogAFOs$cost_broadcastimpcover[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],type=1)-pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],5,-hogAFOs$manure_valueimpcover[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],type=0) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_impcover[hogAFOs$Swine.Animal.Units>1000]<-1.27*1/9*3*hogAFOs$manureSA16[hogAFOs$Swine.Animal.Units>1000]+pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>1000],5,-hogAFOs$cost_broadcastimpcover[hogAFOs$Swine.Animal.Units>1000],type=1)-pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>1000],5,-hogAFOs$manure_valueimpcover[hogAFOs$Swine.Animal.Units>1000],type=0) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_impcover[hogAFOs$storagebaseline==0]<-NA
hogAFOs$cost_broadcastimpcover[hogAFOs$storagebaseline==0]<-NA
hogAFOs$manure_valueimpcover[hogAFOs$storagebaseline==0]<-NA
hogAFOs$cost_percover<-1.36*0.1*hogAFOs$manureSA8+1.36*pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastpercover,type=1)-pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$manure_valuepercover,type=0) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_percover[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]<-1.36*0.1*hogAFOs$manureSA12[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]+pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],5,-hogAFOs$cost_broadcastpercover[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],type=1)-pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],5,-hogAFOs$manure_valuepercover[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],type=0) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_percover[hogAFOs$Swine.Animal.Units>1000]<-1.36*0.1*hogAFOs$manureSA16[hogAFOs$Swine.Animal.Units>1000]+pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>1000],5,-hogAFOs$cost_broadcastpercover[hogAFOs$Swine.Animal.Units>1000],type=1)-pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>1000],5,-hogAFOs$manure_valuepercover[hogAFOs$Swine.Animal.Units>1000],type=0) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_percover[hogAFOs$storagebaseline==0]<-NA
hogAFOs$cost_broadcastpercover[hogAFOs$storagebaseline==0]<-NA
hogAFOs$manure_valuepercover[hogAFOs$storagebaseline==0]<-NA
hogAFOs$cost_diet<-1.58*11.67*4*hogAFOs$Swine.Animal.Units-1.58*pv.annuity(hogAFOs$discount_rate,5,-1.42*10*hogAFOs$Swine.Animal.Units,type=0)+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastdiet,type=1)-pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$manure_valuediet,type=0) #feed cost is reduced, labor and capital costs increase
hogAFOs$cost_trees<-pv.annuity(hogAFOs$discount_rate,20,-0.05*4*hogAFOs$Swine.Animal.Units,type=1)
#COST STRUCTURE MAY BE DIFFERENT FOR SMALL OPERATIONS
#COST STRUCTURE MAY BE DIFFERENT IF CANNOT SELL MANURE LOCALLY (transportation costs, emissions)

#5 year horizon without considering added manure value
hogAFOs$cost_inject_wo_manure<-pv.annuity(hogAFOs$discount_rate,5,-(1.66*1.03*65.58*hogAFOs$spreadareainject-hogAFOs$cost_broadcast),type=0)
hogAFOs$cost_segregation_wo_manure<-1.27*9.4*hogAFOs$Swine.Animal.Units+1.27*pv.annuity(hogAFOs$discount_rate,5,-1.2*4*hogAFOs$Swine.Animal.Units,type=0)+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastsegregation,type=1)
hogAFOs$cost_acid_wo_manure<-1.01*pv.annuity(hogAFOs$discount_rate,5,-6*4*hogAFOs$Swine.Animal.Units,type=1)+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastacid,type=1)
hogAFOs$cost_impcover_wo_manure<-1.27*1/9*3*hogAFOs$manureSA8+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastimpcover,type=1) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_impcover_wo_manure[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]<-1.27*1/9*3*hogAFOs$manureSA12[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]+pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],5,-hogAFOs$cost_broadcastimpcover[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],type=1) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_impcover_wo_manure[hogAFOs$Swine.Animal.Units>1000]<-1.27*1/9*3*hogAFOs$manureSA16[hogAFOs$Swine.Animal.Units>1000]+pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>1000],5,-hogAFOs$cost_broadcastimpcover[hogAFOs$Swine.Animal.Units>1000],type=1) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_impcover_wo_manure[hogAFOs$storagebaseline==0]<-NA
hogAFOs$cost_percover_wo_manure<-1.36*0.1*hogAFOs$manureSA8+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastpercover,type=1) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_percover_wo_manure[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]<-1.36*0.1*hogAFOs$manureSA12[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]+pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],5,-hogAFOs$cost_broadcastpercover[hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001],type=1) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_percover_wo_manure[hogAFOs$Swine.Animal.Units>1000]<-1.36*0.1*hogAFOs$manureSA16[hogAFOs$Swine.Animal.Units>1000]+pv.annuity(hogAFOs$discount_rate[hogAFOs$Swine.Animal.Units>1000],5,-hogAFOs$cost_broadcastpercover[hogAFOs$Swine.Animal.Units>1000],type=1) #Or 12,16,20,24 depending on AFO size
hogAFOs$cost_percover_wo_manure[hogAFOs$storagebaseline==0]<-NA
hogAFOs$cost_diet_wo_manure<-1.58*11.67*4*hogAFOs$Swine.Animal.Units-1.58*pv.annuity(hogAFOs$discount_rate,5,-1.42*10*hogAFOs$Swine.Animal.Units,type=0)+pv.annuity(hogAFOs$discount_rate,5,-hogAFOs$cost_broadcastdiet,type=1) #feed cost is reduced, labor and capital costs increase


#COST STRUCTURE MAY BE DIFFERENT FOR SMALL OPERATIONS
#COST STRUCTURE MAY BE DIFFERENT IF CANNOT SELL MANURE LOCALLY (transportation costs, emissions)
hogAFOs$cost_base<-0

#Segregation is cost-beneficial in the first year if you consider manure value for more than 25% of outdoor storage operations 
#Segregation is not cost-beneficial in the first year if you consider manure value for more than 75% of pit operations 
#Becomes cost-beneficial for more than 75% overall by the third year
#Seems ideal for a loan or cost-share  program

#Cost per AU (~4 hogs)
hogAFOs$cost_scrubbers_wo_manure_perAU<-hogAFOs$cost_scrubbers/hogAFOs$Swine.Animal.Units
hogAFOs$cost_biofilter_wo_manure_perAU<-hogAFOs$cost_biofilter/hogAFOs$Swine.Animal.Units
hogAFOs$cost_segregation_wo_manure_perAU<-hogAFOs$cost_segregation_wo_manure/hogAFOs$Swine.Animal.Units
hogAFOs$cost_acid_wo_manure_perAU<-hogAFOs$cost_acid_wo_manure/hogAFOs$Swine.Animal.Units
hogAFOs$cost_impcover_wo_manure_perAU<-hogAFOs$cost_impcover_wo_manure/hogAFOs$Swine.Animal.Units
hogAFOs$cost_inject_wo_manure_perAU<-hogAFOs$cost_inject_wo_manure/hogAFOs$Swine.Animal.Units
hogAFOs$cost_percover_wo_manure_perAU<-hogAFOs$cost_percover_wo_manure/hogAFOs$Swine.Animal.Units
hogAFOs$cost_diet_wo_manure_perAU<-hogAFOs$cost_diet_wo_manure/hogAFOs$Swine.Animal.Units
hogAFOs$cost_trees_wo_manure_perAU<-hogAFOs$cost_trees/hogAFOs$Swine.Animal.Units
hogAFOs$cost_inject_wo_manure_perAU<-hogAFOs$cost_inject_wo_manure/hogAFOs$Swine.Animal.Units

hogAFOs$cost_scrubbers_perAU<-hogAFOs$cost_scrubbers/hogAFOs$Swine.Animal.Units
hogAFOs$cost_biofilter_perAU<-hogAFOs$cost_biofilter/hogAFOs$Swine.Animal.Units
hogAFOs$cost_segregation_perAU<-hogAFOs$cost_segregation/hogAFOs$Swine.Animal.Units
hogAFOs$cost_acid_perAU<-hogAFOs$cost_acid/hogAFOs$Swine.Animal.Units
hogAFOs$cost_impcover_perAU<-hogAFOs$cost_impcover/hogAFOs$Swine.Animal.Units
hogAFOs$cost_inject_perAU<-hogAFOs$cost_inject/hogAFOs$Swine.Animal.Units
hogAFOs$cost_percover_perAU<-hogAFOs$cost_percover/hogAFOs$Swine.Animal.Units
hogAFOs$cost_diet_perAU<-hogAFOs$cost_diet/hogAFOs$Swine.Animal.Units
hogAFOs$cost_trees_perAU<-hogAFOs$cost_trees/hogAFOs$Swine.Animal.Units
hogAFOs$cost_inject_perAU<-hogAFOs$cost_inject/hogAFOs$Swine.Animal.Units

hogAFOs$totalbase<-hogAFOs$baseemission
write.csv(hogAFOs,"completehogsimulationdataLOW_May_4_2018.csv")
#write.csv(hogAFOs,"optimizationtableforexcelLOW.csv")

############################################################################################
#Make choice based on cost and effectiveness over whole farm (multiply by 4*Swine.Animal.Units)
#Average effectiveness for operation type and size
Comparison<-data.frame('operation_type'=c('pit','pit','pit','outdoor.storage','outdoor.storage','outdoor.storage','anaerobic.lagoon','anaerobic.lagoon','anaerobic.lagoon'),'operation_size'=c('small','medium','large','small','medium','large','small','medium','large'))
Comparison$Avgeffectiveness_scrubbers<-0
Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_segregation<-0
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_acid<-0
Comparison$Avgeffectiveness_acid[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_impcover<-0
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_inject<-0
Comparison$Avgeffectiveness_inject[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_biofilter<-0
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_diet<-0
Comparison$Avgeffectiveness_diet[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_trees<-0
Comparison$Avgeffectiveness_trees[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_percover<-0
Comparison$Avgeffectiveness_percover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_acid[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_inject[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_diet[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_trees[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_percover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_acid[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_inject[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_diet[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_trees[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_percover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])

Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_acid[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_inject[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_diet[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_trees[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_percover[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_acid[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_inject[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_diet[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_trees[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_percover[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_acid[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_inject[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_diet[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_trees[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_percover[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])

Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_acid[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500], na.rm=TRUE))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500], na.rm=TRUE)
Comparison$Avgeffectiveness_inject[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_diet[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_trees[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgeffectiveness_percover[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='small']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units<500])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units<500]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_acid[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_inject[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_diet[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_trees[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgeffectiveness_percover[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='medium']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgeffectiveness_scrubbers[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalscrubbers[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_segregation[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalsegregation[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_acid[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalacid[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_impcover[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalimpcover[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_inject[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalinject[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_biofilter[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalbiofilter[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_diet[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totaldiet[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_trees[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totaltrees[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgeffectiveness_percover[Comparison$operation_type=="anaerobic.lagoon"&Comparison$operation_size=='large']<- -(mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])-mean(hogAFOs$totalpercover[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000]))/mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])

Comparison$Avgcost_scrubbers<-0
Comparison$Avgcost_scrubbers[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_segregation<-0
Comparison$Avgcost_segregation[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_acid<-0
Comparison$Avgcost_acid[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_impcover<-0
Comparison$Avgcost_impcover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_inject<-0
Comparison$Avgcost_inject[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_biofilter<-0
Comparison$Avgcost_biofilter[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_diet<-0
Comparison$Avgcost_diet[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_trees<-0
Comparison$Avgcost_trees[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_percover<-0
Comparison$Avgcost_percover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgcost_scrubbers[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_segregation[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_acid[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_impcover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_inject[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_biofilter[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_diet[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_trees[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_percover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgcost_scrubbers[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_segregation[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_acid[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_impcover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_inject[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_biofilter[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_diet[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_trees[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_percover[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])

Comparison$Avgcost_scrubbers[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_segregation[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_acid[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_impcover[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_inject[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_biofilter[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_diet[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_trees[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_percover[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgcost_scrubbers[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_segregation[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_acid[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_impcover[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_inject[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_biofilter[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_diet[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_trees[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_percover[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgcost_scrubbers[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_segregation[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_acid[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_impcover[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_inject[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_biofilter[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_diet[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_trees[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_percover[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])

Comparison$Avgcost_scrubbers[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_segregation[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_acid[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_impcover[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_inject[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_biofilter[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_diet[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_trees[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_percover[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgcost_scrubbers[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_segregation[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_acid[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_impcover[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_inject[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_biofilter[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_diet[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_trees[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_percover[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgcost_scrubbers[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_scrubbers_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_segregation[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_segregation_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_acid[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_acid_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_impcover[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_impcover_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_inject[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_inject_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_biofilter[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_biofilter_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_diet[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_diet_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_trees[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_trees_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_percover[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_percover_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])

Comparison$Avgcost_scrubbers_wo_manure<-0
Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_segregation_wo_manure<-0
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_acid_wo_manure<-0
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_impcover_wo_manure<-0
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_inject_wo_manure<-0
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_biofilter_wo_manure<-0
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_diet_wo_manure<-0
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_trees_wo_manure<-0
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_percover_wo_manure<-0
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])


Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])

Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units<500])
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units<500])

Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])

Comparison$Avgcost_scrubbers_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_scrubbers_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_segregation_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_segregation_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_acid_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_acid_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_impcover_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_impcover_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_inject_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_inject_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_biofilter_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_biofilter_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_diet_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_diet_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_trees_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_trees_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avgcost_percover_wo_manure[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$cost_percover_wo_manure_perAU[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])

Comparison$Avg.swine.units[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units<500])
Comparison$Avg.swine.units[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avg.swine.units[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avg.swine.units[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units<500])
Comparison$Avg.swine.units[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avg.swine.units[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Avg.swine.units[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units<500])
Comparison$Avg.swine.units[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Avg.swine.units[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$Swine.Animal.Units[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])

Comparison$No.operations[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- length(which(hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units<500))
Comparison$No.operations[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- length(which(hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001))
Comparison$No.operations[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- length(which(hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000))
Comparison$No.operations[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- length(which(hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units<500))
Comparison$No.operations[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- length(which(hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001))
Comparison$No.operations[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- length(which(hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000))
Comparison$No.operations[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- length(which(hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units<500))
Comparison$No.operations[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- length(which(hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001))
Comparison$No.operations[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- length(which(hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000))

Comparison$Baseemission[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='small']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units<500])
Comparison$Baseemission[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='medium']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Baseemission[Comparison$operation_type=='outdoor.storage'&Comparison$operation_size=='large']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='outdoor'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Baseemission[Comparison$operation_type=='pit'&Comparison$operation_size=='small']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units<500])
Comparison$Baseemission[Comparison$operation_type=='pit'&Comparison$operation_size=='medium']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Baseemission[Comparison$operation_type=='pit'&Comparison$operation_size=='large']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='pit'&hogAFOs$Swine.Animal.Units>1000])
Comparison$Baseemission[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='small']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units<500])
Comparison$Baseemission[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='medium']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'& hogAFOs$Swine.Animal.Units>499.5&hogAFOs$Swine.Animal.Units<1001])
Comparison$Baseemission[Comparison$operation_type=='anaerobic.lagoon'&Comparison$operation_size=='large']<- mean(hogAFOs$baseemission[hogAFOs$Storage_type=='lagoon'&hogAFOs$Swine.Animal.Units>1000])

write.csv(Comparison,file="perAUcomparisonMitigations5yrhorLOW_May_4_2018.csv")

