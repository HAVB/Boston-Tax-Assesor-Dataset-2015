require(sqldf)
require(stringr)
require(tidyr)
require(dplyr)
require(plyr)


# Get the total number of residential buildings – include apartment buildings, 1,2,3,4 family 
# homes, Condo Main buildings
totalResUnits<-sqldf("select CT_ID_10, count(t.R_BLDG_STYL) from taxdata t where t.LU in ('R1','R2','R3','R4','A','CM') GROUP BY CT_ID_10")
colnames(totalResUnits)[2]<-"NumResBuildings"


# get total 1,2,3 Family Homes – use this to compute kitchens per building
# we don’t have data for number of kitchens in Apartment buildings, 4 family homes, and
# condo main buildings
total13ResUnits<-sqldf("select CT_ID_10, count(t.R_BLDG_STYL) from taxdata t where t.LU in ('R1','R2','R3') GROUP BY CT_ID_10")
colnames(total13ResUnits)[2]<-"NumResBuildings"

# Get our count of triple deckers
tripleDeckers<-sqldf("select CT_ID_10, count(t.R_BLDG_STYL) from taxdata t where t.R_BLDG_STYL=='DK'
                     and t.LU in ('R1','R2','R3','R4','A','CM')
                     GROUP BY CT_ID_10")
colnames(tripleDeckers)[2]<-"NumTripleDeckers"

# Now get percent of triple deckers
tripleDeckers2<-sqldf("select t.CT_ID_10, 1.0 * t.NumTripleDeckers/tt.NumResBuildings as Percent3Deck
                      from tripleDeckers t,
                      totalResUnits tt
                      where t.CT_ID_10=tt.CT_ID_10")

# Get count of kitchens
kitchens<-sqldf("select CT_ID_10, sum(t.R_KITCH) from taxdata t where t.LU in ('R1','R2','R3') GROUP BY CT_ID_10")
colnames(kitchens)[2]<-"NumKitchens"

# Now get kitchens per building
kitchens2<-sqldf("select t.CT_ID_10, 1.0 * NumKitchens/tt.NumResBuildings as KitchensPerBuilding
                 from kitchens t,
                 total13ResUnits tt
                 where t.CT_ID_10=tt.CT_ID_10")

# Fudge the year remodeled to make it easier to filter on with sqldf
taxdata$YR_REMOD<-ifelse(is.na(taxdata$YR_REMOD), 0, taxdata$YR_REMOD)

# Get number of not remodeled homes
notRemodeled<-sqldf("select CT_ID_10, count(t.YR_REMOD) from taxdata t where t.YR_REMOD=0 and t.LU in ('R1','R2','R3','R4','A','CM') GROUP BY CT_ID_10")
colnames(notRemodeled)[2]<-"NumNotRemodled"
# Get percent of not remodeled homes
notRemodeled2<-sqldf("select t.CT_ID_10, 1.0 * NumNotRemodled/tt.NumResBuildings as PercentNotRemodeled
                     from notRemodeled t,
                     totalResUnits tt
                     where t.CT_ID_10=tt.CT_ID_10")

# Same thing here – make sure year built is NA instead of 0
taxdata$YR_BUILT<-ifelse(taxdata$YR_BUILT==0, NA, taxdata$YR_BUILT)

# Get count of structures built before 1940
before1940<-sqldf("select CT_ID_10, count(t.YR_BUILT) from taxdata t where t.YR_BUILT<1940 and t.LU in ('R1','R2','R3','R4','A','CM') GROUP BY CT_ID_10")
colnames(before1940)[2]<-"NumBefore1940"

# Get Percent of Structures built before 1940
before19402<-sqldf("select t.CT_ID_10, 1.0 * NumBefore1940/tt.NumResBuildings as PercentBefore1940
                   from before1940 t,
                   totalResUnits tt
                   where t.CT_ID_10=tt.CT_ID_10")

# Read in Census Tract data
tracts=read.table('./data/Tracts/Tract Census Data.tab', sep="\t", header=TRUE)
tracts2=read.table('./data/Tracts/Tracts_Boston_2015_BARI CSV.tab', sep="\t", header=TRUE)

# Now merge all the data together
all_data<-merge(tracts, tracts2, by='CT_ID_10', all=TRUE)
all_data<-merge(all_data, tripleDeckers2, by='CT_ID_10', all=TRUE)
all_data<-merge(all_data, notRemodeled2, by='CT_ID_10', all=TRUE)
all_data<-merge(all_data, kitchens2, by='CT_ID_10', all=TRUE)
all_data<-merge(all_data, before19402, by='CT_ID_10', all=TRUE)

# Take care of some NAs that are assumed to be zeros—will need to check this
all_data$Percent3Deck<-ifelse(is.na(all_data$Percent3Deck), 0, all_data$Percent3Deck)
all_data$KitchensPerUnits<-ifelse(is.na(all_data$KitchensPerUnits), 0, all_data$KitchensPerUnits)
all_data$PercentBefore1940<-ifelse(is.na(all_data$PercentBefore1940), 0, all_data$PercentBefore1940)
all_data$PercentNotRemodeled<-ifelse(is.na(all_data$PercentNotRemodeled), 0, all_data$PercentNotRemodeled)

# Now add our Fire Index - divide kitchens by 4 to avoid giving too much weight to this 
# variable
all_data$FireRisk<-all_data$Percent3Deck +
  all_data$prentocc +
  all_data$PercentBefore1940 +
  all_data$KitchensPerBuilding/4 +
  all_data$PercentNotRemodeled +
  all_data$ppubassis +
  all_data$propimmigrant

# Merge selected variables into an overall aggregate data frame to use for group portion of project
taxdata_agg<-sqldf("select CT_ID_10, Percent3Deck, KitchensPerBuilding,PercentNotRemodeled
                       from all_data")


## Create Function to find Mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Find Mode Decade Built by Geography -- Swap out Census Tracts (CT_ID_10) to change 
taxdata_agg_modedec <- aggregate(DecadeBuilt~CT_ID_10, data=taxdata, Mode)

taxdata_agg_modedec <- rename(taxdata_agg_modedec, c("DecadeBuilt"="Mod_Dec_Built"))

taxdata_agg<-merge(taxdata_agg, taxdata_agg_modedec, by="CT_ID_10")

## Find Mode Decade remodeled by Geography -- Swap out Census Tracts (CT_ID_10) to change 
taxdata_agg_modedec <- aggregate(DecadeRemod~CT_ID_10, data=taxdata, Mode)

taxdata_agg_modedec <- rename(taxdata_agg_modedec, c("DecadeRemod"="Mod_Dec_Remod"))

taxdata_agg<-merge(taxdata_agg, taxdata_agg_modedec, by="CT_ID_10")


# Calculate the land use diversity index

taxdata_agg.lu.index <- summarise(group_by(filter(taxdata, !is.na(CT_ID_10)), CT_ID_10),
                         LAND_USE_DIV = diversity(table(SIMPLIFIED_LU)))


#Create a home value dispersion index, by Census tract

taxdata_agg.av.dispersion <- summarise(group_by(filter(taxdata, !is.na(CT_ID_10), HOME == 1), 
                                                CT_ID_10),
                                       HOME_VALUE_DISP = var(BLDG_RANK, na.rm = TRUE)/
                                mean(BLDG_RANK, na.rm = TRUE))


taxdata_agg<-merge(taxdata_agg, taxdata_agg.lu.index, by="CT_ID_10", all.x = TRUE)
taxdata_agg<-merge(taxdata_agg, taxdata_agg.hv.index, by="CT_ID_10", all.x = TRUE)


## Aggregate Heat, Cool, Age and EE Score at Tract Level
heat.mean <- aggregate(HEAT_SCORE~CT_ID_10, data = PTax_R_EE,mean, na.rm = TRUE)
age.mean <- aggregate(AGE_SCORE~CT_ID_10, data = PTax_R_EE,mean, na.rm = TRUE)
cool.mean <- aggregate(COOL_SCORE~CT_ID_10, data = PTax_R_EE,mean, na.rm = TRUE)
EE.mean <- aggregate(EE_SCORE~CT_ID_10, data = PTax_R_EE,mean, na.rm = TRUE)
View(EE.mean)

## Aggregate building and total value per gross area
bldg.mean <- aggregate(AV_BLDG_PER_SF~CT_ID_10, data = PTax_R_EE,mean, na.rm = TRUE)
value.mean <- aggregate(TOTAL_PER_SF~CT_ID_10, data = PTax_R_EE,mean, na.rm = TRUE)

##merge tract level scores
taxdata_agg <- merge(taxdata_agg, heat.mean, by='CT_ID_10',all.x=TRUE)
taxdata_agg <- merge(taxdata_agg, age.mean, by='CT_ID_10',all.x=TRUE)
taxdata_agg <- merge(taxdata_agg,cool.mean,by= 'CT_ID_10',all.x=TRUE)
taxdata_agg <- merge(taxdata_agg,EE.mean,by= 'CT_ID_10',all.x=TRUE)
taxdata_agg <- merge(taxdata_agg,bldg.mean,by= 'CT_ID_10', all.x = TRUE)
taxdata_agg <- merge(taxdata_agg,value.mean,by= 'CT_ID_10', all.x = TRUE)

