PTax <- read.csv('Tax Assessor 2015 - Data.csv', stringsAsFactors = FALSE)

## Residential subset
PTax_R_EE<- PTax[PTax$LU =='R1'| PTax$LU == 'R2'| PTax$LU == 'R3'| PTax$LU == 'R4'| PTax$LU == 'A'| PTax$LU == 'RC',]

## Allocating Residential Heat Type Energy Efficiency Score
PTax_R_EE$HEAT_SCORE <- NA
PTax_R_EE$HEAT_SCORE <- ifelse(PTax_R_EE$R_HEAT_TYP == 'S', 0, PTax_R_EE$HEAT_SCORE)
PTax_R_EE$HEAT_SCORE <- ifelse(PTax_R_EE$R_HEAT_TYP == 'W', 1, PTax_R_EE$HEAT_SCORE)
PTax_R_EE$HEAT_SCORE <- ifelse(PTax_R_EE$R_HEAT_TYP == 'P', 2, PTax_R_EE$HEAT_SCORE)
PTax_R_EE$HEAT_SCORE <- ifelse(PTax_R_EE$R_HEAT_TYP == 'F', 3, PTax_R_EE$HEAT_SCORE)
PTax_R_EE$HEAT_SCORE <- ifelse(PTax_R_EE$R_HEAT_TYP == 'E', 4, PTax_R_EE$HEAT_SCORE) 

## Allocating age to Residential buildings
PTax_R_EE$YR_BUILT <- ifelse(PTax_R_EE$YR_BUILT == '',NA,PTax_R_EE$YR_BUILT)
PTax_R_EE$YR_BUILT <- ifelse(PTax_R_EE$YR_BUILT == 0,NA,PTax_R_EE$YR_BUILT)
PTax_R_EE$YR_REMOD <- ifelse(PTax_R_EE$YR_REMOD == '',NA,PTax_R_EE$YR_REMOD)
PTax_R_EE$YR_REMOD <- ifelse(PTax_R_EE$YR_REMOD == 0,NA,PTax_R_EE$YR_REMOD)
PTax_R_EE$BLDG_AGE <- ifelse(is.na(PTax_R_EE$YR_REMOD), (2015 - PTax_R_EE$YR_BUILT), (2015 - PTax_R_EE$YR_REMOD))
PTax_R_EE$BLDG_AGE <- ifelse(PTax_R_EE$BLDG_AGE <=0,NA,PTax_R_EE$BLDG_AGE)

##Allocating Building Age Score
PTax_R_EE$AGE_SCORE <- NA
PTax_R_EE$AGE_SCORE <- ifelse(PTax_R_EE$BLDG_AGE < 50,4,PTax_R_EE$AGE_SCORE)
PTax_R_EE$AGE_SCORE <- ifelse(PTax_R_EE$BLDG_AGE >= 50,3,PTax_R_EE$AGE_SCORE)
PTax_R_EE$AGE_SCORE <- ifelse(PTax_R_EE$BLDG_AGE >= 100,2,PTax_R_EE$AGE_SCORE)
PTax_R_EE$AGE_SCORE <- ifelse(PTax_R_EE$BLDG_AGE >= 150,1,PTax_R_EE$AGE_SCORE)
PTax_R_EE$AGE_SCORE <- ifelse(PTax_R_EE$BLDG_AGE >= 200,0,PTax_R_EE$AGE_SCORE)

## Allocating Residential Air Conditioner Energy Efficiency Score
PTax_R_EE$COOL_SCORE <- NA
PTax_R_EE$COOL_SCORE <- ifelse(PTax_R_EE$R_AC == 'C', 1, PTax_R_EE$COOL_SCORE)
PTax_R_EE$COOL_SCORE <- ifelse(PTax_R_EE$R_AC == 'D', 2, PTax_R_EE$COOL_SCORE)
PTax_R_EE$COOL_SCORE <- ifelse(PTax_R_EE$R_AC == 'N', 3, PTax_R_EE$COOL_SCORE)

## Aggregate Energy Efficiency score at building level
PTax_R_EE$EE_SCORE <- PTax_R_EE$AGE_SCORE+0.75*PTax_R_EE$HEAT_SCORE+0.75*PTax_R_EE$COOL_SCORE

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
tract.score1 <- merge(heat.mean, age.mean, by='CT_ID_10',all.x=TRUE)
tract.score2 <- merge(tract.score1,cool.mean,by= 'CT_ID_10',all.x=TRUE)
tract.score3 <- merge(tract.score2,EE.mean,by= 'CT_ID_10',all.x=TRUE)
tract.score4 <- merge(tract.score3,bldg.mean,by= 'CT_ID_10', all.x = TRUE)
tract.score5 <- merge(tract.score4,value.mean,by= 'CT_ID_10', all.x = TRUE)

#Add a column with a 1 to 10 ranking (decile) for the parcel's sq foot value
library(dplyr)

PTax <- mutate(PTax,
                 BLDG_RANK = ceiling(rank(AV_BLDG_PER_SF,na.last="keep")/ 
                                       length(which(!is.na(AV_BLDG_PER_SF)))*10) )

# fix a single parcel in the entire dataset with LU = "XX", 
#a land use code that is not registered in the datasat dictionary. 
#Since the parcel is owned by a church, it's assumed a Tax Exempt parcel.

PTax[PTax$LU == 'XX',]$LU <- "E"

# Reduce land use categories

simplify_LU <- function(LU) {
  if (LU %in% c("R1", "R2", "R3", "R4", "RL", "A")) {
    return("RESIDENTIAL")
  } else if (LU %in% c("CM", "CP")) {
    return("CONDO")
  } else if (LU == "CD") {
    return("CONDO_UNIT")
  } else if (LU == "RC") {
    return("MIX_RC")
  } else if (LU %in% c("CC", "C", "CL")) {
    return("COMMERCIAL")
  } else if (LU == "AH") {
    return("AGRICULTURAL")
  } else if (LU == "I") {
    return("INDUSTRIAL")
  } else if (LU == "E") {
    return("TAX_EXEMPT")
  } else if (LU == "EA") {
    return("TAX_EXEMPT_BRA")
  } else {
    return(NA)
  }
}

#Create a new column by applying the simplifyLU function
PTax <- transform(PTax, SIMPLIFIED_LU = sapply(LU, simplify_LU))

# Identify homes

isHome <- function(SIMPLIFIED_LU) {
  if (SIMPLIFIED_LU %in% c("RESIDENTIAL", "CONDO_UNIT", "MIX_RC")) {
    return(1)
  } else {
    return(0)
  }
}

# Create a new column by applying the isHome function
PTax <- transform(PTax, HOME = sapply(SIMPLIFIED_LU, isHome))

# Calculate indices, by CT and neighborhood

library(vegan)

ct.lu.index <- summarise(group_by(filter(PTax, !is.na(CT_ID_10)), CT_ID_10), 
                         LAND_USE_DIV = diversity(table(SIMPLIFIED_LU)))

nb.lu.index <- summarise(group_by(filter(PTax, !is.na(BRA_PD)), BRA_PD),
                         LAND_USE_DIV = diversity(table(SIMPLIFIED_LU)))

#Create a home value dispersion index, by CT and neighborhood


ct.hv.dispersion <- summarise(group_by(filter(PTax, !is.na(CT_ID_10), HOME == 1), 
                                       CT_ID_10),
                              HOME_VALUE_DISP = var(BLDG_RANK, na.rm = TRUE)/
                                mean(BLDG_RANK, na.rm = TRUE))

nb.hv.dispersion <-summarise(group_by(filter(PTax, !is.na(BRA_PD), HOME == 1), 
                                      BRA_PD),
                             HOME_VALUE_DISP = var(BLDG_RANK, na.rm = TRUE)/
                               mean(BLDG_RANK, na.rm = TRUE))


