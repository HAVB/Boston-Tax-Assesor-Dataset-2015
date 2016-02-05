require(stringr)
require(tidyr)
require(dplyr)
require(sqldf)

# read in the revised data frame from midterm part 1
taxdata <- read.csv("/home/havb/Dropbox/MSUI/Big Data for Cities - PPUA 5262 - 01/R/data/Tax Assessor/TAdata.csv", stringsAsFactors = FALSE)

# Calculate the Fire Risk for Residential Properties

# First, get a subset of residential buildings using the LU type 
resSub<-taxdata[which(taxdata$LU %in% c('R1','R2','R3','R4','A','CM')),]
resSub$R_KITCH<-ifelse(is.na(resSub$R_KITCH),0,resSub$R_KITCH)

# Run the calculation
resSub$R_FIRE_RISK<-(1 * as.integer(resSub$YR_BUILT<1940 & is.na(resSub$YR_REMOD)))+
  (0.5 * as.integer(resSub$R_KITCH>0)) +
  (1 * as.integer(resSub$R_KITCH>1)) +
  (1 * as.integer(resSub$R_KITCH>2)) +
  (1 * as.integer(resSub$R_KITCH>3)) +
  (1 * as.integer(resSub$R_HEAT_TYP=="S"))

# Merge the fire risk back into the taxdata data frame

colnames(taxdata)[1]<-"X1"
colnames(resSub)[1]<-"X1"
taxdata<-sqldf("select t.*, r.R_FIRE_RISK from 
               taxdata t 
               LEFT OUTER JOIN resSub r on r.X1 = t.X1")
colnames(taxdata)[1]<-"X.1"



## create split function that create's new field of first three digits of YR_BUILT
split_decade <- function(df){
  df <- extract(df, 'DecadeBuilt', into=c('Decade', 'Year'), '(.{3})(.{1})') 
}

## create new data frame after running split function
taxdata$DecadeBuilt <- taxdata$YR_BUILT
taxdata <- split_decade(taxdata)

## create new field that includes a "0s" at the end and remove "split" year field
taxdata$DecadeBuilt <- paste0(taxdata$Decade,"0s")
taxdata$Year<-NULL
taxdata$Decade<-NULL

## create split function that create's new field of first three digits of YR_BUILT
split_decade <- function(df){
  df <- extract(df, 'DecadeRemod', into=c('Decade', 'Year'), '(.{3})(.{1})') 
}

## create new data frame after running split function
taxdata$DecadeRemod <- taxdata$YR_Remod
taxdata <- split_decade(taxdata)

## create new field that includes a "0s" at the end and remove "split" year field
taxdata$DecadeRemod <- paste0(taxdata$Decade,"0s")
taxdata$Year<-NULL
taxdata$Decade<-NULL

## Residential subset
taxdata_R_EE<- taxdata[taxdata$LU =='R1'| taxdata$LU == 'R2'| taxdata$LU == 'R3'| taxdata$LU == 'R4'| taxdata$LU == 'A'| taxdata$LU == 'RC',]

## Allocating Residential Heat Type Energy Efficiency Score
taxdata_R_EE$HEAT_SCORE <- NA
taxdata_R_EE$HEAT_SCORE <- ifelse(taxdata_R_EE$R_HEAT_TYP == 'S', 0, taxdata_R_EE$HEAT_SCORE)
taxdata_R_EE$HEAT_SCORE <- ifelse(taxdata_R_EE$R_HEAT_TYP == 'W', 1, taxdata_R_EE$HEAT_SCORE)
taxdata_R_EE$HEAT_SCORE <- ifelse(taxdata_R_EE$R_HEAT_TYP == 'P', 2, taxdata_R_EE$HEAT_SCORE)
taxdata_R_EE$HEAT_SCORE <- ifelse(taxdata_R_EE$R_HEAT_TYP == 'F', 3, taxdata_R_EE$HEAT_SCORE)
taxdata_R_EE$HEAT_SCORE <- ifelse(taxdata_R_EE$R_HEAT_TYP == 'E', 4, taxdata_R_EE$HEAT_SCORE) 

## Allocating age to Residential buildings
taxdata_R_EE$YR_BUILT <- ifelse(taxdata_R_EE$YR_BUILT == '',NA,taxdata_R_EE$YR_BUILT)
taxdata_R_EE$YR_BUILT <- ifelse(taxdata_R_EE$YR_BUILT == 0,NA,taxdata_R_EE$YR_BUILT)
taxdata_R_EE$YR_REMOD <- ifelse(taxdata_R_EE$YR_REMOD == '',NA,taxdata_R_EE$YR_REMOD)
taxdata_R_EE$YR_REMOD <- ifelse(taxdata_R_EE$YR_REMOD == 0,NA,taxdata_R_EE$YR_REMOD)
taxdata_R_EE$BLDG_AGE <- ifelse(is.na(taxdata_R_EE$YR_REMOD), (2015 - taxdata_R_EE$YR_BUILT), (2015 - taxdata_R_EE$YR_REMOD))
taxdata_R_EE$BLDG_AGE <- ifelse(taxdata_R_EE$BLDG_AGE <=0,NA,taxdata_R_EE$BLDG_AGE)

##Allocating Building Age Score
taxdata_R_EE$AGE_SCORE <- NA
taxdata_R_EE$AGE_SCORE <- ifelse(taxdata_R_EE$BLDG_AGE < 50,4,taxdata_R_EE$AGE_SCORE)
taxdata_R_EE$AGE_SCORE <- ifelse(taxdata_R_EE$BLDG_AGE >= 50,3,taxdata_R_EE$AGE_SCORE)
taxdata_R_EE$AGE_SCORE <- ifelse(taxdata_R_EE$BLDG_AGE >= 100,2,taxdata_R_EE$AGE_SCORE)
taxdata_R_EE$AGE_SCORE <- ifelse(taxdata_R_EE$BLDG_AGE >= 150,1,taxdata_R_EE$AGE_SCORE)
taxdata_R_EE$AGE_SCORE <- ifelse(taxdata_R_EE$BLDG_AGE >= 200,0,taxdata_R_EE$AGE_SCORE)

## Allocating Residential Air Conditioner Energy Efficiency Score
taxdata_R_EE$COOL_SCORE <- NA
taxdata_R_EE$COOL_SCORE <- ifelse(taxdata_R_EE$R_AC == 'C', 1, taxdata_R_EE$COOL_SCORE)
taxdata_R_EE$COOL_SCORE <- ifelse(taxdata_R_EE$R_AC == 'D', 2, taxdata_R_EE$COOL_SCORE)
taxdata_R_EE$COOL_SCORE <- ifelse(taxdata_R_EE$R_AC == 'N', 3, taxdata_R_EE$COOL_SCORE)

## Aggregate Energy Efficiency score at building level
taxdata_R_EE$EE_SCORE <- taxdata_R_EE$AGE_SCORE+0.75*taxdata_R_EE$HEAT_SCORE+0.75*taxdata_R_EE$COOL_SCORE


#Add a column with a 1 to 10 ranking (decile) for the parcel's sq foot value
taxdata <- mutate(taxdata,
               BLDG_RANK = ceiling(rank(AV_BLDG_PER_SF,na.last="keep")/ 
                                     length(which(!is.na(AV_BLDG_PER_SF)))*10) )

# fix a single parcel in the entire dataset with LU = "XX", 
#a land use code that is not registered in the datasat dictionary. 
#Since the parcel is owned by a church, it's assumed a Tax Exempt parcel.

taxdata[taxdata$LU == 'XX',]$LU <- "E"

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
taxdata <- transform(taxdata, SIMPLIFIED_LU = sapply(LU, simplify_LU))

# Identify homes

isHome <- function(SIMPLIFIED_LU) {
  if (SIMPLIFIED_LU %in% c("RESIDENTIAL", "CONDO_UNIT", "MIX_RC")) {
    return(1)
  } else {
    return(0)
  }
}

# Create a new column by applying the isHome function
taxdata <- transform(taxdata, HOME = sapply(SIMPLIFIED_LU, isHome))