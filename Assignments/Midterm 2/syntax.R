# Addenda to the 2015 Boston Tax Assessor's data set


# Reading the data into data frame:
PropTax <- read.csv('Tax Assessor 2015 - Data.csv') 

# Fixing the Gross Tax:
PropTax$GROSS_TAX <- PropTax$GROSS_TAX/100

# Fixing the Gross Area (where less than 100 sq feet):
PropTax$GROSS_AREA <- ifelse(PropTax$GROSS_AREA < 100, NA, PropTax$GROSS_AREA)

# Fixing the Living Area (where less than 100 sq feet):
PropTax$LIVING_AREA <- ifelse(PropTax$LIVING_AREA < 100, NA, PropTax$LIVING_AREA)

# Fixing the Year Built and Year Remodeled (where the year is 0):
PropTax$YR_BUILT <- ifelse(PropTax$YR_BUILT == 0, NA, PropTax$YR_BUILT)
PropTax$YR_REMOD <- ifelse(PropTax$YR_REMOD == 0, NA, PropTax$YR_REMOD)
PropTax$YR_REMOD[PropTax$YR_REMOD == 995] <- 1995

# Fixing the BRA Planning District (where value is blank):
PropTax$BRA_PD<-ifelse(PropTax$BRA_PD=="",NA,PropTax$BRA_PD)

# Fixing the Inspectional Service Department Neighborhood Statistical Area (where value is blank):
PropTax$NSA_NAME<-ifelse(PropTax$NSA_NAME=="",NA,PropTax$NSA_NAME)

# Adding the Distance to Boston Common Variable
if (!require("geosphere")) install.packages("geosphere")
distDxMi<-function(x){
  distCosine(c(-71.0656,42.3550),c(x[1],x[2])) * 0.00062137
}
PropTax$DIST_TO_DX_MI<-apply(PropTax[,c('X','Y')], 1, distDxMi)

#Adding the Normalized Land and Property value Variables
PropTax <- transform(PropTax, AV_LAND_PER_SF = PropTax$AV_LAND/PropTax$LAND_SF, AV_BLDG_PER_SF = PropTax$AV_BLDG/PropTax$GROSS_AREA)
View(PropTax)

#Fixing the Normalized Land and Property value Variables
PropTax$AV_LAND_PER_SF <- ifelse(PropTax$AV_LAND_PER_SF == 0, NA, PropTax$AV_LAND_PER_SF)
PropTax$AV_BLDG_PER_SF <- ifelse(PropTax$AV_BLDG_PER_SF == 0, NA, PropTax$AV_BLDG_PER_SF)

# Fix the single parcel in the entire dataset with LU = "XX"
PropTax [PropTax $LU == 'XX',]$LU <- "E"
