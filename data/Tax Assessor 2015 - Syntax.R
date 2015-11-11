rm(list=ls())

library(plyr)

check_geo <- function(x) {
  for (columnName in c("X","Y","LocationID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")) {
    print(paste((columnName),"-", (length(which( is.na(x[,columnName]) | x[,columnName] == 0  ))) ) )
  }
}

tax_assessor = read.csv("/Users/henrygomory/Documents/Research/BARI/Course Data/Tax Assessor Cross/DATA FULL.txt",stringsAsFactors = FALSE)
library(sqldf)


#-----------------------------------------------------#
#     Aggregating parcel files by parcel_num          #
#-----------------------------------------------------#

parcels2015 <- read.csv("/Users/henrygomory/Documents/Research/BARI/General Geographic Files/Parcels2015_final.csv",stringsAsFactors=FALSE)
parcels2014 <- read.csv("/Users/henrygomory/Documents/Research/BARI/General Geographic Files/Parcels Boston 2014 BARI.csv",stringsAsFactors=FALSE) 

#check that Blk_IDs weren't cut off
options(scipen=999)
parcels2015$Blk_ID_10[10]
parcels2014$Blk_ID_10[10]

# the following syntax is from from parcelsToAddresses.R
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# making the parcel geographic data be accessible at the parcel num level, so they can be merged onto the tax assessor data
by_parcel_2015 <- sqldf("select parcel_num, max(Blk_ID_10) `Blk_ID_10`,max(BG_ID_10) `BG_ID_10`,
                    max(CT_ID_10) `CT_ID_10`, max(X) `X`,
                   max(Y) `Y`, max(LocationID) `LocationID`, max(TLID) `TLID`
                    from parcels2015 group by parcel_num")
# When we aggregate parcels to addresses we take max for this type of aggregation (on LocationID, Blk_ID, etc.)
# Using mode has no return when there are two modes

# I'm aggregating these strings differently from the other
by_parcel_BRAPD <- aggregate(BRA_PD ~ parcel_num, data=parcels2015, FUN=Mode)
by_parcel_2015<-merge(by_parcel_2015,by_parcel_BRAPD,by="parcel_num",all.x=TRUE)
by_parcel_NSA <- aggregate(NSA_NAME ~ parcel_num, data=parcels2015, FUN=Mode)
by_parcel_2015<-merge(by_parcel_2015,by_parcel_NSA,by="parcel_num",all.x=TRUE)
rm(by_parcel_BRAPD,by_parcel_NSA)

# the same aggregation procedure is then repeated for the 2014 parcels
by_parcel_2014 <- sqldf("select parcel_num, max(Blk_ID_10) `Blk_ID_10`,max(BG_ID_10) `BG_ID_10`,
                    max(CT_ID_10) `CT_ID_10`, max(X) `X`,
                        max(Y) `Y`, max(LocationID) `LocationID`, max(TLID) `TLID`
                        from parcels2014 group by parcel_num")

by_parcel_BRAPD <- aggregate(BRA_PD ~ parcel_num, data=parcels2014, FUN=Mode)
by_parcel_2014<-merge(by_parcel_2014,by_parcel_BRAPD,by="parcel_num",all.x=TRUE)
by_parcel_NSA <- aggregate(NSA_NAME ~ parcel_num, data=parcels2014, FUN=Mode)
by_parcel_2014<-merge(by_parcel_2014,by_parcel_NSA,by="parcel_num",all.x=TRUE)
rm(by_parcel_BRAPD,by_parcel_NSA)

#now we append the two parcel files (aggregated to the parcel num level)
by_parcel_long = rbind(by_parcel_2015,by_parcel_2014)

#delete duplicates between the two files, taking the 2015 values because they are earlier in the data
by_parcel_geo <- by_parcel_long[!duplicated(by_parcel_long$parcel_num),]

#checks how much geographic data we have in the parcels file
for (columnName in c("X","Y","LocationID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")) {
  print(paste((columnName),"-", (length(which( is.na(by_parcel_geo[,columnName]) | by_parcel_geo[,columnName] == 0  ))) ) )
}

#-------------------------------------------#
#       Adding Geo from parcel files        #
#-------------------------------------------#
#merge the parcel geographic data onto the tax assessor data
tax_assessor_geo<-merge(tax_assessor,by_parcel_geo,by.x="PID",by.y="parcel_num",all.x=TRUE)

#checks how many observations in the tax assessor file were missing parcel nums
length(which(tax_assessor_geo$PID==0 | tax_assessor_geo$PID=="NULL" | is.na(tax_assessor_geo$PID)))

#counts missing data
for (columnName in c("X","Y","LocationID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")) {
  print(paste((columnName),"-", (length(which( is.na(tax_assessor_geo[,columnName]) | tax_assessor_geo[,columnName] == 0  ))) ) )
}

#-------------------------------------------#
#            Adding Extra Geo               #
#-------------------------------------------#
extra_geo = read.csv("/Users/henrygomory/Documents/Research/BARI/Course Data/missing addresses/ADDRESSES_WITH_GEO.csv",stringsAsFactors = FALSE)
#the geographic data file has data for multiple files, so we subset it to just licenses using type == TAC
extra_geo = extra_geo[extra_geo$type=="TAC",c("ID","X","Y","TLID","GEOID10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")]
extra_geo <- rename(extra_geo, replace=c("GEOID10" = "Blk_ID_10"))

#check that the Blk ID wasn't truncated
head(extra_geo[!is.na(extra_geo$Blk_ID_10),"Blk_ID_10"])

#Use Blk ID and BG ID to fill in possibly missing CTs and BGs
sub_1 <- function(x) {
  return(substr(as.character(as.numeric(x)),1,12))
}
sub_2 <- function(x) {
  return(substr(as.character(as.numeric(x)),1,11))
}
Blk_missing =  extra_geo[,"Blk_ID_10"]==0 | extra_geo[,"Blk_ID_10"]=="NULL" | is.na(extra_geo[,"Blk_ID_10"])   
BG_missing =  extra_geo[,"BG_ID_10"]==0 | extra_geo[,"BG_ID_10"]=="NULL" | is.na(extra_geo[,"BG_ID_10"])   
CT_missing =  extra_geo[,"CT_ID_10"]==0 | extra_geo[,"CT_ID_10"]=="NULL" | is.na(extra_geo[,"CT_ID_10"])
extra_geo[ BG_missing & !Blk_missing , "BG_ID_10"  ]= (as.character(lapply(extra_geo$Blk_ID_10,sub_1)))[BG_missing & !Blk_missing]
extra_geo[ CT_missing & !Blk_missing , "CT_ID_10"  ]=as.character(lapply(extra_geo$Blk_ID_10,sub_2))[CT_missing & !Blk_missing]
extra_geo[ CT_missing & !BG_missing , "CT_ID_10"  ]=as.character(lapply(extra_geo$BG_ID_10,sub_2))[CT_missing & !BG_missing]

#check how much geographic data we have
for (columnName in c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")) {
  print(paste((columnName),"-", (length(which(!(extra_geo[,columnName]==0 | extra_geo[,columnName]=="NULL" | is.na(extra_geo[,columnName])))))))
}

#get rid of duplicates by License Number (every License Number has the same address attached so geographic data will be the same)
extra_geo = extra_geo[!duplicated(extra_geo$ID),]

#merge in extra geographic info
tax_assessor_geo = merge(tax_assessor_geo,extra_geo,by.x="PID",by.y="ID",all.x=TRUE)

tax_assessor_geo$X.x[!is.na(tax_assessor_geo$X.y)] = tax_assessor_geo$X.y[!is.na(tax_assessor_geo$X.y)]
tax_assessor_geo$Y.x[!is.na(tax_assessor_geo$Y.y)] = tax_assessor_geo$Y.y[!is.na(tax_assessor_geo$Y.y)]
tax_assessor_geo$TLID.x[!is.na(tax_assessor_geo$TLID.y)] = tax_assessor_geo$TLID.y[!is.na(tax_assessor_geo$TLID.y)]
tax_assessor_geo$Blk_ID_10.x[!is.na(tax_assessor_geo$Blk_ID_10.y)] = tax_assessor_geo$Blk_ID_10.y[!is.na(tax_assessor_geo$Blk_ID_10.y)]
tax_assessor_geo$BG_ID_10.x[!is.na(tax_assessor_geo$BG_ID_10.y)] = tax_assessor_geo$BG_ID_10.y[!is.na(tax_assessor_geo$BG_ID_10.y)]
tax_assessor_geo$CT_ID_10.x[!is.na(tax_assessor_geo$CT_ID_10.y)] = tax_assessor_geo$CT_ID_10.y[!is.na(tax_assessor_geo$CT_ID_10.y)]
tax_assessor_geo$NSA_NAME.x[!is.na(tax_assessor_geo$NSA_NAME.y)] = tax_assessor_geo$NSA_NAME.y[!is.na(tax_assessor_geo$NSA_NAME.y)]
tax_assessor_geo$BRA_PD.x[!is.na(tax_assessor_geo$BRA_PD.y)] = tax_assessor_geo$BRA_PD.y[!is.na(tax_assessor_geo$BRA_PD.y)]

tax_assessor_geo = rename(tax_assessor_geo, c("X.x"="X", "Y.x"="Y","TLID.x"="TLID","Blk_ID_10.x"="Blk_ID_10", "BG_ID_10.x"="BG_ID_10","CT_ID_10.x"="CT_ID_10","BRA_PD.x"="BRA_PD","NSA_NAME.x"="NSA_NAME"))
tax_assessor_geo = tax_assessor_geo[,setdiff(colnames(tax_assessor_geo),c("X.y","Y.y","TLID.y","Blk_ID_10.y","BG_ID_10.y","CT_ID_10.y","NSA_NAME.y","BRA_PD.y"))]


#check how much geographic data we have
for (columnName in c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","BRA_PD","NSA_NAME")) {
  print(paste((columnName),"-", (length(which(!(tax_assessor_geo[,columnName]==0 | tax_assessor_geo[,columnName]=="NULL" | is.na(tax_assessor_geo[,columnName]) ))))))
}

tax_assessor_geo = rename(tax_assessor_geo,c("Blk_ID_10"="Blk_ID"))
write.csv(tax_assessor_geo,"/Users/henrygomory/Documents/Research/BARI/Course Data/Finished Files/Tax Assessor (2015 Clean).csv")
write.csv(by_parcel_geo,"/Users/henrygomory/Documents/Research/BARI/Course Data/Tax Assessor Cross/by_parcel_geo.csv")

