# Calculate the neighborhood land use diversity index

library(vegan)
library(dplyr)

nb.lu.index <- summarise(group_by(filter(taxdata, !is.na(BRA_PD)), BRA_PD),
                                           LAND_USE_DIV = diversity(table(SIMPLIFIED_LU)))

#Create a home value dispersion index, by neighborhood

nb.hv.dispersion <-summarise(group_by(filter(taxdata, !is.na(BRA_PD), 
                                                               HOME == 1), BRA_PD),
                             HOME_VALUE_DISP = var(BLDG_RANK, na.rm = TRUE)/
                               mean(BLDG_RANK, na.rm = TRUE))

#Aggregate indices

taxdata_agg_neighborhood <-merge(nb.lu.index, nb.hv.dispersion, by="CT_ID_10")
