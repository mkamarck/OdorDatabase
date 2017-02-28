#clean the symrise odor database
# library("dplyr")
# library("tidyr")
library("stringr")
library("pipeR")
# library("readr")
# library("rvest")
# library("httr")
# library("purrr")
# library("webchem")
library("splitstackshape")
#import df
df <- read.csv("/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/SymriseOdorList_forChemSpiPy.csv") %>>%
cSplit("CAS.no.",";") #split up the CAS

#How many compounds don't have a CAS number - split into two dataframes so we can use one to search in python by CAS number and the other to search in python by name
df.noCAS <- df[which(is.na(df$CAS.no._1)),]
df.CAS <- df[which(!is.na(df$CAS.no._1)),]

# write.csv(df.noCAS, "/Users/mkamarck/Documents/chemspipy/Symrise.noCAS.csv")
# write.csv(df.CAS, "/Users/mkamarck/Documents/chemspipy/Symrise.CAS.csv")



#compare some csid stuff and check yourself #####
#import list for csid
df <- read.csv("/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/csid_compare.csv")
