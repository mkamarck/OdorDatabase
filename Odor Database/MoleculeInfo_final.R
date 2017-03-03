#libraries
library("dplyr")
library("tidyr")
library("stringr")
library("pipeR")
library("readr")
library("rvest")
library("httr")
library("purrr")
library("webchem")

#Take CAS and CID and turn it into odor information from pubchem and chemSpider
df <- read.csv("/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/intermediate\ databases/final_CIDandCAS.csv")
#subset out those that have no identifying information but hold them in a different database
#we will combine them back in at the end so we have the full list of odors. 
df.noID <- df[which(is.na(df$CID)),]
df <- df[which(!is.na(df$CID)),]
#set a test dataset to make sure this works
df.test <- df[1:10,]

#Using CID, search pubchem for canonical smiles and molecular weight
#function
get_properties_from_cid = function(MY_CID) {
    #canSmiles = pc_prop(cid = MY_CID, properties = "CanonicalSMILES")
    #MolW = pc_prop(cid = MY_CID, properties = "MolecularWeight")
    properties_byCID = pc_prop(cid = MY_CID, properties = c("CanonicalSMILES", "MolecularWeight"))
  return(properties_byCID)
}

return.properties = get_properties_from_cid(df$CID)

#df combine properties
df.merge <- merge(df, return.properties)
df.merge <- unique(df.merge)

#Using df.merge, use the csid to scrape chemspider for density information
#function
get_density_from_csid = function(csid) {
  if (is.na(csid)) {
    out = "NA" #I changed this from NA without quotation marks
  } 
  else {
    url = sprintf("www.chemspider.com/Chemical-Structure.%s.html", csid)
    out = tryCatch({
      url_got = GET(url, user_agent('6c2e700b-6a92-4551-9cc1-70f28c021f23'))
      source_ChemSpi = read_html(url_got) %>>% html_text()
      density = str_replace_all(source_ChemSpi, '\\"', '') %>>% 
        str_split('Density:\r\n                                            \r\n                                                ') %>>% 
        unlist() %>>% 
        map_chr(~ str_split(.x, "\n", n = 2) %>>% (.[[1]][1])) %>>% 
        (.[str_detect(., "\\d+")]) %>>%
        str_split("[^0-9.]",n = 2) %>>% (.[[1]][1]) #added in this line to just get the number of the density and not the units
      return(density)
    }, 
    error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      density = "NA"
      return(density)
    }, 
    silent = TRUE
    )
  }
  return(out)
}

#get density function
return_density = df.merge %>>% rowwise() %>>% mutate(density = get_density_from_csid(csid)) %>>% ungroup()
#add df.noID
df.noID$MolecularWeight = ""
df.noID$CanonicalSMILES = ""
df.noID$density = ""
final.output = rbind(return_density, df.noID)
#write out the database
write.csv(final.output, "/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/SymriseOdors_AllInfo.csv")
