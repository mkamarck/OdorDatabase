#BROKEN SCRIPT DON"T USE##


## Packages
library("dplyr")
library("tidyr")
library("stringr")
library("pipeR")
library("readr")
library("rvest")
library("httr")
library("purrr")
library("webchem")

#Functions
# get_smiles_from_cid = function(CID) {
#   MY_CID = get_cid(query = MY_SMILES, from = "smiles", first = T)
#   if (!is.na(MY_CID)) {
#     out = pc_prop(cid = MY_CID, properties = "CanonicalSMILES") %>>%
#       mutate(input = MY_SMILES, check = CanonicalSMILES == input)
#   } else {
#     out = data_frame(CID = NA, CanonicalSMILES = NA, input = MY_SMILES, check = NA)
#   }
#   return(out)
# }


get_cid_from_CAS = function(CAS) {
  if (is.na(CAS)) {
    out = "" #I changed this from NA without quotation marks
  } 
  else {
    url = sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/xref/RegistryID/%s/JSON", CAS)
    out = tryCatch({
      url_got = GET(url, user_agent("myagent"))
      source_PubChem = read_html(url_got) %>>% html_text()
      cid = str_replace_all(source_PubChem, '\\"', '') %>>% 
        str_split('cid: ') %>>% unlist() %>>% 
        map_chr(~ str_split(.x, "\n", n = 2) %>>% (.[[1]][1])) %>>% 
        (.[str_detect(., "\\d+")]) %>>% 
        paste(collapse = " | ")
      return(cid)
    }, 
    error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      cid = ""
      return(cid)
    }, 
    silent = TRUE
    )
  }
  return(out)
}


pc_prop(cid, properties = NULL, verbose = TRUE, ...)





# # SMILES -> Pubchem ID
# cid_smiles = smiles$smiles %>>% map_df(get_cid_from_smiles)
# 
# # Pubchem ID -> density
# cid_smiles_density = cid_smiles %>>% rowwise() %>>% mutate(density = get_density_from_cid(CID)) %>>% ungroup()
#   
# #problem with writing out the molecules that have na even though they should be fine. 
# 
# # Save
# #write_csv(cid_smiles_density, "cid_smiles_density.csv")
###########
#trying this with chemspider
df <- read.csv("/Users/mkamarck/Documents/School\ and\ Lab/Mainland\ Lab/OdorDatabase/Symrise\ Odor\ Database/csid_compare.csv")
df.test <- df[1:10,]

# test = "100-06-1"
# url = sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/xref/RegistryID/%s/JSON", test)

add_cid = df %>>% rowwise() %>>% mutate(CID = get_cid_from_CAS(CAS)) %>>% ungroup()
#write.csv(add_cid, "/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/add_cid.csv")
add_cid2 <- add_cid

#get smiles from cid
# get_smiles_from_cid = function(CID) {
#   #MY_CID = get_cid(query = MY_SMILES, from = "smiles", first = T)
#   if (!is.na(CID)) {
#     out = pc_prop(cid = CID, properties = c("MolecularWeight", "CanonicalSMILES")) %>>%
#       mutate(input = SMILE, check = CanonicalSMILES == input)
#   } else {
#     out = data_frame(CID = NA, CanonicalSMILES = NA, check = NA)
#   }
#   return(out)
# }
#df_forloop$test_cas[i] = cir_query(csid, 'cas', resolver = 'name_by_chemspider')
#cid.next = df.test$CID %>>% map_df(get_smiles_from_cid)
df.test = add_cid[1:10,]
# df.test$smiles_fromCID = ""
# df.test$MW_fromCID = ""
# df.test$CID <- as.integer(df.test$CID)
# df_forLoop = df.test
# for (i in 1:length(df_forLoop)){
#   cid = df_forLoop$CID[i]
#   if (!is.na(cid)){
#     print("yes")
#     df_forLoop$smiles_fromCID[i] = pc_prop(cid, properties = "CanonicalSMILES")
#   }
# }

get_smiles_from_CID = function(CID) {
  if (is.na(CID)) {
    out = "" #I changed this from NA without quotation marks
  } 
  else {
    url = sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/%s/property/CanonicalSMILES/JSON", CID)
    out = tryCatch({
      url_got = GET(url, user_agent("myagent"))
      source_PubChem = read_html(url_got) %>>% html_text()
      smile = str_replace_all(source_PubChem, '\\"', '') %>>% 
        str_split('CanonicalSMILES: ') %>>% unlist() %>>% 
        map_chr(~ str_split(.x, "\n", n = 2) %>>% (.[[1]][1])) %>>% (.[2] )
      return(smile)
    }, 
    error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      smile = ""
      return(smile)
    }, 
    silent = TRUE
    )
  }
  return(out)
}
#url = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/1/property/CanonicalSMILES/JSON"
add_smiles = add_cid %>>% rowwise() %>>% mutate(smiles_fromCID = get_smiles_from_CID(CID)) %>>% ungroup()




#now take the original smiles from the chemspider and output more canonical smiles and CID using yusue's original code
#Then check if these smiles match the canonical smiles
#and see if the cids match the other match

#for some reason none of these functions are working

#add_smiles$check= add_smiles$SMILE == add_smiles$smiles_fromCID
add_smiles2 = add_smiles

#write.csv(add_smiles, "checkThisByHand.csv")
#modified from yusuke's density R script
get_cid_from_smiles = function(MY_SMILES) {
  MY_CID = get_cid(query = MY_SMILES, from = "smiles", first = T)
}
#   if (!is.na(MY_CID)) {
#     out = pc_prop(cid = MY_CID, properties = "CanonicalSMILES")
#       
#   } else {
#     out = data_frame(CID = NA, CanonicalSMILES = NA)
#   }
#   return(out)
# }

#df.test <- add_smiles[1:10,]
#run function
#df.test$cid_from_csidSMILE = get_cid_from_smiles(df.test$SMILE)

add_smiles$cid_from_csidSMILE = get_cid_from_smiles(add_smiles$SMILE)
#check that cids match
add_smiles$check= add_smiles$CID == add_smiles$cid_from_csidSMILE

#write.csv(add_smiles, "/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/cid_checkByHand.csv")
