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
df <- read.csv("/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/ChemID_MW_SMILES_everything.csv")
df.test <- df[1:10,]

# test = "100-06-1"
# url = sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/xref/RegistryID/%s/JSON", test)

add_cid = df %>>% rowwise() %>>% mutate(CID = get_cid_from_CAS(CAS)) %>>% ungroup()
add_cid2 <- add_cid

#get smiles from cid
get_smiles_from_cid = function(CID) {
  #MY_CID = get_cid(query = MY_SMILES, from = "smiles", first = T)
  if (!is.na(CID)) {
    out = pc_prop(cid = CID, properties = c("MolecularWeight", "CanonicalSMILES")) %>>%
      mutate(input = SMILE, check = CanonicalSMILES == input)
  } else {
    out = data_frame(CID = NA, CanonicalSMILES = NA, check = NA)
  }
  return(out)
}
#df_forloop$test_cas[i] = cir_query(csid, 'cas', resolver = 'name_by_chemspider')
cid.next = df.test$CID %>>% map_df(get_smiles_from_cid)
