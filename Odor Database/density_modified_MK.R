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

# Functions
get_cid_from_smiles = function(MY_SMILES) {
  MY_CID = get_cid(query = MY_SMILES, from = "smiles", first = T)
  if (!is.na(MY_CID)) {
    out = pc_prop(cid = MY_CID, properties = "CanonicalSMILES") %>>%
      mutate(input = MY_SMILES, check = CanonicalSMILES == input)
  } else {
    out = data_frame(CID = NA, CanonicalSMILES = NA, input = MY_SMILES, check = NA)
  }
  return(out)
}

get_density_from_cid = function(cid) {
  if (is.na(cid)) {
    out = "" #I changed this from NA without quotation marks
  } 
  else {
    url = sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/%s/JSON/", cid)
    out = tryCatch({
      url_got = GET(url, user_agent("myagent"))
      source_PubChem = read_html(url_got) %>>% html_text()
      density = str_replace_all(source_PubChem, '\\"', '') %>>% 
        str_split('Name: Density,\n                    StringValue: ') %>>% unlist() %>>% 
        map_chr(~ str_split(.x, "\n", n = 2) %>>% (.[[1]][1])) %>>% 
        (.[str_detect(., "\\d+")]) %>>% 
        paste(collapse = " | ")
      return(density)
    }, 
    error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      density = NA
      return(density)
    }, 
    silent = TRUE
    )
  }
  return(out)
}

####try to get CAS from this####
# get_CAS_from_cid = function(cid) {
#   if (is.na(cid)) {
#     out = "" #I changed this from NA without quotation marks
#   }
#   else {
#     url = sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/%s/JSON/", cid)
#     out = tryCatch({
#       url_got = GET(url, user_agent("myagent"))
#       source_PubChem = read_html(url_got) %>>% html_text()
#       density = str_replace_all(source_PubChem, '\\"', '') %>>% 
#         str_split('Name: IUPAC Name,\n                    StringValue: ') %>>% unlist() %>>% 
#         map_chr(~ str_split(.x, "\n", n = 2) %>>% (.[[1]][1])) %>>% 
#         (.[str_detect(., "\\d+")]) %>>% 
#         (.[1]) #get just the first CAS
#         #paste(collapse = " | ")
#       return(density)
#     }, 
#     error = function(e) {
#       cat("Error:", conditionMessage(e), "\n")
#       density = NA
#       return(density)
#     }, 
#     silent = TRUE
#     )
#   }
#   return(out)
# }

get_MW_from_cid = function(cid) {
  if (is.na(cid)) {
    out = "" #I changed this from NA without quotation marks
  } 
  else {
    url = sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug_view/data/compound/%s/JSON/", cid)
    out = tryCatch({
      url_got = GET(url, user_agent("myagent"))
      source_PubChem = read_html(url_got) %>>% html_text()
      density = str_replace_all(source_PubChem, '\\"', '') %>>% 
        str_split('Name: Molecular Weight,\n                    NumValue: ') %>>% unlist() %>>% 
        map_chr(~ str_split(.x, "\n", n = 2) %>>% (.[[1]][1])) %>>% 
        (.[str_detect(., "\\d+")]) %>>% 
        paste(collapse = " | ")
      return(density)
    }, 
    error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      density = NA
      return(density)
    }, 
    silent = TRUE
    )
  }
  return(out)
}


# Example
# smiles = data_frame(smiles = c("CC1=CC(=C(C(=C1)C(C)(C)C)O)C(C)(C)C", 
#                                "CC=O",
#                                "CC1CC(C2=C(C1(C)C)C=C(C(=C2)C(=O)C)C)(C)C",
#                                "CSC",
#                                "C1CCCCC1",
#                                "C1CCN(CC1)C(=O)/C=C/C=C/C2=CC3=C(C=C2)OCO3",
#                                "C1CCCCCCCOC(=O)CCCCCC1",
#                                "C1=CC=C(C=C1)CC(C(=O)O)N",
#                                "CCCC(=O)OCC(COC(=O)CCC)OC(=O)CCC",
#                                "C([C@H]([C@H]([C@@H]([C@H](CO)O)O)O)O)O",
#                                "C1CC(=O)OC2=CC=CC=C21",
#                                "CCOC(=O)CCSCC1=CC=CO1",
#                                "C([C@@H]1[C@H]([C@H](C(O1)O)O)O)O",
#                                "C(I)(I)I",
#                                "C1C(SCC(S1)O)O",
#                                "CC1=C(C(=C(C(=C1[N+](=O)[O-])C(C)(C)C)[N+](=O)[O-])C)C(=O)C",
#                                "C1=CC=C2C(=C1)C=CC(=O)O2",
#                                "C1=CC=NC=C1",
#                                "C1=CN=CC=N1",
#                                "C1=CSC=C1"))

#smiles = data_frame(smiles = "C1=CSC=C1")
cid <- 1254
setwd("/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Odor\ Database")
smiles <- read.csv("AKOS.can", header = FALSE, sep = "")
names(smiles) <- c("smiles", "AKOS")

# Read your smiles list
# smiles = read_csv()

#####dataframe output#####
# SMILES -> Pubchem ID
cid_smiles = smiles$smiles %>>% map_df(get_cid_from_smiles)
#write.csv(cid_smiles, "fromDensity_cid_smiles.csv")
cid_smiles<- read.csv("fromDensity_cid_smiles.csv") %>>% (.[,2:ncol(cid_smiles)])

# Pubchem ID -> density
cid_smiles_density = cid_smiles %>>% rowwise() %>>% mutate(density = get_density_from_cid(CID)) %>>% ungroup()
# Save
#write.csv(cid_smiles_density, "cid_smiles_density.csv")

#get MW too
cid_smiles_MW = cid_smiles_density %>>% rowwise() %>>% mutate(MW = get_MW_from_cid(CID)) %>>% ungroup() 
#cid_smiles_MW <- cid_smiles_MW_backup
#remove the commas
cid_smiles_MW$MW <- gsub(",", "", cid_smiles_MW$MW)
odors_dataframe <- merge(cid_smiles_MW, smiles, by.x = "input", by.y = "smiles")#add AKOS numbers
write.csv(odors_dataframe, "odorDatabasefromAKOS.csv") #saves this midpoint



