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

# Functions
get_cid_from_name = function(name, MY_SMILES) {
  MY_CID = get_cid(query = name, from = "name", first = T, name_type = word)
  if (!is.na(MY_CID)) {
    out = pc_prop(cid = MY_CID, properties = "CanonicalSMILES") %>>%
      mutate(input = MY_SMILES, check = CanonicalSMILES == input)
  } else {
    out = data_frame(CID = NA, CanonicalSMILES = NA, input = MY_SMILES, check = NA)
  }
  return(out)
}
#none of this works
https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/xref/CAS/%s/cid/JSON
https://pubchem.ncbi.nlm.nih.gov/rest/pug/substance/xref/CAS/688-82-4/cid/JSON
https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/xref/RN/688-82-4/cid/JSON
get_density_from_cid = function(cid) {
  if (is.na(cid)) {
    out = "NA" #I changed this from NA without quotation marks
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

#smiles = data_frame(smiles = c("CC1=C(C(=C(C(=C1[N+](=O)[O-])C(C)(C)C)[N+](=O)[O-])C)C(=O)C"))
smiles <- read.csv("AKOS.can", header = FALSE, sep = "")
names(smiles) <- c("smiles", "AKOS")

# Read your smiles list
# smiles = read_csv()

# SMILES -> Pubchem ID
cid_smiles = smiles$smiles %>>% map_df(get_cid_from_smiles)

# Pubchem ID -> density
cid_smiles_density = cid_smiles %>>% rowwise() %>>% mutate(density = get_density_from_cid(CID)) %>>% ungroup()
  
#problem with writing out the molecules that have na even though they should be fine. 

# Save
#write_csv(cid_smiles_density, "cid_smiles_density.csv")
###########
#trying this with chemspider
df <- read.csv("/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/ChemID_MW_SMILES_everything.csv")
df.test <- df[1:10,]

cid_smiles <- get_cid_from_name(df.test$Name, df.test$SMILE)
