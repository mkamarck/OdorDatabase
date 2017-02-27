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



get_density_from_csid = function(csid) {
  if (csid == 0) {
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

#read in data
df <- read.csv("/Volumes/mainland/Projects/TAARs/Symrise\ -\ New\ Odors/Symrise\ Odor\ Database/ChemID_MW_SMILES_everything.csv")
df.test <- df[1:10,]

# # ChemSpider csid ID -> density
csid_density = df %>>% rowwise() %>>% mutate(density = get_density_from_csid(csid)) %>>% ungroup()

csid_density$CAS <- toString(csid_density$CAS)
# # Save
#write_csv(csid_density, "SymriseOdors_Final.csv")
