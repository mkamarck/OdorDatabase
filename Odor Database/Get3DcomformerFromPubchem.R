rm(list = ls()) # Clean your environment
# Packages
loadPackages <- function(package) {
  package <- as.character(package)
  if (require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    cat(sprintf("%s is loaded correctly\n", package))
  } else {
    cat(sprintf("trying to install %s...\n", package))
    install.packages(package)
    stopifnot(require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
    cat(sprintf("could not install %s\n", package))
  }
}
## Packages
loadPackages("dplyr")
loadPackages("tidyr")
loadPackages("stringr")
loadPackages("pipeR")
loadPackages("readr")
loadPackages("rvest")
loadPackages("httr")
loadPackages("purrr")

## Function to get 3D molecular structure from Pubchem
## You need CID as an input
getPubchem3Dconformer <- function(cid) {
  cid <- as.character(cid)
  url <- sprintf("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/%s/record/SDF/?record_type=3d&response_type=display", cid)
  cat("Accessing...", url, "\n")
  Sys.sleep(0.2)
  
  out <- tryCatch({
    url_got <- GET(url, user_agent("myagent"))
    source_PubChem <- read_html(url_got)
    output <- html_text(source_PubChem) %>>% str_split("M  END") %>>% (.[[1]][1]) %>>% (paste0("\n CID ", ., "M  END\n$$$$")) #%>>% str_replace("\n  -OEChem-\\d+3D", "")
    return(output)
  }, 
  error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    #message("URL does not seem to exist")
    output <- "NA"
    return(output)
  }, 
  silent = TRUE
  )
  
  return(out)
}

# ## Path
# myPath <- getwd() %>>% str_split("code") %>>% (.[[1]][1])
## Put the directory to your data
DIRECTORY = ""

## Read your data (replace 'YOUR INPUT FILE NAME' with your input file's name)
data = read_csv(paste0(DIRECTORY, "YOUR INPUT FILE NAME.csv"))
## If you want tsv file rather than csv, try the line below
# data = read_tsv(paste0(DIRECTORY, "YOUR FILE NAME.txt"))

## Get 3D comformer from Pubchem
## Your data must have 'CID' column that has Pubchem IDs.
## Structure information will be stored in 'MOL' column.
data$MOL = data$CID %>>% map_chr(~ getPubchem3Dconformer(.x))
## Output your structure information as SDF (replace 'YOUR OUTPUT FILE NAME' with your output file's name)
OUTPUT = data$MOL
write(OUTPUT, paste0(DIRECTORY, "YOUR OUTPUT FILE NAME.sdf"))
