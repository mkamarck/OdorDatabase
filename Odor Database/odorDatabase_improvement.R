#import
df <- read.csv("odorDatabasefromAKOS_modified.csv", header = TRUE, na.strings = "")
#put in order of AKOS number
df$AKOS <- gsub("AKOS", "", df$AKOS) 
df.2 <- df[order(df$AKOS),]
write.csv(df.2, "odorDatabase_sorted.csv", na = "")
