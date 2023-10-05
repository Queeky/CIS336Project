# You will need to run these lines if DBI, RSQLite, & rstudioapi aren't installed
#install.package("DBI")
#install.package("RSQLite")
#install.package("rstudioapi")

library(DBI)
library(RSQLite)

# Sets the working directory to location of R script
currentDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(currentDir)

# Checks if connection is possible
if (!dbCanConnect(RSQLite::SQLite(), "SMATDB.db")) {
  print("Database failed to connect.")
} else {
  db = dbConnect(RSQLite::SQLite(), "SMATDB.db")
  
  #test = dbGetQuery(db, "SELECT * FROM MARKET;")
  
}
