# You will need to run these lines if the packages aren't already installed
#install.packages("DBI")
#install.packages("RSQLite")
#install.packages("rstudioapi")
#install.packages("tidyverse")

library(DBI)
library(RSQLite)
library(rstudioapi)
library(tidyverse)

connect = function() {
  # Sets the working directory to location of R script
  currentDir = dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(currentDir)
  
  # Checks if connection is possible
  if (dbCanConnect(RSQLite::SQLite(), "SMATDB.db")) {
    db = dbConnect(RSQLite::SQLite(), "SMATDB.db")
    print("Connection established.")
    
    return(db)
  } else {
    print("Database failed to connect.")
    stop()
  }
}

seeMenu = function(db) {
  choice = 1; 
  
  print("///// Stock Market Analysis Tool /////")
  
  while (choice != 0) {
    print("[1] See line graph of a company's change in share price")
    choice = readline("Select an action (0 to exit): ")
    
    result = switch(
      choice, 
      "1" = showShareLGraph(db)
    )
  }
}

# Shows a line graph of a company's share price over the past month
showShareLGraph = function(db) {
  query = "SELECT COMP_ID, COMP_NAME FROM COMPANY ORDER BY COMP_NAME ASC;"
  
  companies = dbGetQuery(db, query)
  
  print(companies[c("COMP_NAME", "COMP_ID")])
  
  # Checking if the ID selected is valid
  repeat {
    company = readline("Select a company by COMP_ID: ")
    
    query2 = paste("SELECT SHARE_DATE AS DATE, SHARE_PRICE AS PRICE FROM SHARE WHERE COMP_ID = ", company, " ORDER BY SHARE_DATE;")
    shares = dbGetQuery(db, query2)
    
    # If rows exist, the COMP_ID is valid; otherwise, loop continues
    if (nrow(shares) != 0) {
      break
    }
    
    print("This is not a valid COMP_ID.")
  }
  
  graph = ggplot(data = shares, aes(x = DATE, y = PRICE, group = 1)) + geom_line()
  print(graph)
}

db = connect()
seeMenu(db)
dbDisconnect(db)
