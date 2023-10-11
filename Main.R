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
  } else {
    print("Database failed to connect.")
    stop()
  }
}

seeMenu = function() {
  choice = 1; 
  
  print("///// Stock Market Analysis Tool /////")
  
  while (choice != 0) {
    print("[1] See line graph of a company's change in share price")
    choice = (readline("Select an action (0 to exit): "))
    
    result = switch(
      choice, 
      "1" = showShareLGraph()
    )
  }
}

# Shows a line graph of a company's share price over a certain time period
showShareLGraph = function() {
  # NOTE: This dataframe is just for testing! It will be replaced with info from the database
  fakeFrame = data.frame(
    month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    sharePrice = c(1.0, 2.0, 2.0, 3.0, 2.0, 6.0, 7.0, 4.0, 4.0, 3.0, 3.0, 3.0)
  )
  
  graph = ggplot(data = fakeFrame, aes(x = month, y = sharePrice)) + geom_line()
  print(graph)
}

connect()
seeMenu()

#test = dbGetQuery(db, "SELECT * FROM MARKET;")
