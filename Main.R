# You will need to run these lines if DBI, RSQLite, & rstudioapi aren't installed
# You will need to run these lines if DBI, RSQLite, & rstudioapi aren't installed
#install.packages("DBI")
#install.packages("RSQLite")
#install.packages("rstudioapi")
#install.packages("tidyverse")



library(DBI)
library(RSQLite)
library(rstudioapi)
library(tidyverse)



# Sets the working directory to location of R script
currentDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(currentDir)



# Checks if connection is possible
if (!dbCanConnect(RSQLite::SQLite(), "SMATDB.db")) {
  print("Database failed to connect.")
} else {
  db = dbConnect(RSQLite::SQLite(), "SMATDB.db")
  
  choice = readline("Select an action (0 to exit): ")
  
  result = switch(
    choice, 
    "2" = createHistogram()
  )
}




createHistogram = function(){
  
  # Asks for user input
  choice = readline("Select 1 to view the highest 3 shares and 2 to view the lowest 3 shares:  ")
  
  
  #Calculates highest shares
  if (choice == "1") {
    query1 <- "SELECT X.COMP_ID, X.SHARE_PRICE
               FROM SHARE X
               ORDER BY X.SHARE_PRICE DESC
               LIMIT 3"
    shares <- dbGetQuery(db, query1)
  }
  
  
  #Calculates lowest shares
  if (choice == "2") {
    query2 <- "SELECT X.COMP_ID, X.SHARE_PRICE
               FROM SHARE X
               ORDER BY X.SHARE_PRICE ASC
               LIMIT 3"
    shares <- dbGetQuery(db, query2)
  }
  

  #Creates graph based on the selected shares
  mygraph = ggplot(data = shares, aes(x = COMP_ID, y = SHARE_PRICE)) + geom_bar(stat = "identity") +
    labs(title = "Share Prices", x = "Company ID", y = "Share Price")
  
  print(mygraph)
}
