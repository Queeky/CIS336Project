# You will need to run these lines if the packages aren't already installed
# install.packages("DBI")
# install.packages("RSQLite")
# install.packages("rstudioapi")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("openxlsx")

library(DBI)
library(RSQLite)
library(rstudioapi)
library(tidyverse)
library(readxl)
library(openxlsx)

connect = function() {
  # Setting the working directory to location of R script
  currentDir <<- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(currentDir)
  
  # Saving excel file paths
  market <<- "data/constituents-financials.xlsx" 
  allShares <<- "data/individual-shares.xlsx"
}

seeMenu = function() {
  choice = 1; 
  
  print("///// Stock Market Analysis Tool /////")
  
  while (choice != 0) {
    print("[1] See line graph of a company's change in share price")
    print("[2] See histogram of the highest/lowest 3 share prices")
    choice = readline("Select an action (0 to exit): ")
    
    result = switch(
      choice, 
      "1" = showShareLGraph(),
      "2" = createHistogram()
    )
  }
}

# Shows a line graph of a company's share price over the past month
showShareLGraph = function() {
  companies = read_excel(market)
  continue = FALSE
  
  # Displaying company options
  print(companies[c("Symbol", "Name")], n = Inf)
  
  while (!continue) {
    choice1 = readline("Enter a ticker symbol to select a company: ")
    
    # Checking if user input is valid
    if (choice1 %in% companies[["Symbol"]]) {
        continue = TRUE
    } else {
      print("That is not a valid ticker.")
    }
  }
  
  # Displaying time frame options
  print("Time Frames:")
  print("1) 1 Month")
  print("2) 6 Months")
  print("3) 1 Year")
  print("4) 5 Years")
  print("5) Max")
  
  choice2 = 0
  
  while (choice2 < 1 || choice2 > 5) {
    choice2 = readline("Enter a time frame by the associated number: ")
    company = NULL
    
    printGraph = function(time) {
      company = read_excel(allShares, sheet = choice1, n_max = time)
      
      graph = ggplot(data = company, aes(x = Date, y = `Close/Last`, group = 1)) + geom_line()
      print(graph)
    }
    
    # Choosing amount of rows (days) to be used
    result = switch(
      choice2, 
      "1" = printGraph(30), 
      "2" = printGraph(180), 
      "3" = printGraph(365), 
      "4" = printGraph(1825), 
      "5" = printGraph(Inf)
    )
    
    if (choice2 < 1 || choice2 > 5) {
      print("That is not a valid option.")
    }
  }
}

createHistogram = function(){
  
  # Asks for user input
  choice = readline("Select 1 to view the highest 3 shares and 2 to view the lowest 3 shares:  ")
  
  #Calculates highest shares
  if (choice == "1") {
    query1 <- "SELECT X.COMP_ID, X.SHARE_PRICE
               FROM SHARE X
               GROUP BY X.COMP_ID
               ORDER BY X.SHARE_PRICE DESC
               LIMIT 3"
    shares <- dbGetQuery(db, query1)
    print(shares)
  }
  
  #Calculates lowest shares
  if (choice == "2") {
    query2 <- "SELECT X.COMP_ID, X.SHARE_PRICE
               FROM SHARE X
               GROUP BY X.COMP_ID
               ORDER BY X.SHARE_PRICE ASC
               LIMIT 3"
    shares <- dbGetQuery(db, query2)
  }
  
  #Creates graph based on the selected shares
  mygraph = ggplot(data = shares, aes(x = COMP_ID, y = SHARE_PRICE)) + geom_bar(stat = "identity") +
    labs(title = "Share Prices", x = "Company ID", y = "Share Price")
  
  print(mygraph)
}

# Running the program
connect()
seeMenu()
