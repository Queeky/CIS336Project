
  # You will need to run these lines if the packages aren't already installed

#install.packages("rstudioapi")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("openxlsx")




library(rstudioapi)
library(tidyverse)
library(readxl)
library(openxlsx)




connect = function() {
  
  # Setting the working directory to location of R script
  currentDir <<- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(currentDir)
  
  # Saving excel file paths
  market <<- "constituents-financials.xlsx" 
  allShares <<- "individual-shares.xlsx"
  
  
}



seeMenu = function() {
  choice = 1; 
  
  print("///// Stock Market Analysis Tool /////")
  
  while (choice != 0) {
    print("[1] See line graph of a company's change in share price")
    print("[2] See histogram of the highest/lowest share prices")
    print("[3] See boxplot of the top companies")
    choice = readline("Select an action (0 to exit): ")
    
    result = switch(
      choice, 
      "1" = showShareLGraph(),
      "2" = createHistogram(),
      "3" = createBoxPlot()

      
    )
  }
}





createBoxPlot <- function() {
  
  # Read data from Excel files
  marketData <- read_excel(market)
  shareData <- read_excel(allShares)
  
  
  
  # Select number of companies
  print("Select the number of companies you would like to see")
  choice = as.numeric(readline("Enter a number (maximum of 10): "))
  
  
  
  
  while (choice > 10 || choice < 1) {
    print("Choice is not within range")
    choice = as.numeric(readline("Enter a number (maximum of 10): "))
  }
  
  
  # Find top 10 companies
  high_comp <- head(arrange(marketData, desc(Price)), choice)
  
  
  
  # Initialize a list to store data frames for each company
  company_data_list <- list()
  
  
  
  
  # Loop through each top company
  for (i in 1:nrow(high_comp)) {
    
    company_name <- as.character(high_comp[i, "Symbol"])  # Convert to character
    
    # Read data from the corresponding sheet
    company_data <- read_excel(allShares, sheet = company_name)
    
    # Add the data to the list
    company_data_list[[company_name]] <- company_data
  }
  
  
  # Combine the data for graphing
  combined_data <- bind_rows(company_data_list, .id = "Company")
  
  
  
  
  # Create boxplot
  myGraph <- ggplot(combined_data, aes(x = Company, y = Price)) +
    geom_boxplot() +
    labs(title = "Top Companies", x = "Company", y = "Share Price")
  
  print(myGraph)
}






createHistogram <- function() {
  
  
  # read from excel file
  marketData <- read_excel(market)


  # Convert for sorting purposes
  marketData$Price <- as.numeric(as.character(marketData$Price))

  
  
  # Get user input for highest / lowest
  print("[1] See histogram of the highest share prices")
  print("[2] See histogram of the lowest share prices")
  choice = readline("Select an action: ")
  
  while (choice > 2 || choice < 1) {
    print("Choice is not within range")
    choice = as.numeric(readline("Select an action: "))
  }
  
  
  
  # Select number of shares
  print("Select the number of shares you would like to see")
  choice2 = as.numeric(readline("Enter a number (maximum of 15): "))
  
  
  
  
  while (choice2 > 15 || choice2 < 1) {
    print("Choice is not within range")
    choice2 = as.numeric(readline("Enter a number (maximum of 15): "))
  }
  
  
  
  # Sort data
  high_shares <- head(arrange(marketData, desc(Price)), choice2)
  low_shares <- head(arrange(marketData, Price), choice2)
  

  
  # Create graph based on user input
  mygraph = switch(
    choice,
    "1" = ggplot(data = high_shares, aes(x = Symbol, y = Price)) +
      geom_bar(stat = "identity") +
      labs(title = "Highest Three Share Prices", x = "Company Symbol", y = "Share Price"),
    "2" = ggplot(data = low_shares, aes(x = Symbol, y = Price)) +
      geom_bar(stat = "identity") +
      labs(title = "Lowest Three Share Prices", x = "Company Symbol", y = "Share Price")
  )
  
  print(mygraph)
}



# Connect to the database
connect()



# Call the main / menu function
seeMenu()
