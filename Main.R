


# You will need to run these lines if the packages aren't already installed

#install.packages("rstudioapi")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("openxlsx")


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
    print("[4] See lollipop graph of select companies")
    choice = readline("Select an action (0 to exit): ")
    
    result = switch(
      choice, 
      "1" = showShareLine(),
      "2" = createHistogram(),
      "3" = createBoxPlot(), 
      "4" = showLollipop()
    )
  }
}


createBoxPlot <- function() {
  

  marketData <- read_excel(market)
  shareData <- read_excel(allShares)
  
  
  
  # Select the number of companies
  print("Select the number of companies you would like to see")
  choice = as.numeric(readline("Enter a number (maximum of 10): "))
  
  
  
  while (choice > 10 || choice < 1) {
    print("Choice is not within range")
    choice = as.numeric(readline("Enter a number (maximum of 10): "))
  }
  
  
  
  # Find top 10 companies
  high_comp <- head(arrange(marketData, desc('Close/Last')), choice)
  

  
  # Initialize empty list
  company_data_list <- list()
  
  
  
  # Loop through each top company
  for (i in 1:nrow(high_comp)) {
    company_name <- as.character(high_comp[i, "Symbol"])  # Convert to character
    
    # Read data from corresponding sheet
    company_data <- read_excel(allShares, sheet = company_name)
    
    # Add data to the list
    company_data_list[[company_name]] <- company_data
  }
  
  
  
  # Combine the data for graphing
  combined_data <- bind_rows(company_data_list, .id = "Company")
  
  
  
  # Create boxplot
  myGraph <- ggplot(combined_data, aes(x = Company, y = `Close/Last`)) +
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

# Displaying list of companies and prompting user to select from list
selectCustom = function(num, choices) {
  companies = read_excel(market)
  count = 0
  
  print(companies[c("Symbol", "Name")], n = Inf)
  
  while (count < num) {
    # Reads out choices user submitted so far
    if (length(choices) > 0) {
      print("Current Choices: ")
      
      for (i in 1:length(choices)) {
        print(paste("[", i, "] ", choices[i], sep = ""))
      }
    }
    
    choice = readline("Enter a ticker symbol to select a company: ")
    
    if (!choice %in% companies[["Symbol"]]) {
      print("That is not a valid ticker.")
    } 
    else {
      count = count + 1
      choices = append(choices, choice)
    }
  }
  
  return(choices)
}

# Selecting highest/lowest 30 companies regarding price/earnings column
selectEarning = function(earn, choices) {
  companies = read_excel(market)
  
  result = switch(
    earn, 
    "high" = {
      companies = head(companies %>% arrange(desc(`Price/Earnings`)), n = 30)
    }, 
    "low" = {
      companies = head(companies %>% arrange(`Price/Earnings`), n = 30)
    }
  )
  
  return(companies[["Symbol"]])
}

# Shows lollipop graph comparing the 52 Week High and 52 Week Low of select companies
showLollipop = function() {
  print("Group data by:")
  print("[1] 2 custom-selected companies")
  print("[2] 5 custom-selected companies")
  print("[3] 10 custom-selected companies")
  print("[4] 30 highest-earning companies")
  print("[5] 30 lowest-earning companies")
  
  choice = 0
  choices = c()
  companies = read_excel(market)
  
  while (choice < 1 || choice > 4) {
    choice = readline("Enter the number of your choice: ")
    
    if (choice < 1 || choice > 4) {
      print("That is not a valid option.")
    }
  }
  
  order = NULL
  
  result = switch(
    choice,
    "1" = {
      choices = selectCustom(2, choices)
    }, 
    "2" = {
      choices = selectCustom(5, choices)
    }, 
    "3" = {
      choices = selectCustom(10, choices)
    }, 
    "4" = {
      choices = selectEarning("high", choices)
      order = "highest"
    }, 
    "5" = {
      choices = selectEarning("low", choices)
      order = "lowest"
    }
  )
  
  data = data.frame(matrix(ncol = 14, nrow = 0))
  
  colnames(data) = c("Symbol", "Name", "Sector", "Price", "Price/Earnings", 
                     "Dividend Yield", "Earnings/Share", "52 Week Low", 
                     "52 Week High", "Market Cap", "EBITDA", "Price/Sales", 
                     "Price/Book", "SEC Filings")
  
  for (row in 1:nrow(companies)) {
    if (companies[row, "Symbol"] %in% choices) {
      data[nrow(data) + 1,] = companies[row, ]
    }
  }
  
  graph = ggplot(data) + 
    geom_segment( aes(x = `52 Week Low`, xend = `52 Week High`, y = Symbol, yend = Symbol)) + 
    geom_point( aes(x = `52 Week Low`, y = Symbol), color = rgb(0, 0, 0), size = 1) + 
    geom_point( aes(x = `52 Week High`, y = Symbol), color = rgb(0, 0, 0), size = 1) + 
    labs(
      x = "52 Week High/Low Difference", 
      y = "Company", 
      title = "52 Week High/Low Comparison", 
      subtitle = paste("Showing the 52 week high/low difference of 30 companies with the ", order, " price/earnings.", sep = "")) + 
    theme_classic()
  
  print(graph)
}

# Shows a line graph of a company's share price over a certain time frame
showShareLine = function() {
  choices = c()
  
  choices = selectCustom(1, choices)
  
  # Displaying time frame options
  print("Time Frames:")
  print("[1] 1 Month")
  print("[2] 6 Months")
  print("[3] 1 Year")
  print("[4] 5 Years")
  print("[5] Max")
  
  choice2 = 0
  
  while (choice2 < 1 || choice2 > 5) {
    choice2 = readline("Enter the number of your choice: ")
    company = NULL
    
    if (choice2 < 1 || choice2 > 5) {
      print("That is not a valid option.")
    }
  }
  
  days = NULL
  time = NULL 
  
  # Choosing the amount of days (rows) to be used
  result = switch(
    choice2, 
    "1" = {
      days = 30
      time = "1 month."
    }, 
    "2" = {
      days = 180
      time = "6 months."
    }, 
    "3" = {
      days = 365
      time = "1 year."
    }, 
    "4" = {
      days = 1825
      time = "5 years."
    }, 
    "5" = {
      days = Inf
      time = "the max available time."
    }
  )
  
  company = read_excel(allShares, sheet = choices, n_max = days)
  
  graph = ggplot(data = company, aes(x = Date, y = `Close/Last`, group = 1)) + 
    geom_line() + 
    labs(
      x = "Date", 
      y = "Share Price", 
      title = "Share Price Evolution", 
      subtitle = paste("Showing ", choices, "'s share prices over ", time, sep = "")) + 
    theme_classic()
  
  print(graph)
}

# Connect to the database
connect()

# Call the main / menu function
seeMenu()
