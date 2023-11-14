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

# Displaying menu
seeMenu = function() {
  choice = 1; 
  
  print("///// Stock Market Analysis Tool /////")
  
  while (choice != 0) {
    print("[1] See line graph of a company's change in share price")
    print("[2] See lollipop graph of select companies")
    choice = readline("Select an action (0 to exit): ")
    
    result = switch(
      choice, 
      "1" = showShareLGraph(),
      "2" = showShareLollipop(), 
    )
  }
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
showShareLollipop = function() {
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
showShareLGraph = function() {
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

# Running the program
connect()
seeMenu()
