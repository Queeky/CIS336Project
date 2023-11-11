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
      companies = head(companies %>% arrange(desc(`Price/Earnings`)), n = 50)
      print(companies[c("Symbol", "Name", "Price/Earnings")], n = Inf)
    }, 
    "low" = {
      companies = head(companies %>% arrange(`Price/Earnings`), n = 50)
      print(companies[c("Symbol", "Name", "Price/Earnings")], n = Inf)
    }
  )
}

# Shows lollipop graph comparing the 52 Week High and 52 Week Low of select companies
showShareLollipop = function() {
  print("Group data by:")
  print("[1] 5 custom-selected companies")
  print("[2] 10 custom-selected companies")
  print("[3] 50 highest-earning companies")
  print("[4] 50 lowest-earning companies")
  
  choice = 0
  choices = c()
  companies = read_excel(market)
  # This should probably be global
  
  while (choice < 1 || choice > 4) {
    choice = readline("Enter the number of your choice: ")
    
    if (choice < 1 || choice > 4) {
      print("That is not a valid option.")
    }
  }
  
  result = switch(
    choice,
    "1" = {
      choices = selectCustom(5, choices)
    }, 
    "2" = {
      choices = selectCustom(10, choices)
    }, 
    "3" = {
      choices = selectEarning("high", choices)
    }, 
    "4" = {
      choices = selectEarning("low", choices)
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
    geom_point( aes(x = `52 Week High`, y = Symbol), color = rgb(0, 0, 0), size = 1)
  
  print(graph)
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
  print("[1] 1 Month")
  print("[2] 6 Months")
  print("[3] 1 Year")
  print("[4] 5 Years")
  print("[5] Max")
  
  printGraph = function(time) {
    company = read_excel(allShares, sheet = choice1, n_max = time)
    
    graph = ggplot(data = company, aes(x = Date, y = `Close/Last`, group = 1)) + geom_line()
    print(graph)
  }
  
  choice2 = 0
  
  while (choice2 < 1 || choice2 > 5) {
    choice2 = readline("Enter the number of your choice: ")
    company = NULL
    
    if (choice2 < 1 || choice2 > 5) {
      print("That is not a valid option.")
    }
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
}

# Running the program
connect()
seeMenu()
