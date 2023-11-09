# You will need to run these lines if the packages aren't already installed

#install.packages("rstudioapi")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("openxlsx")




library(rstudioapi)
library(tidyverse)
library(readxl)
library(openxlsx)




# Connect to the database
connect = function() {
  
  myData <- read_excel("C:/Users/abbys/Desktop/CIS336Project/MyData.xlsx")
  
  path <- "C:/Users/abbys/Desktop/CIS336Project/MyData.xlsx"
  
  sheet_names <- getSheetNames(path)
  
  return(list(myData = myData, path = path, sheet_names = sheet_names))
  
}



# Menu / main function
seeMenu = function(db) {
  
  choice = 1
  
  print("///// Stock Market Analysis Tool /////")
  
  while (choice != 0) {
    
    print("[2] See histogram of the highest/lowest 3 share prices")
    choice = readline("Select an action (0 to exit): ")
    
    result = switch(
      choice, 
      "2" = createHistogram("C:/Users/abbys/Desktop/CIS336Project/MyData.xlsx")
      
      
    )
  }
}




# Function to create a histogram
createHistogram = function(path) {
  
  
  # Read the sheet names from the Excel file
  sheets <- excel_sheets(path)
  print("Company options to select from: ")
  print(sheets)
  
  
  
  choice <- readline("Enter a company name to display shares: ")
  
  
  
  if (choice %in% sheets) {
    
    
    # Read data from the specified sheet
    company_data <- read_excel(path, sheet = choice)
    
    
    # Sort data
    high_shares <- company_data %>% arrange(desc(`Close/Last`))
    low_shares <- company_data %>% arrange(`Close/Last`)
    
    
    # Calculate highest / lowest shares
    high_three_shares <- head(high_shares, 10)
    low_three_shares <- head(low_shares, 10)
    
    
    # Get user input for highest / lowest
    print("[1] See histogram of the highest 3 share prices")
    print("[2] See histogram of the lowest 3 share prices")
    choice2 = readline("Select an action (0 to exit): ")
    
    
    # Create graph based on user input
    mygraph = switch(
      choice2, 
      
      "1" =     mygraph <- ggplot(data = high_three_shares, aes(x = rownames(high_three_shares), y = `Close/Last`)) + 
        geom_bar(stat = "identity") +
        labs(title = "Highest Three Share Prices", x = choice, y = "Share Price"),
      
      "2" =     mygraph <- ggplot(data = low_three_shares, aes(x = rownames(low_three_shares), y = `Close/Last`)) + 
        geom_bar(stat = "identity") +
        labs(title = "Lowest Three Share Prices", x = choice, y = "Share Price")
      
    )
    
    
    print(mygraph)
    
    
  } else {
    cat("The selected company is not within the Excel file \n")
  }
}




# Connect to the database
db <- connect()


# Call the main / menu function
seeMenu(db)


# Disconnect from the database
dbDisconnect(db)
