

library(DBI)
library(RSQLite)
library(rstudioapi)
library(tidyverse)
library(readxl)
library(readr)
library(data.table)

# Sets the working directory to location of R script
currentDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(currentDir)


data = fread("constituents-financials_csv.csv", select = c("Symbol"))

print(data)