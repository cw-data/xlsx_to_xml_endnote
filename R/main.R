

#----- load external libraries
library(readxl)
library(dplyr)
library(data.table)
library(xml2)

#----- load project functions
rm(list=ls())
source("R/loadData.R")
# source("R/buildXML.R")

forms_spreadsheet <- "data/20221116_excel_example.xlsx"
loadData(forms_spreadsheet = forms_spreadsheet)
