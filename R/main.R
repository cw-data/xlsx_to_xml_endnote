

#----- load external libraries
library(readxl)
library(dplyr)
library(data.table)
library(xml2)

#----- load project functions
rm(list=ls())
source("R/loadData.R")
# source("R/buildXML.R")
source("R/buildXML.R")

# forms_spreadsheet <- "data/20221116_excel_example.xlsx" # old spec
forms_spreadsheet <- "data/20230129/Forms_output_for_comparison.xlsx"
loadData(forms_spreadsheet)
buildXML(record_list=record_list, write=TRUE)
