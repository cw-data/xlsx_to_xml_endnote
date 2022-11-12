# This script script to converts a real Forms xlsx to match a real endnote-generated xml for one book entry

# This script will:
# A) Convert '~data/example_book_section.xlsx' into xml that exactly matches '~data/example_book_section.xml'

# B) We'll use three methods to validate the script-generated xml:
## B1) visually. Print xml to console and look at it. Does the xml look right?
## B2) Programmatically. Use xml2::xml_validate()
## B3) EndNote. Will EndNote accept the xml we generate as a valid record?

library(xml2)
library(tidyverse)
library(readxl)

#################### A) Convert '~data/example_book_section.xlsx' into xml that exactly matches '~data/example_book_section.xml'

# Step 1: read in xlsx as dataframe
book <- readxl::read_excel("data/example_book_section.xlsx")

# Step 2: combine repeated columns
# column_cleanup.R is in development. Once logic is finalized there, make it into a function that we call from here.


#################### B1) visually. Print xml to console and look at it. Does the xml look right?
cat(as.character(xml2::as_xml_document(records))) # print to console

#################### B2) Programmatically. Use xml2::xml_validate()

schema <- xml2::read_xml("resources/RSXML.xsd")
doc <- xml2::read_xml("data/.xml")
xml_validate(doc, schema)

#################### B3) EndNote. Will EndNote accept the xml we generate as a valid record?
# write to xml and use that xml as in an EndNote import
# XML::saveXML(xml, prefix='<?xml version="1.0" encoding="UTF-8"?>', file = "data/testxml1.xml") # save xml to file, added to .gitignore
