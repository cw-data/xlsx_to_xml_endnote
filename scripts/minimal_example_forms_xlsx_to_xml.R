# a minimial reprex as proof of concept that we can:
### 1) read the Forms .xlsx output into R
### 2) come up with a working XML tree
### 3) use that XML tree to map variables from the dataframe to XML
### 4) write out the XML file to disk

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyverse)
library(XML)

######################################
### 1) read the Forms .xlsx output into R
### simplify the dataset so we can ignore empty columns for now
data <-
  readxl::read_excel("data/R8 OG Reference Entry(1-3).xlsx") %>%
  dplyr::select(`Reference Type`, # grab only few columns for simplicity
                Title,
                `Author(s)`,
                Year,
                `Attach files (i.e., pdf, photograph)`,
                `Where did the research take place/what is the location of the reference?`,
                `Reference description`,
                `Cover type`,
                `Stable URL or DOI`) %>%
  dplyr::rename(reference_type = `Reference Type`, # rename columns for convenience
                title = Title,
                authors = `Author(s)`,
                year = Year,
                files_attached = `Attach files (i.e., pdf, photograph)`,
                where = `Where did the research take place/what is the location of the reference?`,
                description = `Reference description`,
                cover_type = `Cover type`,
                stable_url = `Stable URL or DOI`) %>%
  dplyr::filter(is.na(title) == FALSE) # just grab one row that's not missing any values

######################################
### 2) come up with a working XML tree
### 3) use that XML tree to map variables from the dataframe to XML
### should match ~data/enl_xml_schema.txt
# https://stackoverflow.com/questions/35234863/how-convert-a-data-frame-into-a-xml-file-with-r

xml <- xmlTree()
# names(xml) # list out the functions available in xmlTree()
xml$addTag("xml", close=FALSE) # adds an opening <xml> tag and leaves it open until we close it
xml$addTag("records", close=FALSE) # adds an opening <records> tag and leaves it open until we close it 
for (i in 1:nrow(data)) {
  xml$addTag("record", close=FALSE) # each row from data goes into its own <record> tag, leave the tag open until we close it
  for (j in colnames(data)) {
    xml$addTag(j, data[i, j]) # for each column in data, the value in [row i, column j] goes into a tag named <colname j>
    }
  xml$closeTag() # add a closing tag to each: </record>
  }
xml$closeTag() # close the records tag: </records>
xml$closeTag() # close the xml tag: </xml>
cat(saveXML(xml, prefix='<?xml version="1.0" encoding="UTF-8"?>')) # print to console the xml we just looped to create

### 4) write out the XML file to disk
saveXML(xml, "data/xml_output.xml", prefix='<?xml version="1.0" encoding="UTF-8"?>\n') # https://stackoverflow.com/questions/46006867/xml-encoding-using-r
