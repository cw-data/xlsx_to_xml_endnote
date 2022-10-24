# a script to convert Forms data to EndNote-acceptable XML at scale 

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyverse)
library(XML)

######################################
### read the Forms .xlsx output into R
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

data2 <- data.frame(reference_type = "Journal", # make a fake record, so we can test iterating over multiple records
                    title = "Important science",
                    authors = "Person Name",
                    year = "1999",
                    files_attached = "https://usdagcc-my.sharepoint.com/personal/margaret_woodbridge_usda_gov/Documents/Apps",
                    where = "a place",
                    description = "this is a text description",
                    cover_type = "some cover type",
                    stable_url = "google.com")

data <- rbind(data, data2) # add the fake record to data
######################################
### build the realistic xml tree from xlsx data one node at a time
# make the nodes of our xml match the xml from "data/enl_xml_schema.txt"

xml <- xmlTree() # start with an empty xml tree
xml$addNode("xml", close=FALSE) # add top-level nodes. adds an opening <xml> tag and leaves it open until we close it
xml$addNode("records", close=FALSE) # another top-levle node. adds an opening <records> tag and leaves it open until we close it 
for (i in 1:nrow(data)) { # for each row in our dataframe, we need to add these tags:
  xml$addTag("record", close=FALSE) # each row from data goes into its own <record> tag, leave the tag open until we close it
  xml$addNode(name = "database", # how to add nodes with attributes (or namespaces) # from ?xmlTree
              attrs = c(
                name = "My Test library.enl",
                path = "C:\\Users\\mwroberts\\Documents\\My Test library.enl"
              ),
              "My Test library.enl")
  # <source-app name="EndNote" version="20.4">EndNote</source-app>
  xml$addNode(name = "source-app",
              attrs = c(name = "EndNote",
                        version = "20.4"),
              
              "EndNote")
  xml$addNode(name = "rec-number",
              i) # I don't know how EndNote assigns 'rec-number' so, for now, I'm guessing it's just index. May need to write logic to start increment at nrow(existing number of rows) + 1. tbd
  for (j in colnames(data)) { # we need to add tags at the intersection of row and column
    xml$addTag(j, data[i, j]) # for each column in data, the value in [row i, column j] goes into a tag named <colname j>
    }
  xml$closeTag() # add a closing tag to each: </record>
  }
xml$closeTag() # close the records tag: </records>
xml$closeTag() # close the xml tag: </xml>
cat(saveXML(xml, prefix='<?xml version="1.0" encoding="UTF-8"?>')) # print to console the xml we just looped to create


# known problems with this xml tree, based on "scripts/xml_vs_schema_validation.R":

### 1. Despite <xml> being the root node in endnote's xml export, validating EndNote's exprot XML against its own schema fails at the root <xml> node,
### <xml> tag is not present in "resources/RSXML.xsd" # resolved by removing tags <xml></xml> from "data/real_enl_xml.xml"
### need to test a real endnote import to see if wants the root <xml> tag or not.
### 2. Same thing as #1. <foreign-keys> tag is not present in "resources/RSXML.xsd" # resolved by removing tags and data <foreign-keys>data</foreign-keys> from "data/real_enl_xml.xml"
### 3. Same thingas #1. got <pdf-urls> but expected <image-urls>. # resolved by replacing tags <pdf-urls></pdf-urls> with <image-urls></image-urls> in "data/real_enl_xml.xml"
### with those changes, "data/real_enl_xml.xml" validates as TRUE "resources/RSXML.xsd"
