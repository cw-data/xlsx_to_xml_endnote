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

######################################
### make and print a basic xml tree

xml <- xmlTree()
# names(xml) # list out the functions available in xmlTree()
xml$addNode("xml", close=FALSE) # adds an opening <xml> tag and leaves it open until we close it
xml$addNode("records", close=FALSE) # adds an opening <records> tag and leaves it open until we close it 
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

# known problems with this basic xml tree:
### 1. Despite <xml> being the root tag in endnote's xml export, the schema rejects a root <xml> tag.
### need to test a real endnote import to see if wants the root <xml> tag or not
### 2. 



node <- xmlNode("foo", attrs=c(a="1", b="my name"))

xmlGetAttr(node, "a")
xmlGetAttr(node, "doesn't exist", "My own default value")

xmlGetAttr(node, "b", "Just in case")
