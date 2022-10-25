# this is a scratch script where I'm working through how to do this process in the library(xml2) instead of library(XML)
# xml2 is a tidyverse package and XML is not
# I think using xml2 instead of XML will be less likely to cause deprecation problems in the future
# before I go any further scripting this routine in library(XML) I want to make the switch to library(xml2)

library(xml2)

### a minimal exam#ple based on
# https://stackoverflow.com/questions/69471448/using-xml2-to-add-child-node-with-an-attribute
testxml <- xml2::xml_new_document()
xml2::xml_add_child(testxml, "records")
xml_add_child(.x = testxml, .value = "database", name = "My Test library.enl", path = "C:\\Users\\mwroberts\\Documents\\My Test library.enl")

# how to print xml to console? for visual inspection
testxml # this sort of works, not really
xml_structure(testxml) # doesn't work
xml2::read_xml(testxml) # doesn't work



### scaled-up example with our two-record dataset
#########
rm(list = ls())
library(readxl)
library(dplyr)
library(tidyverse)
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
#########






testxml <- xml2::xml_new_document()
xml2::xml_add_child(.x = testxml, .value = "records")
for (i in 1:nrow(data)) { # for each row in our dataframe, we need to add these tags:
    xml2::xml_add_child(.x = testxml, .value = "record") # each row from data goes into its own <record> tag, leave the tag open until we close it
    xml_add_child(.x = testxml, .value = "database", name = "My Test library.enl", path = "C:\\Users\\mwroberts\\Documents\\My Test library.enl")
}
testxml
# xml_remove(.x = "records")
testxml

rm(testxml)
testxml <- xml2::xml_new_document() # how to instantiate a new xml document
xml_new_root(.x = testxml, .value = "test2") # how to add a root node to a new xml document
xml2::xml_add_child(.x = testxml, "records")
testxml




# need to replicate this looping logic
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



















xml_add_child(.x = testxml,
              .value = "record",
              .where = nrow(data))
xml_add_child(.x = testxml,
              .value = "database",
              name = "My Test library.enl",
              path = "C:\\Users\\mwroberts\\Documents\\My Test library.enl",
              .where = nrow(data))



h <- read_html("<p>Hi!</p>")

tmp <- tempfile(fileext = ".xml")
write_xml(h, tmp, options = "format")
readLines(tmp)
read_html(tmp)

read_xml(tmp)

# write formatted HTML output
write_html(h, tmp, options = "format")
readLines(tmp)
