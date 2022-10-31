# a script to convert Forms data to EndNote-acceptable XML at scale 

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyverse)
library(XML)
library(xml2)

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
                    authors = "Jane Doe",
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

xml <- XML::xmlTree() # start with an empty xml tree
# xml$addNode("xml", close=FALSE) # add top-level nodes. adds an opening <xml> tag and leaves it open until we close it
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
# xml$closeTag() # close the xml tag: </xml>
cat(XML::saveXML(xml, prefix='<?xml version="1.0" encoding="UTF-8"?>')) # print to console the xml we just looped to create
# XML::saveXML(xml, prefix='<?xml version="1.0" encoding="UTF-8"?>', file = "data/testxml1.xml") # save xml to file, added to .gitignore

# known problems with this xml tree, based on "scripts/xml_vs_schema_validation.R":

### 1. Despite <xml> being the root node in endnote's xml export, validating EndNote's exprot XML against its own schema fails at the root <xml> node,
### <xml> tag is not present in "resources/RSXML.xsd" # resolved by removing tags <xml></xml> from "data/real_enl_xml.xml"
### need to test a real endnote import to see if wants the root <xml> tag or not.
### 2. Same thing as #1. <foreign-keys> tag is not present in "resources/RSXML.xsd" # resolved by removing tags and data <foreign-keys>data</foreign-keys> from "data/real_enl_xml.xml"
### 3. Same thingas #1. got <pdf-urls> but expected <image-urls>. # resolved by replacing tags <pdf-urls></pdf-urls> with <image-urls></image-urls> in "data/real_enl_xml.xml"
### with those changes, "data/real_enl_xml.xml" validates as TRUE "resources/RSXML.xsd"


######################################
# convert a real xml to dataframe
# https://stackoverflow.com/questions/33446888/r-convert-xml-data-to-data-frame

doc<-XML::xmlParse("data/real_enl_xml.xml")
xmldf <- XML::xmlToDataFrame(nodes = getNodeSet(doc, "//record"))

######################################
# xml is basically the same data structure as a list in R
# so how do we get our dataframe into nested list format that matches the format EndNote provides?

# First, test that we can translate from list to xml and back reliably
xml_test_in <- XML::xmlToList(XML::xmlParse("data/real_enl_xml.xml")) # translation from xml to list
# xml_test_in <- xml2::as_list(xml2::read_xml("data/enl_xml_schema.txt"))
xml_test_out <- xml2::as_xml_document(xml_test_in) # does not work, back-translate list to xml
# XML::saveXML(xml_test, prefix='<?xml version="1.0" encoding="UTF-8"?>', file = "data/testxml1.xml") # save

# simple list as proof of concept
records <- list(records = list(
  record = list(title = "titlehere")
))
cat(as.character(xml2::as_xml_document(records)))
##

# slightly more complicated nested list as proof of concept
# create list with one record and nest down to a single nonsense text value
records <- list(records = list(
  record = list(
    contributors = list(
      authors = list(
        author = list(
          text = "something"
        )
        )
      )
    )
  )
)
cat(as.character(xml2::as_xml_document(records)))


# again, slightly more complicated nested list as proof of concept
# create list with one record and nest down to a two nonsense text values
records <- list(records = list(
  record = list(
    contributors = list(
      authors = list(
        author = list(
          text = "something"
          )
        ),
      titles = list(
        title = list(
          text = "a title"
          )
        )
      )
    )
  )
  )

cat(as.character(xml2::as_xml_document(records)))
cat(as.character(xml2::as_xml_document("data/enl_xml_schema.txt"))) # for comparison
# xml2::write_xml(xml2::as_xml_document(records),
#                 "data/testxml1.xml")# save this output for testing in endnote


# again, slightly more complicated nested list as proof of concept
# take the nested structure from above and loop to 'decide' how many <record> tags to create
data3 <- data.frame( # a dataframe with 1 records, 2 fields, 2 author names in a character string in one of the fields
  record_id = c(1,2),
  author = c("Jimi Hendrix", "John Frusciante")
)

# the basic structure we're shooting for, a list named 'records' with two elements named 'record'
# but we want a loop to decide how many <record> tags to add, depending on how many rows are in our dataframe 'data3'
# here's what the output is supposed to look like:
records <- list( # create list named 'records'
  records = list( # add root element named 'records'
    record = list(), # add child element named 'record' for each record (i.e., a row in the df)
    record = list() # add child element named 'record' for each record (i.e., a row in the df)
    )
  )
cat(as.character(xml2::as_xml_document(records))) # print to console

### step 1, create empty list with only a root element
records <- list( # create list named 'records'
  records = list( # add root element named 'records'
  )
)
cat(as.character(xml2::as_xml_document(records))) # print to console

### step 2, loop to add a list element for each record
n_rows <- seq(1:nrow(data3)) # a plain-english iterator variable (instead of 'i' and 'j')
# for (row in rows) { # sanity check for syntax
#   print("hello")
# }
for (row in n_rows) {
  records[["records"]][[row]] <- list() # loop to add an element to 'records' for each row in df 'data3'
}
names(records[["records"]]) <- rep("record", nrow(data3)) # set the name of each 'records' element to 'record', still troubleshooting how to add this into the loop
cat(as.character(xml2::as_xml_document(records))) # print to console

### step 3, loop to add columns from df 'data3' as tags in each <record>
# in a perfect world, I'd build a list of colnames and iterate that list into each <record> tag like this:
# df_cols_list <- vector(mode = "list", length = ncol(data3))
# names(df_cols_list) <- colnames(data3)
# for (row in rows) {
#     records[["records"]][[row]] <- df_cols_list # loop to add a list with element names that match colnames(data3)
#     }
# cat(as.character(xml2::as_xml_document(records))) # but that breaks the xml output ??!!??!!

# since building lists piecemeal breaks xml2::as_xml_document, I'll hard code the colnames for now
# this is bad practice and I need to find a scaleable way to do this
for (row in n_rows) {  # loop to add a record_id and author element to each <record>
  records[["records"]][[row]] <- list(
    record_id = list(), # add a record_id element for each <record> and add style arguments to match endnote
    author = list(style = structure(list(), face="normal", font="default", size="100%")) # an author element for each <record> and add style arguments to match endnote
  )
}
cat(as.character(xml2::as_xml_document(records))) # print to console


### step 4, add values from df 'data3' as values in each <record>
for (row in n_rows) {
  records[["records"]][[row]][[1]]$text <- data3[row, 1] # loop data3$record_id values into $text for each <record>
}
for (row in n_rows) {
    records[["records"]][[row]][[2]]$style$text <- data3[row, 2] # loop data3$author values into $style$text for each <record>
}
cat(as.character(xml2::as_xml_document(records))) # print to console











##






# useful syntax to build nested lists:

as_xml_document(list(foo = list(
  bar = structure(list(), id = "a"),
  bar = structure(list(), id = "b")))) # how to add attributes to a list and output as xml













##








