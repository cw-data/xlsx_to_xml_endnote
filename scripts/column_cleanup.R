# a script to combine repeated xlsx columns into single dataframe columns

rm(list = ls())
library(xml2)
library(tidyverse)
library(readxl)
library(tidyr)
library(data.table)
library(tibble)

########## Step 1: read in xlsx as dataframe
book <- readxl::read_excel("data/example_book_section.xlsx")
# book <- readxl::read_excel("data/R8 OG Reference Entry(1-3).xlsx")
book <-
    book %>% # remove columns from 'book' that we know won't have an xml tag
    select(-c('ID', 'Start time', 'Completion time', 'Email', 'Name')) # confirm that we don't want to Forms metadata in EndNote
##########  Step 2: make a lookup table. A dataframe that has each xlsx column name and its equivalent xml tag.
xlsx_colname <- colnames(data) # capture the xlsx column names in an array
lookup <- data.frame( # make a dataframe named 'lookup'
    xlsx_colname = xlsx_colname, # add a column named $xlsx_colnames to 'lookup' and assign it values 'xlsx_colnames'
    xml_tag = NA # add empty column named xml_tags so we can fill correct values in next steps
    )
### assign values to lookup$xml_tags
### the $xml_tags value logic here is taken from 'map of xlsx -> xml fields' table in '20221022_endnote_dev.docx'
# <ref-type>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("reference type", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word:
    'ref-type', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <secondary-title>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("book title", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # if $xlsx_colnames[row] contains this word OR
        grepl("journal", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # this word OR
        grepl("newspaper", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # this word OR
        grepl("Academic Department", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # this word OR
        grepl("Conference name", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # this word:
    'secondary-title', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <tertiary-title>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("Series Title", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word AND
    'tertiary-title', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <date>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("year", lookup$xlsx_colname, ignore.case = TRUE) == TRUE |
        grepl("date", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word AND
    'date', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <title>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("title", lookup$xlsx_colname, ignore.case = TRUE) == TRUE & # if $xlsx_colnames[row] contains this word AND
        is.na(lookup$xml_tag) == TRUE, # and $xml_tag hasn't been assigned a value (this prevents over-writing <secondary-title>):
    'title', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <author>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("author", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'authors', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <attach-files>
lookup$xml_tag <- ifelse(
    grepl("Attach files", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'pdf-urls', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <keyword>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("where did the", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # if $xlsx_colnames[row] contains this word
        grepl("cover and/or", lookup$xlsx_colname, ignore.case = TRUE) == TRUE ,# or this word :
    'location', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <research-notes>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("reference description", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,
    'research-notes', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <electronic-resource-num>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("Stable URL", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'electronic-resource-num', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <volume>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("volume", lookup$xlsx_colname, ignore.case = TRUE) == TRUE |  # if $xlsx_colnames[row] contains this word OR
        grepl("degree", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # this word:
    'volume', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <number>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("issue", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'number', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <pages>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("Page Range", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'pages', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <tertiary-authors>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("series editor", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'tertiary-authors', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <pub-location>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("place published", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # if $xlsx_colnames[row] contains this word OR
        grepl("Conference location", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # this word
    'pub-location', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <publisher>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("institution", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # if $xlsx_colnames[row] contains this word OR
        grepl("University", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # this word:
    'publisher', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <number>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("document number", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'number', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <pages>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("number of pages", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'pages', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <publisher>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("publisher", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'publisher', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <edition>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("edition", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'edition', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <num-vols>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("session", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'num-vols', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <section>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("paper number", lookup$xlsx_colname, ignore.case = TRUE) == TRUE |   # if $xlsx_colnames[row] contains this word OR
        grepl("chapter number", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # this word OR
        grepl("section", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # this word:
    'section', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <secondary-authors>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("editor", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
    'secondary-authors', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <image-urls>
# <author>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("photographer", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'authors', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# Photograph caption
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("caption", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'caption', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# Map description
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("map description", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'caption', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# Is this photograph in the public domain?
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("public domain", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'ppv-rev', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# Cartographer(s)
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("Cartographer", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'authors', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# Advisor
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("advisor", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'authors', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# Cover type
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("cover type", lookup$xlsx_colname, ignore.case = TRUE) == TRUE, # if $xlsx_colnames[row] contains this word
    'cover-type', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
######################## find what we still need to fill in on 'map of xlsx -> xml fields' table in '20221022_endnote_dev.docx'
missing_values <- lookup %>%
    filter(is.na(xml_tag)) %>%
    arrange(xlsx_colname)

########################  write lookup table to file
data.table::fwrite(lookup, "resources/colname_tagname_dictionary.csv")

