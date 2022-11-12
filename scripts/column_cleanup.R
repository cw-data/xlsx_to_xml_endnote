# a script to combine repeated xlsx columns into single dataframe columns

rm(list = ls())
library(xml2)
library(tidyverse)
library(readxl)

########## Step 1: read in xlsx as dataframe
book <- readxl::read_excel("data/example_book_section.xlsx")
book <- 
    book %>% # remove columns from 'book' that we know won't have an xml tag
    select(-c('ID', 'Start time', 'Completion time', 'Email', 'Name'))
##########  Step 2: make a lookup table. A dataframe that has each xlsx column name and its equivalent xml tag.
xlsx_colname <- colnames(book) # capture the xlsx column names in an array
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
    'author', # assign this value
    lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
)
# <attach-files>
# tbd, leaving a placeholder so I don't forget to update this
# lookup$xml_tag <- ifelse(
#     grepl("year", lookup$xlsx_colname, ignore.case = TRUE) == TRUE,  # if $xlsx_colnames[row] contains this word
#     'year', # assign this value
#     lookup$xml_tag # else: just leave the value of lookup$xml_tags as it was
# )
# <keyword>
lookup$xml_tag <- ifelse( # the value in lookup$xml_tags depends on the following logic:
    grepl("where did the", lookup$xlsx_colname, ignore.case = TRUE) == TRUE | # if $xlsx_colnames[row] contains this word
        grepl("cover and/or", lookup$xlsx_colname, ignore.case = TRUE) == TRUE ,# or this word :
    'keyword', # assign this value
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
# tbd, a placeholder so I don't forget to update this
# pages <- book[,grep("pages", colnames(book), ignore.case=TRUE)]
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
######################## find what we still need to fill in on 'map of xlsx -> xml fields' table in '20221022_endnote_dev.docx'
missing_values <- lookup %>%
    filter(is.na(xml_tag)) %>%
    arrange(xlsx_colname)


















# Step 3: create an empty dataframe with the colnames we want
# use unique(lookup$xml_tag) to assign colnames for this empty dataframe

# scratchpad:
# extract any column with 'title' in the colum name
titles <- book[,grep("Title", colnames(book), ignore.case=TRUE)] # https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r
# keep only columns with non-NA values
test <- titles %>% dplyr::select_if(~ !any(is.na(.))) # https://www.statology.org/remove-columns-with-na-in-r/



