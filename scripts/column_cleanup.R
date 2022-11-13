# a script to combine repeated xlsx columns into single dataframe columns

rm(list = ls())
library(xml2)
library(tidyverse)
library(readxl)
library(tidyr)
library(data.table)

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



########################  how to change colnames based on logic
# Method 1: rename_at statements
# a small example to wrap my head around:
test_book <- book[,grep("Title", colnames(book), ignore.case=TRUE)] %>% # select column names that contain the case-insensitive word "title" (for an example only; real would take df book)
    dplyr::select_if(~ !any(is.na(.))) %>% # then keep only "title" columns that have non-NA values
    dplyr::rename_at(vars(contains("series title")), # if the colname contains case-insensitive "series title" # https://blog.exploratory.io/renaming-column-names-for-multiple-columns-together-9d216e37bf41
                     funs(str_replace(., "Series Title.*", "title"))) # regex replace # https://stackoverflow.com/questions/16577432/non-greedy-string-regular-expression-matching
# my concern with method 1:
# we're going to have a ton of repeated code. I think we'd repeat most of the string-matching logic for each type of reference (i.e., each group of columns with non-NA values)
# I think it's going to get confusing to write regex to make rename_at matching work right.
# When we eventually had good regex logic, it'd still be tough to maintain it in the future if column names changed.

# Method 2: lookup table
# a small example to wrap my head around:
test_book <- book
data.table::setDT(test_book)
lookup2 <- lookup %>% # using the lookup table means you only have to maintain the rename logic in one place: the lookup table
    dplyr::filter(!is.na(xml_tag)) # filtering out NAs until we completely fill in the lookup table
test_book <- test_book %>%
    dplyr::select_if(~ !any(is.na(.))) # select only the columns with non-NA values
data.table::setnames(test_book, old = lookup2$xlsx_colname, new = lookup2$xml_tag, skip_absent = TRUE) # use the lookup table to set column names
# I think method 2 will require far less code which will make it easier to understand and maintain



# scratchpad:
# extract any column with 'title' in the column name
titles <- book[,grep("Title", colnames(book), ignore.case=TRUE)] # https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r
# keep only columns with non-NA values
test <- titles %>% dplyr::select_if(~ !any(is.na(.))) # https://www.statology.org/remove-columns-with-na-in-r/
