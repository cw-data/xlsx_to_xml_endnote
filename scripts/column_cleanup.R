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
    select(-c('ID', 'Start time', 'Completion time', 'Email', 'Name')) # confirm that we don't want to Forms metadata in EndNote
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
# test_book <- book[,grep("Title", colnames(book), ignore.case=TRUE)] %>% # select column names that contain the case-insensitive word "title" (for an example only; real would take df book)
#     dplyr::select_if(~ !any(is.na(.))) %>% # then keep only "title" columns that have non-NA values
#     dplyr::rename_at(vars(contains("series title")), # if the colname contains case-insensitive "series title" # https://blog.exploratory.io/renaming-column-names-for-multiple-columns-together-9d216e37bf41
#                      funs(str_replace(., "Series Title.*", "title"))) # regex replace # https://stackoverflow.com/questions/16577432/non-greedy-string-regular-expression-matching
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

########################  change dataframe to list
### step 1, create empty list with only a root element
nextexample <- list(
    xml = list(
        records = list()
    ) # create list named 'records'
)
cat(as.character(xml2::as_xml_document(nextexample))) # print to console

### step 2, loop to add a list element for each record
n_rows <- seq(1:nrow(test_book)) # a plain-english iterator variable (instead of 'i' and 'j')
# for (row in rows) { # sanity check for syntax
#   print("hello")
# }
for (row in n_rows) {
    nextexample[["xml"]][["records"]][[row]] <- list() # loop to add an element to 'records' for each row in df 'data3'
}
names(nextexample[["xml"]][["records"]]) <- rep("record", nrow(test_book)) # set the name of each 'records' element to 'record', still troubleshooting how to add this into the loop
cat(as.character(xml2::as_xml_document(nextexample))) # print to console

### step 3, loop to add columns from df 'data3' as tags in each <record>
# instead, I need to add into each <record> an empty list() named for each column in test_book:
df_cols_list <- vector(mode = "list", length = ncol(test_book))
names(df_cols_list) <- colnames(test_book)
# now, add correct nesting structure for each list
# df_cols_list$`ref-type` <- list(name = structure(list()), test = "test")
# figure out what the correct structure is:
endnote_example <- xml2::read_xml("data/20221116_endnote_example.xml")
xml_structure(endnote_example) # can I just take this structure and copy it like a template for each record of each ref-type???

# step 3.x: repeat for each tag so the structure is correct
# step 3.a: make <ref-type> match xml endnote_example
# list(style = structure(list(), face="normal", font="default", size="100%"))
# <ref-type name="Web Page">12</ref-type>
library(xml2)
library(tibble)
# re-create the ref-type dictionary EndNote uses:
xml_find_all(endnote_example, xpath = "//ref-type") # find the key-value pairs we need to extract 
ref_lookup <- tibble(number = xml2::xml_text(xml2::xml_find_all(endnote_example, xpath = "//ref-type")), # taken from here: https://www.robwiederstein.org/2021/03/05/xml-to-dataframe/
                     value = xml2::xml_attrs(xml2::xml_find_all(endnote_example, xpath = "//ref-type")))

df_cols_list$`ref-type` <- structure(list(), name = as.character(test_book[1, 1]))
for (i in 2:length(df_cols_list)) {
    df_cols_list[[i]] <- list()
}

for (row in n_rows) {
    nextexample[["xml"]][["records"]][[row]] <- df_cols_list # loop to add a list with element names that match colnames(data3)
}

### step 4, add values from df 'data3' as values in each <record>
for (row in n_rows) {
    nextexample$xml$records[[row]]$`ref-type`$text <- as.character(ref_lookup[2, 1]) # loop ref_lookup$value into $text for each <record><ref-type>
    # must be as.character() in this loop to change pointer `test_book[row, 1]` into xml2-readable character string...
}
# for (row in n_rows) {
#     nextexample[["xml"]][["records"]][[row]][["author"]]$style$text <- as.character(test_book[row, 2]) # loop test_book$author values into $style$text for each <record>
# }
cat(as.character(xml2::as_xml_document(nextexample))) # print to console
# nextexample2 <- xml2::as_xml_document(nextexample)




# scratchpad:
# extract any column with 'title' in the column name
titles <- book[,grep("Title", colnames(book), ignore.case=TRUE)] # https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r
# keep only columns with non-NA values
test <- titles %>% dplyr::select_if(~ !any(is.na(.))) # https://www.statology.org/remove-columns-with-na-in-r/


excel_example <- readxl::read_excel("data/20221116_excel_example.xlsx")
library(xml2)
library(XML)

# how to make xml into a list
# endnote_example <- XML::xmlToList(XML::xmlParse("data/20221116_endnote_example.xml")) # the top-level tag here is <records> which does not match the top tag in data/20221116_endnote_example.xml
endnote_example2 <- xml2::as_list(xml2::read_xml("data/20221116_endnote_example.xml")) # the top-level tag here is <xml>, which seems to match the top tag in data/20221116_endnote_example.xml

endnote_example3 <- xml2::read_xml("data/20221116_endnote_example.xml")
cat(as.character(endnote_example3))
