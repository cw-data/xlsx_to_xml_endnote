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
# book <- 
#     book %>% # remove columns from 'book' that we know won't have an xml tag
#     select(-c('ID', 'Start time', 'Completion time', 'Email', 'Name')) # confirm that we don't want to Forms metadata in EndNote
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
######################## find what we still need to fill in on 'map of xlsx -> xml fields' table in '20221022_endnote_dev.docx'
missing_values <- lookup %>%
    filter(is.na(xml_tag)) %>%
    arrange(xlsx_colname)



########################  Use a lookup table to change colnames
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
    xml = list( # root element
        records = list() # create list named 'records'
    )
)
cat(as.character(xml2::as_xml_document(nextexample))) # print to console

### step 2, make template xml tag structure from 'Map of XLSX <- XML fields' in '20221022_endnote_dev.docx' into a list we loop into each <record>

# figure out what the correct structure is:
endnote_example <- xml2::read_xml("data/20221116_endnote_example.xml")
xml_structure(endnote_example) # this shows the tag structure we need to match

# build a list to match the tag structure of `endnote_example`
# each <record> will get a copy of this top-level tag structure
# if the <record> doesn't have data to route into these tags, we'll send empty tags to EndNote (and hopefully EndNote can deal with empty tags...)
tag_template <- vector(mode = "list", length = 15) # an empty list to hold tag-structure for each <record>
# I found that length = number by counting XML tags in 'Map of XLSX <- XML fields' in '20221022_endnote_dev.docx'
# the top-level tags are:
# 1: <ref-type>
# 2: <titles>
# 3: <contributors>
# 4: <dates>
# 5: <keywords>
# 6: <research-notes>
# 7: <electronic-resource-num>
# 8: <volume>
# 9: <number>
# 10: <pages>
# 11: <pub-location>
# 12: <publisher>
# 13: <edition>
# 14: <num-vols>
# 15: <section>

names(tag_template)[1] <- "ref-type"
names(tag_template)[2] <- "titles"
names(tag_template)[3] <- "contributors"
names(tag_template)[4] <- "dates"
names(tag_template)[5] <- "keywords"
names(tag_template)[6] <- "research-notes"
names(tag_template)[7] <- "electronic-resource-num"
names(tag_template)[8] <- "volume"
names(tag_template)[9] <- "number"
names(tag_template)[10] <- "pages"
names(tag_template)[11] <- "pub-location"
names(tag_template)[12] <- "publisher"
names(tag_template)[13] <- "edition"
names(tag_template)[14] <- "num-vols"
names(tag_template)[15] <- "section"
tag_template # sanity check

# 1: <ref-type>
# EndNote uses a dictionary to provide a ref-type number for each ref-type name (e.g., "Book" is ref-type #6)
# we need to re-create that dictionary
endnote_example <- xml2::read_xml("data/20221116_endnote_example.xml")
xml_find_all(endnote_example, xpath = "//ref-type") # these are the key-value pairs we need to extract 
ref_dict <- tibble::tibble(number = xml2::xml_text(xml2::xml_find_all(endnote_example, xpath = "//ref-type")), # extract key-value pairs
                           value = xml2::xml_attrs(xml2::xml_find_all(endnote_example, xpath = "//ref-type"))) # protocol from: https://www.robwiederstein.org/2021/03/05/xml-to-dataframe/
tag_template$`ref-type` <- structure(list(), # tag should be an empty list that we will loop ref_dict$number into
                                     name = list()) # an empty attribute named $name that we will loop ref_dict$value into
# tag_template[["ref-type"]] # sanity check

# 2: <titles>
tag_template$`titles` <- structure(list( # $titles has three sub-tags: 1) title, 2) secondary-title, and 3: tertiary title
    title = structure(list( # each sub-tag has a <style> tag
        style = structure(list(), # each <style> tag is an empty list (where we route the reference's title data) and three attributes: 1) face, 2) font, and 3) size
                          face = "normal",
                          font = "default",
                          size = "100%"))),
    `secondary-title` = structure(list( # each sub-tag has a <style> tag
        style = structure(list(), # each <style> tag is an empty list (where we route the reference's title data) and three attributes: 1) face, 2) font, and 3) size
                          face = "normal",
                          font = "default",
                          size = "100%"))),
    `tertiary-title` = structure(list( # each sub-tag has a <style> tag
        style = structure(list(), # each <style> tag is an empty list (where we route the reference's title data) and three attributes: 1) face, 2) font, and 3) size
                          face = "normal",
                          font = "default",
                          size = "100%")))))# need to loop <author> tags for each author name
# tag_template$`titles` # sanity check

# 3: <contributors>
tag_template$`contributors` <- structure(list( # $titles has three sub-tags: 1) title, 2) secondary-title, and 3: tertiary title
    authors = list(),
    `secondary-authors` = list(),
    `tertiary-authors` = list())) # an empty list for us to loop <author> tags into
# tag_template$`contributors` # sanity check
author <- structure(list( # an empty list we loop to add an author name into to $contributors$authors or $contributors$`secondary-authors` or $contributors$`tertiary-authors`
    style = structure(list(), # each <style> tag is an empty list (where we route the reference's author name) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# author # sanity check

# 4: <dates>
tag_template$`dates` <- structure(list( # $dates has two possible sub-tags: 1) year, 2) pub-date
    year = structure(list( # <dates><year><style>
        style = structure(list(), # each <style> tag is an empty list (where we route the reference's year data) and three attributes: 1) face, 2) font, and 3) size
                          face = "normal",
                          font = "default",
                          size = "100%"))),
    `pub-date` = structure(list( # <dates><pub-date><date><style>
        date = structure(list( # each <pub-date> tag has one <date> tag which has a <style> tag
            style =structure(list(), # each <style> tag is an empty list (where we route the reference's $`pub-date`$date data) and three attributes: 1) face, 2) font, and 3) size
                             face = "normal",
                             font = "default",
                             size = "100%")))))))
# tag_template$`dates` # sanity check

# 5: <keywords>
tag_template$keywords <- list() # $keywords needs to be an empty list to receive <keyword> tags
`keyword` <- structure(list( # an empty list we loop to add a <keyword> element $keywords
    style = structure(list(), # each <keyword> has <style> tag is an empty list (where we route individual keywords) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# keyword

# 6: <research-notes>
tag_template$`research-notes` <- structure(list( # one empty list that receives research notes
    style = structure(list(), # each <research-notes> tag has a <style> tag is an empty list (where we route the research notes for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`research-notes`

# 7: <electronic-resource-num>
tag_template$`electronic-resource-num` <- structure(list( # one empty list that receives the record's single resource number (Stable URL or DOI)
    style = structure(list(), # each <electronic-resource-num> tag has a <style> tag is an empty list (where we route the URL for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`electronic-resource-num`

# 8: <volume>
tag_template$`volume` <- structure(list( # one empty list that receives the record's single volume (Volume # or Degree)
    style = structure(list(), # each <volume> tag has a <style> tag is an empty list (where we route the volume or degree for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`volume`
# 9: <number>
tag_template$`number` <- structure(list( # one empty list that receives the record's 'Issue` or `Document Number` (col AJ)
    style = structure(list(), # each <section> tag has a <style> tag is an empty list (where we route the 'Paper Number` for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`number`
# 10: <pages>
tag_template$`pages` <- structure(list( # $pages has two possible sub-tags: 1) end, 2) start
    end = structure(list( # <pages><end><style>
        style = structure(list(), # each <style> tag is an empty list (where we route the reference's end page data) and three attributes: 1) face, 2) font, and 3) size
                          face = "normal",
                          font = "default",
                          size = "100%"))),
    start = structure(list( # <pages><start><style>
        style = structure(list(), # each <style> tag is an empty list (where we route the reference's start page data) and three attributes: 1) face, 2) font, and 3) size
                          face = "normal",
                          font = "default",
                          size = "100%")))))
# tag_template$`pages` # sanity check
# 11: <pub-location>
tag_template$`pub-location` <- structure(list( # one empty list that receives the record's 'Conference location' (col BR) or 'Place Published'
    style = structure(list(), # each <pub-location> tag has a <style> tag is an empty list (where we route the 'Conference location' (col BR) or 'Place Published' for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`pub-location` # sanity check

# 12: <publisher>
tag_template$`publisher` <- structure(list( # one empty list that receives the record's 'Publisher', 'University' (col CF), or 'Institution' (col AG)
    style = structure(list(), # each <publisher> tag has a <style> tag is an empty list (where we route the 'Publisher', 'University' (col CF), or 'Institution' (col AG) for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`publisher` # sanity check

# 13: <edition>
tag_template$`edition` <- structure(list( # one empty list that receives the record's Edition'
    style = structure(list(), # each <publisher> tag has a <style> tag is an empty list (where we route the 'Edition' for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`edition` # sanity check

# 14: <num-vols>
tag_template$`num-vols` <- structure(list( # one empty list that receives the record's 'Session` (col BN)
    style = structure(list(), # each <section> tag has a <style> tag is an empty list (where we route the 'Paper Number` for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`num-vols` # sanity check

# 15: <section>
tag_template$`section` <- structure(list( # one empty list that receives the record's 'Paper Number` (column BO) or `Chapter Number` (col DD)
    style = structure(list(), # each <section> tag has a <style> tag is an empty list (where we route the 'Paper Number` for a record) and three attributes: 1) face, 2) font, and 3) size
                      face = "normal",
                      font = "default",
                      size = "100%")))
# tag_template$`section` # sanity check
# tag_template # sanity check

### step 3, add template list from step 2 for each record
n_rows <- seq(1:nrow(test_book))
for (row in n_rows) { # a plain-english iterator variable (instead of 'i' and 'j')
    nextexample[["xml"]][["records"]][[row]] <- tag_template # loop to add a template list of tags
}
# cat(as.character(xml2::as_xml_document(nextexample))) # won't print lists with NULL values

### step 4, route values from df into each <record>
nextexample2 <- nextexample # making a copy so I don't have to re-run lines to debug
# 1: <ref-type>
# <ref-type name = >
# <ref-type><style>{text}
# 2: <titles>
# 3: <contributors>
n_authors <- str_count(test_book$authors[1], "\r\n") + 1 # calculate how many <author> tags we need 
for (auth in n_authors) { # a plain-english iterator variable (instead of 'i' and 'j')
    nextexample$xml$records$record$contributors$authors$author[[auth]] <- author # loop to add a template list of tags
}
n_editors <- str_count(test_book$authors[1], "\r\n") + 1 # calculate how many <author> tags we need 
for (edit in n_editors) { # a plain-english iterator variable (instead of 'i' and 'j')
    nextexample$xml$records$record$contributors$`secondary-authors`[[edit]] <- author # loop to add a template list of tags
}
n_series_editors <- str_count(test_book$authors[1], "\r\n") + 1 # calculate how many <author> tags we need 
for (edit in n_editors) { # a plain-english iterator variable (instead of 'i' and 'j')
    nextexample$xml$records$record$contributors$`tertiary-authors`[[edit]] <- author # loop to add a template <author> tag
}
# 4: <dates>
# 5: <keywords>
# 6: <research-notes>
# 7: <electronic-resource-num>
# 8: <volume>
# 9: <number>
# 10: <pages>
# 11: <pub-location>
# 12: <publisher>
# 13: <edition>
# 14: <num-vols>
# 15: <section>


for (row in n_rows) {
    nextexample$xml$records[[row]]$`ref-type`$text <- as.character(ref_lookup[2, 1]) # loop ref_lookup$value into $text for each <record><ref-type>
    # must be as.character() in this loop to change pointer `test_book[row, 1]` into xml2-readable character string...
}
# for (row in n_rows) {
#     nextexample[["xml"]][["records"]][[row]][["author"]]$style$text <- as.character(test_book[row, 2]) # loop test_book$author values into $style$text for each <record>
# }
nextexample$xml$records$record$`ref-type`
cat(as.character(xml2::as_xml_document(nextexample))) # print to console
# nextexample2 <- xml2::as_xml_document(nextexample)


