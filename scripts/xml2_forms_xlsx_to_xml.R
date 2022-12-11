# minimal viable product
# using xml2::xml_new_root() to build xml to match data/example_book_section.xlsx

# this is an alternative to building an R list and then converting the list to xml
# I'm abandoning the list-to-xml approach in favor of directly building xml
# because I can't consistently cat() the list to console, so I can't debug, which makes development impossible

library(xml2)

########## Step 1: read in xlsx as dataframe
book <- read.csv("data/test_book.csv") # run lines 1 through 187 of `scripts/column_cleanup.R` to reproduce this csv
book$X <- NULL

########## Step 2: read in example xml
endnote_example <- xml2::read_xml("data/example_book_section.xml") # read
# 2a. pretty-print example to console
cat(as.character(xml2::as_xml_document(endnote_example)))
# 2b. examine xml tag structure of example
xml2::xml_structure(endnote_example)

########## Step 3: build a minimal reprex xml from scratch
# read the docs: https://cran.r-project.org/web/packages/xml2/xml2.pdf
# 3a. instantiate new xml document and add root node
x <- xml2::xml_new_root("root") # from docs
cat(as.character(xml2::as_xml_document(x))) # sanity check
# 3b. hard-code a child-node and nest another node inside it
l1 <- xml_add_child(x, # the node into which you want to add a child node
                    "step1") # the name of the node you're adding
l2 <- xml_add_child(l1,
                    "step2",
                    "test value")
cat(as.character(xml2::as_xml_document(x))) # sanity check
# based on pg 11-12 of https://cran.r-project.org/web/packages/xml2/xml2.pdf
# assign attribute to first-level node
doc1 <- xml2::xml_children(x)[[1]] # find 'level' you need to access, then index to the correct node
xml2::xml_attr(doc1, "id") <- "one" # use xml_attr() to create attribute and assign it a value
cat(as.character(xml2::as_xml_document(x))) # sanity check
# assign attribute to second-level node
doc2 <- xml2::xml_children(l1)[[1]]
xml2::xml_attr(doc2, "id") <- "two"
cat(as.character(xml2::as_xml_document(x))) # sanity check

########## Step 3: hard-code xml that matches `data/example_book_section.xml`