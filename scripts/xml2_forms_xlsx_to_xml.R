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
# 2c. examine tags with values
xml2::xml_children(xml_children(xml_children(endnote_example))) # navigate nested child nodes 


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

########## Step 4: repeat syntax from step 3 to hard-code xml that matches `data/example_book_section.xml`
# 4a. instantiate new xml document and add root node
real <- xml2::xml_new_root("xml") # instantiate root node
cat(as.character(xml2::as_xml_document(real))) # sanity check

# 4b. nest a level-1 child-node inside root (level-zero) node
xml_add_child(real, # the node into which you want to nest a child node
              "records") # the name of the node you're adding
cat(as.character(xml2::as_xml_document(real))) # sanity check
l1 <- xml2::xml_children(real) # define what the level-1 tags are

# 4c. nest a level-2 child node inside level-1 node
xml_add_child(l1, "record")
cat(as.character(xml2::as_xml_document(real))) # sanity check
l2 <- xml2::xml_children(l1) # define what the level-2 tags are

# 4d. nest level-3 child nodes inside level 2 node
xml_add_child(l2, "database", "Mesophication and PhD.enl") # follows syntax: xml_add_child(location, name, value)
xml_add_child(l2, "source-app", "EndNote")
xml_add_child(l2, "rec-number", 442)
xml_add_child(l2, "foreign-keys")
xml_add_child(l2, "ref-type", 5)
xml_add_child(l2, "contributors")
xml_add_child(l2, "titles")
xml_add_child(l2, "foreign-keys")
xml_add_child(l2, "pages")
xml_add_child(l2, "volume")
xml_add_child(l2, "dates")
xml_add_child(l2, "pub-location")
xml_add_child(l2, "publisher")
xml_add_child(l2, "urls")
xml_add_child(l2, "remote-database-name")
xml_add_child(l2, "language")
cat(as.character(xml2::as_xml_document(real))) # sanity check
l3 <- xml2::xml_children(l2) # define what the level-3 tags are

# 4d. nest level-4 child nodes inside level 3 nodes
xml_add_child(l3[4], "key", 442) # "foreign-keys"
xml_add_child(l3[6], "authors") # "contributors"
xml_add_child(l3[6], "secondary-authors")  # "contributors"
xml_add_child(l3[7], "title") # "titles"
xml_add_child(l3[7], "secondary-title") # "titles"
xml_add_child(l3[7], "tertiary-title") # "titles"
xml_add_child(l3[8], "style", 844) # "pages"
xml_add_child(l3[9], "style", 1) # "volume"
xml_add_child(l3[10], "year") # "dates"
xml_add_child(l3[11], "style", "International Institute of Tropical Forestry San Juan, PR Rocky Mountain Research Station Fort Collins, CO") # "pub-location"
xml_add_child(l3[12], "style", "Forest Service, United States Department of Agriculture") # "publisher"
xml_add_child(l3[14], "style", "Zotero") # "remote-database-name"
xml_add_child(l3[15], "style", "en") # "language"
cat(as.character(xml2::as_xml_document(real))) # sanity check
l4 <- xml2::xml_children(l3) # define what the level-4 tags are

# 4e. nest level-5 child nodes inside level 4 nodes
xml_add_child(l4[2], "author") # "authors"
xml_add_child(l4[2], "author") # "authors"
xml_add_child(l4[3], "author") # "secondary-authors"
xml_add_child(l4[4], "style", "Kalmia latifolia") # "title"
xml_add_child(l4[5], "style", "Wildland shrubs of the United States and its Territories: thamnic descriptions: Volume 1") # "secondary-title"
xml_add_child(l4[6], "style", "General Technical Report IITF-GTR-26") # "tertiary-title"
cat(as.character(xml2::as_xml_document(real))) # sanity check
l5 <- xml2::xml_children(l4) # define what the level-5 tags are

# 4f. nest level-6 child nodes inside level 5 nodes
xml_add_child(l5[1], "style", "McNab, W. Henry")
xml_add_child(l5[2], "style", "Clinton, Barton D.")
xml_add_child(l5[2], "style", "Clinton, Barton D.")
cat(as.character(xml2::as_xml_document(real))) # sanity check
l6 <- xml2::xml_children(l5) # define what the level-5 tags are