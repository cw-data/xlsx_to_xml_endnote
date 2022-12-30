# minimal viable product
# using xml2::xml_new_root() to build xml to match data/example_book_section.xlsx

# this is an alternative to building an R list and then converting the list to xml
# I'm abandoning the list-to-xml approach in favor of directly building xml
# because I can't consistently cat() the list to console, so I can't debug, which makes development impossible

library(xml2)
library(stringr)
library(readxl)

########## Step 1: read in xlsx as dataframe
# book <- read.csv("data/20221230/20221230_test_book.csv") # run lines 1 through 187 of `scripts/column_cleanup.R` to reproduce this csv
clean_book <- read.csv("data/test_book.csv") # run lines 1 through 187 of `scripts/column_cleanup.R` to reproduce this csv
book <- test_book # test_book is the result of running scripts/column_cleanup.R in its entirety
clean_book$X <- NULL

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
# 4.1. instantiate new xml document and add root node
real <- xml2::xml_new_root("xml") # instantiate root node
cat(as.character(xml2::as_xml_document(real))) # sanity check

# 4.2. nest a level-1 child-node inside root (level-zero) node
xml_add_child(real, # the node into which you want to nest a child node
              "records") # the name of the node you're adding
cat(as.character(xml2::as_xml_document(real))) # sanity check

# 4.3. nest a level-2 child node inside level-1 node
l1 <- xml2::xml_children(real) # define what the level-1 tags are
xml2::xml_children(l1)
# xml_add_child(l1, "record")
for(row in 1:nrow(test_book)){ # loop that adds one <record> tag for each row in the df
    xml_add_child(l1, "record")
}
xml2::xml_children(l1)
cat(as.character(xml2::as_xml_document(real))) # sanity check

# ref-type lookup table
ref_type_lookup <- readxl::read_excel("data/endnote_ref-type_dictionary.xlsx")
ref_type_lookup$key <- ref_type_lookup$`Reference type (from Form)`
ref_type_lookup$value <- stringr::str_extract(ref_type_lookup$XML, "([0-9]+)")

# 4.4. nest level-3 child nodes inside level 2 node
l2 <- xml2::xml_children(l1) # define what the level-2 tags are
xml2::xml_children(l2) # look at the l4 tags
# xml_add_child(l2, "database", "Mesophication and PhD.enl") # follows syntax: xml_add_child(location, name, value)
# xml_add_child(l2, "source-app", "EndNote")
# xml_add_child(l2, "rec-number", 442)
# xml_add_child(l2, "foreign-keys")
xml_add_child(l2, "ref-type", 5)
xml_add_child(l2, "contributors")
xml_add_child(l2, "titles")
xml_add_child(l2, "pages")
xml_add_child(l2, "volume")
xml_add_child(l2, "dates")
xml_add_child(l2, "pub-location")
xml_add_child(l2, "publisher")
xml_add_child(l2, "urls")
xml_add_child(l2, "remote-database-name")
xml_add_child(l2, "language")
xml2::xml_children(l2) # sanity check
cat(as.character(xml2::as_xml_document(real))) # sanity check

# 4.5. nest level-4 child nodes inside level 3 nodes
l3 <- xml2::xml_children(l2) # define what the level-3 tags are
xml2::xml_children(l3) # look at the l4 tags
# xml_add_child(l3[4], "key", 442) # <foreign-keys>
xml_add_child(l3[6], "authors") # <contributors>
xml_add_child(l3[6], "secondary-authors")  # <contributors>
xml_add_child(l3[7], "title") # <titles>
xml_add_child(l3[7], "secondary-title") # <titles>
xml_add_child(l3[7], "tertiary-title") # <titles>
xml_add_child(l3[8], "style", 844) # <pages>
xml_add_child(l3[9], "style", 1) # <volume>
xml_add_child(l3[10], "year") # <dates>
xml_add_child(l3[10], "pub-dates") # <dates>
xml_add_child(l3[11], "style", "International Institute of Tropical Forestry San Juan, PR Rocky Mountain Research Station Fort Collins, CO") # <pub-location>
xml_add_child(l3[12], "style", "Forest Service, United States Department of Agriculture") # <publisher>
xml_add_child(l3[14], "style", "Zotero") # <remote-database-name>
xml_add_child(l3[15], "style", "en") # <language>
xml2::xml_children(l3) # sanity check
cat(as.character(xml2::as_xml_document(real))) # sanity check

# 4.6. nest level-5 child nodes inside level 4 nodes
l4 <- xml2::xml_children(l3) # define what the level-4 tags are
xml2::xml_children(l4) # look at the l4 tags
xml_add_child(l4[2], "author") # <authors>
xml_add_child(l4[2], "author") # <authors>
xml_add_child(l4[3], "author") # <secondary-authors>
xml_add_child(l4[4], "style", "Kalmia latifolia") # <title>
xml_add_child(l4[5], "style", "Wildland shrubs of the United States and its Territories: thamnic descriptions: Volume 1") # <secondary-title>
xml_add_child(l4[6], "style", "General Technical Report IITF-GTR-26") # <tertiary-title>
xml_add_child(l4[9], "style", 2004) # <year>
xml_add_child(l4[10], "date") # <year>
xml2::xml_children(l4) # sanity check
cat(as.character(xml2::as_xml_document(real))) # sanity check

# 4.7. nest level-6 child nodes inside level 5 nodes
l5 <- xml2::xml_children(l4) # define what the level-5 tags are
xml2::xml_children(l5) # look at the tags
xml_add_child(l5[1], "style", "McNab, W. Henry") # <authors><author>
xml_add_child(l5[2], "style", "Clinton, Barton D.") # <authors><author>
xml_add_child(l5[3], "style", "Francis, John K.") # <secondary-authors><author>
xml_add_child(l5[8], "style", 2004) # <secondary-authors><author>
xml2::xml_children(l5) # sanity check
cat(as.character(xml2::as_xml_document(real))) # sanity check

# 4.8. Add attributes to level-2 tags
xml2::xml_children(l2) # these are the level-2 tags
xml2::xml_children(xml_children(xml_children(endnote_example))) # compare to the example
as.character(xml2::xml_children(l2)) == as.character(xml2::xml_children(xml_children(xml_children(endnote_example))))

database_tag <- xml2::xml_children(l2)[[1]] # <database>
xml2::xml_attr(database_tag, "name") <- "Mesophication and PhD.enl" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(database_tag, "path") <- "C:\\Users\\mwroberts\\Documents\\Endnote libraries\\Mesophication and PhD.enl" # use xml_attr() to create attribute and assign it a value
xml2::xml_children(l2) # sanity check

sourceapp_tag <- xml2::xml_children(l2)[[2]] # <source-app>
xml2::xml_attr(sourceapp_tag, "name") <- "EndNote" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(sourceapp_tag, "version") <- "20.4" # use xml_attr() to create attribute and assign it a value
xml2::xml_children(l2) # sanity check

reftype_tag <- xml2::xml_children(l2)[[5]] # <ref-type>
xml2::xml_attr(reftype_tag, "name") <- "Book Section" # use xml_attr() to create attribute and assign it a value
xml2::xml_children(l2) # sanity check

# sanity check
as.character(xml2::xml_children(l2)) == as.character(xml2::xml_children(xml_children(xml_children(endnote_example))))

# 4.9. Add attributes to level-3 tags
xml2::xml_children(l3) # these are the level-3 tags
xml2::xml_children(xml_children(xml_children(xml_children(endnote_example)))) # compare to the example
as.character(xml2::xml_children(l3)) == as.character(xml2::xml_children(xml_children(xml_children(xml_children(endnote_example)))))

key_tag <- xml2::xml_children(l3)[[1]] # <source-app>
xml2::xml_attr(key_tag, "app") <- "EN" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(key_tag, "db-id") <- "s2psv0vp3ws2pfez2v0pxaedexv2vedxars2" # use xml_attr() to create attribute and assign it a value
xml2::xml_children(l3) # sanity check

as.character(xml2::xml_children(l3)) == as.character(xml2::xml_children(xml_children(xml_children(xml_children(endnote_example)))))# sanity check

# 4.10 Add attributes to all <style> tags
# For this minimum viable product, I'm manually updating these tags. This will become loops.
xml2::xml_children(l3) # these are the level-3 tags
style_tag <- xml2::xml_children(l3)[[7]] # this is a level-3 <style> tag
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l3)[[8]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l3)[[11]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l3)[[12]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l3)[[13]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l3)[[14]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

xml2::xml_children(l3) # sanity check
xml2::xml_children(xml_children(xml_children(xml_children(endnote_example)))) # example's l3 tags
as.character(xml2::xml_children(l3)) == as.character(xml2::xml_children(xml_children(xml_children(xml_children(endnote_example))))) # sanity check

xml2::xml_children(l4) # these are the level-4 tags
style_tag <- xml2::xml_children(l4)[[4]] # these are the level-3 <style> tags
xml2::xml_attr(style_tag, "face") <- "italic" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l4)[[5]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l4)[[6]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l4)[[7]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

xml2::xml_children(l4) # sanity check
xml2::xml_children(xml_children(xml_children(xml_children(xml_children(endnote_example))))) # example's l4 tags
as.character(xml2::xml_children(l4)) == as.character(xml2::xml_children(xml_children(xml_children(xml_children(xml_children(endnote_example)))))) # sanity check

xml2::xml_children(l5) # these are the level-5 tags
style_tag <- xml2::xml_children(l5)[[1]] # these are the level-3 <style> tags
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l5)[[2]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l5)[[3]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

style_tag <- xml2::xml_children(l5)[[4]] # <style>
xml2::xml_attr(style_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
xml2::xml_attr(style_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value

xml2::xml_children(l5) #l5 tags
xml2::xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(endnote_example)))))) # example's l5 tags
as.character(xml2::xml_children(l5)) == as.character(xml2::xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(endnote_example))))))) # sanity check


########## Step 5: compare: is our product `real` the same as EndNote's product `endnote_example`?
cat(as.character(xml2::as_xml_document(real))) # sanity check
cat(as.character(xml2::as_xml_document(endnote_example))) # sanity check
as.character(xml2::as_xml_document(real)) == as.character(xml2::as_xml_document(endnote_example)) # TRUE means our xml output is identical to EndNote's

########## Step 5: write output to xml
# write_xml(real, paste0("data/",format(Sys.time(), "%Y%m%d"), "_book_output.xml"), options = "format")
real <- stringr::str_remove_all(real, "(\n +|\n)")
# write(real, "data/20221228/20221228_testoutput.xml")


######### END OF WORKING CODE, BELOW IS FOR DEVELOPMENT
# loop over indices
# IN DEVELOPMENT, THIS DOES NOT WORK YET
# style_tag_indices <- c(7,8,10,11,12,13)
# for (tag in 1:length(style_tag_indices)) {
#     firststyle_tag <- xml2::xml_children(l3)[[tag]]
#     for (tag in 1:length(style_tag_indices)) {
#         xml2::xml_attr(firststyle_tag, "face") <- "normal" # use xml_attr() to create attribute and assign it a value
#         xml2::xml_attr(firststyle_tag, "font") <- "default" # use xml_attr() to create attribute and assign it a value
#         xml2::xml_attr(firststyle_tag, "size") <- "100%" # use xml_attr() to create attribute and assign it a value
#     }
# }
#  # eventually, I want to just "find" the style tags and update them all at once
# xml_find_all(endnote_example, ".//style", flatten = FALSE) # pg 17 https://cran.r-project.org/web/packages/xml2/xml2.pdf
# xml2::xml_attr(xml2::xml_find_all(real, ".//style", flatten = FALSE), "id") <- "test"
# xml_find_all(endnote_example, ".//style", flatten = FALSE) # sanity check



