library(readxl)
library(dplyr)
library(data.table)
library(xml2)

# data <- readxl::read_excel("data/20221116_excel_example.xlsx")
# # ref-type lookup table
# ref_type_lookup <- readxl::read_excel("data/endnote_ref-type_dictionary.xlsx")
# ref_type_lookup$key <- ref_type_lookup$`Reference type (from Form)`
# ref_type_lookup$value <- stringr::str_extract(ref_type_lookup$XML, "([0-9]+)")
# # empty list to receive data subsets
# record_list <- vector(mode = "list", length = length(unique(ref_type_lookup$`Reference type (from Form)`))) # create list
# names(record_list) <- unique(ref_type_lookup$`Reference type (from Form)`) # name list elements
# # sort references into `record_list` by ref-type
# for(i in 1:length(record_list)){
#     record_list[[i]] <- data %>% 
#         subset(`Reference Type` == ref_type_lookup$`Reference type (from Form)`[i]) %>%
#         dplyr::select(ref_type_lookup$start_col_no[i]:ref_type_lookup$end_col_no[i])
# }
# lookup <- data.table::fread("resources/colname_tagname_dictionary.csv")
# # use lookup table from scripts/column_cleanup.R to rename columns in `record_list`
# for(elm in 1:length(record_list)){ # loop through each element in `record_list`
#     data.table::setnames(record_list[[elm]], #  reset column names for each `record_list` element
#                          old = lookup$xlsx_colname, # based on the key-value pairs established in `test_lookup`
#                          new = lookup$xml_tag, skip_absent = TRUE) # based on key
# }

####### # a reprex to build tag structure for one record using the logic defined above:
data <- readxl::read_excel("data/20221116_excel_example.xlsx")
# data <- data[c(2:3),]
ref_type_lookup <- readxl::read_excel("resources/endnote_ref-type_dictionary.xlsx")
ref_type_lookup$key <- ref_type_lookup$`Reference type (from Form)`
ref_type_lookup$value <- stringr::str_extract(ref_type_lookup$XML, "([0-9]+)")
lookup <- data.table::fread("resources/colname_tagname_dictionary.csv")
data <- dplyr::left_join(data, 
                         ref_type_lookup %>% select("Reference type (from Form)", "value"),
                         by = c("Reference Type" = "Reference type (from Form)"))
# empty list to receive data subsets
record_list <- vector(mode = "list", length = length(unique(ref_type_lookup$`Reference type (from Form)`))) # create list
names(record_list) <- unique(ref_type_lookup$`Reference type (from Form)`) # name list elements
# sort references into `record_list` by ref-type
for(i in 1:length(record_list)){
    record_list[[i]] <- data %>%
        subset(`Reference Type` == ref_type_lookup$`Reference type (from Form)`[i]) %>%
        dplyr::select(6,ref_type_lookup$start_col_no[i]:ref_type_lookup$end_col_no[i])
}

# use lookup table from scripts/column_cleanup.R to rename columns in `record_list`
for(elm in 1:length(record_list)){ # loop through each element in `record_list`
    data.table::setnames(record_list[[elm]], #  reset column names for each `record_list` element
                         old = lookup$xlsx_colname, # based on the key-value pairs established in `test_lookup`
                         new = lookup$xml_tag, skip_absent = TRUE) # based on key
}
# make a sandbox list with only one ref-type and one dataframe with only one record
record_list2 <- list()
record_list2$Book <- list(record_list$`Book`)
data2 <- data %>% subset(`Reference Type` == "Book")

# 4.1. instantiate new xml document and add root node
#----- <xml>
real <- xml2::xml_new_root("xml") # instantiate root node
# cat(as.character(xml2::as_xml_document(real))) # sanity check
# 4.2. nest a level-1 child-node inside root (level-zero) node
#----- <records>
xml_add_child(real, # the node into which you want to nest a child node
              "records") # the name of the node you're adding
# cat(as.character(xml2::as_xml_document(real))) # sanity check
# 4.3. nest a level-2 child node inside level-1 node
l1 <- xml2::xml_children(real) # define what the level-1 tags are
# xml2::xml_children(l1)
#----- <record>
for(row in 1:nrow(data2)){ # loop that adds one <record> tag for each row in the df
    xml_add_child(l1, "record")
}
# xml2::xml_children(l1)
# cat(as.character(xml2::as_xml_document(real))) # sanity check
# 4.4. nest level-3 child nodes inside level 2 node
l2 <- xml2::xml_children(l1) # define what the level-2 tags are
#-----  <ref-type>
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$`ref-type`[i]) == FALSE){
        xml_add_child(l2, "ref-type", data2$`value`[i])    
    }
    }
# xml2::xml_children(l2)
# cat(as.character(xml2::as_xml_document(real))) # sanity check
l3 <- xml2::xml_children(l2)
for(i in 1:nrow(data2)){
    xml_set_attr(l3, "name", data2$`Reference Type`[i])
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <author>
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$author[i]) == FALSE){
        xml_add_child(l2[i], "contributors")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "authors")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "author")
        l5 <- xml2::xml_children(l4)
        xml_set_attr(l5[length(l5)], "role", "author")
        l6 <- xml2::xml_children(l5)
        xml_add_child(l5[length(l5)], "style", record_list2$Book[[i]]$author)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <title>
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$title[i]) == FALSE){
        xml_add_child(l2[i], "titles")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "title")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", record_list2$Book[[i]]$title)
        l5 <- xml2::xml_children(l4)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <location>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$location[i]) == FALSE){
        xml_add_child(l2[i], "location")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", record_list2$Book[[i]]$location)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <cover-type>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$`cover-type`[i]) == FALSE){
        xml_add_child(l2[i], "cover-type")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", record_list2$Book[[i]]$`cover-type`)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <year>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$`year`[i]) == FALSE){
        xml_add_child(l2[i], "dates")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "year") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", record_list2$Book[[1]]$`year`[i])
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <pub-location>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$`pub-location`[i]) == FALSE){
        xml_add_child(l2[i], "pub-location")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", record_list2$Book[[1]]$`pub-location`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <publisher>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$`publisher`[i]) == FALSE){
        xml_add_child(l2[i], "publisher")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", record_list2$Book[[1]]$`publisher`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <web-urls>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$`web-urls`[i]) == FALSE){
        xml_add_child(l2[i], "urls")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "web-urls") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "url")
        l5 <- xml2::xml_children(l4)
        xml_add_child(l5[length(l5)], "style", record_list2$Book[[1]]$`web-urls`[i])
        l6 <- xml2::xml_children(l5)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check

#----- <research-notes>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$`research-notes`[i]) == FALSE){
        xml_add_child(l2[i], "research-notes")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", record_list2$Book[[1]]$`research-notes`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <pages>
cat(as.character(xml2::as_xml_document(real))) # sanity check
# xml_add_child(l2, "pages")
for(i in 1:nrow(data2)){
    if (is.na(record_list2$Book[[1]]$`pages`[i]) == FALSE){
        xml_add_child(l2[i], "cover-type", record_list2$Book[[i]]$`cover-type`)
        l3 <- xml2::xml_children(l2)
    }
}





xml_add_child(l2[length(l3)], "year")
l3[6]




