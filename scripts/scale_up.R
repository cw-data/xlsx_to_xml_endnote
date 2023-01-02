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
for(row in 1:nrow(data2)){ # loop that adds one <record> tag for each row in the df
    xml_add_child(l1, "record")
}
xml2::xml_children(l1)
cat(as.character(xml2::as_xml_document(real))) # sanity check
# 4.4. nest level-3 child nodes inside level 2 node
l2 <- xml2::xml_children(l1) # define what the level-2 tags are
xml2::xml_children(l2) # look at the l4 tags
# xml_add_child(l2, "ref-type", 5)

# xml_add_child(l2, "ref-type") # every <record> tag gets a <ref-type> tag; this is a required tag

for(i in 1:nrow(data2)){
    xml_add_child(l2, "ref-type", data2$`value`[i])
}







xml2::xml_children(l2)
cat(as.character(xml2::as_xml_document(real))) # sanity check

l3 <- xml2::xml_children(l2)
# xml_set_attr(l3[1], "name", names(record_list)[1]) # how to create an attribute and set its value

for(i in 1:nrow(data2)){
    xml_set_attr(l3, "name", data2$`Reference Type`[i])
}

cat(as.character(xml2::as_xml_document(real))) # sanity check





















