library(readxl)
library(dplyr)
library(data.table)

data <- readxl::read_excel("data/20221116_excel_example.xlsx")

# ref-type lookup table
ref_type_lookup <- readxl::read_excel("data/endnote_ref-type_dictionary.xlsx")
ref_type_lookup$key <- ref_type_lookup$`Reference type (from Form)`
ref_type_lookup$value <- stringr::str_extract(ref_type_lookup$XML, "([0-9]+)")

# empty list to receive data subsets
record_list <- vector(mode = "list", length = length(unique(ref_type_lookup$`Reference type (from Form)`))) # create list
names(record_list) <- unique(ref_type_lookup$`Reference type (from Form)`) # name list elements
# sort references into `record_list` by ref-type
for(i in 1:length(record_list)){
    record_list[[i]] <- data %>% 
        subset(`Reference Type` == ref_type_lookup$`Reference type (from Form)`[i]) %>%
        dplyr::select(ref_type_lookup$start_col_no[i]:ref_type_lookup$end_col_no[i])
}

lookup <- data.table::fread("resources/colname_tagname_dictionary.csv")
# use lookup table from scripts/column_cleanup.R to rename columns in `record_list`
for(elm in 1:length(record_list)){ # loop through each element in `record_list`
    data.table::setnames(record_list[[elm]], #  reset column names for each `record_list` element
                         old = lookup$xlsx_colname, # based on the key-value pairs established in `test_lookup`
                         new = lookup$xml_tag, skip_absent = TRUE) # based on key
}


