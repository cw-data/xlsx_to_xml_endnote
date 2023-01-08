library(dplyr)
library(data.table)
library(stringr)

validateAuthors <- function(forms_spreadsheet, ref_type_lookup){
    tryCatch(
        expr = {
            # here we 
            lookup <- data.table::fread("resources/colname_tagname_dictionary.csv")
            
            record_list <- vector(mode = "list", length = length(unique(ref_type_lookup$`Reference type (from Form)`))) # create list
            names(record_list) <- unique(ref_type_lookup$`Reference type (from Form)`) # name list elements
            # sort references into `record_list` by ref-type
            for(i in 1:length(record_list)){
                record_list[[i]]$data <- forms_spreadsheet %>%
                    subset(`Reference Type` == ref_type_lookup$`Reference type (from Form)`[i]) %>%
                    dplyr::select(6,ref_type_lookup$start_col_no[i]:ref_type_lookup$end_col_no[i])
            }
            # use lookup table from scripts/column_cleanup.R to rename columns in `record_list`
            for(elm in 1:length(record_list)){ # loop through each element in `record_list`
                data.table::setnames(record_list[[elm]]$data, #  reset column names for each `record_list` element
                                     old = lookup$xlsx_colname, # based on the key-value pairs established in `test_lookup`
                                     new = lookup$xml_tag, skip_absent = TRUE) # based on key
            }
            # parse author names
            for(i in 1:length(record_list)){
                for(j in 1:nrow(record_list[[i]]$data)){
                    if("author" %in% colnames(record_list[[i]]$data) == TRUE){
                        record_list[[i]]$author_list <- stringr::str_split(record_list[[i]]$data$author, "\r\n")
                    } else if("cartographer" %in% colnames(record_list[[i]]$data) == TRUE){
                        record_list[[i]]$cartographer_list <- stringr::str_split(record_list[[i]]$data$cartographer, "\r\n")
                    } else if("photographer" %in% colnames(record_list[[i]]$data) == TRUE){
                        record_list[[i]]$photographer_list <- stringr::str_split(record_list[[i]]$data$photographer, "\r\n")
                    }
                }
            }
            assign("record_list", record_list, envir = globalenv())
        },
        finally = {
            message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
        }
    )
}






