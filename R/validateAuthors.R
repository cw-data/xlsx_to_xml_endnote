#-----------------------------------------------------------------------------------------
#---`buildXML.R` routes static assets (e.g., `record_list`)  to other getter functions----
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------



validateAuthors <- function(forms_spreadsheet, ref_type_lookup){
    tryCatch(
        expr = {
            # here we 
            library(dplyr)
            library(data.table)
            library(stringr)
            # lookup <- data.table::fread("resources/colname_tagname_dictionary.csv") # old_spec
            lookup <- data.table::fread("resources/colname_tagname_dictionary_20230130.csv") # new_spec
            
            record_list <- vector(mode = "list", length = length(unique(ref_type_lookup$`Reference type (from Form)`))) # create list
            names(record_list) <- unique(ref_type_lookup$`Reference type (from Form)`) # name list elements
            # sort references into `record_list` by ref-type
            for(i in 1:length(record_list)){
                record_list[[i]]$data <- forms_spreadsheet %>%
                    subset(`Reference Type` == ref_type_lookup$`Reference type (from Form)`[i]) %>%
                    dplyr::select(6,ref_type_lookup$start_col_no[i]:ref_type_lookup$end_col_no[i],159)
                # col 6 is $ref-type (the plain text reference type)
                # col 158 is $value (the Endnote key for reference type plain text)
            }
            # use lookup table from scripts/column_cleanup.R to rename columns in `record_list`
            for(elm in 1:length(record_list)){ # loop through each element in `record_list`
                data.table::setnames(record_list[[elm]]$data, #  reset column names for each `record_list` element
                                     old = lookup$xlsx_colname, # based on the key-value pairs established in `lookup`
                                     new = lookup$xml_tag, skip_absent = TRUE) # based on key
            }
            # parse names
            for(i in 1:length(record_list)){
                # authors
                if("author" %in% colnames(record_list[[i]]$data)){ # if there's an $author column
                    try(
                        record_list[[i]]$author_list <- 
                            stringr::str_split(record_list[[i]]$data$author, "\r\n") # split the string at each newline character 
                    )
                }
                if("cartographer" %in% colnames(record_list[[i]]$data)){
                    try(
                        record_list[[i]]$cartographer_list <-
                            stringr::str_split(record_list[[i]]$data$cartographer, "\r\n")
                    )
                }
                if("photographer" %in% colnames(record_list[[i]]$data)){
                    try(
                        record_list[[i]]$photographer_list <-
                            stringr::str_split(record_list[[i]]$data$photographer, "\r\n")
                    )
                }
                if("editor" %in% colnames(record_list[[i]]$data)){
                    try(
                        record_list[[i]]$editor_list <-
                            stringr::str_split(record_list[[i]]$data$editor, "\r\n")
                    )
                }
                if("series-editor" %in% colnames(record_list[[i]]$data)){
                    try(
                        record_list[[i]]$series_editor_list <-
                            stringr::str_split(record_list[[i]]$data$`series-editor`, "\r\n")
                    )
                }
                if("cover-type" %in% colnames(record_list[[i]]$data)){
                    try(
                        record_list[[i]]$cover_type_list <-
                            stringr::str_split(record_list[[i]]$data$`cover-type`, "\r\n")
                    )
                }
                if("tertiary-author" %in% colnames(record_list[[i]]$data)){
                    try(
                        record_list[[i]]$tertiary_author_list <-
                            stringr::str_split(record_list[[i]]$data$`tertiary-author`, "\r\n")
                    )
                }
            }
            # return(record_list)
            assign("record_list", record_list, envir = globalenv())
        },
        finally = {
            message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
        }
    )
}






