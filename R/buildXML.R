#-----------------------------------------------------------------------------------------
#---`buildXML.R` routes static assets (e.g., `record_list`)  to other getter functions----
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------
options(warn=-1)
buildXML <- function(record_list, write){
    tryCatch(
        expr = {
            if("record_list" %in% ls() & class(record_list) == "list"){ # run the function only if user has entered record_list
                #----- load external libraries
                library(readxl)
                library(dplyr)
                library(data.table)
                library(xml2)
                
                #----- load project functions
                source("R/book.R")
                # source("R/fsPub.R")
                # source("R/journal.R")
                # source("R/govDoc.R")
                # source("R/thesis.R")
                # source("R/map.R")
                # source("R/bookSection.R")
                # source("R/confPaper.R")
                # source("R/photo.R")
                # source("R/website.R")
                # source("R/newspaper.R")
                # source("R/confProceed.R")
                
                #----- start an empty xml doc
                if(nrow(forms_spreadsheet>0)){
                    #----- <xml>
                    real <- xml2::xml_new_root("xml") # instantiate root node
                    #----- <records>
                    xml_add_child(real, # the node into which you want to nest a child node
                                  "records") # the name of the node you're adding
                } else {
                    print("There are zero rows of data in `forms_spreadsheet`. Build cancelled.")
                    break
                }
                
                #----- subset data by ref-type and call the appropriate getter function for each ref-type
                for( i in 1:length(record_list)){
                    data <- record_list[[i]]$data
                    # double-check that `loadData()` worked correctly:
                    if(length(unique(data$`ref-type`))>1){ # if `loadData()` didn't subset data into `ref-type` subsets
                        break # stop the program
                        # because the program builds xml one ref-type at a time in the following steps
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Book")){ # if the ref-type is "Book"
                            real <- getBook(real, data, record_list) # call the function that builds xml for books
                        }
                    }
                }
                
                #----- send output to console and save to global environment
                assign("xml_output", real, envir = globalenv()) # save output to global environment so user can see it
                cat(as.character(xml2::as_xml_document(real))) # print output to console for user
                
                
                #----- write output to file if `write` flag is TRUE
                if(write==TRUE){
                    real <- stringr::str_remove_all(real, "(\n +|\n)") # remove newline characters because endnote doesn't like them
                    real <- as.character(real) # set real as character for
                    data.table::fwrite(real, "data/xmlouttest.xml")
                }
            }
        }
        # finally = {
        #     message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
        # }
    )
}