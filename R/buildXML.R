#-----------------------------------------------------------------------------------------
#---`buildXML.R` routes static assets (e.g., `record_list`) to other getter functions-----
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
                source("R/ref_builders/book.R")
                source("R/ref_builders/fs_pub.R")
                source("R/ref_builders/gov_doc.R")
                source("R/ref_builders/un_pub.R")
                source("R/ref_builders/journal.R")
                source("R/ref_builders/thesis.R")
                source("R/ref_builders/map.R")
                source("R/ref_builders/book_section.R")
                source("R/ref_builders/conf_paper.R")
                source("R/ref_builders/photo.R")
                source("R/ref_builders/website.R")
                source("R/ref_builders/newspaper.R")
                source("R/ref_builders/conf_proceedings.R")
                
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
                for(i in 1:length(record_list)){
                    data <- record_list[[i]]$data
                    reftype_name <- names(record_list)[i]
                    # double-check that `loadData()` worked correctly:
                    if(length(unique(data$`ref-type`))>1){ # if `loadData()` didn't subset data into `ref-type` subsets
                        print("more than one data type")
                        break # stop the program
                        # because the program builds xml one ref-type at a time in the following steps
                    } else {
                        if(nrow(data>0) & reftype_name == "Journal article"){ # if there's at least one row of data & the ref-type is x
                            real <- getJournal(real, record_list) # call x's ref-builder function
                        } else
                            if(nrow(data>0) & reftype_name == "Forest Service publication (e.g., general technical report)"){
                                real <- getFSPub(real,record_list)
                            } else
                                if(nrow(data)>0 & reftype_name == "Unpublished report (e.g., establishment or progress report)"){
                                    real <- getUnPub(real, record_list)
                                } else
                                    if(nrow(data)>0 & reftype_name == "Government document (other than FS publications)"){
                                        real <- getGovDoc(real, record_list)
                                    } else
                                        if(nrow(data)>0 & reftype_name == "Book"){
                                            real <- getBook(real, record_list)
                                        } else
                                            if(nrow(data)>0 & reftype_name == "Book section"){
                                                    real <- getBookSection(real, record_list)
                                            } else
                                                if(nrow(data)>0 & reftype_name == "Conference proceedings"){
                                                    real <- getConfProceed(real, record_list)
                                                } else
                                                    if(nrow(data)>0 & reftype_name == "Conference paper"){
                                                        real <- getConferencePaper(real, record_list)
                                                    } else
                                                        if(nrow(data)>0 & reftype_name == "Thesis/Dissertation"){
                                                            real <- getThesis(real, record_list)
                                                        } else
                                                            if(nrow(data)>0 & reftype_name == "Photograph"){
                                                                real <- getPhoto(real, record_list)
                                                            } else
                                                                if(nrow(data)>0 & reftype_name == "Map"){
                                                                    real <- getMap(real, record_list)
                                                                } else
                                                                    if(nrow(data)>0 & reftype_name == "Newspaper article"){
                                                                        real <- getNewspaper(real, record_list)
                                                                    } else
                                                                        if(nrow(data)>0 & reftype_name =="Online reference/website"){
                                                                            real <- getWebsite(real, record_list) # call x's ref-builder function
                                                                        }
                    }
                }
                
                #----- send output to console and save to global environment
                assign("xml_output", real, envir = globalenv()) # save output to global environment so user can see it
                cat(as.character(xml2::as_xml_document(real))) # print output to console for user
                message("`forms_spreadsheet` parsed to XML...\nXML printed to console for review...\n")
                
                #----- write output to file if `write` flag is TRUE
                if(write==TRUE){
                    real <- stringr::str_remove_all(xml_output, "(\n +|\n)") # remove newline characters because endnote doesn't like them
                    real <- as.character(real) # set real as character for output
                    # data.table::fwrite(real, "data/xmlouttest.xml")
                    write(real, file.choose())
                    message("`forms_spreadsheet` parsed to XML...\nOutput saved to file...\n")
                }
            }
        }
    )
}