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
                for( i in 1:length(record_list)){
                    data <- record_list[[i]]$data
                    # double-check that `loadData()` worked correctly:
                    if(length(unique(data$`ref-type`))>1){ # if `loadData()` didn't subset data into `ref-type` subsets
                        print("more than one data type")
                        break # stop the program
                        # because the program builds xml one ref-type at a time in the following steps
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Journal article")){ # if the ref-type is "Book"
                            real <- getJournal(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Forest Service publication (e.g., general technical report)")){ # if the ref-type is "Book"
                            real <- getFSPub(real,record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Unpublished report (e.g., establishment or progress report)")){ # if the ref-type is "Book"
                            real <- getUnPub(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Government document (other than FS publications)")){ # if the ref-type is "Book"
                            real <- getGovDoc(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Book")){ # if the ref-type is "Book"
                            real <- getBook(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Book section")){ # if the ref-type is "Book"
                            real <- getBookSection(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Conference proceedings")){ # if the ref-type is "Book"
                            real <- getConfProceed(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Conference paper")){ # if the ref-type is "Book"
                            real <- getConferencePaper(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Thesis/Dissertation")){ # if the ref-type is "Book"
                            real <- getBook(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Photograph")){ # if the ref-type is "Book"
                            real <- getPhoto(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Map")){ # if the ref-type is "Map"
                            real <- getMap(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Newspaper article")){ # if the ref-type is "Book"
                            real <- getNewspaper(real, record_list) # call the function that builds xml for books
                        }
                    }
                    if(nrow(data>0)){
                        if(unique(data$`ref-type`=="Online reference/website")){ # if the ref-type is "Book"
                            real <- getWebsite(real, record_list) # call the function that builds xml for books
                        }
                    }
                }
                
                
                #----- send output to console and save to global environment
                assign("xml_output", real, envir = globalenv()) # save output to global environment so user can see it
                cat(as.character(xml2::as_xml_document(real))) # print output to console for user
                message("`forms_spreadsheet` parsed to XML...\nXML printed to console for review...\n")
                
                #----- write output to file if `write` flag is TRUE
                if(write==TRUE){
                    real <- stringr::str_remove_all(real, "(\n +|\n)") # remove newline characters because endnote doesn't like them
                    real <- as.character(real) # set real as character for output
                    data.table::fwrite(real, "data/xmlouttest.xml")
                    message("`forms_spreadsheet` parsed to XML...\nOutput saved to file...\n")
                }
            }
        }
        # finally = {
        #     message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
        # }
    )
}