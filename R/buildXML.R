#-----------------------------------------------------------------------------------------
#---`buildXML.R` routes static assets (e.g., `record_list`)  to other getter functions----
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

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
                source("R/journal.R")
                # source("R/govDoc.R")
                # source("R/thesis.R")
                # source("R/map.R")
                # source("R/bookSection.R")
                # source("R/confPaper.R")
                # source("R/photo.R")
                # source("R/website.R")
                # source("R/newspaper.R")
                # source("R/confProceed.R")
                
                
                book <- getBook(record_list)
                assign("book", book, envir = globalenv())
                journal <-getJournal(record_list)
                assign("journal", journal, envir = globalenv())
                
                if(write==TRUE)(
                    data.table::fwrite(xmlout, "data/xmlouttest.xml")
                )
            }
        }
        # finally = {
        #     message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
        # }
    )
}