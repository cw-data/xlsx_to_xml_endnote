#-----------------------------------------------------------------------------------------
#---`book.R` is a getter function that creates xml for book records from `record_list` ---
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getBook <- function(real, data, record_list, authors=NULL, cartographers=NULL, photographers=NULL, editors=NULL, series_editors=NULL){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R")
            source("R/tag_builders/ref_type_name.R")
            source("R/tag_builders/title.R")
            source("R/tag_builders/author.R")
            source("R/tag_builders/year.R")
            source("R/tag_builders/edition.R")
            source("R/tag_builders/editor.R")
            source("R/tag_builders/num_vols.R")
            source("R/tag_builders/secondary_volume.R")
            source("R/tag_builders/pub_location.R")
            source("R/tag_builders/publisher.R")
            source("R/tag_builders/volume.R")
            source("R/tag_builders/series_editor.R")
            source("R/tag_builders/tertiary_title.R")
            source("R/tag_builders/pdf_urls.R")
            source("R/tag_builders/location.R")
            source("R/tag_builders/research_notes.R")
            source("R/tag_builders/cover_type.R")
            source("R/tag_builders/related_urls.R")
            source("R/tag_builders/pages.R")
            # source("R/tag_builders/pdf_urls.R") # WRITE ME
            # source("R/tag_builders/photographer.R") # WRITE ME
            # source("R/tag_builders/ppv_rev.R") # WRITE ME # "Is this photograph in the public domain?"
            # source("R/tag_builders/caption.R") # WRITE ME
            # source("R/tag_builders/cartographer.R") # WRITE ME
            # source("R/tag_builders/date.R") # WRITE ME
            # source("R/tag_builders/secondary_title.R")
            # source("R/tag_builders/number.R") # WRITE ME
            
            #----- assign static assets
            # send the names we parsed in `validateAuthors.R` to the getter functions along their `data`
            if(nrow(data)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$Book)){
                    authors <- record_list$Book$author_list
                }
                if("cartographer_list" %in% names(record_list$Book)){
                    cartographers <- record_list$Book$cartographer_list
                }
                if("photographer_list" %in% names(record_list$Book)){
                    photographers <- record_list$Book$photographer_list
                }
                if("editor_list" %in% names(record_list$Book)){
                    editors <- record_list$Book$editor_list
                }
                if("series_editor_list" %in% names(record_list$Book)){
                    series_editors <- record_list$Book$series_editor_list
                }
            }
            # 4.3. nest a level-2 child node inside level-1 node
            l1 <- xml2::xml_children(real) # define what the level-1 tags are
            #----- <record>
            for(row in 1:nrow(data)){ # loop that adds one <record> tag for each row in `data`
                xml_add_child(l1, "record")
            }
            #-----  <ref-type>
            real <- getRefType(real, data)
            real <- getRefTypeName(real, data)
            #----- <title>
            real <- getTitle(real, data)
            #----- <author>
            if(!is.na(data$author)){
                real <- getAuthor(real, data, authors)
            }
            #----- <year>
            real <- getYear(real, data)
            #----- <edition>
            real <- getEdition(real, data)
            #----- <editor>
            if(!is.na(data$editor)){
                real <- getEditor(real, data, editors)
            }
            #----- <num-vols>
            real <- getNumVols(real, data)
            #----- <secondary-volume>
            real <- getSecVol(real, data)
            #----- <pub-location>
            real <- getPubLocation(real, data)
            #----- <publisher>
            real <- getPublisher(real, data)
            #----- <volume>
            real <- getVolume(real, data)
            #----- <series-editor>
            real <- getSeriesEditors(real, data, series_editors)
            #----- <tertiary-title>
            real <- getTertiaryTitle(real, data)
            #----- <pdf-urls>
            real <- getPdfUrls(real, data)
            #----- <modified-date> `location`
            real <- getLocation(real, data)
            #----- <research-notes>
            real <- getResearchNotes(real, data)
            #----- <custom7> i.e., `cover-type`
            real <- getCoverType(real, data)
            #----- <related-urls>
            real <- getRelatedUrls(real, data)
            #----- <pages>
            real <- getPages(real, data)
            
            # #----- <photographer>
            # if(!is.null(editors)){
            #     real <- getPhotographers(real, data, photographers)
            # }
            # #----- <ppv-rev> # "Is this photograph in the public domain?"
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # # NEED TO WRITE THIS
            # real <- getPpvRev(real, data)
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # 
            # #----- <caption>
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # # NEED TO WRITE THIS
            # real <- getCaption(real, data)
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # 
            # #----- <cartographer>
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # # NEED TO WRITE THIS
            # real <- getCartographer(real, data, cartographers)
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # 
            # #----- <date>
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # # NEED TO WRITE THIS
            # real <- getDate(real, data)
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # 
            # #----- <secondary-title> i.e., series title
            # real <- getSecondaryTitle(real, data)
            # 
            # #----- <number>
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # # NEED TO WRITE THIS
            # real <- getNumber(real, data)
            # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # 
            # if("secondary-title" %in% colnames(data)){
            #     real <- getSecondaryTitle(real, data)
            # }
            # assign("xml_output", real, envir = globalenv())
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Book` references...\n")
        }
    )
}