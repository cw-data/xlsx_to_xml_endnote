#-----------------------------------------------------------------------------------------
#---`book.R` is a getter function that creates xml for book records from `record_list` ---
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getBook <- function(record_list, real){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R")
            source("R/tag_builders/ref_type_name.R")
            source("R/tag_builders/title.R")
            source("R/tag_builders/author.R")
            source("R/tag_builders/editor.R")
            source("R/tag_builders/year.R")
            # source("R/tag_builders/pdf_urls.R") # WRITE ME
            source("R/tag_builders/location.R")
            source("R/tag_builders/research_notes.R")
            source("R/tag_builders/cover_type.R")
            source("R/tag_builders/related_urls.R")
            # source("R/tag_builders/photographer.R") # WRITE ME
            # source("R/tag_builders/ppv_rev.R") # WRITE ME
            # source("R/tag_builders/caption.R") # WRITE ME
            # source("R/tag_builders/cartographer.R") # WRITE ME
            # source("R/tag_builders/date.R") # WRITE ME
            source("R/tag_builders/secondary_title.R")
            source("R/tag_builders/volume.R")
            # source("R/tag_builders/number.R") # WRITE ME
            source("R/tag_builders/pages.R")
            source("R/tag_builders/series_editor.R")
            source("R/tag_builders/tertiary_title.R")
            source("R/tag_builders/pub_location.R")
            source("R/tag_builders/publisher.R")
            
            
            
            source("R/tag_builders/edition.R")
            
            source("R/tag_builders/num_vols.R")
            
            #----- assign static assets for easier indexing
            data <- record_list$Book$data
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
            
            # 4.3. nest a level-2 child node inside level-1 node
            l1 <- xml2::xml_children(real) # define what the level-1 tags are
            #----- <record>
            for(row in 1:nrow(data)){ # loop that adds one <record> tag for each row in the df
                xml_add_child(l1, "record")
            }
            cat(as.character(xml2::as_xml_document(real))) # sanity check

            #-----  <ref-type>
            real <- getRefType(real, data)
            real <- getRefTypeName(real, data)
            
            #----- <title>
            if("title" %in% colnames(data)){
                real <- getTitle(real, data)
            }
            
            #----- <author>
            if("author_list" %in% names(record_list$Book)){ # only make <author> tags if there are authors
                real <- getAuthor(real, data, authors)
            }
            
            #----- <year>
            if("year" %in% colnames(data)){
                real <- getYear(real, data)
            }
            
            #----- <pdf-urls>
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # NEED TO WRITE THIS
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if("pdf-urls" %in% colnames(data)){
                real <- getPdfUrls(real, data)
            }
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            #----- <modified-date> `location`
            if("location" %in% colnames(data)){
                real <- getLocation(real, data)
            }
            
            #----- <research-notes>
            if("research-notes" %in% colnames(data)){
                real <- getResearchNotes(real, data)
            }
            
            #----- <custom7> i.e., `cover-type`
            if("cover-type" %in% colnames(data)){
                real <- getCoverType(real, data)
            }
            
            #----- <related-urls>
            if("related-urls" %in% colnames(data)){
                real <- getRelatedUrls(real, data)
            }
            
            #----- <photographer>
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # NEED TO WRITE THIS
            if("photographer_list" %in% names(record_list$Book)){
                real <- getPhotographers(real, data, editors)
            }
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            #----- <ppv-rev>
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # NEED TO WRITE THIS
            if("ppv-rev" %in% colnames(data)){
                real <- getPpvRev(real, data)
            }
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            #----- <caption>
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # NEED TO WRITE THIS
            if("caption" %in% colnames(data)){
                real <- getCaption(real, data)
            }
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            #----- <cartographer>
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # NEED TO WRITE THIS
            if("cartographer_list" %in% names(record_list$Book)){
                real <- getCartographer(real, data, cartographers)
            }
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            #----- <date>
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # NEED TO WRITE THIS
            if("date" %in% colnames(data)){
                real <- getDate(real, data)
            }
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            #----- <secondary-title> i.e., series title
            if("secondary-title" %in% colnames(data)){
                real <- getSecondaryTitle(real, data)
            }
            
            #----- <volume>
            if("volume" %in% colnames(data)){
                real <- getVolume(real, data)
            }
            
            #----- <number>
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # NEED TO WRITE THIS
            if("number" %in% colnames(data)){
                real <- getNumber(real, data)
            }
            # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
            #----- <pages>
            if("pages" %in% colnames(data)){
                getPages(real, data)
            }
            
            #----- <series-editor>
            if("series_editor_list" %in% names(record_list$Book)){
                real <- getSeriesEditors(real, data, series_editors)
            }
            
            #----- <tertiary-title>
            if("tertiary-title" %in% colnames(data)){
                real <- getTertiaryTitle(real, data)
            }
            
            #----- <pub-location>
            if("pub-location" %in% colnames(data)){
                real <- getPubLocation(real, data)
            }
            
            #----- <publisher>
            if("publisher" %in% colnames(data)){
                real <- getPublisher(real, data)
            }
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            #----- <editor>
            if("editor_list" %in% names(record_list$Book)){
                real <- getEditor(real, data, editors)
            }

            

            #----- <secondary-title> i.e., series title
            if("secondary-title" %in% colnames(data)){
                real <- getSecondaryTitle(real, data)
            }
            
            
            
            
            
            

            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            
            
            

            
            
            
            
            
            
            #----- <edition>
            if("edition" %in% colnames(data)){
                getEdition(real, data)
            }

            
            
            #----- <num-vols>
            if("num-vols" %in% colnames(data)){
                getNumVols(real, data)
            }
            
            #----- <secondary-volume>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`secondary-volume`[i])){
                    xml_add_child(l2[i], "secondary-volume")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`secondary-volume`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <secondary-authors> "series-editor"
            # for(i in 1:nrow(data)){
            #     if (!is.na(data$`series-editor`[i])){
            #         # xml_add_child(l2[1], "contributors")
            #         l3 <- xml2::xml_children(l2)
            #         xml_add_child(l3[2], "secondary-authors") # confirmed data/20230104/Book_example.xml
            #         l4 <- xml2::xml_children(l3)
            #         xml_add_child(l4[length(l4)], "author")
            #         l5 <- xml2::xml_children(l4)
            #         xml_set_attr(l5[length(l5)], "role", "series-editor")
            #         l6 <- xml2::xml_children(l5)
            #         xml_add_child(l5[length(l5)], "style", data$`series-editor`)
            #     }
            # }
            
            
            
            
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <pdf-urls>
            for(i in 1:nrow(data)){
                if (!is.na(data$`pdf-urls`[i])){
                    xml_add_child(l2[i], "urls")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "pdf-urls") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                    xml_add_child(l4[length(l4)], "url")
                    l5 <- xml2::xml_children(l4)
                    xml_add_child(l5[length(l5)], "style", data$`pdf-urls`[i])
                    l6 <- xml2::xml_children(l5)
                }
            }
            cat(as.character(xml2::as_xml_document(real))) # sanity check
            
            
            
            ########## Step 5: write output to xml
            # write_xml(real, paste0("data/",format(Sys.time(), "%Y%m%d"), "_book_output.xml"), options = "format")
            real <- stringr::str_remove_all(real, "(\n +|\n)")
            real <- as.character(real)
            return(real)
        }
        # finally = {
        #     message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
        # }
    )
}