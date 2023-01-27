#-----------------------------------------------------------------------------------------
#---`journal.R` is a getter function that creates xml for journal records from `record_list` ---
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getJournal <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R") # 1
            source("R/tag_builders/ref_type_name.R") # 2
            source("R/tag_builders/title.R") # 3
            source("R/tag_builders/author.R") # 4
            source("R/tag_builders/year.R") # 5
            source("R/tag_builders/secondary_title.R") # 6
            source("R/tag_builders/volume.R") # 7
            source("R/tag_builders/number.R") # 8
            source("R/tag_builders/pages.R") # 9
            source("R/tag_builders/pdf_urls.R") # 10
            source("R/tag_builders/location.R") # 11
            source("R/tag_builders/research_notes.R") # 12
            source("R/tag_builders/cover_type.R") # 13
            source("R/tag_builders/related_urls.R") # 14
            # source("R/tag_builders/edition.R")
            # source("R/tag_builders/editor.R")
            # source("R/tag_builders/num_vols.R")
            # source("R/tag_builders/secondary_volume.R") # 9
            # source("R/tag_builders/pub_location.R") # 10
            # source("R/tag_builders/publisher.R") # 11
            # source("R/tag_builders/series_editor.R")
            # source("R/tag_builders/tertiary_title.R")
            # source("R/tag_builders/pdf_urls.R") # WRITE ME
            # source("R/tag_builders/photographer.R") # WRITE ME
            # source("R/tag_builders/ppv_rev.R") # WRITE ME # "Is this photograph in the public domain?"
            # source("R/tag_builders/caption.R") # WRITE ME
            # source("R/tag_builders/cartographer.R") # WRITE ME
            # source("R/tag_builders/date.R") # WRITE ME
            
            #----- assign static assets
            data <- record_list$`Journal article`$data
            # send the names we parsed in `validateAuthors.R` to the getter functions along their `data`
            if(nrow(data)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Journal article`)){
                    authors <- list(record_list$`Journal article`$author_list)
                }
                if("cartographer_list" %in% names(record_list$`Journal article`)){
                    cartographers <- record_list$`Journal article`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Journal article`)){
                    photographers <- record_list$`Journal article`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Journal article`)){
                    editors <- record_list$`Journal article`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Journal article`)){
                    series_editors <- record_list$`Journal article`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Journal article`)){
                    cover_types <- record_list$`Journal article`$cover_type_list
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
            #----- <secondary-title>
            real <- getSecondaryTitle(real, data)
            #----- <volume>
            real <- getVolume(real, data)
            #----- <number>
            real <- getNumber(real, data)
            #----- <pages>
            real <- getPages(real, data)
            #----- <pdf-urls>
            real <- getPdfUrls(real, data)
            #----- <modified-date> `location`
            real <- getLocation(real, data)
            #----- <research-notes>
            real <- getResearchNotes(real, data)
            #----- <custom7> i.e., `cover-type`
            if(!is.na(data$`cover-type`)){
                real <- getCoverType(real, data, cover_types)
            }
            #----- <related-urls>
            real <- getRelatedUrls(real, data)
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Journal article` references...\n")
        }
    )
}