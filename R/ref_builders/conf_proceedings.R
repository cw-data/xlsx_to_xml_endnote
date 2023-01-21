#-----------------------------------------------------------------------------------------
#---`conf_proceedings.R` is a getter function that creates xml for Conference proceedings records from `record_list` -
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getNConfProceed <- function(real, data, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R") # 1
            source("R/tag_builders/ref_type_name.R") # 2
            source("R/tag_builders/title.R") # 3
            source("R/tag_builders/author.R") # 4
            source("R/tag_builders/year.R") # 5
            source("R/tag_builders/pdf_urls.R") # 16
            source("R/tag_builders/location.R") # 18
            source("R/tag_builders/research_notes.R") # 17
            source("R/tag_builders/cover_type.R") # 19
            source("R/tag_builders/related_urls.R") # 20
            source("R/tag_builders/pub_location.R") # 8
            source("R/tag_builders/num_vols.R") # 8
            source("R/tag_builders/series_editor.R") # 6
            source("R/tag_builders/tertiary_title.R") # 7
            source("R/tag_builders/edition.R") # 7
            # source("R/tag_builders/secondary_volume.R") # 11
            # source("R/tag_builders/number.R") # 12
            # source("R/tag_builders/tertiary_author.R") # 14
            # source("R/tag_builders/pub_location.R") # 10
            # source("R/tag_builders/secondary_title.R") # 
            # source("R/tag_builders/editor.R")
            # source("R/tag_builders/num_vols.R")
            # source("R/tag_builders/pdf_urls.R") # WRITE ME
            # source("R/tag_builders/photographer.R") # WRITE ME
            # source("R/tag_builders/ppv_rev.R") # WRITE ME # "Is this photograph in the public domain?"
            # source("R/tag_builders/caption.R") # WRITE ME
            # source("R/tag_builders/cartographer.R") # WRITE ME
            # source("R/tag_builders/date.R") # WRITE ME
            
            #----- assign static assets
            data <- record_list$`Conference proceedings`$data
            # send the names we parsed in `validateAuthors.R` to the getter functions along their `data`
            if(nrow(data)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Conference proceedings`)){
                    authors <- list(record_list$`Conference proceedings`$author_list)
                }
                if("cartographer_list" %in% names(record_list$`Conference proceedings`)){
                    cartographers <- record_list$`Conference proceedings`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Conference proceedings`)){
                    photographers <- record_list$`Conference proceedings`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Conference proceedings`)){
                    editors <- record_list$`Conference proceedings`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Conference proceedings`)){
                    series_editors <- record_list$`Conference proceedings`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Conference proceedings`)){
                    cover_types <- record_list$`Conference proceedings`$cover_type_list
                }
                if("tertiary_author_list" %in% names(record_list$`Conference proceedings`)){
                    tertiary_authors <- record_list$`Conference proceedings`$tertiary_author_list
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
            if(!is.na(data$author[1])){
                real <- getAuthor(real, data, authors)
            }
            #----- <year>
            real <- getYear(real, data)
            #----- <pdf-urls>
            real <- getPdfUrls(real, data)
            #-----  <secondary-title>
            getSecondaryTitle <- function(real, data)
            
            #----- <modified-date> `location`
            real <- getLocation(real, data)
            #----- <research-notes>
            real <- getResearchNotes(real, data)
            #----- <custom7> i.e., `cover-type`
            if(!is.na(data$`cover-type`[1])){
                real <- getCoverType(real, data, cover_types)
            }
            #----- <related-urls>
            real <- getRelatedUrls(real, data)
            #----- <pub-location>
            real <- getPubLocation(real, data)
            #----- <publisher>
            real <- getPublisher(real, data)
            #----- <pages>
            real <- getPages(real, data)
            #----- <volume>
            real <- getVolume(real, data)
            #----- <edition>
            real <- getEdition(real, data)
            #----- <section>
            real <- getSection(real, data)
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Conference proceedings` references...\n")
        }
    )
}