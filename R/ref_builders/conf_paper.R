#----------------------------------------------------------------------------------------------------------
#---`conf_paper.R` is a getter function that creates xml for 'Conference paper' records from `record_list` ----
#--- a module for `main.R` that creates xml from forms xlsx data ------------------------------------------
#----------------------------------------------------------------------------------------------------------

getConferencePaper <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R") # 1
            source("R/tag_builders/ref_type_name.R") # 2
            source("R/tag_builders/title.R") # 3
            source("R/tag_builders/author.R") # 4
            source("R/tag_builders/year.R") # 5
            source("R/tag_builders/pdf_urls.R") # 7
            source("R/tag_builders/location.R") # 8
            source("R/tag_builders/research_notes.R") # 8
            source("R/tag_builders/cover_type.R") # 9
            source("R/tag_builders/related_urls.R") # 10
            source("R/tag_builders/edition.R") # 5
            source("R/tag_builders/section.R") # 15
            source("R/tag_builders/editor.R") # 5
            source("R/tag_builders/secondary_title.R") # 11
            source("R/tag_builders/pub_location.R") # 14
            source("R/tag_builders/publisher.R") # 12
            source("R/tag_builders/volume.R") # 13
            source("R/tag_builders/number.R") 
            source("R/tag_builders/pages.R") # 15
            # source("R/tag_builders/caption.R") # 9
            # source("R/tag_builders/date.R") # 6
            # source("R/tag_builders/num_vols.R") # 8
            # source("R/tag_builders/secondary_volume.R") # 9
            # source("R/tag_builders/series_editor.R") # 13
            # source("R/tag_builders/tertiary_title.R") # 14
            # source("R/tag_builders/series_editor.R") # 13
            # source("R/tag_builders/pdf_urls.R") # WRITE ME
            # source("R/tag_builders/photographer.R") # WRITE ME
            # source("R/tag_builders/ppv_rev.R") # WRITE ME # "Is this photograph in the public domain?"
            # source("R/tag_builders/cartographer.R") # 4
            
            #----- assign static assets
            dataset <- record_list$`Conference paper`$data
            # send the names we parsed in `validateAuthors.R` to the getter functions along their `data`
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Conference paper`)){
                    authors <- record_list$`Conference paper`$author_list
                }
                if("cartographer_list" %in% names(record_list$`Conference paper`)){
                    cartographers <- record_list$`Conference paper`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Conference paper`)){
                    photographers <- record_list$`Conference paper`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Conference paper`)){
                    editors <- record_list$`Conference paper`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Conference paper`)){
                    series_editors <- record_list$`Conference paper`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Conference paper`)){
                    cover_types <- record_list$`Conference paper`$cover_type_list
                }
            }
            
            for(row in 1:nrow(dataset)){
                data <- dataset[row,]
                # 4.3. nest a level-2 child node inside level-1 node
                l1 <- xml2::xml_children(real) # define what the level-1 tags are
                #----- <record>
                xml_add_child(l1, "record")
                #-----  <ref-type>
                real <- getRefType(real, data)
                real <- getRefTypeName(real, data)
                #----- <title>
                real <- getTitle(real, data)
                #----- <author>
                if(!is.na(data$author)){
                    real <- getAuthor(real, data, authors, row)
                }
                #----- <year>
                real <- getYear(real, data)
                #----- <pdf-urls>
                real <- getPdfUrls(real, data)
                #----- <modified-date> `location`
                real <- getLocation(real, data)
                #----- <research-notes>
                real <- getResearchNotes(real, data)
                #----- <custom7> i.e., `cover-type`
                if(!is.na(data$`cover-type`)){
                    real <- getCoverType(real, data, cover_types, row)
                }
                #----- <related-urls>
                real <- getRelatedUrls(real, data)
                #----- <edition>
                real <- getEdition(real, data)
                #----- <section>
                real <- getSection(real, data)
                #----- <editor>
                if(!is.na(data$editor)){
                    real <- getEditor(real, data, editors, row)
                }
                #----- <secondary-title>
                real <- getSecondaryTitle(real, data)
                #----- <pub-location>
                real <- getPubLocation(real, data)
                #----- <publisher>
                real <- getPublisher(real, data)
                #----- <volume>
                real <- getVolume(real, data)
                #----- <number>
                real <- getNumber(real, data)
                #----- <pages>
                real <- getPages(real, data)
            }
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Conference paper` references...\n")
        }
    )
}