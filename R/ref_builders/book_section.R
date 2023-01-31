#----------------------------------------------------------------------------------------------------------
#---`book_section.R` is a getter function that creates xml for Book section records from `record_list` ----
#--- a module for `main.R` that creates xml from forms xlsx data ------------------------------------------
#----------------------------------------------------------------------------------------------------------

getBookSection <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R") # 1
            source("R/tag_builders/ref_type_name.R") # 2
            source("R/tag_builders/title.R") # 3
            source("R/tag_builders/secondary_title.R") # 11
            source("R/tag_builders/author.R") # 4
            source("R/tag_builders/year.R") # 5
            source("R/tag_builders/edition.R") # 5
            source("R/tag_builders/editor.R") # 5
            source("R/tag_builders/num_vols.R") # 8
            source("R/tag_builders/secondary_volume.R") # 9
            source("R/tag_builders/pub_location.R") # 14
            source("R/tag_builders/publisher.R") # 12
            source("R/tag_builders/volume.R") # 13
            source("R/tag_builders/series_editor.R") # 13
            source("R/tag_builders/tertiary_title.R") # 14
            source("R/tag_builders/pages.R") # 15
            source("R/tag_builders/section.R") # 15
            source("R/tag_builders/pdf_urls.R") # 6
            source("R/tag_builders/location.R") # 7
            source("R/tag_builders/research_notes.R") # 8
            source("R/tag_builders/cover_type.R") # 9
            source("R/tag_builders/related_urls.R") # 10
            
            #----- assign static assets
            dataset <- record_list$`Book section`$data
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Book section`)){
                    authors <- record_list$`Book section`$author_list
                }
                if("cartographer_list" %in% names(record_list$`Book section`)){
                    cartographers <- record_list$`Book section`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Book section`)){
                    photographers <- record_list$`Book section`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Book section`)){
                    editors <- record_list$`Book section`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Book section`)){
                    series_editors <- record_list$`Book section`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Book section`)){
                    cover_types <- record_list$`Book section`$cover_type_list
                }
            }
            
            #----- loop to create tags for each record (i.e., row)
            for(row in 1:nrow(dataset)){ # loop that adds one of each tag for each row in `data`
                # 4.3. nest a level-2 child node inside level-1 node
                l1 <- xml2::xml_children(real) # define what the level-1 tags are
                data <- dataset[row,]
                #----- <record>
                xml_add_child(l1, "record")
                #-----  <ref-type>
                real <- getRefType(real, data)
                real <- getRefTypeName(real, data)
                #----- <title>
                real <- getTitle(real, data)
                #----- <secondary-title>
                real <- getSecondaryTitle(real, data)
                #----- <author>
                if(!is.na(data$author)){
                    real <- getAuthor(real, data, authors, row)
                }
                #----- <year>
                real <- getYear(real, data)
                #----- <edition>
                real <- getEdition(real, data)
                #----- <editor>
                if(!is.na(data$editor)){
                    real <- getEditor(real, data, editors, row)
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
                if(!is.na(data$`series-editor`)){
                    real <- getSeriesEditors(real, data, series_editors, row)
                }
                #----- <tertiary-title>
                real <- getTertiaryTitle(real, data)
                #----- <pages>
                real <- getPages(real, data)
                #----- <section>
                real <- getSection(real, data)
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
            }
            cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Book section` references...\n")
        }
    )
}