#-----------------------------------------------------------------------------------------
#---`fsPub.R` is a getter function that creates xml for fsPub records from `record_list` -
#--- fsPub == "Forest Service publication (e.g., general technical report)"---------------
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getFSPub <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R") # 1
            source("R/tag_builders/ref_type_name.R") # 2
            source("R/tag_builders/title.R") # 3
            source("R/tag_builders/author.R") # 4
            source("R/tag_builders/year.R") # 5
            source("R/tag_builders/series_editor.R") # 6
            source("R/tag_builders/tertiary_title.R") # 7
            source("R/tag_builders/pub_location.R") # 8
            source("R/tag_builders/publisher.R") # 9
            source("R/tag_builders/volume.R") # 10
            source("R/tag_builders/secondary_volume.R") # 11
            source("R/tag_builders/number.R") # 12
            source("R/tag_builders/pages.R") # 13
            source("R/tag_builders/tertiary_author.R") # 14
            source("R/tag_builders/edition.R") # 15
            source("R/tag_builders/pdf_urls.R") # 16
            source("R/tag_builders/research_notes.R") # 17
            source("R/tag_builders/location.R") # 18
            source("R/tag_builders/cover_type.R") # 19
            source("R/tag_builders/related_urls.R") # 20
            
            #----- assign static assets
            dataset <- record_list$`Forest Service publication (e.g., general technical report)`$data
            # send the names we parsed in `validateAuthors.R` to the getter functions along their `data`
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Forest Service publication (e.g., general technical report)`)){
                    authors <- record_list$`Forest Service publication (e.g., general technical report)`$author_list
                }
                if("cartographer_list" %in% names(record_list$`Forest Service publication (e.g., general technical report)`)){
                    cartographers <- record_list$`Forest Service publication (e.g., general technical report)`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Forest Service publication (e.g., general technical report)`)){
                    photographers <- record_list$`Forest Service publication (e.g., general technical report)`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Forest Service publication (e.g., general technical report)`)){
                    editors <- record_list$`Forest Service publication (e.g., general technical report)`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Forest Service publication (e.g., general technical report)`)){
                    series_editors <- record_list$`Forest Service publication (e.g., general technical report)`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Forest Service publication (e.g., general technical report)`)){
                    cover_types <- record_list$`Forest Service publication (e.g., general technical report)`$cover_type_list
                }
                if("tertiary_author_list" %in% names(record_list$`Forest Service publication (e.g., general technical report)`)){
                    tertiary_authors <- record_list$`Forest Service publication (e.g., general technical report)`$tertiary_author_list
                }
            }
            
            #----- loop to create tags for each record (i.e., row)
            for(row in 1:nrow(dataset)){ # loop that adds one <record> tag for each row in `data`
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
                #----- <secondary-author> i.e., series editor
                if(!is.na(data$`series-editor`)){
                    real <- getSeriesEditors(real, data, series_editors, row)
                }
                #----- <tertiary-title>
                real <- getTertiaryTitle(real, data)
                #----- <pub-location>
                real <- getPubLocation(real, data)
                #----- <publisher>
                real <- getPublisher(real, data)
                #----- <volume>
                real <- getVolume(real, data)
                #----- <secondary-volume>
                real <- getSecVol(real, data)
                #----- <number>
                real <- getNumber(real, data)
                #----- <pages>
                real <- getPages(real, data)
                #----- <tertiary-author> i.e., 'publisher' for fspub, unpublished report, other gov report
                if(!is.na(data$`tertiary-author`)){
                    real <- getTertiaryAuthors(real, data, tertiary_authors, row)
                }
                #----- <edition>
                real <- getEdition(real, data)
                #----- <pdf-urls>
                real <- getPdfUrls(real, data)
                #----- <research-notes>
                real <- getResearchNotes(real, data)
                #----- <modified-date> `location`
                real <- getLocation(real, data)
                #----- <custom7> i.e., `cover-type`
                if(!is.na(data$`cover-type`)){
                    real <- getCoverType(real, data, cover_types, row)
                }
                #----- <related-urls>
                real <- getRelatedUrls(real, data)
            }
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Forest Service publication (e.g., general technical report)` references...\n")
        }
    )
}