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
            
            #----- assign static assets
            dataset <- record_list$`Journal article`$data
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Journal article`)){
                    authors <- record_list$`Journal article`$author_list
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
            
            #----- loop to create tags for each record (i.e., row)
            for(row in 1:nrow(dataset)){ # loop that adds one <record> tag for each row in `data`
                data <- dataset[row,]
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
                #----- <related-urls>
                real <- getRelatedUrls(real, data)
                #----- <modified-date> `location`
                real <- getLocation(real, data)
                #----- <research-notes>
                real <- getResearchNotes(real, data)
                #----- <custom7> i.e., `cover-type`
                if(!is.na(data$`cover-type`)){
                    real <- getCoverType(real, data, cover_types, row)
                }
            }
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Journal article` references...\n")
        }
    )
}