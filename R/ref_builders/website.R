#-----------------------------------------------------------------------------------------
#---`website.R` is a getter function that creates xml for "Online reference/website" records from `record_list` ---
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getWebsite <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R")
            source("R/tag_builders/ref_type_name.R")
            source("R/tag_builders/title.R")
            source("R/tag_builders/author.R")
            source("R/tag_builders/year.R")
            source("R/tag_builders/pdf_urls.R")
            source("R/tag_builders/location.R")
            source("R/tag_builders/research_notes.R")
            source("R/tag_builders/cover_type.R")
            source("R/tag_builders/related_urls.R")
            
            #----- assign static assets
            dataset <- record_list$`Online reference/website`$data
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Online reference/website`)){
                    authors <- record_list$`Online reference/website`$author_list
                }
                if("cartographer_list" %in% names(record_list$`Online reference/website`)){
                    cartographers <- record_list$`Online reference/website`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Online reference/website`)){
                    photographers <- record_list$`Online reference/website`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Online reference/website`)){
                    editors <- record_list$`Online reference/website`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Online reference/website`)){
                    series_editors <- record_list$`Online reference/website`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Online reference/website`)){
                    cover_types <- record_list$`Online reference/website`$cover_type_list
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
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Online reference/website` references...\n")
        }
    )
}