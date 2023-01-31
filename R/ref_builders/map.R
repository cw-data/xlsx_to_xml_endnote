#----------------------------------------------------------------------------------------------------------
#---`map.R` is a getter function that creates xml for 'Map' records from `record_list` ----
#--- a module for `main.R` that creates xml from forms xlsx data ------------------------------------------
#----------------------------------------------------------------------------------------------------------

getMap <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R") # 1
            source("R/tag_builders/ref_type_name.R") # 2
            source("R/tag_builders/title.R") # 3
            source("R/tag_builders/cartographer.R") # 4
            source("R/tag_builders/year.R") # 5
            source("R/tag_builders/date.R") # 6
            source("R/tag_builders/pdf_urls.R") # 7
            source("R/tag_builders/location.R") # 8
            source("R/tag_builders/caption.R") # 9
            source("R/tag_builders/cover_type.R") # 9
            source("R/tag_builders/related_urls.R") # 10
            
            #----- assign static assets
            dataset <- record_list$`Map`$data
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Map`)){
                    authors <- record_list$`Map`$author_list
                }
                if("cartographer_list" %in% names(record_list$`Map`)){
                    cartographers <- record_list$`Map`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Map`)){
                    photographers <- record_list$`Map`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Map`)){
                    editors <- record_list$`Map`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Map`)){
                    series_editors <- record_list$`Map`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Map`)){
                    cover_types <- record_list$`Map`$cover_type_list
                }
            }
            
            #----- loop to create tags for each record (i.e., row)
            for(row in 1:nrow(dataset)){
                data <- dataset[row,]
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
                #----- <cartographer>
                if(!is.na(data$cartographer)){
                    real <- getCartographer(real, data, cartographers, row)
                }
                #----- <year>
                real <- getYear(real, data)
                #----- <date>
                real <- getDate(real, data)
                #----- <pdf-urls>
                real <- getPdfUrls(real, data)
                #----- <related-urls>
                real <- getRelatedUrls(real, data)
                #----- <modified-date> `location`
                real <- getLocation(real, data)
                #----- <caption>
                real <- getCaption(real, data)
                #----- <custom7> i.e., `cover-type`
                if(!is.na(data$`cover-type`)){
                    real <- getCoverType(real, data, cover_types, row)
                }
            }
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Map` references...\n")
        }
    )
}