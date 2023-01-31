#----------------------------------------------------------------------------------------------------------
#---`photo.R` is a getter function that creates xml for 'Photograph' records from `record_list` ----
#--- a module for `main.R` that creates xml from forms xlsx data ------------------------------------------
#----------------------------------------------------------------------------------------------------------

getPhoto <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R")
            source("R/tag_builders/ref_type_name.R")
            source("R/tag_builders/title.R")
            source("R/tag_builders/photographer.R")
            source("R/tag_builders/year.R")
            source("R/tag_builders/ppv_rev.R")
            source("R/tag_builders/pdf_urls.R")
            source("R/tag_builders/location.R")
            source("R/tag_builders/caption.R")
            source("R/tag_builders/cover_type.R")
            source("R/tag_builders/related_urls.R")
            
            #----- assign static assets
            dataset <- record_list$`Photograph`$data
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Photograph`)){
                    authors <- record_list$`Photograph`$author_list
                }
                if("cartographer_list" %in% names(record_list$`Photograph`)){
                    photographers <- record_list$`Photograph`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Photograph`)){
                    photographers <- record_list$`Photograph`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Photograph`)){
                    editors <- record_list$`Photograph`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Photograph`)){
                    series_editors <- record_list$`Photograph`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Photograph`)){
                    cover_types <- record_list$`Photograph`$cover_type_list
                }
            }
            
            #----- loop to create tags for each record (i.e., row)
            for(row in 1:nrow(dataset)){
                data <- dataset[row,]
                l1 <- xml2::xml_children(real) # define what the level-1 tags are
                #----- <record>
                xml_add_child(l1, "record")
                #-----  <ref-type>
                real <- getRefType(real, data)
                real <- getRefTypeName(real, data)
                #----- <title>
                real <- getTitle(real, data)
                #----- <photographer>
                if(!is.na(data$photographer)){
                    real <- getPhotographer(real, data, photographers, row)
                }
                #----- <year>
                real <- getYear(real, data)
                #----- <ppv-rev> # i.e., is this photo in the public domain?
                real <- getPpv(real, data)
                #----- <pdf-urls>
                real <- getPdfUrls(real, data)
                #----- <modified-date> `location`
                real <- getLocation(real, data)
                #----- <caption>
                real <- getCaption(real, data)
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
            message("XML built successfully for `Photograph` references...\n")
        }
    )
}