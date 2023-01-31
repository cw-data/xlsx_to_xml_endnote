#---------------------------------------------------------------------------------------------------------------------
#---`conf_proceedings.R` is a getter function that creates xml for Conference proceedings records from `record_list` -
#--- a module for `main.R` that creates xml from forms xlsx data -----------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------

getConfProceed <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R") # 1
            source("R/tag_builders/ref_type_name.R") # 2
            source("R/tag_builders/title.R") # 3
            source("R/tag_builders/editor.R")
            source("R/tag_builders/editor_conf_proceedings.R")
            source("R/tag_builders/year.R") # 5
            source("R/tag_builders/pdf_urls.R") # 16
            source("R/tag_builders/location.R") # 18
            source("R/tag_builders/research_notes.R") # 17
            source("R/tag_builders/cover_type.R") # 19
            source("R/tag_builders/related_urls.R") # 20
            source("R/tag_builders/num_vols.R") # 8
            source("R/tag_builders/series_editor.R") # 6
            source("R/tag_builders/tertiary_title.R") # 7
            source("R/tag_builders/edition.R") # 7
            source("R/tag_builders/custom1_publisher_location.R") # 15
            source("R/tag_builders/author.R") # 4
            source("R/tag_builders/secondary_title.R") # 11
            source("R/tag_builders/pub_location.R") # 8
            source("R/tag_builders/pages.R") # 8
            # source("R/tag_builders/secondary_volume.R") # 11
            # source("R/tag_builders/number.R") # 12
            # source("R/tag_builders/tertiary_author.R") # 14
            # source("R/tag_builders/pub_location.R") # 10
            # source("R/tag_builders/secondary_title.R") # 
            # source("R/tag_builders/num_vols.R")
            # source("R/tag_builders/pdf_urls.R") # WRITE ME
            # source("R/tag_builders/photographer.R") # WRITE ME
            # source("R/tag_builders/ppv_rev.R") # WRITE ME # "Is this photograph in the public domain?"
            # source("R/tag_builders/caption.R") # WRITE ME
            # source("R/tag_builders/cartographer.R") # WRITE ME
            source("R/tag_builders/date.R") # WRITE ME
            
            #----- assign static assets
            dataset <- record_list$`Conference proceedings`$data
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Conference proceedings`)){
                    authors <- record_list$`Conference proceedings`$author_list
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
            
            #----- loop to create tags for each record (i.e., row)
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
                #----- <editor>
                if(!is.na(data$editor)){
                    real <- getConfEditor(real, data, editors, row)
                }
                #----- <secondary-title>
                real <- getSecondaryTitle(real, data)
                #----- <year>
                real <- getYear(real, data)
                #----- <pub-location>
                real <- getPubLocation(real, data)
                #----- <date>
                real <- getDate(real, data)
                #----- <publisher>
                real <- getPublisher(real, data)
                #----- <custom1> # publisher location for conf proceedings and conf paper
                real <- getCustom(real, data)
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
            message("XML built successfully for `Conference proceedings` references...\n")
        }
    )
}