#----------------------------------------------------------------------------------------------------------
#---`conf_paper.R` is a getter function that creates xml for 'Conference paper' records from `record_list` 
#--- a module for `main.R` that creates xml from forms xlsx data ------------------------------------------
#----------------------------------------------------------------------------------------------------------

getConferencePaper <- function(real, record_list){
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
            source("R/tag_builders/section.R")
            source("R/tag_builders/editor.R")
            source("R/tag_builders/secondary_title.R")
            source("R/tag_builders/pub_location.R")
            source("R/tag_builders/publisher.R")
            source("R/tag_builders/volume.R")
            source("R/tag_builders/number.R") 
            source("R/tag_builders/pages.R")
            source("R/tag_builders/custom1_publisher_location.R")
            source("R/tag_builders/date.R") # 6
            
            #----- assign static assets
            dataset <- record_list$`Conference paper`$data
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
                #----- <author>
                if(!is.na(data$author)){
                    real <- getAuthor(real, data, authors, row)
                }
                #----- <year>
                real <- getYear(real, data)
                #----- <section>
                real <- getSection(real, data)
                #----- <secondary-title>
                real <- getSecondaryTitle(real, data)
                #----- <date>
                real <- getDate(real, data)
                #----- <pub-location> # conference location
                real <- getPubLocation(real, data)
                #----- <editor>
                if(!is.na(data$editor)){
                    real <- getEditor(real, data, editors, row)
                }
                #----- <pages>
                real <- getPages(real, data)
                #----- <publisher>
                real <- getPublisher(real, data)
                #----- <custom1> # publisher location for conf proceedings and conf paper
                real <- getCustom(real, data)
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
            message("XML built successfully for `Conference paper` references...\n")
        }
    )
}