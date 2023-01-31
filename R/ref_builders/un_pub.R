#-----------------------------------------------------------------------------------------
#---`un_pub.R` is a getter function that creates xml for un_pub records from `record_list` -
#--- un_pub == "Unpublished report (e.g., establishment or progress report)"---------------
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getUnPub <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R")
            source("R/tag_builders/ref_type_name.R")
            source("R/tag_builders/title.R")
            source("R/tag_builders/author.R")
            source("R/tag_builders/year.R")
            source("R/tag_builders/series_editor.R")
            source("R/tag_builders/tertiary_title.R")
            source("R/tag_builders/pub_location.R")
            source("R/tag_builders/publisher.R")
            source("R/tag_builders/volume.R")
            source("R/tag_builders/secondary_volume.R")
            source("R/tag_builders/number.R")
            source("R/tag_builders/pages.R")
            source("R/tag_builders/tertiary_author.R")
            source("R/tag_builders/edition.R")
            source("R/tag_builders/pdf_urls.R")
            source("R/tag_builders/research_notes.R")
            source("R/tag_builders/location.R")
            source("R/tag_builders/cover_type.R")
            source("R/tag_builders/related_urls.R")
            
            #----- assign static assets
            dataset <- record_list$`Unpublished report (e.g., establishment or progress report)`$data
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Unpublished report (e.g., establishment or progress report)`)){
                    authors <- record_list$`Unpublished report (e.g., establishment or progress report)`$author_list
                }
                if("cartographer_list" %in% names(record_list$`Unpublished report (e.g., establishment or progress report)`)){
                    cartographers <- record_list$`Unpublished report (e.g., establishment or progress report)`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Unpublished report (e.g., establishment or progress report)`)){
                    photographers <- record_list$`Unpublished report (e.g., establishment or progress report)`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Unpublished report (e.g., establishment or progress report)`)){
                    editors <- record_list$`Unpublished report (e.g., establishment or progress report)`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Unpublished report (e.g., establishment or progress report)`)){
                    series_editors <- record_list$`Unpublished report (e.g., establishment or progress report)`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Unpublished report (e.g., establishment or progress report)`)){
                    cover_types <- record_list$`Unpublished report (e.g., establishment or progress report)`$cover_type_list
                }
                if("tertiary_author_list" %in% names(record_list$`Unpublished report (e.g., establishment or progress report)`)){
                    tertiary_authors <- record_list$`Unpublished report (e.g., establishment or progress report)`$tertiary_author_list
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
                #----- <tertiary-author> i.e., 'publisher' for un_pub, unpublished report, other gov report
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
                    real <- getCoverType(real, data, cover_types)
                }
                #----- <related-urls>
                real <- getRelatedUrls(real, data)
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Unpublished report (e.g., establishment or progress report)` references...\n")
        }
    )
}