#-----------------------------------------------------------------------------------------
#---`newspaper.R` is a getter function that creates xml for Newspaper records from `record_list` -
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getNewspaper <- function(real, record_list){
    tryCatch(
        expr = {
            #----- load project functions
            source("R/tag_builders/ref_type.R")
            source("R/tag_builders/ref_type_name.R")
            source("R/tag_builders/title.R")
            source("R/tag_builders/author.R")
            source("R/tag_builders/year.R")
            source("R/tag_builders/secondary_title.R")
            source("R/tag_builders/pdf_urls.R")
            source("R/tag_builders/related_urls.R")
            source("R/tag_builders/pub_location.R")
            source("R/tag_builders/publisher.R")
            source("R/tag_builders/pages.R")
            source("R/tag_builders/volume.R")
            source("R/tag_builders/edition.R")
            source("R/tag_builders/section.R")
            source("R/tag_builders/location.R")
            source("R/tag_builders/research_notes.R")
            source("R/tag_builders/cover_type.R")
            
            #----- assign static assets
            dataset <- record_list$`Newspaper article`$data
            if(nrow(dataset)>0){ # only attempt to assign these lists if there are records in this `record_list` subset
                if("author_list" %in% names(record_list$`Newspaper article`)){
                    authors <- record_list$`Newspaper article`$author_list
                }
                if("cartographer_list" %in% names(record_list$`Newspaper article`)){
                    cartographers <- record_list$`Newspaper article`$cartographer_list
                }
                if("photographer_list" %in% names(record_list$`Newspaper article`)){
                    photographers <- record_list$`Newspaper article`$photographer_list
                }
                if("editor_list" %in% names(record_list$`Newspaper article`)){
                    editors <- record_list$`Newspaper article`$editor_list
                }
                if("series_editor_list" %in% names(record_list$`Newspaper article`)){
                    series_editors <- record_list$`Newspaper article`$series_editor_list
                }
                if("cover_type_list" %in% names(record_list$`Newspaper article`)){
                    cover_types <- record_list$`Newspaper article`$cover_type_list
                }
                if("tertiary_author_list" %in% names(record_list$`Newspaper article`)){
                    tertiary_authors <- record_list$`Newspaper article`$tertiary_author_list
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
                if(!is.na(data$author[1])){
                    real <- getAuthor(real, data, authors, row)
                }
                #----- <year>
                real <- getYear(real, data)
                #-----  <secondary-title>
                getSecondaryTitle <- function(real, data)
                #----- <pdf-urls>
                real <- getPdfUrls(real, data)
                #----- <related-urls>
                real <- getRelatedUrls(real, data)
                #----- <pub-location>
                real <- getPubLocation(real, data)
                #----- <publisher>
                real <- getPublisher(real, data)
                #----- <pages>
                real <- getPages(real, data)
                #----- <volume>
                real <- getVolume(real, data)
                #----- <edition>
                real <- getEdition(real, data)
                #----- <section>
                real <- getSection(real, data)
                #----- <modified-date> `location`
                real <- getLocation(real, data)
                #----- <research-notes>
                real <- getResearchNotes(real, data)
                #----- <custom7> i.e., `cover-type`
                if(!is.na(data$`cover-type`[1])){
                    real <- getCoverType(real, data, cover_types, row)
                }
            }
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check, print to console
            return(real)
        },
        finally = {
            message("XML built successfully for `Newspaper article` references...\n")
        }
    )
}