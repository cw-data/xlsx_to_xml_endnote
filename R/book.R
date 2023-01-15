#-----------------------------------------------------------------------------------------
#---`book.R` is a getter function that creates xml for book records from `record_list` ---
#--- a module for `main.R` that creates xml from forms xlsx data -------------------------
#-----------------------------------------------------------------------------------------

getBook <- function(record_list){
    tryCatch(
        expr = {
            #----- assign static assets for easier indexing
            data <- record_list$Book$data
            if("author_list" %in% names(record_list$Book)){
                authors <- record_list$Book$author_list
            }
            if("cartographer_list" %in% names(record_list$Book)){
                cartographers <- record_list$Book$cartographer_list
            }
            if("photographer_list" %in% names(record_list$Book)){
                photographers <- record_list$Book$photographer_list
            }
            if("editor_list" %in% names(record_list$Book)){
                editors <- record_list$Book$editor_list
            }
            if("series_editor_list" %in% names(record_list$Book)){
                series_editors <- record_list$Book$series_editor_list
            }
            
            #---- build xml
            # 4.1. instantiate new xml document and add root node
            #----- <xml>
            real <- xml2::xml_new_root("xml") # instantiate root node
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            # 4.2. nest a level-1 child-node inside root (level-zero) node
            #----- <records>
            xml_add_child(real, # the node into which you want to nest a child node
                          "records") # the name of the node you're adding
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            # 4.3. nest a level-2 child node inside level-1 node
            l1 <- xml2::xml_children(real) # define what the level-1 tags are
            # xml2::xml_children(l1)
            #----- <record>
            for(row in 1:nrow(data)){ # loop that adds one <record> tag for each row in the df
                xml_add_child(l1, "record")
            }
            # xml2::xml_children(l1)
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            # 4.4. nest level-3 child nodes inside level 2 node
            l2 <- xml2::xml_children(l1) # define what the level-2 tags are
            #-----  <ref-type>
            for(i in 1:nrow(data)){
                if (is.na(data$`ref-type`[i]) == FALSE){
                    xml_add_child(l2, "ref-type", data$`value`[i])    
                }
            }
            # xml2::xml_children(l2)
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            l3 <- xml2::xml_children(l2)
            for(i in 1:nrow(data)){
                xml_set_attr(l3, "name", data$`ref-type`[i])
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <author>
            if("author_list" %in% names(record_list$Book)){ # only make <author> tags if there are authors
                for(i in 1:nrow(data)){
                    if (!is.na(data$author[i])){
                        xml_add_child(l2[i], "contributors")
                        l3 <- xml2::xml_children(l2)
                        xml_add_child(l3[length(l3)], "authors")
                        l4 <- xml2::xml_children(l3)
                        for(j in 1:length(authors[[i]])){
                            xml_add_child(l4[length(l4)], "author")
                            l5 <- xml2::xml_children(l4)
                            xml_set_attr(l5[length(l5)], "role", "author")
                            l6 <- xml2::xml_children(l5)
                            xml_add_child(l5[length(l5)], "style", authors[[i]][[j]])
                        }
                    }
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <editor>
            if("editor_list" %in% names(record_list$Book)){
                for(i in 1:nrow(data)){
                    if (!is.na(data$editor[i])){
                        l3 <- xml2::xml_children(l2)
                        xml_add_child(l3[2], "tertiary-authors") # confirmed data/20230104/Book_example.xml
                        l4 <- xml2::xml_children(l3)
                        for(j in 1:length(authors[[i]])){
                            xml_add_child(l4[length(l4)], "author")
                            l5 <- xml2::xml_children(l4)
                            xml_set_attr(l5[length(l5)], "role", "editor")
                            l6 <- xml2::xml_children(l5)
                            xml_add_child(l5[length(l5)], "style", editors[[i]][[j]])
                        }
                    }
                }
            }
            
            # for(i in 1:nrow(data)){
            #     if (is.na(data$editor[i]) == FALSE){
            #         # xml_add_child(l2[1], "contributors")
            #         l3 <- xml2::xml_children(l2)
            #         xml_add_child(l3[2], "tertiary-authors") # confirmed data/20230104/Book_example.xml
            #         l4 <- xml2::xml_children(l3)
            #         xml_add_child(l4[length(l4)], "author")
            #         l5 <- xml2::xml_children(l4)
            #         xml_set_attr(l5[length(l5)], "role", "editor")
            #         l6 <- xml2::xml_children(l5)
            #         xml_add_child(l5[length(l5)], "style", data$editor)
            #     }
            # }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <title>
            for(i in 1:nrow(data)){
                if (!is.na(data$title[i])){
                    xml_add_child(l2[i], "titles")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "title")
                    l4 <- xml2::xml_children(l3)
                    xml_add_child(l4[length(l4)], "style", data$title)
                    l5 <- xml2::xml_children(l4)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <secondary-title> i.e., series title
            for(i in 1:nrow(data)){
                if (!is.na(data$title[i])){
                    xml_add_child(l2[i], "titles")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "secondary-title")
                    l4 <- xml2::xml_children(l3)
                    xml_add_child(l4[length(l4)], "style", data$title)
                    l5 <- xml2::xml_children(l4)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <modified-date> `location`
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$location[i])){
                    xml_add_child(l2[i], "modified-date")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$location)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <custom7> i.e., `cover-type`
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`cover-type`[i])){
                    xml_add_child(l2[i], "custom7") # per data/20240104/Book_example.xml
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`cover-type`)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <year>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`year`[i])){
                    xml_add_child(l2[i], "dates")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "year") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                    xml_add_child(l4[length(l4)], "style", data$`year`[i])
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <pub-location>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`pub-location`[i])){
                    xml_add_child(l2[i], "pub-location")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`pub-location`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <publisher>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`publisher`[i])){
                    xml_add_child(l2[i], "publisher")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`publisher`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <related-urls>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`related-urls`[i])){
                    xml_add_child(l2[i], "urls")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "related-urls") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                    xml_add_child(l4[length(l4)], "url")
                    l5 <- xml2::xml_children(l4)
                    xml_add_child(l5[length(l5)], "style", data$`related-urls`[i])
                    l6 <- xml2::xml_children(l5)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <research-notes>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`research-notes`[i])){
                    xml_add_child(l2[i], "research-notes")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`research-notes`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <pages>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`pages`[i])){
                    xml_add_child(l2[i], "pages")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`pages`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <edition>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`edition`[i])){
                    xml_add_child(l2[i], "edition")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`edition`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <volume>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`volume`[i])){
                    xml_add_child(l2[i], "volume")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`volume`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <num-vols>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`num-vols`[i])){
                    xml_add_child(l2[i], "num-vols")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`num-vols`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <secondary-volume>
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            for(i in 1:nrow(data)){
                if (!is.na(data$`secondary-volume`[i])){
                    xml_add_child(l2[i], "secondary-volume")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "style", data$`secondary-volume`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <tertiary-title>
            for(i in 1:nrow(data)){
                if (!is.na(data$`tertiary-title`[i])){
                    # xml_add_child(l2[i], "titles")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[3], "tertiary-title")
                    l4 <- xml2::xml_children(l3)
                    xml_add_child(l4[length(l4)], "style", data$`tertiary-title`)
                    l5 <- xml2::xml_children(l4)
                }
            }
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <secondary-authors> "series-editor"
            # for(i in 1:nrow(data)){
            #     if (!is.na(data$`series-editor`[i])){
            #         # xml_add_child(l2[1], "contributors")
            #         l3 <- xml2::xml_children(l2)
            #         xml_add_child(l3[2], "secondary-authors") # confirmed data/20230104/Book_example.xml
            #         l4 <- xml2::xml_children(l3)
            #         xml_add_child(l4[length(l4)], "author")
            #         l5 <- xml2::xml_children(l4)
            #         xml_set_attr(l5[length(l5)], "role", "series-editor")
            #         l6 <- xml2::xml_children(l5)
            #         xml_add_child(l5[length(l5)], "style", data$`series-editor`)
            #     }
            # }
            
            if("series_editor_list" %in% names(record_list$Book)){
                for(i in 1:nrow(data)){
                    if (!is.na(data$`series-editor`[i])){
                        l3 <- xml2::xml_children(l2)
                        xml_add_child(l3[2], "secondary-authors") # confirmed data/20230104/Book_example.xml
                        l4 <- xml2::xml_children(l3)
                        for(j in 1:length(authors[[i]])){
                            xml_add_child(l4[length(l4)], "author")
                            l5 <- xml2::xml_children(l4)
                            xml_set_attr(l5[length(l5)], "role", "series-editor")
                            l6 <- xml2::xml_children(l5)
                            xml_add_child(l5[length(l5)], "style", series_editors[[i]][[j]])
                        }
                    }
                }
            }
            
            
            
            # cat(as.character(xml2::as_xml_document(real))) # sanity check
            #----- <pdf-urls>
            for(i in 1:nrow(data)){
                if (!is.na(data$`pdf-urls`[i])){
                    xml_add_child(l2[i], "urls")
                    l3 <- xml2::xml_children(l2)
                    xml_add_child(l3[length(l3)], "pdf-urls") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
                    l4 <- xml2::xml_children(l3)
                    xml_add_child(l4[length(l4)], "url")
                    l5 <- xml2::xml_children(l4)
                    xml_add_child(l5[length(l5)], "style", data$`pdf-urls`[i])
                    l6 <- xml2::xml_children(l5)
                }
            }
            cat(as.character(xml2::as_xml_document(real))) # sanity check
            
            
            
            ########## Step 5: write output to xml
            # write_xml(real, paste0("data/",format(Sys.time(), "%Y%m%d"), "_book_output.xml"), options = "format")
            real <- stringr::str_remove_all(real, "(\n +|\n)")
            real <- as.character(real)
            return(real)
        }
        # finally = {
        #     message("`forms_spreadsheet` parsed to `record_list`...\nOutput available as `record_list` in global envrionment...\n")
        # }
    )
}