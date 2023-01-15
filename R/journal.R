rm(list=ls())
library(readxl)
library(dplyr)
library(data.table)
library(xml2)

data <- record_list$`Journal article`$data
authors <- record_list$`Journal article`$author_list
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
    if (!is.na(data$`ref-type`[i])){
        xml_add_child(l2, "ref-type", data$`value`[i])    
    }
}
# xml2::xml_children(l2)
# cat(as.character(xml2::as_xml_document(real))) # sanity check
l3 <- xml2::xml_children(l2)
for(i in 1:nrow(data)){
    xml_set_attr(l3, "name", data$`ref-type`[i])
}
cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <author>
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$author[i]) == FALSE){
        xml_add_child(l2[i], "contributors")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "authors")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "author")
        l5 <- xml2::xml_children(l4)
        xml_set_attr(l5[length(l5)], "role", "author")
        l6 <- xml2::xml_children(l5)
        xml_add_child(l5[length(l5)], "style", data[[i]]$author)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <editor>
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$editor[i]) == FALSE){
        # xml_add_child(l2[1], "contributors")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[2], "tertiary-authors") # confirmed data/20230104/Book_example.xml
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "author")
        l5 <- xml2::xml_children(l4)
        xml_set_attr(l5[length(l5)], "role", "editor")
        l6 <- xml2::xml_children(l5)
        xml_add_child(l5[length(l5)], "style", data[[i]]$editor)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <title>
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$title[i]) == FALSE){
        xml_add_child(l2[i], "titles")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "title")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", data[[i]]$title)
        l5 <- xml2::xml_children(l4)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <secondary-title> i.e., series title
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$title[i]) == FALSE){
        xml_add_child(l2[i], "titles")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "secondary-title")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", data[[i]]$title)
        l5 <- xml2::xml_children(l4)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <modified-date> `location`
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$location[i]) == FALSE){
        xml_add_child(l2[i], "modified-date")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[i]]$location)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <custom7> i.e., `cover-type`
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`cover-type`[i]) == FALSE){
        xml_add_child(l2[i], "custom7") # per data/20240104/Book_example.xml
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[i]]$`cover-type`)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <year>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`year`[i]) == FALSE){
        xml_add_child(l2[i], "dates")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "year") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", data[[1]]$`year`[i])
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <pub-location>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`pub-location`[i]) == FALSE){
        xml_add_child(l2[i], "pub-location")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[1]]$`pub-location`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <publisher>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`publisher`[i]) == FALSE){
        xml_add_child(l2[i], "publisher")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[1]]$`publisher`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <related-urls>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`related-urls`[i]) == FALSE){
        xml_add_child(l2[i], "urls")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "related-urls") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "url")
        l5 <- xml2::xml_children(l4)
        xml_add_child(l5[length(l5)], "style", data[[1]]$`web-urls`[i])
        l6 <- xml2::xml_children(l5)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <research-notes>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`research-notes`[i]) == FALSE){
        xml_add_child(l2[i], "research-notes")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[1]]$`research-notes`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <pages>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`pages`[i]) == FALSE){
        xml_add_child(l2[i], "pages")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[1]]$`pages`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <edition>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`edition`[i]) == FALSE){
        xml_add_child(l2[i], "edition")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[1]]$`edition`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <volume>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`volume`[i]) == FALSE){
        xml_add_child(l2[i], "volume")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[1]]$`volume`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <num-vols>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`num-vols`[i]) == FALSE){
        xml_add_child(l2[i], "num-vols")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[1]]$`num-vols`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <secondary-volume>
# cat(as.character(xml2::as_xml_document(real))) # sanity check
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`secondary-volume`[i]) == FALSE){
        xml_add_child(l2[i], "secondary-volume")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data[[1]]$`secondary-volume`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <tertiary-title>
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`tertiary-title`[i]) == FALSE){
        # xml_add_child(l2[i], "titles")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[3], "tertiary-title")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", data[[i]]$`tertiary-title`)
        l5 <- xml2::xml_children(l4)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <secondary-authors> "series-editor"
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`series-editor`[i]) == FALSE){
        # xml_add_child(l2[1], "contributors")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[2], "secondary-authors") # confirmed data/20230104/Book_example.xml
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "author")
        l5 <- xml2::xml_children(l4)
        xml_set_attr(l5[length(l5)], "role", "series-editor")
        l6 <- xml2::xml_children(l5)
        xml_add_child(l5[length(l5)], "style", data[[i]]$`series-editor`)
    }
}
# cat(as.character(xml2::as_xml_document(real))) # sanity check
#----- <pdf-urls>
for(i in 1:nrow(data)){
    if (is.na(data[[1]]$`pdf-urls`[i]) == FALSE){
        xml_add_child(l2[i], "urls")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "pdf-urls") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "url")
        l5 <- xml2::xml_children(l4)
        xml_add_child(l5[length(l5)], "style", data[[1]]$`pdf-urls`[i])
        l6 <- xml2::xml_children(l5)
    }
}
cat(as.character(xml2::as_xml_document(real))) # sanity check



########## Step 5: write output to xml
# write_xml(real, paste0("data/",format(Sys.time(), "%Y%m%d"), "_book_output.xml"), options = "format")
real <- stringr::str_remove_all(real, "(\n +|\n)")
real <- as.character(real)
# data.table::fwrite(real, "data/20230102/testoutput.xml")
write(real, "data/20230104/20230104testoutput.xml")
