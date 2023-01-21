#----- <tertiary-author> i.e., 'publisher' for fspub, unpublished report, other gov report
getTertiaryAuthors <- function(real, data, tertiary_authors){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    l4 <- xml2::xml_children(l3)
    l5 <- xml2::xml_children(l4)
    l6 <- xml2::xml_children(l5)
    for(i in 1:nrow(data)){
        if (!is.na(data$`series-editor`[i])){
            l3 <- xml2::xml_children(l2)
            xml_add_child(l3[2], "tertiary-authors") # confirmed data/20230104/Book_example.xml
            l4 <- xml2::xml_children(l3)
            for(j in 1:length(authors[[i]])){
                xml_add_child(l4[length(l4)], "author")
                l5 <- xml2::xml_children(l4)
                xml_set_attr(l5[length(l5)], "role", "publisher")
                l6 <- xml2::xml_children(l5)
                xml_add_child(l5[length(l5)], "style", tertiary_authors[[i]][[j]])
            }
        }
    }
    return(real)
}



