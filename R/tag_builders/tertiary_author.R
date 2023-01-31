#----- <tertiary-author> i.e., 'publisher' for fspub, unpublished report, other gov report
getTertiaryAuthors <- function(real, data, tertiary_authors, row){
    if(is.na(data$author)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        if(is.na(data$`series-editor`)){
            xml_add_child(l2[length(l2)], "contributors") # add a contributor tag only if we didn't already add one for the authors tag(s)
        }
        l1 <- xml2::xml_children(real)
        l3 <- xml_find_all(l1, "//contributors")
        xml_add_child(l3[length(l3)], "tertiary-authors")
        for(j in 1:length(tertiary_authors[[row]])){
            l4 <- xml2::xml_children(l3)
            xml_add_child(l4[length(l4)], "author")
            l5 <- xml2::xml_children(l4)
            xml_set_attr(l5[length(l5)], "role", "publisher")
            l6 <- xml2::xml_children(l5)
            xml_add_child(l5[length(l5)], "style", trimws(tertiary_authors[[row]][[j]]))
        }
    }
    return(real)
}



