#----- <related-urls>
getRelatedUrls <- function(real, data){
    if(!is.na(data$`related-urls`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        if(is.na(data$`pdf-urls`)){
            xml_add_child(l2[length(l2)], "urls")
        }
        l3 <- xml_find_all(l2, "//urls")
        xml_add_child(l3[length(l3)], "related-urls") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "url")
        l5 <- xml2::xml_children(l4)
        xml_add_child(l5[length(l5)], "style", data$`related-urls`)
        l6 <- xml2::xml_children(l5)
    }
    return(real)
}



