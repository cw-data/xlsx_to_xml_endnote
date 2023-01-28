#----- <tertiary-title>
getTertiaryTitle <- function(real, data){
    if (!is.na(data$`tertiary-title`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        if(!is.na(data$title)){
            xml_add_child(l2[length(l2)], "titles")
            l3 <- xml2::xml_children(l2)
        }
        l3 <- xml_find_all(l2, "//titles")
        xml_add_child(l3[length(l3)], "tertiary-title")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", data$`tertiary-title`)
        l5 <- xml2::xml_children(l4)
    }
    return(real)
}



