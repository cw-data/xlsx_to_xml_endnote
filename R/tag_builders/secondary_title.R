#-----  <secondary-title>
getSecondaryTitle <- function(real, data){
    if(!is.na(data$`secondary-title`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        if (is.na(data$title)){
            xml_add_child(l2[length(l2)], "titles")
        }
        l3 <- xml_find_all(l1, "//titles")
        xml_add_child(l3[length(l3)], "secondary-title")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", data$`secondary-title`) # book title
    }
    return(real)
}