#----- <edition>
getEdition <- function(real, data){
    if (!is.na(data$`edition`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "edition")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data$`edition`) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
    }
    return(real)
}



