#----- <custom1> "publisher location" for conference proceedings and conference paper
getCustom <- function(real, data){
    if (!is.na(data$`custom1`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "custom1")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data$`custom1`) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
    }
    return(real)
}



