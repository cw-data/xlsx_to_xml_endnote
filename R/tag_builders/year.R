#----- <year>
getYear <- function(real, data){
    if (!is.na(data$`year`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "dates")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "year") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", data$`year`)
    }
    return(real)
}



