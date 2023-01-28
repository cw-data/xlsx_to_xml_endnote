#----- <date>
getDate <- function(real, data){
    if (!is.na(data$`date`)){
        if (is.na(data$`year`)){
            l1 <- xml2::xml_children(real)
            l2 <- xml2::xml_children(l1)
            xml_add_child(l2[length(l2)], "dates")
        }
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "date") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "pub-dates")
        l5 <- xml2::xml_children(l4)
        xml_add_child(l5[length(l5)], "style", data$`date`)
    }
    return(real)
}



