#----- <year>
getYear <- function(real, data){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    l4 <- xml2::xml_children(l3)
    l5 <- xml2::xml_children(l4)
    l6 <- xml2::xml_children(l5)
    for(i in 1:nrow(data)){
        if (!is.na(data$`year`[i])){
            xml_add_child(l2[i], "dates")
            l3 <- xml2::xml_children(l2)
            xml_add_child(l3[length(l3)], "year") # pointing the index to length() adds sub-tags inside the most recently added tag of that level
            l4 <- xml2::xml_children(l3)
            xml_add_child(l4[length(l4)], "style", data$`year`[i])
        }
    }
    return(real)
}



