#----- <ppv-rev> # i.e., is this photo in the public domain?
getPpv <- function(real, data){
    if (!is.na(data$`ppv-rev`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "ppv-rev")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data$`ppv-rev`) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
        l4 <- xml2::xml_children(l3)
    }
    return(real)
}



