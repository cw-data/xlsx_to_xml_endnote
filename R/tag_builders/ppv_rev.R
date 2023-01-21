#----- <ppv-rev> # i.e., is this photo in the public domain?
getPpv <- function(real, data){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    l4 <- xml2::xml_children(l3)
    l5 <- xml2::xml_children(l4)
    l6 <- xml2::xml_children(l5)
    for(i in 1:nrow(data)){
        if (!is.na(data$`ppv-rev`[i])){
            xml_add_child(l2[i], "ppv-rev")
            l3 <- xml2::xml_children(l2)
            xml_add_child(l3[length(l3)], "style", data$`ppv-rev`[i]) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
            l4 <- xml2::xml_children(l3)
        }
    }
    return(real)
}



