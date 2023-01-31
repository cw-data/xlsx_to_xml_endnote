#----- <num-vols>
getNumVols <- function(real, data){
    if (!is.na(data$`num-vols`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "num-vols")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data$`num-vols`) # pointing the index to length() adds sub-tags inside the most recently added tag of that level
    }
    return(real)
}



