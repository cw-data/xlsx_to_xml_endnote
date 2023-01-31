#----- <modified-date> i.e., location
getLocation <- function(real, data){
    if (!is.na(data$location)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "modified-date")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "style", data$location)
    }
    return(real)
}