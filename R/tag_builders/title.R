#-----  <title>
getTitle <- function(real, data){
    if (!is.na(data$title)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "titles")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "title")
        l4 <- xml2::xml_children(l3)
        xml_add_child(l4[length(l4)], "style", data$title)
    }
    return(real)
}