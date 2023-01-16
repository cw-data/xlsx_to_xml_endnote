#-----  <title>
getTitle <- function(real, data, l2, l3, l4, l5, l6){
    for(i in 1:nrow(data)){
        if (!is.na(data$title[i])){
            xml_add_child(l2[i], "titles")
            l3 <- xml2::xml_children(l2)
            xml_add_child(l3[length(l3)], "title")
            l4 <- xml2::xml_children(l3)
            xml_add_child(l4[length(l4)], "style", data$title)
            l5 <- xml2::xml_children(l4)
        }
    }
    return(real)
}