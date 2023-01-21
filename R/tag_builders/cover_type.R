#----- <cover-type>
getCoverType <- function(real, data, cover_types){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    l4 <- xml2::xml_children(l3)
    l5 <- xml2::xml_children(l4)
    l6 <- xml2::xml_children(l5)
    for(i in 1:nrow(data)){
        if (!is.na(data$`cover-type`[i])){
            xml_add_child(l2[i], "custom7")
            for(j in 1:length(cover_types[[i]])){
                l3 <- xml2::xml_children(l2)
                xml_add_child(l3[length(l3)], "style", trimws(cover_types[[i]][[j]]))
            }
        }
    }
    return(real)
}