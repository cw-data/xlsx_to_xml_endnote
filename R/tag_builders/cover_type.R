#----- <cover-type>
getCoverType <- function(real, data, cover_types, row){
    if(!is.na(data$`cover-type`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "custom7")
        for(j in 1:length(cover_types[[row]])){
            l3 <- xml2::xml_children(l2)
            xml_add_child(l3[length(l3)], "style", trimws(cover_types[[row]][[j]]))
        }
    }
    return(real)
}