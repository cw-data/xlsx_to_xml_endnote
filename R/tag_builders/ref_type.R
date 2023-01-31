#-----  <ref-type>
getRefType <- function(real, data){
    if (!is.na(data$`ref-type`)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "ref-type", data$`value`)
    }
    return(real)
}



