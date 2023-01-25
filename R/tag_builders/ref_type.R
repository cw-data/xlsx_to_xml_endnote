#-----  <ref-type>
getRefType <- function(real, data){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    for(i in 1:nrow(data)){
        if (is.na(data$`ref-type`[i]) == FALSE){
            xml_add_child(l2[length(l2)], "ref-type", data$`value`[i])
        }
    }
    return(real)
}



