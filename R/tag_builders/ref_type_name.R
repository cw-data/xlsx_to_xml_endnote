#-----  <ref-type> name
getRefTypeName <- function(real, data){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    l4 <- xml2::xml_children(l3)
    l5 <- xml2::xml_children(l4)
    l6 <- xml2::xml_children(l5)
    for(i in 1:nrow(data)){
        xml_set_attr(l3[length(l3)], "name", data$`ref-type`[i])
    }
    return(real)
}