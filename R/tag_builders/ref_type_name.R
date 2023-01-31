#-----  <ref-type> name
getRefTypeName <- function(real, data){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    xml_set_attr(l3[length(l3)], "name", data$`ref-type`)
    return(real)
}