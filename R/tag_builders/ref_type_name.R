#-----  <ref-type> name
getRefTypeName <- function(real, data, l2, l3, l4, l5){
    for(i in 1:nrow(data)){
        xml_set_attr(l3, "name", data$`ref-type`[i])
    }
    return(real)
}