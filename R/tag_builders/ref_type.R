#-----  <ref-type>
getRefType <- function(record_list, real, data, l2){
    for(i in 1:nrow(data)){
        if (is.na(data$`ref-type`[i]) == FALSE){
            xml_add_child(l2, "ref-type", data$`value`[i])
        }
    }
    return(real)
}



