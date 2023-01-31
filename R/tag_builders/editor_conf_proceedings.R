#-----  <editor> name
getConfEditor <- function(real, data, editors, row){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    l4 <- xml2::xml_children(l3)
    l5 <- xml2::xml_children(l4)
    l6 <- xml2::xml_children(l5)
    if(is.na(data$editor)){
        xml_add_child(l2[length(l2)], "contributors") # add a contributor tag only if we didn't already add one for the authors tag(s)
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "authors")
        for(j in 1:length(editors[[row]])){
            l4 <- xml2::xml_children(l3)
            xml_add_child(l4[length(l4)], "author")
            l5 <- xml2::xml_children(l4)
            xml_set_attr(l5[length(l5)], "role", "editor")
            l6 <- xml2::xml_children(l5)
            xml_add_child(l5[length(l5)], "style", trimws(editors[[row]][[j]]))
        }
    }

    
    
    return(real)
}