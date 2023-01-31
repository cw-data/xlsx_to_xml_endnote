#-----  <cartographer> name
getCartographer <- function(real, data, cartographers, row){
    if (!is.na(data$cartographer)){
        l1 <- xml2::xml_children(real)
        l2 <- xml2::xml_children(l1)
        xml_add_child(l2[length(l2)], "contributors")
        l3 <- xml2::xml_children(l2)
        xml_add_child(l3[length(l3)], "tertiary-authors")
        for(j in 1:length(cartographers)){
            l4 <- xml2::xml_children(l3)
            xml_add_child(l4[length(l4)], "author")
            l5 <- xml2::xml_children(l4)
            xml_set_attr(l5[length(l5)], "role", "cartographer")
            l6 <- xml2::xml_children(l5)
            xml_add_child(l5[length(l5)], "style", trimws(cartographers[[row]][[j]]))
        }
    }
    return(real)
}