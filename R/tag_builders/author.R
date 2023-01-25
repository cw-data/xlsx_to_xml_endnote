#-----  <author> name
getAuthor <- function(real, data, authors){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    l4 <- xml2::xml_children(l3)
    l5 <- xml2::xml_children(l4)
    l6 <- xml2::xml_children(l5)
    for(i in 1:nrow(data)){
        if (!is.na(data$author[i])){
            xml_add_child(l2[length(l2)], "contributors")
            l3 <- xml2::xml_children(l2)
            xml_add_child(l3[length(l3)], "authors")
            l4 <- xml2::xml_children(l3)
            for(j in 1:length(authors[[i]][[1]])){
                xml_add_child(l4[length(l4)], "author")
                l5 <- xml2::xml_children(l4)
                xml_set_attr(l5[length(l5)], "role", "author")
                l6 <- xml2::xml_children(l5)
                xml_add_child(l5[length(l5)], "style", trimws(authors[[i]][[1]][[j]]))
            }
        }
    }
    return(real)
}