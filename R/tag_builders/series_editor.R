#----- <secondary-author> i.e., series editor
getSeriesEditors <- function(real, data, series_editors){
    l1 <- xml2::xml_children(real)
    l2 <- xml2::xml_children(l1)
    l3 <- xml2::xml_children(l2)
    l4 <- xml2::xml_children(l3)
    l5 <- xml2::xml_children(l4)
    l6 <- xml2::xml_children(l5)
    for(i in 1:nrow(data)){
        if (!is.na(data$`series-editor`[i])){
            l3 <- xml2::xml_children(l2)
            xml_add_child(l3[2], "secondary-authors") # confirmed data/20230104/Book_example.xml
            l4 <- xml2::xml_children(l3)
            for(j in 1:length(authors[[i]])){
                xml_add_child(l4[length(l4)], "author")
                l5 <- xml2::xml_children(l4)
                xml_set_attr(l5[length(l5)], "role", "series-editor")
                l6 <- xml2::xml_children(l5)
                xml_add_child(l5[length(l5)], "style", series_editors[[i]][[j]])
            }
        }
    }
    return(real)
}



