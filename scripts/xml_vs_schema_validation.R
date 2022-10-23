# script to validate our xml output against a verified endnote xml schema

# We need a way to programmatically verify that our tags match what EndNote is expecting
# Without programmatically verifying our tags, it'll be difficult to keep our xml tags aligned with EndNote's tags

########################################
# minimal reprex
# https://www.r-bloggers.com/2017/01/using-xml-schema-and-xslt-in-r/

library(xml2)
# Example order
doc <- read_xml(system.file("extdata/order-doc.xml", package = "xml2"))

# Example schema
schema <- read_xml(system.file("extdata/order-schema.xml", package = "xml2"))
xml_validate(doc, schema)
# TRUE

########################################
# compare endnote's real xml schema to a real endnote xml output

# 1) first, we need an endnote xml schema
### # resources/RSXML.dtd is a document type definition file, not an XML schema. However, the two filetypes are closely related.
### A google search showed that IntelliJ IDEs (e.g., pycharm) have an in-built DTD to XSD (xml schema) converter. # https://www.jetbrains.com/help/idea/generating-dtd.html
### I opened pycharm, a FWS-approved python editor that I already use for python scripting.
### In pycharm: tools -> xml actions -> convert schema...
### I chose output type 'RELAX NG - XML Syntax' with UTF-8 encoding, indent 4, and line length 120.
### I don't know if these output parameters are EndNote-acceptable, but it's a start.
schema2 <- read_xml("resources/RSXML.xsd") # this is a real xml schema document converted from from endnote's official document type definition # https://support.clarivate.com/Endnote/s/article/EndNote-XML-Document-Type-Definition?language=en_US

# 2) second, we need a real endnote-generated xml from a real .enl library
# doc2 <- read_xml("data/enl_xml_schema.txt") # This is the original xml of one record exported from endnote
doc2 <- read_xml("data/real_enl_xml.xml") # this is a copy of enl_xml_schema.txt, saved as .xml, and then edited (as shown below) until it passed verification against "resources/RSXML.xsd"
# enl_xml_schema.txt failed validation because:
### <xml> tag is not present in "resources/RSXML.xsd" # resolved by removing tags <xml></xml> from "data/real_enl_xml.xml"
### <foreign-keys> tag is not present in "resources/RSXML.xsd" # resolved by removing tags and data <foreign-keys>data</foreign-keys> from "data/real_enl_xml.xml"
### got <pdf-urls> but expected <image-urls>. # resolved by replacing tags <pdf-urls></pdf-urls> with <image-urls></image-urls> in "data/real_enl_xml.xml"
### with those changes, "data/real_enl_xml.xml" validates as TRUE "resources/RSXML.xsd"

# 3) then, we need to validate that the schema we generated and the the xml record from endnote align
xml_validate(doc2, schema2)
