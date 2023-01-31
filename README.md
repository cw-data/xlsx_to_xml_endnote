# Converting MS forms .xlsx output into EndNote-acceptable .xml

## Getting started
1. Clone this GitHub repo to a local RStudio project. [Instructions here](https://argoshare.is.ed.ac.uk/healthyr_book/clone-an-existing-github-project-to-new-rstudio-project.html)
2. Navigate to the "R" folder in your working directory. Probably "~xlsx_to_xml_endnote/R".
3. Open "R/main.R"
4. Run each line of "main.R" to generate xml from a spreadsheet of Forms data. E.g., "data/20230129/Forms_output_for_comparison.xlsx"
**note: setting the `write=` flag to `TRUE` will prompt the user to save xml output to file. 
5. If saving the xml to file:
6. In the file dialog box, navigate to the folder you want to save your xml in Hint: use the dialog's "Look in" drop-down menu.
7. In the dialog's "File name" box, enter your filename. You must include the ".xml" file extension. E.g., xml_output.xml