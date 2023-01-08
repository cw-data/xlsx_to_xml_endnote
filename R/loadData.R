library(xml2)
library(dplyr)

source("R/validateAuthors.R")

# loadData <- function(forms_spreadsheet){
#     print("got here")
# }


loadData <- function(forms_spreadsheet){
    tryCatch(
        expr = {
            # validate the user-provided `forms_spreadsheet` is a spreadsheet
            if(tools::file_ext(forms_spreadsheet) == "xlsx"){
                forms_spreadsheet <- readxl::read_excel(forms_spreadsheet)
                assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
                message("`forms_spreadsheet` is xlsx...")
            } else if(tools::file_ext(forms_spreadsheet) == "xls"){
                forms_spreadsheet <- readxl::read_excel(forms_spreadsheet)
                assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
                message("`forms_spreadsheet` is xls...")
            } else if(tools::file_ext(forms_spreadsheet) == "csv"){
                data <- data.table::fread(forms_spreadsheet)
                assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
                message("`forms_spreadsheet` is csv...")
                # print(tools::file_ext(forms_spreadsheet))
            } else {
                message("`loadData()` requires argument `forms_spreadsheet` to be one of these filetypes: 'xlsx', 'xls', 'csv'. The `forms_spreadsheet` that you provided is not one of the acceptable filetypes. Input a `forms_spreadsheet`with an acceptable filetype and try `loadData(forms_spreadsheet)` again.")
                break
            }
            # validate the user-provided `forms_spreadsheet` matches known colnames
            # we'll compare user input against an example that has the correct column format
            # check for things that we know are wrong and would produce errors later in the script
            example_forms_spreadsheet <- readxl::read_excel("data/20221116_excel_example.xlsx") # read example
            # ncol
            if(ncol(forms_spreadsheet) == ncol(example_forms_spreadsheet)){
                message("`forms_spreadsheet` ncol() is acceptable...")
            } else if(ncol(forms_spreadsheet) != ncol(example_forms_spreadsheet)){
                message("The number of columns in the `forms_spreadsheet` you provided does not match the required format. Re-format your input and try `loadData(forms_spreadsheet)` again.")
                rm(example_forms_spreadsheet)
                break
            }
            # colnames
            if(nrow(setdiff(example_forms_spreadsheet, forms_spreadsheet))==0){
                message("`forms_spreadsheet` colnames are acceptable...")
            } else if(nrow(setdiff(example_forms_spreadsheet, forms_spreadsheet))!=0){
                message("The column names in the `forms_spreadsheet` that you provided do not match the required format. Re-format your input and try `loadData(forms_spreadsheet)` again.")
                rm(example_forms_spreadsheet)
                break
            }
            # blank $`Reference Type`
            blanks <- forms_spreadsheet %>%
                subset(is.na(`Reference Type`) == TRUE)
            if(nrow(blanks)>0){
                for (i in 1:nrow(blanks)){
                    message(paste("Row", i, "in your `forms_spreadsheet` contains NA in the $`Reference Type` column. $`Reference Type` cannot be NA for a valid input. Correct your input and try `loadData(forms_spreadsheet)` again."))
                }
                rm(blanks)
                rm(example_forms_spreadsheet)
                break
            }
            # validateAuthors()
        },
        finally = {
            message("\nData passed validation and loaded successfully to global environment as `forms_spreadsheet`...\n") # message indicating the function job completed
        }
    )
}

# teststring <- "test.csv"
# file_ext(teststring)
# tools::file_ext(teststring)
