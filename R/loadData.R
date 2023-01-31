#----- load external libraries
library(xml2)
library(dplyr)
#----- load project functions
source("R/validateAuthors.R")

loadData <- function(forms_spreadsheet){
    tryCatch(
        expr = {
            # validate the user-provided `forms_spreadsheet` is a spreadsheet
            if(tools::file_ext(forms_spreadsheet) == "xlsx"){
                forms_spreadsheet <- readxl::read_excel(forms_spreadsheet)
                # assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
                message("`forms_spreadsheet` is xlsx...")
            } else if(tools::file_ext(forms_spreadsheet) == "xls"){
                forms_spreadsheet <- readxl::read_excel(forms_spreadsheet)
                # assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
                message("`forms_spreadsheet` is xls...")
            } else if(tools::file_ext(forms_spreadsheet) == "csv"){
                forms_spreadsheet <- data.table::fread(forms_spreadsheet)
                # assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
                message("`forms_spreadsheet` is csv...")
                # print(tools::file_ext(forms_spreadsheet))
            } else {
                message("`loadData()` requires argument `forms_spreadsheet` to be one of these filetypes: 'xlsx', 'xls', 'csv'. The `forms_spreadsheet` that you provided is not one of the acceptable filetypes. Input a `forms_spreadsheet`with an acceptable filetype and try `loadData(forms_spreadsheet)` again.")
                break
            }
            # validate the user-provided `forms_spreadsheet` matches known colnames
            # we'll compare user input against an example that has the correct column format
            # check for things that we know are wrong and would produce errors later in the script
            # example_forms_spreadsheet <- readxl::read_excel("data/20221116_excel_example.xlsx") # read example (old_spec)
            example_forms_spreadsheet <- readxl::read_excel("resources/forms_example_20230130.xlsx") # read example (new_spec)
            # check that ncol is legit
            if(ncol(forms_spreadsheet) == ncol(example_forms_spreadsheet)){ # if the number of columns is correct
                message("`forms_spreadsheet` ncol() is acceptable...") # print success message
            } else if(ncol(forms_spreadsheet) != ncol(example_forms_spreadsheet)){ # if number of columns is incorrect
                message("The number of columns in the `forms_spreadsheet` you provided does not match the required format. Re-format your input and try `loadData(forms_spreadsheet)` again.")
                # take out trash
                rm(example_forms_spreadsheet)
                break # end the program
            }
            # check that colnames are legit
            if(nrow(setdiff(example_forms_spreadsheet, forms_spreadsheet))==0){ # if there aren't any mismatched columns
                message("`forms_spreadsheet` colnames are acceptable...") # print success message
            } else if(nrow(setdiff(example_forms_spreadsheet, forms_spreadsheet))!=0){ # if there are mismatched columns
                message("The column names in the `forms_spreadsheet` that you provided do not match the required format. Re-format your input and try `loadData(forms_spreadsheet)` again.")
                # take out trash
                rm(example_forms_spreadsheet)
                break # end the program
            }
            # check for blank $`Reference Type`
            blanks <- forms_spreadsheet %>%
                subset(is.na(`Reference Type`) == TRUE)
            if(nrow(blanks)>0){
                for (i in 1:nrow(blanks)){
                    message(paste("Row", i, "in your `forms_spreadsheet` contains NA in the $`Reference Type` column. $`Reference Type` cannot be NA for a valid input. Correct your input and try `loadData(forms_spreadsheet)` again."))
                }
                # take out trash
                rm(blanks)
                rm(example_forms_spreadsheet)
                break # end the program
            }
            # check that ref-types are legit
            # ref_type_lookup <- readxl::read_excel("resources/endnote_ref-type_dictionary.xlsx") # a lookup table of reference types (old_spec)
            ref_type_lookup <- readxl::read_excel("resources/endnote_ref-type_dictionary_20230130.xlsx") # a lookup table of reference types (new_spec)
            ref_type_lookup$value <- stringr::str_extract(ref_type_lookup$XML, "([0-9]+)") # extract the value from inside the xml tags
            forms_spreadsheet <- dplyr::left_join(forms_spreadsheet, # add the $value column to `forms_spreadsheet`
                                                  ref_type_lookup %>% select("Reference type (from Form)", "value"),
                                                  by = c("Reference Type" = "Reference type (from Form)"))
            weird_refs <- subset(forms_spreadsheet, !(`Reference Type` %in% ref_type_lookup$`Reference type (from Form)`)) # find ref-types that aren't in the lookup table
            if(nrow(weird_refs)==0){ # if all of the unique values in forms_spreadsheet$`Reference Type` are in `ref_type_lookup$`Reference type (from Form)``
                message("`forms_spreadsheet` reference types are acceptable...") # print success message
            } else if(nrow(weird_refs)>0){ # if there are any ref-types that aren't in the lookup table
                for (i in 1:nrow(weird_refs)){ # print the weird refs so the user can do something about them
                    message(paste("Row", i, "in your `forms_spreadsheet` contains an unacceptable $`Reference Type`. Correct $`Reference Type` at row", i, "or add the reference type to `resources/endnote_ref-type_dictionary.xlsx` and add a module to the `R` directory of this project to accomodate for this new reference type."))
                }
                # take out trash
                rm(weird_refs)
                rm(blanks)
                rm(example_forms_spreadsheet)
                break # end the program
            }
            # take out trash
            rm(weird_refs)
            rm(blanks)
            rm(example_forms_spreadsheet)
            # success message
            message("`forms_spreadsheet` validated...loaded to global environment as `forms_spreadsheet`...\n\nParsing and preparing `record_list`...")
            
            # move on to author parsing and validation
            # pass the validated `forms_spreadsheet` to validateAuthors()
            validateAuthors(forms_spreadsheet = forms_spreadsheet, # pass params to validateAuthors()
                            ref_type_lookup = ref_type_lookup)
            assign("forms_spreadsheet", forms_spreadsheet, envir = globalenv())
            # assign("record_list", record_list, envir = globalenv())
        },
        finally = {
            message("Ready to build xml!\n\n") # message indicating the function job completed
        }
    )
}

