library(janitor)
library(readxl)
library(gt)
# compare the old spec to the new spec

new_spec <- readxl::read_excel("data/20230129/Forms_output_for_comparison.xlsx") # new spec
old_spec <- readxl::read_excel("data/20221116_excel_example.xlsx") # old spec

inconsistencies <- janitor::compare_df_cols(old_spec, new_spec) %>% 
    subset(is.na(old_spec) | is.na(new_spec))

inconsistencies %>% 
    gt() %>%
    cols_label(
        column_name = "Column name",
        old_spec = "Old spec",
        new_spec = "New spec"
    ) %>%
    tab_footnote("NA indicates the column name is not present in df 'Old spec' or 'New spec'")
