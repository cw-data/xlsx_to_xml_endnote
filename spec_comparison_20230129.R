library(janitor)
library(readxl)
# compare the old spec to the new spec

new_spec <- readxl::read_excel("data/20230129/Forms_output_for_comparison.xlsx") # new spec
old_spec <- readxl::read_excel("data/20221116_excel_example.xlsx") # old spec


inconsistencies <- janitor::compare_df_cols(old_spec, new_spec) %>% 
    subset(is.na(old_spec) | is.na(new_spec))
