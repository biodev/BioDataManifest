#' @import GenomicDataCommons
#' @import ggplot2
#' @importFrom stringr regex
#' @importFrom dplyr group_by summarize n inner_join rename mutate bind_cols bind_rows all_of everything
#' @importFrom tibble is_tibble as_tibble tibble tribble
#' @importFrom rvest html_elements html_text2 read_html html_table
#' @importFrom jinjar render jinjar_config package_loader
#' @importFrom rlang .data
#' @importFrom readxl read_excel
#' @importFrom stats na.omit setNames
#' @importFrom knitr knit2html
#' @importFrom utils globalVariables
NULL

#simply to remove NOTES upon R CMD check
utils::globalVariables(c("access", "sample_type", "value", "Description", "ShortName",
                         "Variable Name", "Persona", "ParseMarkdown", "Variable Type",
                         "X2", "header", "cases.project.project_id", "access", "id",
                         "submitter_id", "tumor_code_id", "sample_id", "tumor_descriptor",
                         "sample_type", "tissue_type", "data_category", "project.project_id",
                         "disease_type", "specimen_type", "primary_diagnosis", "case_id",
                         "disease_type_fac", "disease_type", "name", "age_at_diagnosis", 
                         "Variable Type"))
