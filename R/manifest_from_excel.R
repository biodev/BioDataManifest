
#' Generate an HTML summary from a survey
#'
#' Given an Excel output from a survey tool such as Microsoft Forms populate
#' a BioMedical Data Manifest HTML template for a given Persona.  Changing
#' Personas removes questions/answers according to Bottomly et al In Prep.
#' 
#' @param excel.file An Excel file containing survey results
#' @param persona The type of Persona to use, one of 'DM/Comp' or 'Bench/Clinical'
#' @param output.file Name of the HTML file to output.  Defaults to 
#' 'biomedical_data_manifest.html'
#' @param format.file A CSV file that overrides the use of the formatting table 
#' bundled with the package.  
#' @return "The path to the generated HTML file"
#' @export
#' @examples
#' \dontrun{
#' example.xl <- system.file("extdata", "Biomedical Data Manifest Template Example.xlsx", package = "BioDataManifest")
#' manifest_from_excel(example.xl)
#' }
manifest_from_excel <- function(excel.file, persona=c("DM/Comp", "Bench/Clinical"), output.file=NULL, format.file=NULL){
    
    if (missing(format.file) || is.null(format.file) || all(is.na(output.file))){
        cur.format <- readr::read_delim(system.file("extdata/bio_data_manif_formatting_w_persona.csv", package="BioDataManifest"), show_col_types = FALSE)
    }
    
    if (missing(output.file) || is.null(output.file) || all(is.na(output.file))){
        output.file <- "biomedical_data_manifest.html"
    }
    
    persona <- match.arg(persona)
    
    persona.map <- c(`DM/Comp`="Data Managers/Computationalists", `Bench/Clinical`="Bench or Clinical Researchers")
    
    #expects the Excel file to be in the 'wide' format as output by Microsoft Forms 
    init.data <- readxl::read_excel(excel.file)
    
    #in case the names have spaces from the Form/survey creations
    names(init.data) <- sub("\\r*\\n", "", names(init.data))
    
    if (nrow(init.data) != 1){
        stop("ERROR: Currently we only support a single entry survey at a time")
    }
    
    init.data <- init.data[,-c(1:6)]
    
    long.data <- tidyr::pivot_longer(init.data, 
                                     cols=everything(),
                                     values_transform=as.character,
                                     names_to="Description")
    
    #remove '\r's if present
    
    long.data <- dplyr::mutate(long.data,
                               value = gsub("\\r", "", value))
    
    missing.descriptions <- setdiff(long.data$Description, cur.format$Description)
    
    #don't include descriptions that are pivots or alternatively processed
    
    missing.descriptions <- setdiff(missing.descriptions, 
                                    c("Require citation? (to use the data)",
                                      "Is there a data license or data use agreement needed for this data?",
                                      "What sensitive human attributes are present?", 
                                      "Were labels attributed to raw data such as images or text by expert curation, semi-automated annotation etc?",
                                      "Is the dataset maintained?", 
                                      "Is this human subjects research?",
                                      "Dataset owner",
                                      "Were labels attributed to raw data such as images or text by expert curation, semi-automated annotation etc for use in predictive models?"))
    
    if (length(missing.descriptions) > 0){
        stop(paste("ERROR: Mismatch between template and input excel file:", paste(missing.descriptions, collapse=",")))
    }
    
    
    tmp.pub <- dplyr::filter(long.data, Description %in% c("Dataset publisher(s)", "Dataset owner"))
    tmp.pub.val <- strsplit(na.omit(tmp.pub$value), "\\n") |>
        unlist() |>
        paste(collapse="\n")
    
    long.data <- dplyr::mutate(long.data, 
                               value = ifelse(Description == "Dataset publisher(s)", tmp.pub.val, value))
    
    merged.data <- dplyr::inner_join(long.data, 
                                     dplyr::select(cur.format, Description, ShortName, `Variable Name`, Persona), 
                                     by="Description")
    
    #add in the short descriptions
    
    data.desc <- lapply(setNames(merged.data$ShortName, paste0(merged.data$`Variable Name`, "Desc")), function(x){
        x
    })
    
    #remove entries depending on persona
    
    merged.data <- dplyr::mutate(merged.data, 
                          value = dplyr::case_when(
                              (Persona == persona) | (Persona == "Both") ~ value,
                              .default = NA_character_
                          ))
    
    merged.data <- dplyr::group_by(merged.data, `Variable Name`)
    
    #this is the main data structure a list keyed by the variable names
    split.data <- lapply(dplyr::group_split(merged.data), function(x) x$value)
    names(split.data) <- dplyr::group_keys(merged.data)$`Variable Name`
    
    split.data <- c(data.desc, split.data)
    
    split.data$persona_type <- persona.map[persona] |> unname()
    
    #the short name goes to the page title, fill in suitable default if it 
    #can't be found
    split.data$DShortName <- stringr::str_match(split.data$DName, "\\((.+)\\)")[,2]
    
    if (all(is.na(split.data$DShortName))){
        split.data$DShortName <- "Generated"
    }
    
    split.data$HasCWarning <- length(split.data$CWarning) > 0 && split.data$CWarning %in% c("", "No", "no") == FALSE
    
    split.data$HasDAudit <- length(split.data$DAudit) > 0 && split.data$DAudit %in% c("", "No", "no") == FALSE
    
    split.data$FundingInfoAvail <- (is.na(split.data$FundingSummaries) == FALSE) && split.data$FundingSummaries != ""
    
    split.data$DSuitUnsuitAvail <-  (is.na(split.data$DSuitable) == FALSE) || 
        (is.na(split.data$DUnsuitable) == FALSE)
    
    #if below is Yes, then add the citation section
    split.data$ReqCite <- dplyr::filter(long.data, Description == c("Require citation? (to use the data)")) |>
        _$value %in% c("Yes")
    
    #this may be an artifact of the multiple choice selecting procedure
    split.data$SamplingInfo <- sub(";$", "", split.data$SamplingInfo)
    
    #These tables are currently only used in the aggregate version
    split.data$PopulateSummaryTables <- FALSE
    
    #Is this human subjects research?
    
    split.data$IsHuRes <- dplyr::filter(long.data, Description == "Is this human subjects research?") |>
        _$value == "Yes"
    
    
    #What sensitive human attributes are present?--make this into table for CSensTable
    ##may make sense to instead have this be Markdown format
    
    sens.attrs <- dplyr::filter(long.data, Description == "What sensitive human attributes are present?") |>
        _$value
    
    #This version doesn't have figures so will harcode this here
    split.data$SensAttrsHasFigures <- FALSE
    
    sens.attr.tbl <- readr::read_delim(file=sens.attrs, 
                                      col_names=FALSE, progress=FALSE,
                                      show_col_types = FALSE,
                                      delim=":")
    
    names(sens.attr.tbl) <- c("header", "value")
    
    split.data$CSensTable <- sens.attr.tbl
    
    
    #if below is Yes, then add the LICENSING section below
    
    split.data$HasLicense <- dplyr::filter(long.data, Description == "Is there a data license or data use agreement needed for this data?") |>
        _$value == "Yes"
    
    split.data$RBDidApprove <- split.data$RBApproval == "Yes"
    split.data$WereConsented <- split.data$Consent == "Yes"
    
    #Only show the labeling section if below is true
    #Were labels attributed to raw data such as images or text by expert curation, semi-automated annotation etc?
    
    split.data$AnnotLabelAvail <- dplyr::filter(long.data, Description == "Were labels attributed to raw data such as images or text by expert curation, semi-automated annotation etc for use in predictive models?") |>
        _$value == "Yes"
    
    #Only show maintenance section if below is true
    
    split.data$MaintainAvail <- dplyr::filter(long.data, Description == "Is the dataset maintained?") |>
        _$value == "Yes"
    
    #Also parse markdown for below
    
    markdown.vars <- dplyr::filter(cur.format, ParseMarkdown == "y") |>
        _$`Variable Name`
    
    for(i in markdown.vars){
        if (all(is.na(split.data[[i]]))==FALSE){
            split.data[[i]] <- knitr::knit2html(text=split.data[[i]], template=F)
        }
    }
    
    optional.vals <- dplyr::filter(cur.format, Persona %in% c("Both", persona) == F) |> 
        _$`Variable Name`
    
    list.vals <- dplyr::filter(cur.format,  `Variable Type` == "list") |>
        _$`Variable Name` |>
        intersect(names(split.data))
    
    split.data.proc <- .filter.variable.list(split.data, optional.vals, list.vals)
    
    tmp.rend <- do.call(render, append(list(fs::path(system.file("extdata/bio_data_manif_tmpl.html", package="BioDataManifest")),
                                            .config=jinjar_config(loader = package_loader("BioDataManifest", "extdata"))), 
                                       split.data.proc))
    
    cat(tmp.rend, file=output.file)
    
    output.file
}