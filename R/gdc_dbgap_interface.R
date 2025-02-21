#' Query GDC/dbGaP to form an HTML summary
#'
#' Using the supplied Genomic Data Commons (GDC) project ID, retrieves information about
#' the dataset including general information, access, authors, composition and basic
#' information on ethical, legal and social aspects to form an 'Aggregator' Persona BioMedical
#' Data Manifest HTML file.  NOTE: This functionality is very fragile since the information
#' it attempts to gather is not consistently well structured.
#' 
#' @param project.id A single string value describing a dataset in GDC.
#' @param output.file Name of the resulting HTML file.  By default it is based off of the project ID.
#' @param format.file A CSV file that overrides the use of the formatting table 
#' bundled with the package.
#' @return Returns the path to the generated HTML file.
#' @export
#' @rdname gdc_dbgap
#' @examples
#' \dontrun{manifest_from_gdc("TARGET-AML")}
manifest_from_gdc <- function(project.id, output.file=NULL, format.file=NULL){
    
    if (missing(format.file) || is.null(format.file) || all(is.na(output.file))){
        
        format.file <- system.file("extdata/bio_data_manif_formatting_w_persona.csv", package="BioDataManifest")
    }
    
    cur.format <- readr::read_delim(format.file, show_col_types = FALSE)
    
    #not sure how to set this...
    httr::set_config(httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"))
    
    split.data <- list()
    
    split.data$IsHuRes <- TRUE
    
    message("Checking for GDC Summary page...")
    
    summary.page <- tryCatch(read_html(paste0("https://gdc.cancer.gov/content/",tolower(project.id),"-publication-summary")), error=function(x) NA)
    
    if (all(is.na(summary.page))==F){
        
        message("Found, parsing information")
        
        summary.text <- summary.page |> 
            html_elements(xpath="//h2[text() = 'Program Description']/following-sibling::p") |>
            html_text2()
        
        split.data$DSummary <- paste(paste0("<p>", summary.text, "</p>"), collapse="")
        
        split.data$DName <- summary.page |>
            html_elements("title") |>
            html_text2() |>
            strsplit("\\-|\\|") |>
            {\(x) sub("^ ", "", sub(" $", "", x[[1]][2]))}()
        
        split.data$DName <- paste0(split.data$DName, " (", project.id, ")")
        
        # summary.links <- summary.page |> 
        #     html_elements("div.publication-list ~ p a") |>
        #     as.character()
        # 
        # split.data$DWebsite <- summary.links
        
        split.data$DShortName <- project.id
        
    }else{
        
        message("Not Found, adding placeholders")
        split.data$DSummary <- "GDC Summary page not available"
        split.data$DName <- paste("GDC Project ID:", project.id)
        #split.data$DWebsite <- ""
        split.data$DShortName <- project.id
    }
    
    message("Retrieving data on project...")
    
    #initial query
    pquery = projects() |>
        filter(.data$project_id == project.id) |>
        select(available_fields('projects')) |>
        results()
    
    message("Retrieving dbgap page...")
    
    #setdiff(names(split.data), cur.format$`Variable Name`)
    
    dbgap.page <- read_html(paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=", pquery$program$dbgap_accession_number))
    
    #if substudies are present, try to choose the relevant one
    #note this example works for TARGET, probably not in other contexts
    
    substudies <- dbgap.page |>
        html_elements("dl.report table tr") |>
        html_text2()
    substudies <- grep("^phs", substudies, value=T)
    
    best.substudy <- grep(tolower(pquery$name), tolower(substudies))
    
    if (length(best.substudy) == 1){
        dbgap.id <- strsplit(substudies[best.substudy], "\t")[[1]][1]
        
        message("Found relevant substudy, re-retrieving...")
        
        #re-retrieve the dbgap page if needed
        dbgap.page <- read_html(paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=", dbgap.id))
        
    }else{
        
        dbgap.id <- dbgap.page |>
            html_elements("#study-id") |>
            html_text2()
    }
    
    message("Parsing info from dbGaP...")
    
    #point to dbgap and gdc instructions
    raw.data.links <- c(
        paste0('<a href="https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=', dbgap.id, '">dbGaP</a>'),
        '<a href="https://gdc.cancer.gov/access-data/obtaining-access-controlled-data">Genomic Data Commons</a>'
    )
    
    processed.data.links <- paste0('<a href="https://portal.gdc.cancer.gov/projects/', project.id, '">Genomic Data Commons</a>')
    
    split.data$DataLinks <- paste0(
        "<ul>",
        "<li><strong>Raw:</strong>",
            "<ul>",
                paste(paste0("<li>", raw.data.links, "</li>"), collapse=""),
            "</ul></li>",
        "<li><strong>Processed:</strong>",
            "<ul>",
                paste(paste0("<li>", processed.data.links, "</li>"), collapse=""),
            "</ul></li>",
        "</ul>"
    )
    
    split.data$DDocLinks <- 'For GDC specific workflows see this <a href="https://docs.gdc.cancer.gov/Data/Introduction/">link</a>, otherwise accompanying dataset manuscripts'
    
    split.data$DSAuthors <- "Data Manifest aggregated from GDC"
    
    #GDC version info and release date
    
    release.info <- status()
    
    split.release <- strsplit(release.info$data_release, "\\s\\-\\s")[[1]]
    
    split.data$DVersion <- split.release[1]
    
    split.data$RelDate <- split.release[2]
    
    split.data$Keywords <- setdiff(unlist(pquery[c("primary_site", "disease_type")]), c("Unknown", "Not Applicable"))
    
    split.data$EMechanisms <- "Contact Dataset Owners/Publishers for ways to contribute"
    
    split.data$DContact <- dbgap.page |>
        html_elements(xpath="//dt[text() = 'Study Attribution']/following-sibling::dd/ul/li") |>
        html_text2() |>
        tibble::as_tibble() |>
        tidyr::separate_wider_delim(cols=value, delim="\n", names=c("header", "value")) |>
        _$value
    
    dbgap.restricts <- read_html(paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetRestrictedAccess.cgi?study_id=", dbgap.id))
    
    #check for a redirect
    
    is.redirect <- dbgap.restricts |>
        html_elements("div#restricted-access-body font") |>
        html_text2()
    
    if (length(is.redirect) > 0){
        redir.id <- dbgap.restricts |>
            html_elements("div#restricted-access-body a") |>
            html_text2() |>
            grep("phs", x=_, value=TRUE)
        
        dbgap.restricts <- read_html(paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetRestrictedAccess.cgi?study_id=",  redir.id ))
    }
        
    tmp.uns <- dbgap.restricts |>
        html_elements("table") |>
        html_table()
    
     uns.table <- tmp.uns[[1]] |>
        tidyr::separate_wider_delim(cols=X2, delim=stringr::regex("\\n\\s+\\t+"), names=c("header", "value")) |>
        dplyr::select(header, value) 
     
    split.data$DUnsuitable <- paste0("<strong>", uns.table$header, ":</strong> ", gsub("\\n\\n", "\n", uns.table$value))
    
    dbgap.attrs <- read_html(paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetStudyAttribution.cgi?study_id=", dbgap.id)) |>
        html_elements("p") |>
        html_text2()
    
    cite.tbl <- tibble::tribble(
        ~header,~value,
        "Use of data from dbGaP:", sub("Acknowledgement Statement:", "", strsplit(dbgap.attrs[1], "\\.")[[1]][1]),
        "Acknowledgement Statement:", dbgap.attrs[2]
    )
    
    split.data$ReqCite <- nrow(cite.tbl) > 0
    
    split.data$ReqCiteLinks <- paste0("<strong>", cite.tbl$header, "</strong> ", cite.tbl$value)
    
    split.data$InstanceDef <- "Each experimental unit refers to a patient sample"
    split.data$InstanceRels <- "Each patient (Case) can have multiple experimental units corresponding to for example different timepoints or tissue sources etc."
    
    message("Retrieving File info from GDC...")
    
    file.info <- files() |>
        filter( cases.project.project_id == project.id) |>
        select(c("file_size", "data_format","data_category", "access", "cases.case_id")) |>
        expand(c("cases.samples" )) |>
        results_all() |>
        tibble::as_tibble()
    
    file.summary <- dplyr::group_by(file.info, access) |>
        dplyr::summarize(`Size of Dataset`=.choose.units(sum(.data$file_size)),
                         `Number of Cases`=scales::comma(length(unique(unlist(lapply(cases, function(x) x$case_id))))),
                         `Number of Files`=scales::comma(dplyr::n()),
                         `File Types`=paste(unique(.data$data_format), collapse=",")
        ) |>
        tidyr::pivot_longer(cols=-access, names_to = "header")
    
    split.data$OpenInstanceTable <- dplyr::filter(file.summary, .data$access=="open")
    
    split.data$OpenInstTableCaption <- "Overall statistics of the open dataset."
    
    split.data$ClosedInstanceTable <- dplyr::filter(file.summary, .data$access=="controlled")
    
    split.data$ClosedInstTableCaption <- "Overall statistics of the controlled dataset."
    
    split.data$Inclusion <- dbgap.page |>
        html_elements(xpath="//dt[text()='Study Inclusion/Exclusion Criteria']/following-sibling::dd[1]") |>
        html_text2()
    
    split.data$Modalities <- pquery$summary$data_categories[[1]]$data_category
    
    split.data$MissingInfo <- c("Clinical data due to lack of access to records or curation, missing typically represented by blank, NA, Unknown entries etc",
                                "Not all genomics data is captured for each patient sample",
                                "Additionally, all genomic assays may have regions below LOD/background")
    
    samp.info <- unnest.expands(file.info, expand_col="cases.samples")
    
    samp.info <- dplyr::select(samp.info, file_id=id, sample_submitter_id=submitter_id, tumor_code_id, sample_id, tumor_descriptor, sample_type, tissue_type )
    
    files.to.samples <- dplyr::inner_join(samp.info, file.info, by=c("file_id"))
    
    sample.type.tmat <- dplyr::select(files.to.samples, sample_type, sample_id, data_category, access) |>
        unique() |>
        dplyr::group_by(access, sample_type, data_category) |>
        dplyr::summarize(n=dplyr::n(), .groups="drop") |>
        tidyr::pivot_wider( id_cols=c("access", "sample_type"), names_from=data_category, values_from=n, values_fill=0)
    
    open.tmat <- dplyr::filter(sample.type.tmat, .data$access=="open")
    
    open.dt.table <- .process.datatype.table(
        open.tmat 
    )
    
    split.data$InstModTableCaption <- "Number of instances per sample type and modality for open access data"
    
    split.data$InstModTableHeader <- c("Instance Type", names(open.dt.table[[1]]))
    
    split.data$InstModTable <- tibble::tibble(header=open.tmat$sample_type, value=open.dt.table)
    
    closed.tmat <- dplyr::filter(sample.type.tmat, .data$access=="controlled")
    
    closed.dt.table <- .process.datatype.table(
        closed.tmat 
    )
    
    split.data$ClosedInstModTableCaption <- "Number of instances per sample type and modality for controlled access data"
    
    split.data$ClosedInstModTableHeader <- c("Instance Type", names(closed.dt.table[[1]]))
    
    split.data$ClosedInstModTable <- tibble::tibble(header=closed.tmat$sample_type, value=closed.dt.table)
    
    split.data$InstModTableAddtl <- "Not all these instances are used in a given analyses. See respective publications or other details in this document for information"
    
    split.data$RBApproval <- paste0("Generally, for these studies all patients have provided informed consent but see <a href='", paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=", dbgap.id), "'>dbGaP</a> for more details")
    
    #To simplify the resulting file and not populating information that isn't readily
    #available, set to false
    
    split.data$RBDidApprove <- FALSE
    
    message("Retrieving Case info...")
    
    case.tbl <- cases() |>
        filter( project.project_id == project.id) |>
        select(c("samples.specimen_type", "disease_type")) |>
        expand(c("demographic", "diagnoses")) |>
        results_all() |>
        tibble::as_tibble()
    
    #Instance x Diagnosis table
    
    case.samp.type <- unnest.expands(case.tbl, expand_col="samples.specimen_type") |>
        dplyr::rename(specimen_type="value")
    
    case.samps <- dplyr::inner_join(dplyr::select(case.tbl, id, disease_type), case.samp.type, by="id")
    
    type.disease.tmat <- dplyr::select(case.samps, sample_type=specimen_type, disease_type) |>
        dplyr::group_by(sample_type, disease_type) |>
        dplyr::summarize(n=dplyr::n(), .groups="drop") |>
        tidyr::pivot_wider( id_cols=c("sample_type"), names_from=disease_type, values_from=n, values_fill=0)
    
    inst.diag.table <- .process.table(type.disease.tmat)
    
    split.data$InstDiagTableCaption <- "Number of instances per sample type and diagnosis"
    
    split.data$InstDiagTableHeader <- c("Instance Type", names(inst.diag.table[[1]]))
    
    split.data$InstDiagTable <- tibble::tibble(header=type.disease.tmat$sample_type, value=inst.diag.table)
    
    #Primary diagnosis x Diagnosis table
    
    case.prim.diag <- unnest.expands(case.tbl, expand_col="diagnoses.primary_diagnosis") |>
        dplyr::rename(primary_diagnosis="value")
    
    case.samps <- dplyr::inner_join(case.samps, case.prim.diag, by="id")
    
    prim.disease.tmat <- dplyr::select(case.samps, primary_diagnosis, disease_type) |>
        dplyr::group_by(primary_diagnosis, disease_type) |>
        dplyr::summarize(n=dplyr::n(), .groups="drop") |>
        tidyr::pivot_wider( id_cols=c("primary_diagnosis"), names_from=disease_type, values_from=n, values_fill=0)
    
    prim.disease.table <- .process.table(prim.disease.tmat)
    
    split.data$SpecDiagTableCaption <- "Number of instances per specific diagnosis and diagnosis"
    
    split.data$SpecDiagTableHeader <- c("Primary Diagnosis", names(prim.disease.table[[1]]))
    
    split.data$SpecDiagTable <- tibble::tibble(header=prim.disease.tmat$primary_diagnosis, value=prim.disease.table)
    
    message("Retrieving Clinical info...")
    
    clin.list <- gdc_clinical(case.tbl$case_id) |> 
        suppressWarnings()
    
    #Clinical vars x Diagnosis table
    
    clin.list$main <- dplyr::mutate(clin.list$main, disease_type_fac=factor(disease_type))
    
    clin.disease.tmat <- lapply(clin.list[setdiff(names(clin.list), "main")], function(x){
        
        if (nrow(x) > 0){
            na.cols <- sapply(names(x), function(x.name) all(is.na(x[[x.name]])))
            
            non.empty.x <- dplyr::inner_join(x[,na.cols==F], dplyr::select(clin.list$main, case_id, disease_type=disease_type_fac), by="case_id")
            
            x.summary <- dplyr::bind_cols(disease_type=non.empty.x$disease_type, 
                                          tibble::as_tibble(is.na(dplyr::select(non.empty.x, -disease_type)))
            ) |>
                tidyr::pivot_longer(cols=-disease_type) |>
                dplyr::summarize(n=sum(value==F), .by=c("disease_type", "name"))
            
            dplyr::filter(x.summary, grepl("_id", name) ==F & grepl("_datetime", name) == F) |>
                tidyr::pivot_wider(id_cols=name, names_from=disease_type, values_from=n, names_expand=T,
                                   values_fill=0)
        }else{
            NULL
        }
        
    }) |> dplyr::bind_rows()
    
    clin.disease.table <- .process.table(clin.disease.tmat, remove.empty=F)
    
    split.data$ClinDiagTableCaption <- "Number of cases per clinical variable and diagnosis"
    
    split.data$ClinDiagTableHeader <- c("Clinical Variable",names(clin.disease.table[[1]]))
    
    split.data$ClinDiagTable <- tibble::tibble(header=clin.disease.tmat$name, value=clin.disease.table)
    
    split.data$CSensTable <- tibble::tribble(
        ~header, ~value, ~table, ~has_figure,
        "gender", field_description('cases', 'demographic.gender'), 'demographic', T,
        "race", field_description('cases', 'demographic.race'), 'demographic', T,
        "ethnicity", field_description('cases', 'demographic.ethnicity'),'demographic', T,
        "age_at_diagnosis", field_description('cases', 'diagnoses.age_at_diagnosis'), 'diagnoses', T,
        "submitter_id", "All submitted IDs can potentially contain identifiable information",'none', F
    )
    
    #generate figures
    
    case.tbl$diagnoses <- dplyr::bind_rows(lapply(case.tbl$diagnoses, function(x){
        if (is.null(x)==F){
            dplyr::select(tibble::as_tibble(x), age_at_diagnosis)
        }else{
            tibble::tibble(age_at_diagnosis=NA_real_)
        }
        
    }))
    
    plot.locs <- sapply(1:nrow(split.data$CSensTable), function(x){
        
        tmp.tbl <- split.data$CSensTable[x,]
        
        if (tmp.tbl$has_figure){
            
            knitr::image_uri(plot.fun(tibble::as_tibble(case.tbl[[tmp.tbl$table]]), split.data$CSensTable$header[x]))
            
        }else{
            ""
        }
    })
    
    split.data$CSensTable$value <- mapply(function(x, y){
        
        c(x, y)
        
    }, split.data$CSensTable$value, plot.locs, SIMPLIFY=F)
    
    split.data$CSensTable$table <- NULL
    split.data$CSensTable$has_figure <- NULL
    
    split.data$IPTerms <- paste0('Raw data available from the <a href="https://gdc.cancer.gov/about-gdc/gdc-policies">Genomic Data Commons</a> with access granted through <a href="',
                                 paste0("https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=", dbgap.id), '">dbGaP</a> and adherence to their <a href="',
                                 paste0("https://dbgap.ncbi.nlm.nih.gov/aa/wga.cgi?view_pdf&stacc=", dbgap.id), '">policies</a>')
    
    split.data$DConf <- "Usage of Open Access data has been deemed safe for general usage with minimal risk to patients. Restricted Access data could be used to potentially re-identify a given patient and so needs special requirements to access."
    
    split.data$PreprocWorkflow <- c(
        paste0('<strong>Pre-processing:</strong> Generation and processing of raw data is described in detail in existing <a href="https://gdc.cancer.gov/content/',tolower(project.id),'-publication-summary">publications</a>'),
        paste0('<strong>Post-processing and derived features:</strong> Generation of processed data by the Genomic Data Commons is described <a href="https://gdc.cancer.gov/about-data/gdc-data-processing">here</a>')
    )
    
    #need a more robust way of doing this
    table.vars <- grep("Table$", names(split.data), value=TRUE)
    
    split.data[table.vars] <- lapply(split.data[table.vars], function(x){
        if("value" %in% names(x) && is.list(x$value)){
            x$value <- lapply(x$value, function(y){
                if (length(y) == 1){
                    I(y)
                }else{
                    y
                }
            })
            
        }
        
        x
    })
    
    #Finally deal with remaining optional sections
    
    split.data$HasCWarning <- FALSE
    split.data$FundingInfoAvail <- FALSE
    split.data$AnnotLabelAvail <- FALSE
    split.data$HasDAudit <- FALSE
    split.data$MaintainAvail <- FALSE
    
    split.data$DSuitUnsuitAvail <- TRUE
    split.data$PopulateSummaryTables <- TRUE
    split.data$SensAttrsHasFigures <- TRUE
    split.data$HasLicense <- TRUE
    
    split.data$InstTableAddtl <- NA_character_
    
    missing.vars <- setdiff(cur.format$`Variable Name`, names(split.data))
    
    na.split.data <- lapply(setNames(missing.vars, missing.vars), function(x){
        NA_character_
    })
    
    #add in the short descriptions
    data.desc <- lapply(setNames(cur.format$ShortName, paste0(cur.format$`Variable Name`, "Desc")), function(x){
        x
    })
    
    split.data$persona_type <- "Aggregator"
    
    split.data.w.names <- c(data.desc, split.data, na.split.data)
    
    optional.vals <- "InstTableAddtl"
    
    list.vals <- dplyr::filter(cur.format,  `Variable Type` == "list") |>
        _$`Variable Name` |>
        intersect(names(split.data.w.names))
    
    split.data.proc <- .filter.variable.list(split.data.w.names, optional.vals, list.vals)
    
    tmp.rend <- do.call(render, append(list(fs::path(system.file("extdata/bio_data_manif_tmpl.html", package="BioDataManifest")),
                                            .config=jinjar_config(loader = package_loader("BioDataManifest", "extdata"))), 
                                       split.data.proc))
    
    if (missing(output.file) || is.null(output.file) || all(is.na(output.file))){
        output.file <- paste0(project.id, "-biomedical_data_manifest.html")
    }
    
    cat(tmp.rend, file=output.file)
    
    output.file
}