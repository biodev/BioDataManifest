
#' Retrieve available project names
#'
#' Retrieve a character vector of all available project names from the GDC.  These
#' can be use with the `gdc_datashard` function
#' 
#' @return character vector of project ids
#' @rdname utils
avail_projects <- function(){
    
    avail.projs <- projects() |>
        select("project_id") |>
        results_all()
    
    avail.projs$project_id
}


#' Un-nest list elements
#'
#' Especially when using `GenomicDataCommons::expands` tibbles are returned that
#' have columns with lists containing nested tibbles.  This function expands those
#' to standard tibbles for easy merging with the original.
#' 
#' @param tbl The input tibble with a nested list column
#' @param expand_col Columns of the tibble to expand
#' @return New tibble from the expansion of the list-column
#' @rdname utils
unnest.expands <- function(tbl, expand_col){
    
    nest.levs <- strsplit(expand_col, "\\.")[[1]]
    
    if (length(nest.levs) == 1){
        
        dplyr::bind_rows(lapply(tbl[[nest.levs[1]]], function(y) tibble::as_tibble(y)), .id="id")
        
    }else if (length(nest.levs) == 2){
        
        dplyr::bind_rows(lapply(tbl[[nest.levs[1]]], function(x){
            
            dplyr::bind_rows(lapply(x[[nest.levs[2]]], function(y) tibble::as_tibble(y)))
            
        }), .id="id")
        
    }else{
        stop("Only 1 or 2 levels currnetly supported")
    }
    
    
    
}

#' Generate small summary figures
#'
#' Given a tibble, and the column to plot, create a basic summary figure, either
#' a bar chart or histogram depending on whether the data is character/factor or
#' numeric.
#' 
#' @param tbl A tibble
#' @param plot.col The tibble column name used to form the plot
#' @param out.dir A directory to save the plots to
#' @return A character vector indicating the plot path.
#' @rdname utils
plot.fun <- function(tbl, plot.col, out.dir=NULL){
    
    if (is.null(out.dir) || all(is.na(out.dir))){
        out.dir <- tempdir()
    }
    
    if (dir.exists(out.dir) == F){
        dir.create(out.dir)
    }
    
    tbl$value <- tbl[[plot.col]]
    
    if (class(tbl$value) %in% c("character", "factor")){
        p1 <- ggplot(data=tbl, mapping=aes(x=value)) + geom_bar() +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,  size=rel(.4)),
                  axis.text.y=element_text(size=rel(.4)),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank())
    }else{
        p1 <- ggplot(data=tbl, mapping=aes(x=value)) + geom_histogram(bins=30) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1,  size=rel(.4)),
                  axis.text.y = element_text(size=rel(.4)),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            theme()
    }
    
    ggsave(p1, filename=paste0(out.dir, "/", make.names(plot.col), ".png"), width=1, height=1)
    
    paste0(out.dir, "/", make.names(plot.col), ".png")
}

.choose.units <- function(x){
    if ((x / 1e+12) > 1){
        paste(round(x / 1e+12, digits=3), "TB")
    }else if ((x / 1e+9) > 1){
        paste(round(x / 1e+9, digits=3), "GB")
    }else{
        paste(round(x / 1e+6, digits=3), "MB")
    }
}

.process.datatype.table <- function(sample.type.tmat){
    
    .process.table(dplyr::select(sample.type.tmat, -(access:sample_type)))
}

.process.table <- function(tmat, remove.empty=T){
    
    tmat <- dplyr::select(tmat, -1) 
    
    if (remove.empty){
        nz.cols <- tmat |>
            colSums()
        
        nz.cols <- nz.cols[nz.cols > 0]
        
        tmat <- dplyr::select(tmat, dplyr::all_of(names(nz.cols)))
    }
    
    #remove all zero columns
    
    apply(tmat, 1, function(x) x, simplify=F)
}

.filter.variable.list <- function(split.data, optional.vals, list.vals){
    
    new.data <- lapply(split.data, function(x){
        x 
    })
    
    #set optional values with NAs to empty
    new.data[optional.vals] <- lapply(new.data[optional.vals], function(x){
        if (tibble::is_tibble(x)){
            if (all(is.na(x$value))){
                character(0)
            }else{
                x
            }
        }else{
            if (all(is.na(x))){
                character(0)
            }else{
                x
            }
        }
        
    })
    
    #otherwise set NAs to character N/A
    new.data <- lapply(new.data, function(x){
        if (tibble::is_tibble(x)){
            x$value <- ifelse(is.na(x$value), "", x$value)
            x
        }else{
            if (length(x) > 0 & all(is.na(x))){
                "N/A"
            }else{
                x
            }
        }
        
    })
    
    #for variables exptecting lists, add I() to length 1 vectors
    new.data[list.vals] <- lapply(new.data[list.vals], function(x){
        
        if (tibble::is_tibble(x)){
            if (nrow(x) == 1){
                I(x)
            }else{
                x
            }
        }else{
            
            if (length(x) > 0){
                split.x <- strsplit(x, "\\n")[[1]]
                
                if(length(split.x) == 1){
                    I(x)
                }else{
                    split.x
                }
            }else{
                x
            }
            
        }
    })
    
    new.data
}
