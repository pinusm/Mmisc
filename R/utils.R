is.defined <- function(sym) {
    sym <- deparse(substitute(sym))
    env <- parent.frame()
    exists(sym, env)
}

`%not_in%` <- Negate(`%in%`)

view <- View

omit.na <- stats::na.omit

quick.csv <- function(x) {
    name <- deparse(substitute(x))
    readr::write_csv(x, path = paste0(name, ".csv"), na = "")
}

quick.xls <- function(x) {
    name <- deparse(substitute(x))
    writexl::write_xlsx(x, path = paste0(name, ".xlsx"))
}


flipPath <- function(text) {   # flip back-slash to double slahes, and vice-versa
    changes <- as.integer(gregexpr("[/\\]", text)[[1]])
    if (length(changes) == 1 && changes == -1) {
        return(text)
    }
    else {
        replacement <- strsplit(text, "")[[1]]
        for (pos in changes) {
            replacement[pos] <- ifelse(replacement[pos] == "/",
                                       "\\", "/")
        }
        return(paste0(replacement, collapse = ""))
    }
}

exploreWD <- function() { # Explore the current working directroy in Windows Explorer
    wd <- flipPath(getwd())
    shell(paste("explorer", wd, sep = " "), intern = TRUE)
}

cleanBulidLeftOver <- function(){
    if (file.exists("_bookdown.Rmd")) file.remove("_bookdown.Rmd")
}


