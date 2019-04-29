
#' check if an object is defined
#'
#' @param sym an object to check
#' @return bollian. TRUE if sym is defined, otherwise FALSE.
#' @export

is.defined <- function(sym) {
    sym <- deparse(substitute(sym))
    env <- parent.frame()
    exists(sym, env)
}

#' A not_in operator
#' @param a an object to search if it's in b
#' @param b an object where to look for object a
#' @aliases %not_in%
#' @return bolian. TRUE if a is not %in% b
#' @export

`%out%` <- function(a,b) {
    ! a %in% b
}

#' An alias for na.omit
#' @param x an object from which to omit NAs
#' @return x without the NAs
#' @export

omit.na <- function(x){
    stats::na.omit(x)
}

#' quickly export a data.frame to a CSV file
#'
#' @param x a data.frame
#' @return nothing. output is saved as a csv file named as per the data.frame, in the working directroy
#' @export

quick.csv <- function(x) {
    name <- deparse(substitute(x))
    readr::write_csv(x, path = paste0(name, ".csv"), na = "")
}

#' quickly export a data.frame to an Excel XLSX file
#'
#' @param x a data.frame
#' @return nothing. output is saved as a xlsx file named as per the data.frame, in the working directroy
#' @export

quick.xls <- function(x) {
    name <- deparse(substitute(x))
    writexl::write_xlsx(x, path = paste0(name, ".xlsx"))
}

#' flip back-slash to double slahes, and vice-versa
#'
#' @param text a char string
#' @return a char string x, with flipped slashes
#' @export

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

#' Open data.frame (or any R object) in an external app.
#'
#' Allows analysis in exteranl Stat apps, such as JASP or jamovi
#'
#' @param x a data.frame/tibble
#' @param app_path a string with the path for the app
#' @return none
#' @export

toEXTERNAL <- function(x, app_path) {
    program_loc <- app_path
    full_path <- tempfile(fileext = ".csv")
    readr::write_csv(x, path = full_path, na = "")
    if (!file.exists(program_loc)){
        stop("Can't locate program at location")
    }
    if (!file.exists(full_path)){
        stop("Can't create temp file")
    }
    xopen::xopen(full_path, app = program_loc, quiet = TRUE)
}

#' Open data.frame/tibble in an JASP
#'
#' Allows analysis in JASP
#'
#' @param x a data.frame/tibble
#' @return none
#' @export

toJASP <- function(x) {
    toEXTERNAL(x, app_path = "C:\\Program Files\\JASP\\JASP.exe")
}

#' Open data.frame/tibble in an jamovi
#'
#' Allows analysis in jamovi. Dynamically sets the path to jamovi (might fail if more than one version is installed)
#'
#' @param x a data.frame/tibble
#' @return none
#' @export

toJAMOVI <- function(x) {
    listOFprograms <- list.dirs("C:\\Program Files\\", full.names=F, recursive = FALSE)
    jamoviVersion <- listOFprograms[grep(listOFprograms , pattern = "jamovi*")][-1]
    program_loc <- paste0("C:\\Program Files\\",jamoviVersion,"\\bin\\jamovi.exe")
    toEXTERNAL(x, app_path = program_loc)
}

#' open windows explorer to the working directroy
#'
#' @return none
#' @export

exploreWD <- function() { # Explore the current working directroy in Windows Explorer
    wd <- flipPath(getwd())
    invisible(suppressWarnings(shell(paste("explorer", wd, sep = " "), intern = TRUE)))
}

#' clear knittr cache, for use with shortcut key
#'
#' @export

cleanBulidLeftOver <- function(){
    if (file.exists("_bookdown.Rmd")) file.remove("_bookdown.Rmd")
}


