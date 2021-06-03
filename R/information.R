#' print the standard BF criteria I use to the console
#'
#' @return none. A print method is used instead to print the criteria to the console
#' @export

#'
bayesvalues <- function(){
    BFvalues <- dplyr::data_frame(BFxy = c(">100" , "30-100" , "10-30" , "3-10" , "1-3" , "1" , "0.3-1" , "0.1-0.3" , "0.03-0.1" , "0.01-0.03" , "<0.01"),
                           Interpretation = c("Decisive for X" , "Very strong for X" , "Strong for X" , "Moderate for X" , "Ancedotal for X" , "No evidence" , "Ancedotal for Y" , "Moderate for Y" , "Strong for Y" , "Very strong for Y" , "Decisive for Y")
    )
    BFvalues
}

#' print the standart fit criteria I use to the console
#'
#' @return none. A print method is used instead to print the criteria to the console
#' @export
#'

fitvalues <- function(){
    print("Taken from http://www.sicotests.com/psyarticle.asp?id=277")
    print("Fit is good if:")
    print("NFI > .90 (Byrne, 1994) or .95 (Schumacker & Lomax, 2004)")
    print("GFI > .90 (Byrne, 1994)")
    print("CFI > .93 (Byrne, 1994)")
    print("RMSEA < .08 (Browne & Cudeck, 1993), ideally RMSEA < .05 (Stieger, 1990).")
    print("RMSEA upper confidence interval < .08 (Hu & Bentler, 1998)")
}
