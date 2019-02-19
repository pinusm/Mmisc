#' convert numeric p-values to strings for reporting, without leading zero
#' @param p a p-value in numeric value
#' @export

pvalue.correct <- function(p){
    p <- p %>% as.numeric
    pvalue = dplyr::case_when(p < 0.001 ~ "< .001",
                       p < 0.01 ~ "< .01",
                       p < 0.05 ~ "< .05",
                       TRUE ~ p %>% round(2) %>% formatC(digits = 2, format = "f") %>% as.character() %>% str_replace("0.", ".")
    )
    return(pvalue)
}

#' ggplot2 APA-style
#'
#' theme modified version of papaja's theme_apa()
#'
#' @param base_size  base_size
#' @param base_family  base_family
#' @param box box
#' @importFrom grDevices windowsFonts
#' @export
#
theme_apa <- function(base_size = 14, base_family = "", box = FALSE)
{
    grDevices::windowsFonts(Times = grDevices::windowsFont("TT Times New Roman"))
    adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(1.1),
                                                          margin = ggplot2::margin(0, 0, ggplot2::rel(14),
                                                                                   0)), axis.title = ggplot2::element_text(size = ggplot2::rel(1.1)),
                       axis.title.x = ggplot2::element_text(margin = ggplot2::margin(ggplot2::rel(18),
                                                                                     0, 0, 0)), axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0,
                                                                                                                                                              ggplot2::rel(18), 0, 0)), axis.ticks.length = ggplot2::unit(ggplot2::rel(6),
                                                                                                                                                                                                                          "points"), axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
                       axis.text.x = ggplot2::element_text(margin = ggplot2::margin(ggplot2::rel(6),
                                                                                    0, 0, 0)), axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0,
                                                                                                                                                            ggplot2::rel(8), 0, 0)), axis.line.x = ggplot2::element_line(),
                       axis.line.y = ggplot2::element_line(), legend.title = ggplot2::element_text(),
                       legend.key = ggplot2::element_rect(fill = NA, color = NA),
                       legend.key.width = ggplot2::unit(ggplot2::rel(20),
                                                        "points"), legend.key.height = ggplot2::unit(ggplot2::rel(25),
                                                                                                     "points"), legend.spacing = ggplot2::unit(ggplot2::rel(18),
                                                                                                                                               "points"), panel.spacing = ggplot2::unit(ggplot2::rel(16),
                                                                                                                                                                                        "points"), panel.grid.major.x = ggplot2::element_line(size = NA),
                       panel.grid.minor.x = ggplot2::element_line(size = NA),
                       panel.grid.major.y = ggplot2::element_line(size = NA),
                       panel.grid.minor.y = ggplot2::element_line(size = NA),
                       strip.background = ggplot2::element_rect(fill = NA,
                                                                color = NA), strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.1),
                                                                                                                  margin = ggplot2::margin(0, 0, ggplot2::rel(16),
                                                                                                                                           0)), strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.1),
                                                                                                                                                                                     margin = ggplot2::margin(0, 0, 0, ggplot2::rel(16))))
    if (box) {
        adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
    }
    else {
        adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
    }
    adapted_theme
}

#' convert string p-values to strings for reporting, without leading zero
#' @param p a p-value in string value
#' @export

pvalue.report <- function(p){
    pvalue <- p %>% formatC(digits = 2, format = "f")
    if (p < 0.001) {
        pvalue <- " < .001"
    } else {
        if (p < 0.01) {
            pvalue <- " < .001"
        } else {
            if (p < 0.05) {
                pvalue <- " < .05"
            } else {
                pvalue <- paste0(" = ", gsub(pattern = "0\\.", replacement = "\\.", as.character(p %>% formatC(digits = 2, format = "f"))))
            }
        }
    }
    paste0("*p*", pvalue)
}

#' report cocor's different of correlations
#' @param cor_p a cocor object
#' @export
#'

cor_diff_report <- function(cor_p){
    pvalue <- pvalue.report(cor_p$fisher1925$p.value)
    cohensQ <- round(psych::fisherz(cor_p$fisher1925$estimate[1]) - psych::fisherz(cor_p$fisher1925$estimate[2]),2)
    return(paste0("*z* = ",round(cor_p$fisher1925$statistic,2),", ", pvalue, ", *Cohen's q* = ", cohensQ))
}

#' report var.test's different of variances
#' @param var_d a var.test's different of variances object
#' @export

var_diff_report <- function(var_d){
    pvalue <- pvalue.report(var_d$p.value)
    DFnum <- var_d$parameter[["num df"]]
    DFdenom <- var_d$parameter[["denom df"]]
    return(paste0("*F*$\\textsubscript{(",DFnum,",",DFdenom,")}$ = ",round(var_d$statistic,2),", ", pvalue))
}

#' report chi-square different of proportions
#' @param var_d a chi-square different of proportions object
#' @export


chi_prop_diff_report <- function(var_d){
    pvalue <- pvalue.report(var_d$p.value)
    DFnum <- var_d$parameter[["num df"]]
    DFdenom <- var_d$parameter[["denom df"]]
    return(paste0("*F*$\\textsubscript{(",DFnum,",",DFdenom,")}$ = ",round(var_d$statistic,2),", ", pvalue))
}

#' give M and SD per group, with the 'apa' package, on the results of t_test()
#'
#' @param t_test an apa::t_test object
#' @param x the name of a group in the apa::t_test object
#' @export

apa.desc <- function(t_test, x){
    group <- as.character(x)
    group_data <- t_test$data[[group]]
    mean <- mean(group_data, na.rm = T)
    sd <- sd(group_data, na.rm = T)
    return(paste0("(*M* = ",round(mean,2),", *SD* = ", round(sd,2),")"))
}

#' report correlation with BF, created by cor.bf()
#'
#' @param corObject a cor.bf() object
#' @param BF01 should the BF be 10, or 01 based
#' @export

report_cor.bf <- function(corObject , BF01 = F) {
    BFtype <- "10"
    BFvalue <- ifelse("jzs_med" %in% class(corObject$bf), corObject$bf$BayesFactor,
                      ifelse("BFBayesFactor" %in% class(corObject$bf),BayesFactor::extractBF(corObject$bf)$bf, #corObject$bf@bayesFactor$bf,
                             NA)
    )
    if (BF01) {
        BFtype <- "01"
        BFvalue <- 1 / BFvalue
    }

    paste0(apa::apa(corObject$cor),", $\\textit{BF}_\\textit{",BFtype,"}$ = ",BFvalue %>% round(2))
}

#' convert numbers to literal numbers
#'
#' based on https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
#'
#' @param x a number in numeric format
#' @export

numbers2words <- function(x){
    ## Function by John Fox found here:
    ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
    ## Tweaks by AJH to add commas and "and"
    helper <- function(x){

        digits <- rev(strsplit(as.character(x), "")[[1]])
        nDigits <- length(digits)
        if (nDigits == 1) as.vector(ones[digits])
        else if (nDigits == 2)
            if (x <= 19) as.vector(teens[digits[1]])
        else trim(paste(tens[digits[2]],
                        Recall(as.numeric(digits[1]))))
        else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                          Recall(makeNumber(digits[2:1]))))
        else {
            nSuffix <- ((nDigits + 2) %/% 3) - 1
            if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
            trim(paste(Recall(makeNumber(digits[
                nDigits:(3*nSuffix + 1)])),
                suffixes[nSuffix],"," ,
                Recall(makeNumber(digits[(3*nSuffix):1]))))
        }
    }
    trim <- function(text){
        #Tidy leading/trailing whitespace, space before comma
        text = gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
        #Clear any trailing " and"
        text = gsub(" and$","",text)
        #Clear any trailing comma
        gsub("\ *,$","",text)
    }
    makeNumber <- function(...) as.numeric(paste(..., collapse = ""))
    #Disable scientific notation
    opts <- options(scipen = 100)
    on.exit(options(opts))
    ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine")
    names(ones) <- 0:9
    teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
               "sixteen", " seventeen", "eighteen", "nineteen")
    names(teens) <- 0:9
    tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
              "ninety")
    names(tens) <- 2:9
    x <- round(x)
    suffixes <- c("thousand", "million", "billion", "trillion")
    if (length(x) > 1) return(trim(sapply(x, helper)))
    helper(x)
}


#' a helper for GGally's ggpairs
#'
#' @param data data
#' @param mapping mapping
#' @param ... ...
#' @export

plot_trend_lines <- function(data, mapping, ...){
    p <- ggplot2::ggplot(data = data, mapping = mapping) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method=stats::loess, fill="#A4A4A4", color="#A4A4A4", ...) +
        ggplot2::geom_smooth(method=stats::lm, fill="#2E2E2E", color="#2E2E2E", ...)
    p
}


#' convert numeric p-values to asterik stars
#'
#' @param p a p-value in numeric format
#' @export


pvalue.stars <- function (p)
{
    p <- p %>% as.numeric
    pstar = dplyr::case_when(
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        TRUE ~ "")
    return(pstar)
}
