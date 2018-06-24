pvalue.correct <- function(p){
    p <- p %>% as.numeric
    pvalue = dplyr::case_when(p < 0.001 ~ "< .001",
                       p < 0.01 ~ "< .01",
                       p < 0.05 ~ "< .05",
                       TRUE ~ p %>% round(2) %>% formatC(digits = 2, format = "f") %>% as.character() %>% str_replace("0.", ".")
    )
    return(pvalue)
}


#ggplot2 APA-style theme
windowsFonts(Times = windowsFont("TT Times New Roman"))
#modified version of papaja's theme_apa()
theme_apa <- function(base_size = 14, base_family = "", box = FALSE)
{
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

#report pvalues
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

#report cocor's different of correlations
cor_diff_report <- function(cor_p){
    pvalue <- pvalue.report(cor_p$fisher1925$p.value)
    cohensQ <- round(psych::fisherz(cor_p$fisher1925$estimate[1]) - psych::fisherz(cor_p$fisher1925$estimate[2]),2)
    return(paste0("*z* = ",round(cor_p$fisher1925$statistic,2),", ", pvalue, ", *Cohen's q* = ", cohensQ))
}

#report var.test's different of variances
var_diff_report <- function(var_d){
    pvalue <- pvalue.report(var_d$p.value)
    DFnum <- var_d$parameter[["num df"]]
    DFdenom <- var_d$parameter[["denom df"]]
    return(paste0("*F*$\\textsubscript{(",DFnum,",",DFdenom,")}$ = ",round(var_d$statistic,2),", ", pvalue))
}
#report chi-square different of proportions
chi_prop_diff_report <- function(var_d){
    pvalue <- pvalue.report(var_d$p.value)
    DFnum <- var_d$parameter[["num df"]]
    DFdenom <- var_d$parameter[["denom df"]]
    return(paste0("*F*$\\textsubscript{(",DFnum,",",DFdenom,")}$ = ",round(var_d$statistic,2),", ", pvalue))
}


multi.tests <- function(fun = t_test, df, vars, group.var, ...) {
    sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
           vars,                                                # loop on vector of outcome variable names
           function(var) {
               formula <- stats::as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
               fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
           }
    )
}
# give M and SD per group, with the 'apa' package, on the results of t_test()
apa.desc <- function(t_test, x){
    group <- as.character(x)
    group_data <- t_test$data[[group]]
    mean <- mean(group_data, na.rm = T)
    sd <- sd(group_data, na.rm = T)
    return(paste0("(*M* = ",round(mean,2),", *SD* = ", round(sd,2),")"))
}

#report correlation with BF, created by cor.bf()
report_cor.bf <- function(corObject , BF01 = F) {
    BFtype <- "10"
    BFvalue <- corObject$bf$BayesFactor
    if (BF01) {
        BFtype <- "01"
        BFvalue <- 1 / corObject$bf$BayesFactor
    }

    paste0(apa::apa(corObject$cor),", $\\textit{BF}_\\textit{",BFtype,"}$ = ",BFvalue %>% round(2))
}


#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
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


