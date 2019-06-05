#' internal function for GGally's ggdou, to add a trend line
#' @param data data
#' @param mapping mapping
#' @param ... ...
#' @param method method
#' @export

lm_with_cor <- function(data, mapping, ..., method = "pearson") {
    x <- data[[deparse(mapping$x)]]
    y <- data[[deparse(mapping$y)]]
    cor <- cor(x, y, method = method)
    GGally::ggally_smooth_lm(data, mapping, ...) +
        ggplot2::geom_label(
            data = data.frame(
                 x = min(x, na.rm = TRUE)
                ,y = max(y, na.rm = TRUE)
                #,lab = round(cor, digits = 3)
            ),
            # mapping = ggplot2::aes(x = x, y = y, label = lab),
            mapping = ggplot2::aes(x = x, y = y, label = round(cor, digits = 3)),
            hjust = 0, vjust = 1,
            size = 5, fontface = "bold"
        )
}

#' Multiple statistical tests over a group of variables
#'
#' @param fun A statistical functions, default is apa::t_test
#' @param vars A char vector with the names of the dependent variables to test
#' @param group.var A string with the name of the
#' @param df A data.frame containing `vars` and `group.var`
#' @param ... additional parameters to pass to `fun`
#' @return A list of the results of the statistical test
#' @export

multi.tests <- function(fun = apa::t_test, df, vars, group.var, ...) {
    sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
           vars,                                                # loop on vector of outcome variable names
           function(var) {
               formula <- stats::as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
               fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
           }
    )
}


#' Calculate Pearson correlation, and also the corresponsind Bayes Factor, and sample size
#'
#' @param data a data.frame, where only the first and the second columns will be used
#' @return A list of the results of the statistical test
#' @export

cor.bf <- function(data) {
    data      <- stats::na.omit(data)
    x.name    <- names(data)[1]
    y.name    <- names(data)[2]
    cor_obj   <- stats::cor.test(as.numeric(data[[1]]),as.numeric(data[[2]]))
    n_obj     <- min(nrow(stats::na.omit(data[,1])), nrow(stats::na.omit(data[,2])))
    bf_obj    <- BayesFactor::correlationBF(data[[1]], data[[2]]) #%>% BayesFactor::extractBF()
    return(list(bf = bf_obj, cor = cor_obj, n = n_obj))
}


#' Reverse code a 1-4 scale rating
#'
#' @param vector a vector to reverse code
#' @return A reverse coded vector
#' @export

# reverse coding
rv4 <- function(vector) {
    vector <- as.numeric(vector)
    recoded_vector <- dplyr::case_when(vector == 1 ~ 4,
                                vector == 2 ~ 3,
                                vector == 3 ~ 2,
                                vector == 4 ~ 1,
                                TRUE ~ vector)
    return(recoded_vector)
}

#' Reverse code a 1-5 scale rating
#'
#' @param vector a vector to reverse code
#' @return A reverse coded vector
#' @export

rv5 <- function(vector) {
    vector <- as.numeric(vector)
    recoded_vector <- dplyr::case_when(vector == 1 ~ 5,
                                vector == 2 ~ 4,
                                vector == 3 ~ 3,
                                vector == 4 ~ 2,
                                vector == 5 ~ 1,
                                TRUE ~ vector)
    return(recoded_vector)
}

#' Reverse code a 1-7 scale rating
#'
#' @param vector a vector to reverse code
#' @return A reverse coded vector
#' @export

rv7 <- function(vector) {
    vector <- as.numeric(vector)
    recoded_vector <- dplyr::case_when(vector == 1 ~ 7,
                                vector == 2 ~ 6,
                                vector == 3 ~ 5,
                                vector == 4 ~ 4,
                                vector == 5 ~ 3,
                                vector == 6 ~ 2,
                                vector == 7 ~ 1,
                                TRUE ~ vector)
    return(recoded_vector)
}

#' Reverse code a 0-4 scale rating
#'
#' @param vector a vector to reverse code
#' @return A reverse coded vector
#' @export

rv5zero <- function(vector) {
    vector <- as.numeric(vector)
    recoded_vector <- dplyr::case_when( vector == 0~ 4,
                                 vector == 1 ~ 3,
                                 vector == 2 ~ 2,
                                 vector == 3 ~ 1,
                                 vector == 4 ~ 0,
                                 TRUE ~ vector)
    return(recoded_vector)
}

#' Reverse code a 0-3 scale rating
#'
#' @param vector a vector to reverse code
#' @return A reverse coded vector
#' @export

rv4zero <- function(vector) {
    vector <- as.numeric(vector)
    recoded_vector <- dplyr::case_when(vector == 0 ~ 3,
                                vector == 1 ~ 2,
                                vector == 2 ~ 1,
                                vector == 3 ~ 0,
                                TRUE ~ vector)
    return(recoded_vector)
}

#' Copy Attributes
#'
#' This function copies attributes from one object and assigns them to another.
#'
#'
#' @aliases copy.attributes copy.attributes<-
#' @param from object from which the attributes should be taken
#' @param to object to which the attributes should be written
#' @param delete attributes which should not be copied. By default this are
#' class specific attributes which might cause problems if copied to another
#' object. But you can add or remove attributes from the vector.
#' @param delete2 Identical to delete and just added for convenience for the
#' case that you want to delete additional attributes but do not want to repeat
#' the vector given in delete. In the function both vectors, delete and
#' delete2, are just merged to one deletion vector.
#' @author Jan Philipp Dietrich dietrich AT pik-potsdam.de
#' @export

copy.attributes <- function(from,to,delete=c('names','row.names','class','dim','dimnames'),delete2=NULL) {
    a <- attributes(from)
    a[c(delete,delete2)] <- NULL
    attributes(to) <- c(attributes(to),a)
    return(to)
}

#' Reverse code a scale rating
#'
#' Reverse code a scale rating vector with either specified or empirical minimum and maximum values, and while keeping or discarding the scale attributes (labels)
#'
#' @param vector a vector to reverse code
#' @param minValue set the minimum value of the scale, if missing, minimum is estimated from the empirical minimum value (what's in the vector)
#' @param maxValue set the maximum value of the scale, if missing, maximum is estimated from the empirical maximum value (what's in the vector)
#' @param keepAttr bollian, whether to keep or discard the scale's attributes (labels)
#' @return A reverse coded vector
#' @export

rv <- function(vector , minValue = NA, maxValue = NA, keepAttr = FALSE) {
    num_vector <- as.numeric(vector)
    minV <- ifelse(!is.na(minValue),as.numeric(minValue),min(num_vector,na.rm = T))
    maxV <- ifelse(!is.na(maxValue),as.numeric(maxValue),max(num_vector,na.rm = T))
    if(is.na(minValue)){warning(paste0("Minimum value estimated, and set to ",minV))}
    if(is.na(maxValue)){warning(paste0("Maximum value estimated, and set to ",maxV))}
    recoded_vector <- maxV + minV - num_vector
    if(keepAttr){recoded_vector <- copy.attributes(from = vector, to = recoded_vector)}
    return(recoded_vector)
}

#' rowMeans that JUST WORKS inside a dplyr::mutate() call
#'
#' just name the variable to average, and you shall have their mean
#'
#' @param ... a list of unquoted variable names
#' @param na.rm ow to handle NAs
#' @return A meaned vector
#' @export
row_means = function(..., na.rm=TRUE) rowMeans(cbind(...), na.rm=na.rm)

#' rowSums that JUST WORKS inside a dplyr::mutate() call
#'
#' just name the variable to sum, and you shall have their sum
#'
#' @param ... a list of unquoted variable names
#' @return A summed vector
#' @export
row_sums = function(...) rowSums(cbind(...))
