# for GGally ggdou
lm_with_cor <- function(data, mapping, ..., method = "pearson") {
    x <- data[[deparse(mapping$x)]]
    y <- data[[deparse(mapping$y)]]
    cor <- cor(x, y, method = method)
    GGally::ggally_smooth_lm(data, mapping, ...) +
        ggplot2::geom_label(
            data = data.frame(
                x = min(x, na.rm = TRUE),
                y = max(y, na.rm = TRUE),
                lab = round(cor, digits = 3)
            ),
            mapping = ggplot2::aes(x = x, y = y, label = lab),
            hjust = 0, vjust = 1,
            size = 5, fontface = "bold"
        )
}


multi.tests <- function(fun = apa::t_test, df, vars, group.var, ...) {
    sapply(simplify = FALSE,                                    # sapply(simplify=T) better, elements named
           vars,                                                # loop on vector of outcome variable names
           function(var) {
               formula <- stats::as.formula(paste(var, "~", group.var))# create a formula with outcome and grouping var.
               fun(data = df, formula, ...)                     # perform test with a given fun, default t.test
           }
    )
}


# correlation with Bayes, and sample size
cor.bf <- function(data) {
    data      <- stats::na.omit(data)
    x.name    <- names(data)[1]
    y.name    <- names(data)[2]
    cor_obj   <- stats::cor.test(as.numeric(data[[1]]),as.numeric(data[[2]]))
    n_obj     <- min(nrow(stats::na.omit(data[,1])), nrow(stats::na.omit(data[,2])))
    bf_obj    <- BayesFactor::correlationBF(data[[1]], data[[2]]) #%>% BayesFactor::extractBF()
    return(list(bf = bf_obj, cor = cor_obj, n = n_obj))
}

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

rv4zero <- function(vector) {
    vector <- as.numeric(vector)
    recoded_vector <- dplyr::case_when(vector == 0 ~ 3,
                                vector == 1 ~ 2,
                                vector == 2 ~ 1,
                                vector == 3 ~ 0,
                                TRUE ~ vector)
    return(recoded_vector)
}

copy.attributes <- function(from,to,delete=c('names','row.names','class','dim','dimnames'),delete2=NULL) {
    a <- attributes(from)
    a[c(delete,delete2)] <- NULL
    attributes(to) <- c(attributes(to),a)
    return(to)
}

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
