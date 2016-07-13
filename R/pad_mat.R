#' Pad the ends of a vector with NAs so that it is a specified length.
#'
#' @param x A vector of length less than \code{n}.
#' @param n A positive integer. The desired length of a vector.
#'
#' @author David Gerard
#'
#' @export
pad_length <- function(x, n) {
    assertthat::assert_that(is.vector(x))
    assertthat::assert_that(length(x) <= n)
    length(x) <- n
    return(x)
}

#' Takes a list of vectors and cbinds them to form a data frame.
#'
#' If the vectors are of different lengths, then it will pad the
#' smaller vectors with \code{NA}'s. Any non-vectors will be coerced
#' to be vectors.
#'
#' @param lst A list
#'
#' @author David Gerard
#'
#' @export
#'
#' @examples
#' lst <- list(1:3, 1:4)
#' cbind_list(lst)
#'
#' lst <- append(lst, c("a"))
#' cbind_list(lst)
cbind_list <- function(lst) {
    assertthat::assert_that(is.list(lst))
    ## convert to vectors
    lst <- sapply(lst, c)
    ## max length
    n <- max(sapply(lst, length))
    df_out <- as.data.frame(lapply(lst, pad_length, n = n), stringsAsFactors = FALSE)
    names(df_out) <- paste0("Var", 1:ncol(df_out))
    return(df_out)
}
