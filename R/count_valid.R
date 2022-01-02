#' Counts the number of valid responses for each row in a matrix
#'
#' @description This function is useful for a matrix of questionnaire items.
#' A vector is returned containing a count of how many elements in the row
#' are valid responses. If NA is an
#' acceptable (i.e., valid) response type, then it must be included in valid.
#'
#' @param x A matrix, usually of questionnaire items. If x is a vector, it
#' will silently be transposed and converted to a matrix
#'
#' @param valid A vector containing valid response types (e.g., 1:5). If NA
#' is be taken as a valid response type, then it must be listed as part of
#' the valid vector, as in c(NA, 1:5) or c(1, 2, 3, 4, 5, NA)
#'
#' @return A vector that is a count, for each row of x, the number of elements
#' that are valid (i.e., found in the "valid" parameter).
#' @export
#'
#' @examples
#' \dontrun{
#' count_valid(rrs_data, 1:5)
#' }
count_valid <- function(x, valid) {

  if(is.vector(x)) {
    x <- as.matrix(t(x))
    }

  # coerce x to matrix
  x <- as.matrix(x)

  x <- is_resp_valid(x, valid)

  rowSums(x)

}
