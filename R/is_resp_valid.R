#' Returns TRUE or FALSE depending on whether x is found among valid response types.
#'
#' @description This function is useful for a matrix of questionnaire items.
#' A matrix of logical values is returned depending on whether elements of
#' x are found among valid (a vector containing valid values). If NA is an
#' acceptable (i.e., valid) response type, then it must be included in valid.
#'
#' @param x A matrix, usually of questionnaire items. If x is a vector, it
#' will silently be transposed and converted to a matrix
#'
#' @param valid A vector containing valid response types (e.g., 1:5). If NA
#' is be taken as a valid response type, then it must be listed as part of
#' the valid vector, as in c(NA, 1:5) or c(1, 2, 3, 4, 5, NA)
#'
#' @return A matrix of logical (i.e., TRUE or FALSE) indicating whether the
#' corresponding element of x if found in the "valid" parameter.
#' @export
#'
#' @examples
#' \dontrun{
#' is_resp_valid(rrs_data, 1:5)
#' }
is_resp_valid <- function(x, valid) {

  if(is.vector(x)) {
    x <- as.matrix(t(x))
    }

  # coerce x to matrix
  x <- as.matrix(x)

  apply(x,
        c(1, 2),
        function(x) {x %in% valid})
}
