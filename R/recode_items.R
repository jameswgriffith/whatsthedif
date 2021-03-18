#' recode_items
#'
#' @description This function is used to recode items
#' (e.g., 1 = 4, 2 = 3, 3 = 2, 4 = 1).
#'
#' @details This function is mostly used inside scoring algorithms. In general,
#' the scoring algorithms in whatsthedif are meant to operate on the original
#' (i.e., not recoded) data. If a dataframe is submitted in the "items"
#' argument, it will be converted to a matrix.
#'
#' @param items A vector or matrix of items to be recoded.
#'
#' @param original A vector containing the original coding of the variable
#' (e.g., 1, 2, 3, 4).
#'
#' @param recoded A vector containing the recoding of the variable
#' (e.g., 4, 3, 2, 1). "recoded" and "original" must be the same length.
#'
#' @return Recoded data are returned (usually questionnaire items).
#'
#' @export
#'
#' @examples
#'\dontrun{
#'
#' recode_items(1:4, 1:4, c("A", "B", "C", "D"))
#'
#' }
recode_items <- function(items, original, recoded) {

  if(is.data.frame(items)) {
    items <- as.data.frame(items)
  }

  # Check for errors in the input
  if(is.null(original)) {
    stop("original is required Try again")
  }

  if(is.null(recoded)) {
    stop("recoded is required Try again")
  }

  if(length(original) != length(recoded)) {
    stop("original and recoded must be equal in length. Try again")
  }

  # Find indices for items in original
  i <- match(items, original)

  # Return the recoded data
  recoded[i]

}
