#' Calculates the brooding subscales (5 items) of the Ruminative Responses Scale (RRS)
#'
#' @description Scores the 5-item brooding subscale for the RRS.
#' The five brooding items are rrs2, rrs5, rrs6, rrs9, rrs10.
#' If a vector is supplied as input, it will be converted to a one-row,
#' five-column dataframe.
#'
#' @param brooding_items A matrix (or an object coercible to a matrix) that contains
#' the items of the brooding subscale of the Ruminative Responses Scale (RRS),
#' with each item represented as a number, 1, 2, 3, or 4. Anything not a
#' 1, 2, 3, or 4 will silently be recoded to NA.
#'
#' @param min_num_items The minimum number of items needed to be non-missing
#' in order for a score to be given. If the number of non-missing items is
#' less than min_num_items, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. With prorating the score
#' is (5 * mean item response) The default for the brooding subscale of the RRS
#' in this project is min_num_items = 4.
#'
#' @return Scores for the brooding subscale of the RRS.
#' @export
#'
#' @examples
#'\dontrun{
#'
#' score_rrs_brooding(hl_data[, c("rrs2", "rrs5", "rrs6", "rrs9", "rrs10")])
#'
#' }

score_rrs_brooding <- function(brooding_items,
                               min_num_items = 4) {

  # Handle the case of reflection_items being a single vector
  if (is.vector(brooding_items)) {

    # Convert brooding_items to 1 x 5 dataframe
    brooding_items <- as.data.frame(t(brooding_items))

    return(score_rrs_brooding(brooding_items,
                              min_num_items = min_num_items))
  }

  # Check names
  brooding_names <- c("rrs2", "rrs5", "rrs6", "rrs9", "rrs10")

  if (!all(names(brooding_items) %in% brooding_names)) {
    warning("The variable names of the brooding subscale for this project ",
            "should be ", paste(brooding_names, " "),
            "\nAre you sure your input is ",
            "correct?")
  }

  rrs_range <- 1:4L

  n_brooding_items <- 5L

  # Check the number of columns in the input
  if (ncol(brooding_items) != n_brooding_items) {
    stop("The brooding subscale of the RRS has ",
         n_brooding_items,
         " items, so there should be ",
         n_brooding_items, " columns in brooding_items.")
  }

  # Check the input: min_num_items
  if (min_num_items > n_brooding_items) {
    stop("The brooding subscale of the RRS has ",
         n_brooding_items,
         " items, so min_num_items must be ",
         n_brooding_items,
         " or smaller.")
  }

  if (min_num_items < 1) {
    stop("min_num_items must be greater than 0.")
  }

  brooding_items <- as.matrix(brooding_items)

  # Replace out-of-range values with NA
  brooding_items[which(!brooding_items %in% rrs_range,
                  arr.ind = TRUE)] <- NA

  if (all(is.na(brooding_items))) {
    message("All items are missing in brooding_items.\n")
    message("Check your input.\n")
  } else if (any(is.na(brooding_items))) {
    message("Some items are missing in brooding_items.\n")
  }

  if (min_num_items < n_brooding_items && any(is.na(brooding_items))) {
    message("Scoring will use prorating for items that are missing.\n")
    message(paste("If you do not want to prorate scores, set min_num_items to",
                  n_brooding_items))
  }

  brooding_items <- as.data.frame(brooding_items)

  score_surveys(brooding_items, min_num_items)

}
