#' Scores the Ruminative Responses Scale (RRS; all 10 items)
#'
#' @description Calculates a total score of the 10 RRS items
#' (i.e., rrs1 through rrs10).
#' If a vector is supplied as input, it will be converted to a one-row,
#' ten-column dataframe.
#'
#' @param rrs_items A matrix (or an object coercible to a matrix) that contains
#' the items of the Ruminative Responses Scale (RRS), with each item
#' represented as a number, 1, 2, 3, or 4. Anything not a 1, 2, 3, or 4 will
#' silently be converted to NA.
#'
#' @param min_num_items The minimum number of items needed to be non-missing
#' in order for a score to be given. If the number of non-missing items is
#' less than min_num_items, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. With prorating the score
#' is (10 * mean item response) The default for the RRS
#' in this project is min_num_items = 8.
#'
#' @return Scores for the RRS.
#' @export
#'
#' @examples
#'\dontrun{
#' rrs_items <- paste0("rrs", 1:10)
#' score_rrs(hl_data[rrs_items])
#' score_rrs(rrs)
#' }

score_rrs <- function(rrs_items,
                      min_num_items = 8) {

  # Handle the case of rrs_items being a single vector
  if (is.vector(rrs_items)) {

    # Convert reflection_items to 1 x 10 dataframe
    rrs_items <- as.data.frame(t(rrs_items))

    return(score_rrs(rrs_items,
                     min_num_items = min_num_items))
  }

  rrs_range <- 1:4L

  n_rrs_items <- 10L

  if (ncol(rrs_items) != n_rrs_items) {
    stop("The RRS has",
         n_rrs_items,
         "items, so there should be",
         n_rrs_items, "columns in rrs_items.")
  }

  if (min_num_items > n_rrs_items) {
    stop("The RRS has",
         n_rrs_items,
         "items, so min_num_items must be",
         n_rrs_items,
         "or smaller.")
  }

  if (min_num_items < 1) {
    stop("min_num_items must be greater than 0.")
  }

  rrs_items <- as.matrix(rrs_items)

  rrs_items[which(!rrs_items %in% rrs_range,
                  arr.ind = TRUE)] <- NA

  if (all(is.na(rrs_items))) {
    message("All items are missing in rrs_items.\n")
    message("Check your input.\n")
  } else if (any(is.na(rrs_items))) {
    message("Some items are missing in rrs_items.\n")
  }

  if (min_num_items < n_rrs_items && any(is.na(rrs_items))) {
    message("Scoring will use prorating if some items are missing.\n")
    message(paste("If you do not want to prorate scores, set min_num_items to",
                  n_rrs_items))
  }

  rrs_items <- as.data.frame(rrs_items)

  score_surveys(rrs_items, min_num_items)

}

# Notes: this function requires that the correct names be submitted.
# Consider enforcing or suggesting the variable names
# Add Reflection and Brooding
# Reflection: 1, 3, 4, 7, 8
# Brooding: 2, 5, 6, 9, 10
