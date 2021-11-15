#' Calculates raw scores (using summing with prorating)
#' for NIH Toolbox Meaning and Purpose (8 items, V2.0)
#'
#' @description Scores a matrix of NIH Toolbox Meaning and Purpose items in
#' which each row is an case
#' (at a particular time point). If a single vector is supplied, it will be
#' converted to a 1 row by 7 column data frame.
#'
#' @param mp_items A matrix (or object coercible to a matrix) containing the
#' items of the Meaning and Purpose scale. Items should be 1, 2, 3, 4, 5.
#' Anything not a 1, 2, 3, 4, 5 will silently be re-coded to NA.
#'
#' @param min_num_items The minimum number of items needed to be non-missing
#' in order for a score to be given. If the number of non-missing items is
#' less than min_num_items, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. In other words, in the
#' context of missing data,
#' the score is (7 * mean item response). The default
#' for NIH Toolbox Meaning and Purpose in this project is min_num_items = 6.
#'
#' @return Raw total scores for Meaning and Purpose
#' @export
#'
#' @examples
#' \dontrun{
#' get_raw_score_mp(mp_scores)
#' }
get_raw_score_mp <- function(mp_items,
                     min_num_items = 6) {

  # Handle the case of mp_items being a single vector
  if (is.vector(mp_items)) {

    # Convert items to 1 x 9 dataframe
    mp_items <- as.data.frame(t(mp_items))

    return(get_raw_score_mp(mp_items = mp_items, min_num_items = min_num_items))

  }

  item_range <- 1:5L

  n_items <- 7L

  # Trap some errors
  if (ncol(mp_items) != n_items) {
    stop("Meaning and Purpose has 7 items. Please try again")
  }

  if (min_num_items > n_items) {
    stop("Meaning and Purpose has",
         n_items,
         "min_num_items must be",
         n_items,
         "or smaller.")
  }

  if (min_num_items < 1 || !is.numeric(min_num_items)) {
    stop("min_num_items must be a number greater than 0.")
  }

  mp_items <- as.matrix(mp_items)

  # Replace out-of-range items with NA
  mp_items[which(!mp_items %in% item_range,
                      arr.ind = TRUE)] <- NA

  if (all(is.na(mp_items))) {
    message("All items are missing in mp_items.\n")
    message("Check your input.\n")
  } else if (any(is.na(mp_items))) {
    message("Some items are missing in mp_items.\n")
  }

  if (min_num_items < n_items && any(is.na(mp_items))) {
    message("Scoring will use prorating if some items are missing.\n")
    message(paste("If you do not want to prorate scores, set min_num_items to",
                  n_items,
                  "."))
  }

  mp_items <- as.data.frame(mp_items)

  # Scoring using prorating (if needed) and return output
  score_surveys(mp_items, min_num_items)

}

