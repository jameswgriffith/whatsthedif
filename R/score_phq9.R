#' Scores the PHQ-9 according to the standard scoring algorithm
#'
#' @param phq9_items A matrix (or object coercible to a matrix) containing the
#' ***first nine*** items of the PHQ-9. Items should be 0, 1, 2, or 3.
#' @param min_num_items The minimum number of items needed to be non-missing
#' in order for a score to be given. If the number of non-missing items is
#' less than min_num_items, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. In other words, in the
#' context of missing data,
#' the score is (9 * mean item response). The default
#' for the PHQ-9 in this project is min_num_items = 8.
#'
#' @return Scores for the PHQ-9
#' @export
#'
#' @examples
#' \dontrun{
#' phq9_items_1_9 <- paste0("phq9_", 1:9)
#' score_phq9(hl_data[phq9_items_1_9])
#' }
score_phq9 <- function(phq9_items,
                       min_num_items = 8) {

  phq9_range <- 0:3L

  n_phq9_items <- 9

  if(ncol(phq9_items) == 10) {
    stop("The PHQ-9 has 10 items, but only the first 9 are scored.",
    "Please try again using only the first 9 items of the PHQ-9.")
  }

  if(ncol(phq9_items) != n_phq9_items) {
    stop("The PHQ-9 has 10 items, but only the first 9 are scored.",
    "Please try again using only the first 9 items of the PHQ-9.")
  }

  if(min_num_items > n_phq9_items) {
    stop("The PHQ-9 has 10 items, but only the first 9 are scored. min_num_items must be",
    n_phq9_items, "or smaller.")
  }

  if(min_num_items < 1) {
    stop("min_num_items must be greater than 0.")
  }

  phq9_items <- as.matrix(phq9_items)

  phq9_items[which(!phq9_items %in% phq9_range,
                   arr.ind = TRUE)] <- NA

  if(all(is.na(phq9_items))) {
    message("All items are missing in phq9_items.\n")
    message("Check your input.\n")
  } else if (any(is.na(phq9_items))) {
    message("Some items are missing in phq9_items.\n")
  }

  if(min_num_items < n_phq9_items && !all(is.na(phq9_items))) {
    message("Scoring will use prorating if some items are missing.\n")
    message(paste("If you do not want to prorate scores, set min_num_items to",
                  n_phq9_items,
                  "."))
  }

  phq9_items <- as.data.frame(phq9_items)

  score_surveys(phq9_items, min_num_items)

}
