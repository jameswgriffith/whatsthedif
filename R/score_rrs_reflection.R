#' Calculates the reflection subscales (5 items) of the Ruminative Responses Scale (RRS)
#'
#' @description Scores the 5-item reflection subscale for the RRS.
#' The five reflection items are rrs1, rrs3, rrs4, rrs7, rrs8.
#'
#' @param reflection_items A matrix (or an object coercible to a matrix) that contains
#' the items of the reflection subscale of the Ruminative Responses Scale (RRS),
#' with each item represented as a number, 1, 2, 3, or 4.
#'
#' @param min_num_items The minimum number of items needed to be non-missing
#' in order for a score to be given. If the number of non-missing items is
#' less than min_num_items, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. With prorating the score
#' is (5 * mean item response) The default for the reflection subscale of the
#' RRS in this project is min_num_items = 4.
#'
#' @return Scores for the reflection subscale of the RRS.
#' @export
#'
#' @examples
#'\dontrun{
#'
#' score_rrs_reflection(hl_data[, c("rrs1", "rrs3", "rrs4", "rrs7", "rrs8")])
#'
#' }

score_rrs_reflection <- function(reflection_items,
                                 min_num_items = 4) {
  # Check names
  reflection_names <- c("rrs1", "rrs3", "rrs4", "rrs7", "rrs8")

  if(!all(names(reflection_items) %in% reflection_names)) {
    warning("The variable names of the reflection subscale for this project ",
            "should be ", paste(reflection_names, " "),
            "\nAre you sure your input is ",
            "correct?")
  }

  rrs_range <- 1:4L

  n_reflection_items <- 5

  # Check the number of columns in the input
  if(ncol(reflection_items) != n_reflection_items) {
    stop("The reflection subscale of the RRS has ",
         n_reflection_items,
         " items, so there should be",
         n_reflection_items, " columns in reflection_items.")
  }

  # Check the input: min_num_items
  if(min_num_items > n_reflection_items) {
    stop("The reflection subscale of the RRS has ",
         n_reflection_items,
         " items, so min_num_items must be ",
         n_reflection_items,
         " or smaller.")
  }

  reflection_items <- as.matrix(reflection_items)

  # Replace out-of-range values with NA
  reflection_items[which(!reflection_items %in% rrs_range,
                       arr.ind = TRUE)] <- NA

  if(all(is.na(reflection_items))) {
    message("All items are missing in reflection_items.\n")
    message("Check your input.\n")
  } else if (any(is.na(reflection_items))) {
    message("Some items are missing in reflection_items.\n")
  }

  if(min_num_items < n_reflection_items && !all(is.na(reflection_items))) {
    message("Scoring will use prorating for items that are missing.\n")
    message(paste("If you do not want to prorate scores, set min_num_items to",
                  n_reflection_items))
  }

  if(min_num_items < 1) {
    stop("min_num_items must be greater than 0.")
  }

  reflection_items <- as.data.frame(reflection_items)

  score_surveys(reflection_items, min_num_items)

}
