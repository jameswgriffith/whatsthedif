# whatsthedif internal functions ------------------------------------------

#' score_surveys
#'
#'Takes a dataframe of numeric items and calculates a score
# by multiplying the number of items by the mean item response.
# If no items are missing, this is equivalent to summing the items.
# If one or more items are missing, this is equivalent to treating
# the missing items as the mean value of the non-missing items,
# and then summing all of the items. This practice is known as
# "prorating". A score is only returned if the minimum number
# of items (or greater) is present.
#
# Arguments are:
# items  - A dataframe containing the responses.
# Note: If items is not a dataframe, the function will
# stop and return an error message.
#
# min_num_items - The minimum number of items for scoring the case.
# If the length of items is less than this value, then NA will be returned.
# By default, i.e., if the user does not supply min_num_items, then
# all of the items will be required in order to calculate a score.
#'
#' @param items A dataframe of numeric  questionnaire items.
#' @param min_num_items The minimum number of items needed to score the
#' questionnaire. If not enough items are present, the score will be NA.
#' If some items are missing, but the number of non-missing items is
#' min_num_items or higher, then the score will be prorated.
#'
#' @return Scores for each case (i.e., row)
#'
#' @examples
#' # score_surveys(rrs, 10)
#' # score_surveys(rrs, 8)
score_surveys <- function (items, min_num_items = ncol(items)) {
  # Handle some possible errors
  if (!is.data.frame(items)) {
    stop("This function only handles dataframes of numeric survey data.\n Try again with items as a dataframe.")
  }
  if (min_num_items > ncol(items)) {
    stop("The argument min_num_items is larger than the number of items.")
  }
  if (min_num_items <= 0) {
    stop("The argument min_num_items should not be zero or negative.")
  }
  # Done handling errors, so apply scoring algorithm below.
  apply(X = items,
        MARGIN = 1,
        FUN = function(one_survey)
          ifelse(test = sum(!is.na(one_survey)) >= min_num_items,
                 yes = length(one_survey) * mean(one_survey, na.rm = TRUE),
                 no = NA))
}
