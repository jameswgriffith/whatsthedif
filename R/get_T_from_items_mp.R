#' Returns NIH Toolbox Meaning and Purpose T scores
#' from items
#'
#' @description Given a matrix of items containing NIH Toolbox
#' Meaning and Purpose scores, calculate the raw score
#' (using prorating if needed) and automatically convert it to the
#' corresponding T score
#'
#' @param mp_items A matrix (or object coercible to a matrix) containing the
#' items of the Meaning and Purpose scale. Items should be 1, 2, 3, 4, 5.
#' Anything not a 1, 2, 3, 4, 5 will silently be recoded to NA.
#'
#' @param min_num_items The minimum number of items needed to be non-missing
#' in order for a score to be given. If the number of non-missing items is
#' less than min_num_items, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. In other words, in the
#' context of missing data,
#' the score is (7 * mean item response). The default
#' for NIH Toolbox Meaning and Purpose in this project is min_num_items = 6.
#'
#' @param rnd_nearest_int A TRUE/FALSE variable (TRUE by default) indicating
#' whether the data in raw_score will be rounded to the nearest integer
#' (or not)
#'
#' @return T Scores from the lookup table.
#' @export
#'
#' @examples
#' \dontrun{
#' # See all T scores for each raw score.
#' get_T_from_items_mp(some_items)
#' }
get_T_from_items_mp <- function(mp_items,
                                min_num_items = 6,
                                rnd_nearest_int = TRUE) {

  raw_scores <- get_raw_score_mp(mp_items = mp_items,
                                 min_num_items = min_num_items)

  get_T_from_items_mp(raw_scores,
                      rnd_nearest_int = rnd_nearest_int)

}

