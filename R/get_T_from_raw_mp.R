#' Converts NIH Toolbox Meaning and Purpose v2.0 raw scores using lookup table
#' from the manual
#'
#' @description Given a raw score from the manual (range 7-35), what is the
#' corresponding T Score? See healthmeasures.net for the NIH Toolbox
#' manual.
#'
#' @param raw_scores A vector of raw scores. Because some scoring uses
#' prorating, they will be rounded to the nearest integer by default.
#'
#' @param rnd_nearest_int A TRUE/FALSE variable (TRUE by default) indicating
#' whether the data in raw_score will be rounded to the nearest integer
#' (or not)
#'
#' @return T Scores from the lookup table in the NIH Toolbox manual.
#' @export
#'
#' @examples
#' \dontrun{
#' # See all T scores for each raw score.
#' get_T_from_raw_mp(c(7:35))
#' }
get_T_from_raw_mp <- function(raw_scores,
                              rnd_nearest_int = TRUE) {

  if(!is.vector(raw_scores) || any(!is.numeric(raw_scores))) {
    stop("raw_scores must be a numeric vector")
  }

  if(any(raw_scores < 7, na.rm = TRUE) || any(raw_scores > 35, na.rm = TRUE)) {
    stop("raw_scores should be 7-35. Please check your data and try again")
  }

  if(rnd_nearest_int) raw_scores <- round(raw_scores, 0)

  # T Scores from the lookup table
  # See manual on healthmeasures.net
  mp_Ts <- c(
    12.8, 13.8, 15.3, 17.2, 19.2 ,
    21.2, 23.0, 24.8, 26.4, 28.1,
    29.6, 31.2, 32.8, 34.4, 36.0,
    37.7, 39.3, 41.0, 42.8, 44.7,
    46.7, 48.8, 50.9, 53.1, 55.5,
    58.2, 61.1, 64.4, 68.5)

  raw_score_range <- as.character(7:35)

  names(mp_Ts) <- raw_score_range

  Ts <- mp_Ts[as.character(raw_scores)]

  names(Ts) <- NULL

  return(Ts)

}
