#' span_or_eng: A helper function to combine English and Spanish variables into a single variable. This is specific to the whatsthedif study
#'
#' @description This function looks at a pair of variables - one for English,
#' one for Spanish - and returns one of them depending on the \code{lang} parameter.
#'
#' @details This function is used to combine the English and Spanish variables
#' into a single variable
#'
#' @param lang This parameter should be set to "1" for English, "2" for Spanish
#'
#' @param eng This should be the variable containing the English variable name
#'
#' @param span This should be the variable containing the Spanish variable name
#'
#' @return Returns the value in eng or span
#'
#' @export
#'
#' @examples
#'\dontrun{
#'
#' span_or_eng(nu_screening[1, "language_determination"],
#' nu_screening[1, "age",
#' nu_screening[1, "age_span")
#'
#' }
span_or_eng <- function(lang, eng, span) {

  if(!(lang %in% 1L:2L)) {
    stop("lang must be 1 for English or 2 for Spanish.")
  }

  if(lang == 1) {
    datum <- eng
  } else if(lang == 2) {
    datum <- span
  } else {
    datum <- NA
  }
  return(datum)
}
