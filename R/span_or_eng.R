#' A helper function to combine English and Spanish variables into a single variable. This is specific to the whatsthedif study
#'
#' @description This function looks at a pair of variables - one for English,
#' one for Spanish - and returns one of them depending on the \code{lang} parameter.
#'
#' @details This function is used to combine the English and Spanish variables
#' into a single variable. Warnings are given if lang parameter doesn't
#' match what data are present in eng or span
#' (see parameter description below).
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

  # Check for errors in input
  if(!(lang %in% c(1, 2, NA))) {
    stop("lang must be 1 for English or 2 for Spanish.")
  }

  # Warnings if lang is missing
  if(is.na(lang) && (!is.na(eng) || !is.na(span))) {
    warning("lang is missing, but English or Spanish data are present. Please check data.")
  }

  if(is.na(lang)) return(NA)

  # Check for errors in data
  if(!is.na(eng) && !is.na(span) && eng != span) {
    stop("Both English and Spanish data are present, but they are different.`")
  }

  if(lang == 1 && is.na(eng) && !is.na(span)) {
    warning("Language is English, but only Spanish data are present. Please check data.")
  }

  if(lang == 2 && !is.na(eng) && is.na(span)) {
    warning("Language is Spanish, but only English data are present. Please check data.")
  }

  # Initialise datum to NA
  datum <- NA

  if(!is.na(eng) || !is.na(span)) {
    datum <- max(eng, span, na.rm = TRUE)
  }

  # if(lang == 1) {
  #   datum <- eng
  # } else if(lang == 2) {
  #   datum <- span
  # } else {
  #   datum <- NA
  # }

  return(datum)

}
