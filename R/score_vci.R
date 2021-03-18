#' score_vci
#'
#' @description Scores the 8-item Vaccine Confidence Index
#' The eight items are vci_eng_1-8. The score is a ration with ratios greater
#' then 1.0 indicating vaccine confidence. Ratios less than 1.0 indicate
#' suspicion and/or hestiancy towards vacines.
#'
#' @details
#' Articles that describe background information on the Vaccine Confidence Index are:
#' Paoli, S.et al. Assessing Vaccine Hesitancy among Healthcare Workers: A
#' Cross-Sectional Study at an Italian Paediatric Hospital and the
#' Development of a Healthcare Worker’s Vaccination Compliance Index.
#' Vaccines 2019, 7, 201.
#'
#' Larson, H.et al. State of Vaccine Confidence in the EU 2018; European
#' Commission: Luxembourg, 2018.
#'
#' Larson, et al. Measuring vaccine confidence: Introducing a global vaccine
#' confidence index. PLoS Curr. 2015
#'
#' @param vci_items A matrix (or an object coercible to a matrix) that contains
#' the items of the Vaccine Confidence Index,
#' with each item represented as a number, 1 = Totally agree,
#' 2 = Partially agree, 3 = Partially disagree, or 4 = Totally disagree. The
#' items are internally recoded such that 4 = Totally agree,
#' 3 = Partially agree, 2 = Partially disagree, or 1 = Totally disagree.
#'
#' @param min_num_items The minimum number of items needed to be non-missing
#' in order for a score to be given. If the number of non-missing items is
#' less than min_num_items, then the score will be NA. Otherwise, in the
#' presence of missing data, prorating will be used. With prorating the score
#' is (mean(items 1-4) / 4) / (mean(items 5-8) / 4)
#'
#' @return Scores for the Vaccine Confidence Index
#' @export
#'
#' @examples
#'\dontrun{
#'
#' score_vci(hl_data[, paste0("vci", 1:8))])
#'
#' }

score_vci <- function(vci_items,
                      min_num_items =
                        ceiling(ncol(vci_items) * .8)) {

  vci_range <- 1:4L

  n_vci_items <- 8L

  # Check the number of columns in the input
  if(ncol(vci_items) != n_vci_items) {
    stop("The VCI has ",
         n_vci_items,
         " items, so there should be ",
         n_vci_items, " columns in vci_items.")
  }

  # Check the input: min_num_items
  if(min_num_items > n_vci_items) {
    stop("The VCI has ",
         n_vci_items,
         " items, so min_num_items must be ",
         n_vci_items,
         " or smaller.")
  }

  if(min_num_items < 2) {
    stop("min_num_items cannot be less than 2. We recommend setting it to 7 or 8.")
  }

  vci_items <- apply(vci_items,
                     2,
                     recode_items,
                     original = 1:5,
                     recoded = c(4:1, 5))

  vci_items <- as.matrix(vci_items)

  # Replace out-of-range values with NA
  vci_items[which(!vci_items %in% vci_range,
                       arr.ind = TRUE)] <- NA

  if(all(is.na(vci_items))) {
    message("All items are missing in vci_items.\n")
    message("Check your input.\n")
  } else if (any(is.na(vci_items))) {
    message("Some items are missing in vci_items.\n")
  }

  if(min_num_items < n_vci_items && !all(is.na(vci_items))) {
    message("Scoring will use prorating for items that are missing.\n")
    message(paste("If you do not want to prorate scores, set min_num_items to",
                  n_vci_items))
  }

  vci_items <- as.data.frame(vci_items)

  # Done handling errors, so apply scoring algorithm below.
  apply(X = vci_items,
        MARGIN = 1,
        FUN = function(one_survey)
          ifelse(test = sum(!is.na(one_survey)) >= min_num_items,
                 yes = mean(one_survey[1:4], na.rm = TRUE) / mean(one_survey[5:8], na.rm = TRUE),
                 no = NA))

}
