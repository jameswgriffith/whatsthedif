#' Recodes employment into a single factor, starting with a dataframe
#' of 9 columns of TRUE/FALSE variables
#'
#' @description Return a single factor for employment status;
#' it should be coded as follows:
#' 1 = Employed, full time; 2 = Employed, part time; 3 = Homemaker;
#' 4 = Self-employed; #' 5 = Unable to work; 6 = Student; 7 = Retired;
#' 8 = Out of work less than 1 year; 9 = Out of work more than 1 year
#'
#' @details
#' For cases where more than one option is checked, the following scheme is
#' used to combine into one category
#' Full time and part time - Recode to full time employed
#' Full time and self-employed	- Recode to self-employed
#' Full time and student	- Recode to full time employed
#' Homemaker and unemployed for more than 1 year - Recode to homemaker
#' Homemaker and student -	Recode to homemaker
#' Part-time and retired	- Recode to retired
#' Part time and student	Recode to part time
#' Retired and unemployed more than one year	- Recode to retired
#' Self-employed and retired - Recode to retired
#' Self-employed and student -	Recode to self-employed
#' Unable to work and retired	- Recode to retired
#' Unable to work and unemployed less than one year	- Recode to unable to work
#' Unable to work and unemployed more than one year	- Recode to unable to work
#' Unable to work and working full time -	Recode to unable to work
#'
#' @param empl_vars A dataframe of 9 TRUE/FALSE variables for the nine
#' employment variables (see Description above)
#'
#' @return A nine-level factor, with one value for each row
#'
#' @export
#'
#' @examples
#' \dontrun{
#' recode_employment(hl[, empl_vars])
#' }
recode_employment <- function(empl_vars) {

  # Check for errors

  if(is.vector(empl_vars)) {
    return(recode_employment_vector(empl_vars))
  }

  # If needed, coerce to dataframe
  empl_vars <- as.data.frame(empl_vars)

  if(ncol(empl_vars) != 9) {
    stop("empl_vars must be a dataframe with 9 columns.")
  }

  empl_factor <- apply(empl_vars,
                       1,
                       recode_employment_vector)

  factor(empl_factor,
         levels = 1:9,
         labels = c(
           "Employed, full time",
           "Employed, part time",
           "Homemaker",
           "Self-employed",
           "Unable to work",
           "Student",
           "Retired",
           "Out of work less than 1 year",
           "Out of work more than 1 year"))
}

#' recode_employment_vector
#' @description Return a single factor for employment status.
#' It should be coded as follows:
#' 1 = Employed, full time
#' 2 = Employed, part time
#' 3 = Homemaker
#' 4 = Self-employed
#' 5 = Unable to work
#' 6 = Student
#' 7 = Retired
#' 8 = Out of work less than 1 year
#' 9 = Out of work more than 1 year
#'
#' @details
#' For cases where more than one option is checked, the following scheme is
#' used to combine into one category
#' Full time and part time - Recode to full time employed
#' Full time and self-employed	- Recode to self-employed
#' Full time and student	- Recode to full time employed
#' Homemaker and unemployed for more than 1 year -Recode to homemaker
#' Homemaker and student -	Recode to homemaker
#' Part-time and retired	- Recode to retired
#' Part time and student	Recode to part time
#' Retired and unemployed more than one year	- Recode to retired
#' Self-employed and retired - Recode to retired
#' Self-employed and student -	Recode to self-employed
#' Unable to work and retired	- Recode to retired
#' Unable to work and unemployed less than one year	- Recode to unable to work
#' Unable to work and unemployed more than one year	- Recode to unable to work
#' Unable to work and working full time -	Recode to unable to work
#'
#' @param empl A vector of 9 TRUE/FALSE variables for the nine
#' employment variables (see Description above)
#'
#' @return A nine-level factor (see Description above)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' recode_employment_vector(c(TRUE, FALSE, FALSE, FALSE,
#' FALSE, FALSE, FALSE, FALSE))
#' }
recode_employment_vector <- function(empl) {

  # Check for errors

  if(!is.vector(empl)) {
    stop("empl must be a vector of 9 dummy variables\n\n",
         "Please try again.\n",
         "The empl vector should be ordered as follows:\n",
         "empl_full_time\n",
         "empl_part_time\n",
         "empl_hmaker\n",
         "empl_self\n",
         "empl_unable_work\n",
         "empl_student\n",
         "empl_retired\n",
         "empl_unem_lt1yr\n",
         "empl_unem_mt1yr\n")
  }

  if(length(empl) != 9) {
    stop("There are 9 dummy codes for the employment variables\n.",
         "Please try again.\n",
         "The empl vector should be ordered as follows:\n",
         "empl_full_time\n",
         "empl_part_time\n",
         "empl_hmaker\n",
         "empl_self\n",
         "empl_unable_work\n",
         "empl_student\n",
         "empl_retired\n",
         "empl_unem_lt1yr\n",
         "empl_unem_mt1yr\n")
  }

  if(any(!empl %in% c(TRUE, FALSE, NA))) {
    stop("The elements of empl must be either TRUE or FALSE.")
  }

  # Done checking for errors

  empl_num_missing <- sum(is.na(empl))

  empl_num_checked <- sum(empl, na.rm = TRUE)

  option <- "default"

  if(empl_num_missing == 9) {
    option <- "All_missing"
  }
  if(empl_num_missing > 1 &&  empl_num_missing < 9) {
    option <- "Some_missing"
  }
  if(empl_num_checked == 1 && empl_num_missing == 0) {
    option <- "Single_empl_cat"
  }
  if(empl_num_checked > 1 && empl_num_missing == 0) {
    option <- "Multiple_empl_cats"
  }

  # In theory, default should never be reached
  # This is included her for debugging
  if(option == "default") {
    stop("Internal error in recode_employment_vector.\n",
         "Some cases are missed before switch")
  }

  switch (option,
          All_missing = NA,
          Some_missing = NA,
          Single_empl_cat = recode_single_empl(empl),
          Multiple_empl_cats = recode_multiple_empl(empl),
          default = NA)
}


# Internal helper functions -----------------------------------------------

#'Internal function to recode employment when only a single option is checked
#'as TRUE and all other options are FALSE
#'
#' @param empl A vector of 9 TRUE/FALSE values where only one is TRUE
#'
#' @return Employment status coded as follows:
#'  1 = Employed, full time
#'  2 = Employed, part time
#'  3 = Homemaker;
#'  4 = Self-employed
#'  5 = Unable to work
#'  6 = Student
#'  7 = Retired;
#'  8 = Out of work less than 1 year
#'  9 = Out of work more than 1 year
recode_single_empl <- function(empl) {

  if(sum(empl) != 1) {
    stop("empl does not equal 1 within recode_single_empl.\nPlease try again.")
  }

  empl_table <- c(
    "Employed, full time" = 1,
    "Employed, part time" = 2,
    "Homemaker" = 3,
    "Self-employed" = 4,
    "Unable to work" = 5,
    "Student" = 6,
    "Retired" = 7,
    "Out of work less than 1 year" = 8,
    "Out of work more than 1 year" = 9)

  empl_table[which(empl)]

}

#'Internal function to recode employment when more than one option is TRUE
#'
#' @param empl A vector of 9 TRUE/FALSE values where more than one is TRUE
#'
#' @return Employment status coded as follows:
#'  1 = Employed, full time; 2 = Employed, part time; 3 = Homemaker;
#'  4 = Self-employed; 5 = Unable to work; 6 = Student; 7 = Retired;
#'  8 = Out of work less than 1 year; 9 = Out of work more than 1 year
recode_multiple_empl <- function(empl) {

  if(sum(empl) == 1) {
    stop("Only a single employment category is present in empl.")
  }

  # Set default to NA
  empl_rec <- NA

  # Checked off full-time (1) and part-time employed (2)
  if(all(empl == c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))) {
    empl_rec <- 1 # Full-time
  }

  # Checked off full-time (1) and self-time employed (4)
  if(all(empl == c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))) {
    empl_rec <- 4 # Self-employed
  }

  # Checked off full-time (1) and student (6)
  if(all(empl == c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))) {
    empl_rec <- 1 # Full-time
  }

  # Homemaker (3) and unemployed for more than 1 year (9)
  if(all(empl == c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))) {
    empl_rec <- 3 # Homemaker
  }

  # Homemaker (3) and student (6)
  if(all(empl == c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))) {
    empl_rec <- 3 # Homemaker
  }

  # Part time (2) and retired (7)
  if(all(empl == c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))) {
    empl_rec <- 7 # Retired
  }

  # Part time (2) and student (6)
  if(all(empl == c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))) {
    empl_rec <- 2 # Part-time
  }

  # Retired (7) and unemployed more than one year (9)
  if(all(empl == c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))) {
    empl_rec <- 7 # Retired
  }

  # Self-employed (4) and retired (7)
  if(all(empl == c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))) {
    empl_rec <- 7 # Retired
  }

  # Self-employed (4) and student (6)
  if(all(empl == c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE))) {
    empl_rec <- 4 # Self-employed
  }

  # Unable to work (5) and retired (7)
  if(all(empl == c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE))) {
    empl_rec <- 7 # Retired
  }

  # Unable to work (5) and unemployed less than one year (8)
  if(all(empl == c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))) {
    empl_rec <- 5 # Unable to work
  }

  # Unable to work (5) and unemployed more than one year (9)
  if(all(empl == c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))) {
    empl_rec <- 5 # Unable to work
  }

  # Unable to work (5) and full-time employed (1)
  if(all(empl == c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))) {
    empl_rec <- 5 # Unable to work
  }

  # Unable to work (5) and unemployed more than 1 year (9)
  if(all(empl == c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))) {
    empl_rec <- 5 # Unable to work
  }

  # Unemployed less than 1 year (8) and student (6)
  if(all(empl == c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))) {
    empl_rec <- 8 # Unemployed less than 1 year
  }

  return(empl_rec)

}
