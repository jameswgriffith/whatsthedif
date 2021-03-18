#' build_hl_screening
#'
#' @description This function merges the source files for the screening,
#' demographic, and HealthLiTT data.
#'
#' @details This function...
#'
#' @param items A vector or matrix of items to be recoded.
#'
#' @return Recoded data are returned (usually questionnaire items).
#'
#' @export
#'
#' @examples
#'\dontrun{
#'
#' recode_items(1:4, 1:4, c("A", "B", "C", "D"))
#'
#' }


# Clean up the environment (if needed) ------------------------------------

remove(list = objects(all.names = TRUE))
gc(verbose = TRUE, full = TRUE)

# Clears console
# https://stackoverflow.com/questions/22640016/code-to-clear-all-plots-in-rstudio
cat("\014")

# Clear graphics
# https://stackoverflow.com/questions/22640016/code-to-clear-all-plots-in-rstudio
dev.off() #closes the current graphical device.
graphics.off()
dev.list()


# Install and load packages -----------------------------------------------

install.packages("lubridate") # For age calculation
install.packages("devtools")
install.packages("xlsx")
install.packages("magrittr") # For the %>% pipe
install.packages("tableone")
install.packages("ggplot2")

library(devtools)
install_github("jameswgriffith/whatsthedif",
               auth_token = "56a3f57ab6935c2610336a67db61357108aa2e15")

update.packages(ask = FALSE)

# Are these needed ?
install.packages("pastecs") # For descriptive statistics
install.packages("psych")

library(lubridate) # For working with dates
library(whatsthedif) # For the DIF study specifically
library(xlsx) # For reading in Excel files
library(magrittr) # For the %>% pipe
library(tableone) # For descriptive statistics
library(ggplot2) # For graphical analyses

# May not be needed
# library(pastecs)
# library(psych)

# Read off some information about the session -----------------------------

sessionInfo()
RNGkind()

# Set working directory ---------------------------------------------------

# Check working directory
getwd()

# Things to address -------------------------------------------------------
# Age is whole numbers in BUMC data, to 1 decimal place in NU data
#   This is fixed now
# #1306 - Ethnicity was left blank (Michelle checked and it's missing)
# 1240 was mis-entered into Assess as 1340; this is fixed in the code
# 5120 needs to be excluded - Withdrew ; this person is selected out
# 1060 withdrew - Make sure this person is not in the database
# 1287 - randomization_gender and gender do not match
# 5050 & 5183 - Not enrolled

# Read in data files ------------------------------------------------------

# Read in screening data
# Note : A "raw" copy is created to aid in looking at modifications to the dataframe along the way
nu_screening <- nu_screening_raw <-
  read.csv("R:\\MSS\\Research\\Projects\\Griffith_Lab\\DIF data\\Screening Data\\NUScreeningData with cogscreeners date_02.23.2021.csv",
           header = TRUE)
bumc_dob <- bumc_raw <-
  read.csv("R:\\MSS\\Research\\Projects\\Griffith_Lab\\DIF data\\Screening Data\\Boston Screening Date and DOB_2.8.2021.csv",
           header = TRUE)
bumc_screening <- bumc_screening_raw <- read.xlsx("R:\\MSS\\Research\\Projects\\Griffith_Lab\\DIF data\\Screening Data\\Boston_Screening with MoCA date_02.24.2021.xlsx",
                                                  1,
                                                  startRow = 2)


# Merge Spanish data with English -----------------------------------------

# Make a list of English and Spanish names for NU Screening data
eng_names <- c("calc_age", "dob", "gender",
               "ethnicity", "race___1",
               "race___2", "race___3",
               "race___4", "race___5",
               "race___6", "race_other",
               "education", "national_origin",
               "sils", "marital_status",
               "employment___1", "employment___2",
               "employment___3", "employment___4",
               "employment___5", "employment___6",
               "employment___7", "employment___8",
               "employment___9")
span_names <- c("calc_age_span", "dob_span", "gender_span",
                "ethnicity_span", "race_span___1",
                "race_span___2", "race_span___3",
                "race_span___4", "race_span___5",
                "race_span___6", "acer_otro_span",
                "education_span", "nat_origin_span",
                "sils_span", "marital_status_span",
                "employment_span___1", "employment_span___2",
                "employment_span___3", "employment_span___4",
                "employment_span___5", "employment_span___6",
                "employment_span___7", "employment_span___8",
                "employment_span___9")

# Copy Spanish data into English variable names
for(j in seq_along(eng_names)) {
  for(i in seq_along(row.names(nu_screening))) {
    nu_screening[i, eng_names[j]] <-
      span_or_eng(nu_screening[i, "language_determination"],
                  nu_screening[i, eng_names[j]],
                  nu_screening[i, span_names[j]])
  }
}

# Drop Spanish variables
# These have been merged into the columns with English
nu_screening <- nu_screening[, !(names(nu_screening) %in% span_names)]

# Remove unnecessary variables
rm(eng_names, span_names, i, j)

# Rename race_other to race_other_text
names(nu_screening)[which(names(nu_screening) %in% "race_other")] <- "race_other_text"

# Make a list of English and Spanish variable names for BUMC
eng_names <- c("education", "employment",
               "marital_status", "sils")

span_names <- c("education_span", "employment_span",
                "marital_status_span", "sils_span")

# Copy Spanish data into English variable name
for(j in seq_along(eng_names)) {
  for(i in seq_along(row.names(bumc_screening))) {
    bumc_screening[i, eng_names[j]] <-
      span_or_eng(bumc_screening[i, "language_determination"],
                  bumc_screening[i, eng_names[j]],
                  bumc_screening[i, span_names[j]])
  }
}

# Drop Spanish variables
# These have been merged into the columns with English
bumc_screening <- bumc_screening[, !(names(bumc_screening) %in% span_names)]

rm(eng_names, span_names, i, j)

# Rename race_other to race_other_text
names(bumc_screening)[which(names(bumc_screening) %in% "race_other")] <- "race_other_text"


# Calculate age -----------------------------------------------------------

# Note 5413 (Spanish) is missing screening date and is missing in the file from 2/8/21

# https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf

# Calculate age at screening in years
nu_screening$age_screening <- time_length(
  difftime(mdy(nu_screening$screen_date),
           mdy(nu_screening$dob)), "years")

# Revised and cleaned-up screening data
# We will analyse English and Spanish separately
# Here we will pull out English variables here

to_rename <- c("study_id",
               "language_determination",
               "race___1",
               "race___2",
               "race___3",
               "race___4",
               "race___5",
               "race___6",
               "employment___1",
               "employment___2",
               "employment___3",
               "employment___4",
               "employment___5",
               "employment___6",
               "employment___7",
               "employment___8",
               "employment___9")

new_names <- c("id",
               "language",
               "race_black",
               "race_nat_amer",
               "race_asian",
               "race_pac_islander",
               "race_white",
               "race_other",
               "empl_full_time",
               "empl_part_time",
               "empl_hmaker",
               "empl_self",
               "empl_unable_work",
               "empl_student",
               "empl_retired",
               "empl_unem_lt1yr",
               "empl_unem_mt1yr")

# Rename some items to facilitate interpretation
names(nu_screening)[which(names(nu_screening) %in% to_rename)] <- new_names

nu_screening$site <- "Northwestern"

# Remove objects no longer needed from R environment
rm(to_rename, new_names)

# BUMC Screening
# names(bumc_screening)

to_rename <- c("study_id",
               "language_determination",
               "age_eng",
               "race___1",
               "race___2",
               "race___3",
               "race___4",
               "race___5",
               "race___6",
               "hispanic",
               "origin")

new_names <- c("id",
               "language",
               "age_screening",
               "race_black",
               "race_nat_amer",
               "race_asian",
               "race_pac_islander",
               "race_white",
               "race_other",
               "ethnicity",
               "national_origin")

# Rename some items to facilitate interpretation
names(bumc_screening)[which(names(bumc_screening) %in% to_rename)] <- new_names

# 5060 1 = Employed, full time
# 5025 2 = employed part-time
# 5169 3 = Homemaker
# 5002 4 = self-employed
# 5003 5 = Unable to work
# 5260 6 = Student
# 5001 7 = retired
# 5023 8 = None of the above, out of work for less than 1 year
# 5010 9 = None of the above, out of work for more than 1 year

bumc_screening$empl_full_time <- bumc_screening$employment == 1
bumc_screening$empl_part_time <- bumc_screening$employment == 2
bumc_screening$empl_hmaker <- bumc_screening$employment == 3
bumc_screening$empl_self <- bumc_screening$employment == 4
bumc_screening$empl_unable_work <- bumc_screening$employment == 5
bumc_screening$empl_student <- bumc_screening$employment == 6
bumc_screening$empl_retired <- bumc_screening$employment == 7
bumc_screening$empl_unem_lt1yr <- bumc_screening$employment == 8
bumc_screening$empl_unem_mt1yr <- bumc_screening$employment == 9

bumc_screening$site <- "BUMC"

# Recode BUMC data to match NU --------------------------------------------

# Employment
bumc_screening$empl_full_time <-
  ifelse(bumc_screening$empl_full_time, 1, 0)

bumc_screening$empl_part_time <-
  ifelse(bumc_screening$empl_part_time, 1, 0)

bumc_screening$empl_hmaker <-
  ifelse(bumc_screening$empl_hmaker, 1, 0)

bumc_screening$empl_self <-
  ifelse(bumc_screening$empl_self, 1, 0)

bumc_screening$empl_unable_work <-
  ifelse(bumc_screening$empl_unable_work, 1, 0)

bumc_screening$empl_student <-
  ifelse(bumc_screening$empl_student, 1, 0)

bumc_screening$empl_retired <-
  ifelse(bumc_screening$empl_retired, 1, 0)

bumc_screening$empl_unem_lt1yr <-
  ifelse(bumc_screening$empl_unem_lt1yr, 1, 0)

bumc_screening$empl_unem_mt1yr <-
  ifelse(bumc_screening$empl_unem_mt1yr, 1, 0)

# Use lubridate to calculate age at screening
bumc_dob$age_screening <- time_length(difftime(mdy(bumc_dob$Screening.Date),
                                               mdy(bumc_dob$Date.of.Birth)), "years")

bumc_dob$Screening.Date <- as.Date(mdy(bumc_dob$Screening.Date))
bumc_dob$Date.of.Birth <- as.Date(mdy(bumc_dob$Date.of.Birth))

# Rename "Study.ID" to "id"
names(bumc_dob)[which(names(bumc_dob) == "Study.ID")] <- "id"
names(bumc_dob)[which(names(bumc_dob) == "Date.of.Birth")] <- "dob"
names(bumc_dob)[which(names(bumc_dob) == "Screening.Date")] <- "screen_date"

# Remove unnecessary variables from NU
vars_to_delete <- c("randomization_gender", "calc_age", "literacy_group",
                    "date_time_t1", "date_time_t2", "date_time_t3")

nu_screening <-
  nu_screening[, !(names(nu_screening) %in% vars_to_delete)]

# Remove unnecessary variables from the environment
rm(vars_to_delete)

# Remove unnecessary variables from BUMC
vars_to_delete <- c("age_screening", "literacy_group", "employment")

bumc_screening <-
  bumc_screening[, !(names(bumc_screening) %in% vars_to_delete)]

rm(vars_to_delete)

# Merge BUMC DOBs with screening data
bumc_screening <- merge(bumc_dob[, c("id", "dob", "screen_date", "age_screening")],
                        bumc_screening,
                        by = "id",
                        all.x = FALSE,
                        all.y = TRUE)

new_order <- c("id", "language", "dob", "screen_date",
               "gender",  "ethnicity", "race_black",
               "race_nat_amer", "race_asian", "race_pac_islander",
               "race_white", "race_other", "race_other_text",
               "education", "national_origin", "sils",
               "numi_score", "marital_status", "empl_full_time",
               "empl_part_time", "empl_hmaker", "empl_self",
               "empl_unable_work", "empl_student", "empl_retired",
               "empl_unem_lt1yr", "empl_unem_mt1yr", "mocab_adjustedscore",
               "moca8_1_final_score", "rudas_final_score", "age_screening",
               "site")

# Reorder screening database such that they match exactly
bumc_screening <- bumc_screening[new_order]
nu_screening <- nu_screening[new_order]

# Re-format Northwestern dates
nu_screening$dob <- as.Date(mdy(nu_screening$dob))
nu_screening$screen_date <- as.Date(mdy(nu_screening$screen_date))

# This is the screening data for NU and BUMC merged
screening <- rbind(nu_screening,
                   bumc_screening)

# Have a look at the screening data
# View(screening)
# names(screening)


# Code employment variables as missing if none are checked ----------------

empl_vars <- c("empl_full_time",
               "empl_part_time",
               "empl_hmaker",
               "empl_self",
               "empl_unable_work",
               "empl_student",
               "empl_retired",
               "empl_unem_lt1yr",
               "empl_unem_mt1yr")

# Count the number of employment variables checked
screening$num_empl_cats <- rowSums(screening[empl_vars], na.rm = TRUE)

# Set the responses to NA if no employment variables were checked
screening[screening$num_empl_cats == 0, empl_vars] <- NA

# Make site a factor
screening$site <- factor(screening$site)

# Select English only -----------------------------------------------------

# screening_eng <- screening[screening$language == 1, ]


# Read in Health LiTT (English and Spanish) -------------------------------
health_litt_english <- read.csv("R:\\MSS\\Research\\Projects\\Griffith_Lab\\DIF data\\PRO Data\\HealthLiTTEnglishScores_03.04.2021.csv",
                                header = TRUE)

health_litt_spanish <- read.csv("R:\\MSS\\Research\\Projects\\Griffith_Lab\\DIF data\\PRO Data\\HealthLiTTSpanishScores_03.04.2021.csv",
                                header = TRUE)

health_litt <- rbind(health_litt_english, health_litt_spanish)

# View(health_litt)

health_litt$T_score <- theta_to_T(health_litt$Score)

# Create "id" variable
health_litt$id <- health_litt$Last.Name

# This is our data frame to analyse
# Merge Health LiTT data with screening data
hl_screening <- merge(screening,
                      health_litt[, c("id", "T_score", "Passed")],
                      by = "id",
                      all.x = TRUE,
                      all.y = TRUE)

# Delete cases that have
# Maybe drop 5360 as well for incomplete screening
# 5403 also terminated
hl_screening <- hl_screening[!hl_screening$id %in% c(5330, 5050, 5183, 5360, 5403), ]

# View(hl_screening)


# Recode race and ethnicity into a single variable ------------------------

single_race_var <- function(ethnicity, asian, black,
                            pac_islander, nat_amer,
                            white, other, text) {

  # Set to all possible Hispanic race text in "other"
  hispanic_race <- c("Mexican-American", "Latino", "hispanic", "hispanic/puerto rican",
                     "Puerto Rican", "hISPANIC", "Puerto Rico",
                     "hispanic , puerto rican", "latina", "Puerto rican n colombian")

  # Count number of races checked
  n_races <- sum(c(asian, black, pac_islander, nat_amer, white, other), na.rm = TRUE)

  if (is.na(ethnicity) && n_races == 0) {
    race <- NA
    return(race)
  }

  if (is.na(ethnicity)) {
    ethnicity <- ""
  }

  if(ethnicity == 1 || text %in% hispanic_race) {
    race <- "Hispanic"
  } else if (n_races > 1) {
    race <- "More than one race"
  } else if (asian == 1) {
    race <- "Asian"
  } else if (black == 1) {
    race <- "Black"
  } else if (pac_islander == 1) {
    race <- "Pacific islander"
  } else if (nat_amer == 1) {
    race <- "Native American"
  } else if (white == 1) {
    race <- "White"
  } else if (other == 1) {
    race <- "Other"
  }

  if (is.na(text)) {
    text <- ""
  }

  # Recode based on "other" text
  if (text == "Multirace") {
    race <- "More than one race"
  } else if (text == "Prefer not to say") {
    race <- NA
  } else if (text == "Not specified") {
    race <- "Other"
  } else if (text == "Human Being") {
    race <- NA
  } else if (text == "American") {
    race <- NA
  } else if (text == "African American and Causasian") {
    race <- "More than one race"
  } else if (text == "African American and Caucasian") {
    race <- "More than one race"
  } else if (text == "Irish, Phillipino and American") {
    race <- "More than one race"
  } else if (text == "egyptian") {
    race <- "White"
  }

  return(race)

}

for(i in 1:nrow(hl_screening)) {
  hl_screening[i, "race_sv"] <-
    single_race_var(hl_screening[i, "ethnicity"],
                    hl_screening[i, "race_asian"],
                    hl_screening[i, "race_black"],
                    hl_screening[i, "race_pac_islander"],
                    hl_screening[i, "race_nat_amer"],
                    hl_screening[i, "race_white"],
                    hl_screening[i, "race_other"],
                    hl_screening[i, "race_other_text"])
}

hl_screening$race_sv <- factor(hl_screening$race_sv,
                               ordered = FALSE)

# table(hl_screening_eng$race_sv)
# levels(hl_screening_eng$race_sv)

# Collapse "More than one" and "Other"
hl_screening$race_sv_col <- factor(hl_screening$race_sv,
                                   ordered = FALSE,
                                   levels = levels(hl_screening$race_sv),
                                   labels = c("Asian",
                                              "Black",
                                              "Hispanic",
                                              "More than one; Other",
                                              "More than one; Other",
                                              "More than one; Other",
                                              "White"))

# table(hl_screening$race_sv_col)

# apply(hl_screening[empl_vars],
2,
table,
useNA = "always")

any_checked <- function(x) {

  x <- as.vector(x)

  if(all(is.na(x))){
    checked <- NA
  } else {
    checked <- 1 %in% x
  }

  return(checked)

}

# employed will equal TRUE is the person is full-time, part-time, or self-employed
hl_screening$employed <- apply(hl_screening[, c("empl_full_time",
                                                "empl_part_time",
                                                "empl_self")],
                               1,
                               any_checked)

# table(hl_screening_eng$employed, useNA = "always")

# Collapse education
hl_screening$education_factor <-
  factor(hl_screening$education,
         levels = 0:15,
         labels = c(
           "None",
           "1st grade",
           "2nd grade",
           "3rd grade",
           "4th grade",
           "5th grade",
           "6th grade",
           "7th grade",
           "8th grade",
           "9th grade",
           "10th grade",
           "11th grade",
           "12th grade",
           "Some college/technical degree/AA",
           "College degree (BA/BS)",
           "Advanced Degree (MA, MS, MBA, PhD, MD, JD)"),
         ordered =  TRUE)

# Collapse the education factor
hl_screening$education_factor_col <-
  factor(hl_screening$education,
         levels = 0:15,
         labels = c("Less than 12th grade", # None
                    "Less than 12th grade", # 1st grade
                    "Less than 12th grade", # 2nd grade
                    "Less than 12th grade", # 3rd grade
                    "Less than 12th grade", # 4th grade
                    "Less than 12th grade", # 5th grade
                    "Less than 12th grade", # 6th grade
                    "Less than 12th grade", # 7th grade
                    "Less than 12th grade", # 8th grade
                    "Less than 12th grade", # 9th grade
                    "Less than 12th grade", # 10th grade
                    "Less than 12th grade", # 11th grade
                    "12th grade",           # 12th grade
                    "Some college/technical/Associate's", # Some college
                    "Bachelor's degree of higher", # College degree
                    "Bachelor's degree of higher"), # Advanced degree
         ordered =  TRUE)

# Collapse the education factor - Binary
hl_screening$education_factor_bin <-
  factor(hl_screening$education,
         levels = 0:15,
         labels = c("Some college or less", # None
                    "Some college or less", # 1st grade
                    "Some college or less", # 2nd grade
                    "Some college or less", # 3rd grade
                    "Some college or less", # 4th grade
                    "Some college or less", # 5th grade
                    "Some college or less", # 6th grade
                    "Some college or less", # 7th grade
                    "Some college or less", # 8th grade
                    "Some college or less", # 9th grade
                    "Some college or less", # 10th grade
                    "Some college or less", # 11th grade
                    "Some college or less", # 12th grade
                    "Some college or less", # Some college
                    "Bachelor's degree of higher", # College degree
                    "Bachelor's degree of higher")) # Advanced degree)

hl_screening$sils <- factor(hl_screening$sils,
                            levels = 1:5,
                            labels = c("Never",
                                       "Rarely",
                                       "Sometimes",
                                       "Often",
                                       "Always"),
                            ordered = TRUE)

hl_screening$sils_low_hl <- factor(hl_screening$sils,
                                   levels = c("Never",
                                              "Rarely",
                                              "Sometimes",
                                              "Often",
                                              "Always"),
                                   labels = c("Screen negative: Not low HL",
                                              "Screen negative: Not low HL",
                                              "Screen positive: Low HL",
                                              "Screen positive: Low HL",
                                              "Screen positive: Low HL"))


# Change variables to factors for analyses --------------------------------

hl_screening$gender <- factor(hl_screening$gender,
                              levels = 1:2,
                              labels = c("male", "female"))


# Rename some variables to make them more user friendly -------------------

names(hl_screening)[which(names(hl_screening) == "T_score")] <- "Health_LiTT_T_Score"
names(hl_screening)[which(names(hl_screening) == "Passed")] <- "Health_LiTT_adequate"

View(hl_screening)

save(hl_screening,
     file = "hl_screening.rdata")

load("C:/Users/jgr576/Desktop/Ellie's master's/hl_screening.rdata")


# Randomly divide data into two halves ------------------------------------

# Select English participants
hl_screening_eng <- hl_screening[hl_screening$language == 1, ]

set.seed(010440)

n <- nrow(hl_screening_eng)

prop_to_sample <- .8

n_to_sample <- round(n * prop_to_sample)

training_sample <- sample(hl_screening_eng$id, n_to_sample)

test_sample <- hl_screening_eng$id[!hl_screening_eng$id %in% training_sample]

hl_r1 <- hl_screening_eng[hl_screening_eng$id %in% training_sample, ]

# Clean up environment
rm(i, n, prop_to_sample, n_to_sample)
rm(to_rename, new_names, new_order)

# Have a look at the first half of the data
head(hl_r1)
View(hl_r1)


# Descriptive statistics --------------------------------------------------

# https://www.r-bloggers.com/2016/02/table-1-and-the-characteristics-of-study-population/

names(hl_r1)

# Create a variable list which we want in Table 1
list_vars <- c("age_screening",
               "Health_LiTT_T_Score",
               "gender",
               #              "education_factor_col",
               "education_factor_bin",
               "employed",
               "race_sv_col",
               "sils_low_hl")

# Define categorical variables
cat_vars <- c("gender",
              #              "education_factor_col",
              "education_factor_bin",
              "employed",
              "race_sv_col",
              "sils_low_hl")

# Total Population
table1 <- CreateTableOne(vars = list_vars,
                         data = hl_r1,
                         factorVars = cat_vars,
                         includeNA = TRUE)
table1


# Graphs ------------------------------------------------------------------

# Look at continuous variables first
# Age at screening
age_density <- ggplot(hl_r1) +
  geom_density(aes(age_screening),
               colour = "darkgreen",
               alpha = 1 / 3,
               fill = "darkgreen") +
  geom_vline(data = hl_r1,
             aes(xintercept = mean(age_screening, na.rm = TRUE)),
             linetype = "dashed",
             colour = "darkgreen") +
  xlab("Age at screening (Years)") +
  geom_text(aes(x = 5 + mean(age_screening, na.rm = TRUE),
                label = paste0("Mean\n",
                               mean(age_screening,
                                    na.rm = TRUE) %>% round(., 1)),
                y = 0.01),
            colour = "darkgreen")
age_density

health_litt_density <- ggplot(hl_r1) +
  geom_density(aes(Health_LiTT_T_Score),
               colour = "darkblue",
               alpha = 1 / 3,
               fill = "darkblue") +
  geom_vline(data = hl_r1,
             aes(xintercept = mean(Health_LiTT_T_Score, na.rm = TRUE)),
             linetype = "dashed",
             colour = "darkblue") +
  xlab("Health LiTT T Score") +
  geom_text(aes(x = 3 + mean(Health_LiTT_T_Score, na.rm = TRUE),
                label = paste0("Mean\n",
                               mean(Health_LiTT_T_Score,
                                    na.rm = TRUE) %>% round(., 1)),
                y = 0.02),
            colour = "darkblue")
health_litt_density

# Scatterplot
age_hl <- ggplot(hl_r1, aes(age_screening, Health_LiTT_T_Score)) +
  aes(colour = gender) +
  scale_colour_manual(values = c("navyblue", "red")) +
  geom_point(alpha = 1 / 4) +
  geom_smooth(method = "loess",
              se = FALSE,
              size = 0.8,
              formula = y ~ x) +
  xlim(c(18, 90))
age_hl


# Regressions -------------------------------------------------------------

hl_r1$race_sv_col <- relevel(hl_r1$race_sv_col, ref = "White")
hl_r1$education_factor_bin

# Look at race by education
table(hl_r1$education_factor_col, hl_r1$race_black)

reg_vars <- c("id",
              "gender",
              "age_screening",
              "Health_LiTT_T_Score",
              "race_sv_col",
              "employed",
              "education_factor_bin",
              "sils_low_hl")

hl_reg <- na.omit(hl_r1[, reg_vars])

table1_reg <- CreateTableOne(vars = reg_vars[-1],
                             data = hl_reg,
                             factorVars = reg_vars[c(2, 5:8)],
                             includeNA = TRUE)
table1_reg

# Consider interaction effects
eng_model <- lm(Health_LiTT_T_Score ~
                  sils_low_hl +
                  gender +
                  race_sv_col +
                  education_factor_bin +
                  employed +
                  age_screening,
                data = hl_reg)
summary(eng_model)

# Take out race/ethnicity
reduced_eng_model <- lm(Health_LiTT_T_Score ~
                          sils_low_hl +
                          gender +
                          education_factor_bin +
                          employed +
                          age_screening,
                        data = hl_reg)
summary(reduced_eng_model)
anova(eng_model, reduced_eng_model)

# Save residuals (note "e" is for error, i.e., the residual)
hl_e <- resid(eng_model)
hl_p <- fitted(eng_model)

# Link residual data
e_plot_data <- as.data.frame(cbind(hl_e, hl_p))

# Make the plot
residual_plot <- ggplot(e_plot_data, aes(x = hl_p, y = hl_e)) +
  geom_point(alpha = 1 / 4) +
  geom_hline(yintercept = 0) +
  ggtitle("Health LiTT: residuals plot") +
  ylab("Residual") +
  xlab("Predicted value")
residual_plot

res_density <- ggplot(e_plot_data) +
  geom_density(aes(hl_e),
               colour = "darkblue",
               alpha = 1 / 3,
               fill = "darkblue") +
  geom_vline(data = e_plot_data,
             aes(xintercept = mean(hl_e, na.rm = TRUE)),
             linetype = "dashed",
             colour = "darkblue") +
  xlab("Residual") +
  geom_text(aes(x = 3 + mean(hl_e, na.rm = TRUE),
                label = paste0("Mean\n",
                               mean(hl_e,
                                    na.rm = TRUE) %>% round(., 1)),
                y = 0.02),
            colour = "darkblue") +
  xlim(c(-20, 20))
res_density


# Look at HL as a binary dependent variable -------------------------------

hl_reg$low_hl <- cut(hl_reg$Health_LiTT_T_Score,
                     breaks = c(-Inf, 55, Inf),
                     labels = c("Low HL", "Adequate HL"),
                     right = FALSE) %>%
  relevel(hl_reg$low_hl, ref = "Adequate HL")

hl_glm <- glm(low_hl ~
                sils_low_hl +
                gender +
                race_sv_col +
                education_factor_bin +
                employed +
                age_screening,
              data = hl_reg,
              family = "binomial")
summary(hl_glm)

exp(coef(hl_glm))

hl_glm_red <- glm(low_hl ~
                    sils_low_hl +
                    gender +
                    employed +
                    age_screening,
                  data = hl_reg,
                  family = "binomial")
summary(hl_glm_red)

exp(coef(hl_glm_red))

# k-fold regression -------------------------------------------------------


