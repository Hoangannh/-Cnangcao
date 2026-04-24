# DATA423-26S1 Assignment 2
# Hoang Anh Nguyen - 82885328

# ---- Libraries -----
library(shiny)
library(ggplot2)
library(GGally)
library(vcd)
library(corrgram)
library(visdat)
library(DT)
library(car)
library(dplyr)
library(tidyr)

# Used later by the Strategy and Modeling tabs
library(rpart)
library(rpart.plot)
library(recipes)
library(caret)
library(glmnet)


# ---- Load data ----
# Step 2 of The Details: list common placeholder strings. "--" was observed
# in GOVERN_TYPE during a raw-CSV inspection, so it goes here too.
# Step 3: read.csv with na.strings and stringsAsFactors = TRUE.
dat <- read.csv(
  "Ass2Data.csv",
  header           = TRUE,
  na.strings       = c("", " ", "NA", "N/A", "na", "n/a", "?", ".", "--"),
  stringsAsFactors = TRUE
)

# Step 4: replace -1 and -99 with NA in numeric columns. Both values were
# seen as placeholders in AGE25_PROPTN, AGE50_PROPTN, DOCS and VAX_RATE.
num_idx <- sapply(dat, is.numeric)
dat[num_idx] <- lapply(dat[num_idx], function(x) {
  x[x %in% c(-1, -99)] <- NA
  x
})

# Steps 5 and 6 of The Details (Not-Applicable level for categoricals,
# shadow variable for structural numeric NAs) live in the Strategy tabs
# so the user choice flows through the reactive cascade.


# ---- Column roles ----
id_col           <- "CODE"
split_col        <- "OBS_TYPE"
target_col       <- "DEATH_RATE"

categorical_cols <- c("GOVERN_TYPE", "HEALTHCARE_BASIS", "OBS_TYPE")

numeric_cols     <- c("POPULATION", "AGE25_PROPTN", "AGE_MEDIAN",
                      "AGE50_PROPTN", "POP_DENSITY", "GDP",
                      "INFANT_MORT", "DOCS", "VAX_RATE",
                      "HEALTHCARE_COST", "DEATH_RATE")

predictor_numeric <- setdiff(numeric_cols, target_col)
all_predictors    <- c(setdiff(categorical_cols, split_col), predictor_numeric)

# ---- Helpers ----
# Proportion of NAs in a vector. Used by the cleaning reactives later.
pMiss <- function(x) { sum(is.na(x)) / length(x) }
