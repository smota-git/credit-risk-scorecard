# Credit Risk Scorecard Modeling
#
# End-to-end R script for building a credit-risk scorecard model.
#
# Workflow:
# 1. Load the home equity loan dataset.
# 2. Create variable-level bins while handling missing values separately for
#    each variable.
# 3. Calculate default rates, Weight of Evidence (WoE), and Information Value
#    (IV) for all explanatory variables.
# 4. Fit a logistic regression model using the selected WoE-transformed
#    predictors.
# 5. Evaluate model discrimination on training and validation samples using
#    AUC/Gini.
# 6. Convert predicted default probabilities into scorecard points and produce
#    a validation decile summary.
#
# Modeling conventions:
# - Numeric variables are binned by sorting non-missing values and splitting the
#   ordered observations into approximately equal-sized groups.
# - Missing values are not removed globally; they are handled only for the
#   variable currently being analyzed.
# - DEBTINC missing values are assigned to a separate bin.
# - The final logistic model uses the selected predictors CLAGE, DEBTINC,
#   DELINQ, DEROG, and NINQ.

library(data.table)
library(openxlsx)
library(pROC)

# -----------------------------------------------------------------------------
# User settings

input_file <- "Home_Eq_Dataset.xlsx"
target_var <- "BAD"

# All explanatory variables described in the report.
all_variables <- c(
  "LOAN", "MORTDUE", "VALUE", "REASON", "JOB", "YOJ",
  "DEROG", "DELINQ", "CLAGE", "NINQ", "CLNO", "DEBTINC"
)

# Variables used in the final logistic scorecard model.
model_variables <- c("CLAGE", "DEBTINC", "DELINQ", "DEROG", "NINQ")

# -----------------------------------------------------------------------------
# Helper functions

safe_log_ratio <- function(good, total_good, bad, total_bad) {
  # WoE formula used in the original analysis.
  # eps is kept at zero, so no smoothing is applied.
  eps <- 0
  log(((good + eps) / (total_good + eps)) / ((bad + eps) / (total_bad + eps)))
}

compute_woe_iv <- function(dt, target, bin_var) {
  woe_table <- dt[, .(
    bad = sum(get(target) == 1, na.rm = TRUE),
    good = sum(get(target) == 0, na.rm = TRUE)
  ), by = bin_var]

  woe_table[, defaults_ratio := bad/(bad+good)]
  
  var <- substr(bin_var, 1, nchar(bin_var)-4)

  bin_ranges <- dt[
    ,
    .(
      bin_min = min(get(var), na.rm = TRUE),
      bin_max = max(get(var), na.rm = TRUE)
    ),
    by = bin_var
  ]

  woe_table <- merge(
    woe_table,
    bin_ranges,
    by = bin_var,
    all.x = TRUE
  )
  
  setorderv(woe_table, bin_var)

  total_bad <- sum(woe_table$bad)
  total_good <- sum(woe_table$good)

  woe_table[, `:=`(
    bad_dist = bad / total_bad,
    good_dist = good / total_good
  )]

  woe_table[, woe := safe_log_ratio(good, total_good, bad, total_bad)]
  woe_table[, iv_bin := (good_dist - bad_dist) * woe]
  woe_table[, iv_total := sum(iv_bin)]

  woe_table[]
}

add_numeric_bins <- function(dt, variable, bin_name, n_bins = 5) {
  # Create equal-frequency-style bins by sorting non-missing values and
  # splitting the ordered observations into consecutive groups. No cut() is
  # used, so bin assignment is based on row position after sorting.

  # Store original row positions so that bins can be assigned back to the
  # correct observations after sorting.
  row_id_col <- "__row_id_for_binning__"
  while (row_id_col %in% names(dt)) {
    row_id_col <- paste0(row_id_col, "_")
  }

  dt[, (row_id_col) := .I]
  dt[, (bin_name) := NA_integer_]

  var_dt <- dt[!is.na(get(variable)), .(
    row_id = get(row_id_col),
    value = as.numeric(get(variable))
  )]

  dt[, (row_id_col) := NULL]

  if (nrow(var_dt) == 0) {
    stop("Cannot create bins: variable has no non-missing values.")
  }

  setorder(var_dt, value)

  n <- nrow(var_dt)
  q <- trunc(n / n_bins)

  if (q == 0) {
    stop("Cannot create ", n_bins, " bins: variable has fewer non-missing values than bins.")
  }

  var_dt[, bin := NA_integer_]

  for (i in seq_len(n_bins)) {
    if (i < n_bins) {
      idx_from <- (i - 1L) * q + 1L
      idx_to <- i * q
    } else {
      idx_from <- (i - 1L) * q + 1L
      idx_to <- n
    }

    var_dt[idx_from:idx_to, bin := i]
  }

  # Assign bins back to the original rows using explicit original row ids.
  dt[var_dt$row_id, (bin_name) := var_dt$bin]

  bin_definition <- var_dt[, .(
    lower_bound = min(value, na.rm = TRUE),
    upper_bound = max(value, na.rm = TRUE)
  ), by = bin][order(bin)]

  return(bin_definition)
}

add_binary_zero_positive_bin <- function(dt, variable, bin_name) {
  # Two-bin split for count variables: zero vs. positive values.

  dt[!is.na(get(variable)) & get(variable) < 1, (bin_name) := 1L]
  dt[!is.na(get(variable)) & get(variable) >= 1, (bin_name) := 2L]

  data.table(
    bin = 1:2,
    lower_bound = c(-Inf, 1),
    upper_bound = c(1, Inf)
  )
}

add_ninq_bin <- function(dt, variable, bin_name) {
  # Three-bin split for inquiry counts: 0, 1, and 2 or more.

  dt[!is.na(get(variable)) & get(variable) < 1, (bin_name) := 1L]
  dt[!is.na(get(variable)) & get(variable) >= 1 & get(variable) < 2, (bin_name) := 2L]
  dt[!is.na(get(variable)) & get(variable) >= 2, (bin_name) := 3L]

  data.table(
    bin = 1:3,
    lower_bound = c(-Inf, 1, 2),
    upper_bound = c(1, 2, Inf)
  )
}

add_reason_bin <- function(dt, variable, bin_name) {
  # Keep the natural REASON categories as separate bins.
  categories <- unique(as.character(dt[!is.na(get(variable)), get(variable)]))

  dt[!is.na(get(variable)), (bin_name) := match(as.character(get(variable)), categories)]

  data.table(
    bin = seq_along(categories),
    category = categories
  )
}

add_job_bin <- function(dt, variable, bin_name) {
  # Group JOB categories with similar observed default rates.
  dt[!is.na(get(variable)) & get(variable) %in% c("Sales", "Self"), (bin_name) := 1L]
  dt[!is.na(get(variable)) & get(variable) %in% c("Mgr", "Other"), (bin_name) := 2L]
  dt[!is.na(get(variable)) & get(variable) %in% c("ProfExe", "Office"), (bin_name) := 3L]

  data.table(
    bin = 1:3,
    category = c("Sales + Self", "Mgr + Other", "ProfExe + Office")
  )
}


merge_woe <- function(dt, woe_table, bin_name, woe_name) {
  dt <- merge(
    dt,
    woe_table[, .SD, .SDcols = c(bin_name, "woe")],
    by = bin_name,
    all.x = TRUE
  )
  setnames(dt, "woe", woe_name)
  dt[]
}

# -----------------------------------------------------------------------------
# Load data

raw_data <- read.xlsx(input_file)
setDT(raw_data)

required_columns <- c(target_var, all_variables)
missing_columns <- setdiff(required_columns, names(raw_data))

if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

# Keep only variables needed for the analysis. Rows are not removed globally
# because missing values are handled separately for each variable.
dt <- copy(raw_data[, ..required_columns])

numeric_variables <- c(
  "LOAN", "MORTDUE", "VALUE", "YOJ", "DEROG", "DELINQ",
  "CLAGE", "NINQ", "CLNO", "DEBTINC"
)
dt[, (numeric_variables) := lapply(.SD, as.numeric), .SDcols = numeric_variables]

# -----------------------------------------------------------------------------
# Binning

# Numeric variables: sort non-missing values and split the ordered rows into
# five consecutive groups.
bin_def_LOAN <- add_numeric_bins(dt, variable = "LOAN", bin_name = "LOAN_BIN", n_bins = 5)
bin_def_MORTDUE <- add_numeric_bins(dt, variable = "MORTDUE", bin_name = "MORTDUE_BIN", n_bins = 5)
bin_def_VALUE <- add_numeric_bins(dt, variable = "VALUE", bin_name = "VALUE_BIN", n_bins = 5)
bin_def_YOJ <- add_numeric_bins(dt, variable = "YOJ", bin_name = "YOJ_BIN", n_bins = 5)
bin_def_CLAGE <- add_numeric_bins(dt, variable = "CLAGE", bin_name = "CLAGE_BIN", n_bins = 5)
bin_def_CLNO <- add_numeric_bins(dt, variable = "CLNO", bin_name = "CLNO_BIN", n_bins = 5)
bin_def_DEBTINC <- add_numeric_bins(dt, variable = "DEBTINC", bin_name = "DEBTINC_BIN", n_bins = 5)

# DEBTINC missing values carry useful risk information, so they are assigned
# to a separate bin after the five numeric bins are created.
DEBTINC_missing_bin <- max(dt$DEBTINC_BIN, na.rm = TRUE) + 1L
dt[is.na(DEBTINC), DEBTINC_BIN := DEBTINC_missing_bin]

bin_def_DEBTINC <- rbind(
  bin_def_DEBTINC,
  data.table(
    bin = DEBTINC_missing_bin,
    lower_bound = NA_real_,
    upper_bound = NA_real_
  )
)

# Count variables with a high concentration of zeros.
bin_def_DELINQ <- add_binary_zero_positive_bin(dt, variable = "DELINQ", bin_name = "DELINQ_BIN")
bin_def_DEROG <- add_binary_zero_positive_bin(dt, variable = "DEROG", bin_name = "DEROG_BIN")
bin_def_NINQ <- add_ninq_bin(dt, variable = "NINQ", bin_name = "NINQ_BIN")

# Categorical variables.
bin_def_REASON <- add_reason_bin(dt, variable = "REASON", bin_name = "REASON_BIN")
bin_def_JOB <- add_job_bin(dt, variable = "JOB", bin_name = "JOB_BIN")


print("Bin definitions:")
print(list(
  LOAN = bin_def_LOAN,
  MORTDUE = bin_def_MORTDUE,
  VALUE = bin_def_VALUE,
  REASON = bin_def_REASON,
  JOB = bin_def_JOB,
  YOJ = bin_def_YOJ,
  DEROG = bin_def_DEROG,
  DELINQ = bin_def_DELINQ,
  CLAGE = bin_def_CLAGE,
  NINQ = bin_def_NINQ,
  CLNO = bin_def_CLNO,
  DEBTINC = bin_def_DEBTINC
))

# -----------------------------------------------------------------------------
# WoE/IV calculation
# Calculate WoE/IV separately for each binned variable.

woe_LOAN <- compute_woe_iv(dt[!is.na(LOAN_BIN)], target_var, "LOAN_BIN")
woe_MORTDUE <- compute_woe_iv(dt[!is.na(MORTDUE_BIN)], target_var, "MORTDUE_BIN")
woe_VALUE <- compute_woe_iv(dt[!is.na(VALUE_BIN)], target_var, "VALUE_BIN")
woe_REASON <- compute_woe_iv(dt[!is.na(REASON_BIN)], target_var, "REASON_BIN")
woe_JOB <- compute_woe_iv(dt[!is.na(JOB_BIN)], target_var, "JOB_BIN")
woe_YOJ <- compute_woe_iv(dt[!is.na(YOJ_BIN)], target_var, "YOJ_BIN")
woe_DEROG <- compute_woe_iv(dt[!is.na(DEROG_BIN)], target_var, "DEROG_BIN")
woe_DELINQ <- compute_woe_iv(dt[!is.na(DELINQ_BIN)], target_var, "DELINQ_BIN")
woe_CLAGE <- compute_woe_iv(dt[!is.na(CLAGE_BIN)], target_var, "CLAGE_BIN")
woe_NINQ <- compute_woe_iv(dt[!is.na(NINQ_BIN)], target_var, "NINQ_BIN")
woe_CLNO <- compute_woe_iv(dt[!is.na(CLNO_BIN)], target_var, "CLNO_BIN")
woe_DEBTINC <- compute_woe_iv(dt[!is.na(DEBTINC_BIN)], target_var, "DEBTINC_BIN")

print("Information Value by variable:")
print(data.table(
  variable = c(
    "LOAN", "MORTDUE", "VALUE", "REASON", "JOB", "YOJ",
    "DEROG", "DELINQ", "CLAGE", "NINQ", "CLNO", "DEBTINC"
  ),
  IV = c(
    unique(woe_LOAN$iv_total),
    unique(woe_MORTDUE$iv_total),
    unique(woe_VALUE$iv_total),
    unique(woe_REASON$iv_total),
    unique(woe_JOB$iv_total),
    unique(woe_YOJ$iv_total),
    unique(woe_DEROG$iv_total),
    unique(woe_DELINQ$iv_total),
    unique(woe_CLAGE$iv_total),
    unique(woe_NINQ$iv_total),
    unique(woe_CLNO$iv_total),
    unique(woe_DEBTINC$iv_total)
  )
))

# -----------------------------------------------------------------------------
# Merge WoE values back to the modeling dataset
# Merge all WoE variables for inspection and later modeling.

dt <- merge_woe(dt, woe_LOAN, "LOAN_BIN", "LOAN_WOE")
dt <- merge_woe(dt, woe_MORTDUE, "MORTDUE_BIN", "MORTDUE_WOE")
dt <- merge_woe(dt, woe_VALUE, "VALUE_BIN", "VALUE_WOE")
dt <- merge_woe(dt, woe_REASON, "REASON_BIN", "REASON_WOE")
dt <- merge_woe(dt, woe_JOB, "JOB_BIN", "JOB_WOE")
dt <- merge_woe(dt, woe_YOJ, "YOJ_BIN", "YOJ_WOE")
dt <- merge_woe(dt, woe_DEROG, "DEROG_BIN", "DEROG_WOE")
dt <- merge_woe(dt, woe_DELINQ, "DELINQ_BIN", "DELINQ_WOE")
dt <- merge_woe(dt, woe_CLAGE, "CLAGE_BIN", "CLAGE_WOE")
dt <- merge_woe(dt, woe_NINQ, "NINQ_BIN", "NINQ_WOE")
dt <- merge_woe(dt, woe_CLNO, "CLNO_BIN", "CLNO_WOE")
dt <- merge_woe(dt, woe_DEBTINC, "DEBTINC_BIN", "DEBTINC_WOE")

# -----------------------------------------------------------------------------
# Modeling dataset
# The final logistic model requires complete WoE predictors for the selected
# model variables.

model_vars <- c(
  target_var,
  "CLAGE_WOE", "DEBTINC_WOE", "DELINQ_WOE", "DEROG_WOE", "NINQ_WOE"
)

model_data <- dt[complete.cases(dt[, ..model_vars]), ..model_vars]

if (nrow(model_data) == 0) {
  stop("No complete records available for model fitting after WoE transformation.")
}

# -----------------------------------------------------------------------------
# Train/validation split and logistic regression

set.seed(123)
train_idx <- sample(seq_len(nrow(model_data)), size = floor(0.7 * nrow(model_data)))
train <- model_data[train_idx]
valid <- model_data[-train_idx]

model <- glm(
  BAD ~ CLAGE_WOE + DEBTINC_WOE + DELINQ_WOE + DEROG_WOE + NINQ_WOE,
  family = binomial,
  data = train
)

print(summary(model))

train[, PD := predict(model, newdata = train, type = "response")]
valid[, PD := predict(model, newdata = valid, type = "response")]

roc_train <- roc(train$BAD, train$PD, quiet = TRUE)
roc_valid <- roc(valid$BAD, valid$PD, quiet = TRUE)

gini_train <- as.numeric(2 * auc(roc_train) - 1)
gini_valid <- as.numeric(2 * auc(roc_valid) - 1)

print(data.table(
  sample = c("training", "validation"),
  AUC = c(as.numeric(auc(roc_train)), as.numeric(auc(roc_valid))),
  Gini = c(gini_train, gini_valid)
))

# -----------------------------------------------------------------------------
# Scorecard scaling

PDO <- 20
score_ref <- 600
PD_ref <- 1 / 51  # approximately 2%; odds of non-default to default = 50:1

Factor <- -PDO / log(2)
Offset <- score_ref - Factor * log(PD_ref / (1 - PD_ref))

valid[, logit_score := predict(model, newdata = valid, type = "link")]
valid[, score := Offset + Factor * logit_score]

# -----------------------------------------------------------------------------
# Decile analysis

# Validation deciles based on predicted default probability. Decile 1 contains
# the lowest-risk observations and decile 10 the highest-risk observations.
valid[, decile := cut(
  PD,
  breaks = unique(quantile(PD, probs = seq(0, 1, 0.1), na.rm = TRUE)),
  include.lowest = TRUE,
  labels = FALSE
)]

score_decile <- valid[, .(
  n = .N,
  bad_rate = mean(BAD),
  mean_score = mean(score)
), by = decile][order(decile)]

print("Validation score deciles:")
print(score_decile)

# Optional exports:
# fwrite(woe_LOAN, "woe_LOAN.csv")
# fwrite(woe_MORTDUE, "woe_MORTDUE.csv")
# fwrite(woe_VALUE, "woe_VALUE.csv")
# fwrite(woe_REASON, "woe_REASON.csv")
# fwrite(woe_JOB, "woe_JOB.csv")
# fwrite(woe_YOJ, "woe_YOJ.csv")
# fwrite(woe_DEROG, "woe_DEROG.csv")
# fwrite(woe_DELINQ, "woe_DELINQ.csv")
# fwrite(woe_CLAGE, "woe_CLAGE.csv")
# fwrite(woe_NINQ, "woe_NINQ.csv")
# fwrite(woe_CLNO, "woe_CLNO.csv")
# fwrite(woe_DEBTINC, "woe_DEBTINC.csv")
# fwrite(score_decile, "score_decile.csv")
