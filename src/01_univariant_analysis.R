# Univariate Analysis for Credit Risk Scorecard Modeling
#
# This script summarizes each explanatory variable before model development.
# For numeric variables, non-missing observations are sorted and split into five
# consecutive groups, matching the binning approach used in the original
# analysis. For categorical variables, default rates are calculated by category.

library(data.table)
library(openxlsx)

# -----------------------------------------------------------------------------
# User settings

input_file <- "Home_Eq_Dataset.xlsx"
target_var <- "BAD"

numeric_variables <- c(
  "LOAN", "MORTDUE", "VALUE", "YOJ", "DEROG", "DELINQ",
  "CLAGE", "NINQ", "CLNO", "DEBTINC"
)

categorical_variables <- c("REASON", "JOB")

# -----------------------------------------------------------------------------
# Helper functions

make_position_bins <- function(dt, variable, target = "BAD", n_bins = 5) {
  # Reproduce the original numeric binning logic:
  # 1. remove missing values only for the analyzed variable,
  # 2. sort the remaining observations by the variable value,
  # 3. split ordered rows into n_bins consecutive groups,
  # 4. summarize bad rate and value range in each bin.

  var_dt <- dt[!is.na(get(variable)), .(
    BAD = get(target),
    value = as.numeric(get(variable))
  )]

  if (nrow(var_dt) == 0) {
    stop("Variable ", variable, " has no non-missing observations.")
  }

  setorder(var_dt, value)

  n <- nrow(var_dt)
  q <- trunc(n / n_bins)

  if (q == 0) {
    stop("Variable ", variable, " has fewer non-missing observations than bins.")
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

  var_dt[, .(
    n = .N,
    defaults = sum(BAD == 1, na.rm = TRUE),
    non_defaults = sum(BAD == 0, na.rm = TRUE),
    bad_rate = mean(BAD == 1, na.rm = TRUE),
    min_value = min(value, na.rm = TRUE),
    max_value = max(value, na.rm = TRUE)
  ), by = bin][order(bin)]
}

summarize_categorical <- function(dt, variable, target = "BAD") {
  # Calculate default rates for natural categories of a categorical variable.

  dt[!is.na(get(variable)), .(
    n = .N,
    defaults = sum(get(target) == 1, na.rm = TRUE),
    non_defaults = sum(get(target) == 0, na.rm = TRUE),
    bad_rate = mean(get(target) == 1, na.rm = TRUE)
  ), by = variable][order(get(variable))]
}

summarize_missing <- function(dt, variables) {
  # Count missing observations for all variables used in the analysis.

  data.table(
    variable = variables,
    missing_count = sapply(variables, function(v) sum(is.na(dt[[v]]))),
    total_count = nrow(dt)
  )[, missing_rate := missing_count / total_count][]
}

# -----------------------------------------------------------------------------
# Load data

excel_data <- read.xlsx(input_file)
setDT(excel_data)

all_variables <- c(target_var, numeric_variables, categorical_variables)
missing_columns <- setdiff(all_variables, names(excel_data))

if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

excel_data[, (numeric_variables) := lapply(.SD, as.numeric), .SDcols = numeric_variables]

# -----------------------------------------------------------------------------
# Basic data overview

missing_summary <- summarize_missing(excel_data, all_variables)

target_summary <- excel_data[, .(
  BAD_0 = sum(get(target_var) == 0, na.rm = TRUE),
  BAD_1 = sum(get(target_var) == 1, na.rm = TRUE),
  bad_rate = mean(get(target_var) == 1, na.rm = TRUE),
  n = .N
)]

# -----------------------------------------------------------------------------
# Univariate default-rate summaries

numeric_univariate <- lapply(
  numeric_variables,
  function(v) make_position_bins(excel_data, variable = v, target = target_var, n_bins = 5)
)
names(numeric_univariate) <- numeric_variables

categorical_univariate <- lapply(
  categorical_variables,
  function(v) summarize_categorical(excel_data, variable = v, target = target_var)
)
names(categorical_univariate) <- categorical_variables

# -----------------------------------------------------------------------------
# Selected grouped categorical summaries used later in the scorecard workflow

job_grouped <- copy(excel_data[!is.na(JOB), .(BAD, JOB)])
job_grouped[JOB %in% c("Sales", "Self"), JOB_GROUP := "Sales + Self"]
job_grouped[JOB %in% c("Mgr", "Other"), JOB_GROUP := "Mgr + Other"]
job_grouped[JOB %in% c("ProfExe", "Office"), JOB_GROUP := "ProfExe + Office"]

job_grouped_summary <- job_grouped[!is.na(JOB_GROUP), .(
  n = .N,
  defaults = sum(BAD == 1, na.rm = TRUE),
  non_defaults = sum(BAD == 0, na.rm = TRUE),
  bad_rate = mean(BAD == 1, na.rm = TRUE)
), by = JOB_GROUP][order(JOB_GROUP)]

# -----------------------------------------------------------------------------
# Print key outputs

print("Missing values:")
print(missing_summary)

print("Target summary:")
print(target_summary)

print("Numeric univariate summaries:")
print(numeric_univariate)

print("Categorical univariate summaries:")
print(categorical_univariate)

print("Grouped JOB summary:")
print(job_grouped_summary)

# Optional exports:
# fwrite(missing_summary, "missing_summary.csv")
# fwrite(target_summary, "target_summary.csv")
# fwrite(rbindlist(numeric_univariate, idcol = "variable"), "numeric_univariate.csv")
# fwrite(rbindlist(categorical_univariate, idcol = "variable"), "categorical_univariate.csv")
# fwrite(job_grouped_summary, "job_grouped_summary.csv")
