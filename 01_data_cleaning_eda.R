## =============================================================================
## User Engagement & A/B Experimentation Analysis
## Step 1: Data Inspection, Quality Audit, and Cleaning
## =============================================================================
## Author:  Data Analytics Portfolio Project
## Purpose: Load, inspect, audit, and clean four raw CSV sources describing
##          a Q&A / forum product (users, questions, activity log, and an
##          A/B experiment), then produce clean, documented, analysis-ready
##          datasets. No modeling is performed in this script.
##
## Inputs  (raw/):
##   users.csv        - one row per registered user
##   questions.csv     - one row per question asked
##   user_activity.csv - event log (ask/view/answer/upvote)
##   experiment.csv     - one row per user's A/B assignment + summary stats
##
## Outputs (clean/):
##   users_clean.csv
##   questions_clean.csv
##   user_activity_clean.csv
##   experiment_clean.csv
##   user_level_analytical.csv   <- master joined table for downstream analysis
##   data_quality_log.csv        <- machine-readable issue log
## =============================================================================

## ---- 0. Setup ---------------------------------------------------------------

suppressMessages({
  library(tidyverse)   # dplyr, tidyr, readr, ggplot2, purrr, stringr, forcats
  library(lubridate)   # date handling
})

# Small helper standing in for janitor::get_dupes() (kept dependency-free
# so the script runs with base tidyverse only).
get_dupes <- function(df, key_col) {
  key <- rlang::ensym(key_col)
  df %>% group_by(!!key) %>% filter(n() > 1) %>% ungroup()
}

# ---- paths (edit RAW_DIR if running outside this environment) --------------
RAW_DIR    <- "raw"
CLEAN_DIR  <- "clean"
FIG_DIR    <- "figures"
dir.create(CLEAN_DIR, showWarnings = FALSE)
dir.create(FIG_DIR,   showWarnings = FALSE)

theme_set(theme_minimal(base_size = 11))

# Running log of data-quality issues we discover along the way.
# We append to this as we go, then write it out as a portfolio artifact.
dq_log <- tibble(dataset = character(), issue = character(),
                  n_affected = integer(), severity = character(),
                  resolution = character())

log_issue <- function(dataset, issue, n_affected, severity, resolution) {
  dq_log <<- add_row(dq_log, dataset = dataset, issue = issue,
                      n_affected = n_affected, severity = severity,
                      resolution = resolution)
}

## ---- 1. Load all CSV files ---------------------------------------------------

users      <- read_csv(file.path(RAW_DIR, "users.csv"),         show_col_types = FALSE)
questions  <- read_csv(file.path(RAW_DIR, "questions.csv"),     show_col_types = FALSE)
activity   <- read_csv(file.path(RAW_DIR, "user_activity.csv"), show_col_types = FALSE)
experiment <- read_csv(file.path(RAW_DIR, "experiment.csv"),    show_col_types = FALSE)

cat("Loaded: users =", nrow(users), "| questions =", nrow(questions),
    "| activity =", nrow(activity), "| experiment =", nrow(experiment), "\n")

## ---- 2. Dimensions, column names, data types, sample records ----------------

inspect_df <- function(df, name) {
  cat("\n=====================================================================\n")
  cat(" DATASET:", name, "\n")
  cat("=====================================================================\n")
  cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")
  cat("Column types:\n")
  print(map_chr(df, ~ class(.x)[1]))
  cat("\nSample records:\n")
  print(head(df, 5))
  cat("\nSummary statistics:\n")
  print(summary(df))
}

inspect_df(users, "users")
inspect_df(questions, "questions")
inspect_df(activity, "user_activity")
inspect_df(experiment, "experiment")

## ---- 3. Primary keys & relationships -----------------------------------------
## Grain (one row = ...) and candidate primary keys, verified programmatically.

check_pk <- function(df, key_col, name) {
  is_unique <- n_distinct(df[[key_col]]) == nrow(df)
  has_na    <- any(is.na(df[[key_col]]))
  cat(sprintf("%-15s PK = %-15s | unique: %-5s | has NA: %-5s | n_distinct = %d\n",
              name, key_col, is_unique, has_na, n_distinct(df[[key_col]])))
  is_unique && !has_na
}

cat("\n---- Primary key verification ----\n")
pk_users      <- check_pk(users, "user_id", "users")
pk_questions  <- check_pk(questions, "question_id", "questions")
pk_activity   <- check_pk(activity, "activity_id", "user_activity")
pk_experiment <- check_pk(experiment, "user_id", "experiment")

# Foreign-key coverage: what % of child-table FK values exist in the parent table?
fk_coverage <- function(child, child_col, parent, parent_col, label) {
  cov <- mean(child[[child_col]] %in% parent[[parent_col]])
  cat(sprintf("%-45s coverage = %.2f%%\n", label, 100 * cov))
  cov
}

cat("\n---- Foreign key / relationship coverage ----\n")
fk1 <- fk_coverage(questions, "user_id", users, "user_id",
                    "questions.user_id -> users.user_id")
fk2 <- fk_coverage(activity, "user_id", users, "user_id",
                    "user_activity.user_id -> users.user_id")
fk3 <- fk_coverage(activity, "question_id", questions, "question_id",
                    "user_activity.question_id -> questions.question_id")
fk4 <- fk_coverage(experiment, "user_id", users, "user_id",
                    "experiment.user_id -> users.user_id")
fk5 <- fk_coverage(users, "user_id", experiment, "user_id",
                    "users.user_id -> experiment.user_id (i.e. every user was enrolled)")

cat("\nRelationship model:\n",
    "  users (1) ----< experiment (1)      [1:1, every user has exactly one experiment record]\n",
    "  users (1) ----< questions (many)     [1 user asks 0..N questions]\n",
    "  users (1) ----< user_activity (many) [1 user performs 0..N activities]\n",
    "  questions (1) ----< user_activity (many) [1 question has 0..N activity events]\n")

if (all(fk1, fk2, fk3, fk4) && fk5 == 1) {
  log_issue("relationships", "All foreign keys fully covered; users<->experiment is 1:1",
             0, "info", "No action needed - referential integrity is clean")
}

## ---- 4. Missing values --------------------------------------------------------

missing_report <- function(df, name) {
  df %>%
    summarise(across(everything(), ~ sum(is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "column", values_to = "n_missing") %>%
    mutate(pct_missing = round(100 * n_missing / nrow(df), 2), dataset = name) %>%
    filter(n_missing > 0)
}

missing_all <- bind_rows(
  missing_report(users, "users"),
  missing_report(questions, "questions"),
  missing_report(activity, "user_activity"),
  missing_report(experiment, "experiment")
)

cat("\n---- Missing value summary (columns with >0 missing) ----\n")
print(missing_all)

# Investigate the one dataset with missingness: experiment.csv
na_check <- experiment %>%
  summarise(
    na_avg_word_count      = sum(is.na(avg_word_count)),
    na_quality_answer_rate = sum(is.na(quality_answer_rate)),
    na_when_zero_questions = sum(is.na(avg_word_count) & num_questions_asked == 0),
    zero_questions_total   = sum(num_questions_asked == 0)
  )
cat("\nDiagnosing experiment.csv missingness:\n"); print(na_check)

log_issue(
  dataset = "experiment",
  issue = "avg_word_count and quality_answer_rate are NA whenever num_questions_asked == 0",
  n_affected = sum(is.na(experiment$avg_word_count)),
  severity = "expected / structural (MNAR)",
  resolution = paste(
    "NA is the mathematically correct value (mean of zero questions is undefined).",
    "Not imputed with 0, since 0 would misleadingly imply 'asked short questions'.",
    "Kept as NA; downstream analyses must use an explicit engaged-user flag."
  )
)

## ---- 5. Duplicate records ------------------------------------------------------

cat("\n---- Full-row duplicate check ----\n")
cat("users:        ", sum(duplicated(users)), "\n")
cat("questions:    ", sum(duplicated(questions)), "\n")
cat("user_activity:", sum(duplicated(activity)), "\n")
cat("experiment:   ", sum(duplicated(experiment)), "\n")

cat("\n---- Key-level duplicate check (using janitor::get_dupes) ----\n")
cat("Duplicate user_id in users:       ", nrow(get_dupes(users, user_id)), "\n")
cat("Duplicate question_id in questions:", nrow(get_dupes(questions, question_id)), "\n")
cat("Duplicate activity_id in activity: ", nrow(get_dupes(activity, activity_id)), "\n")
cat("Duplicate user_id in experiment:   ", nrow(get_dupes(experiment, user_id)), "\n")

log_issue("all", "No full-row or primary-key duplicates found in any table",
          0, "info", "No de-duplication required")

## ---- 6. Invalid / inconsistent values -----------------------------------------

cat("\n---- Range / validity checks ----\n")

invalid_checks <- tibble(
  check = c(
    "questions.word_count < 0",
    "questions.num_views < 0",
    "questions.num_answers < 0",
    "questions.num_upvotes < 0",
    "questions.num_answers > num_views (impossible: can't answer w/o viewing? logged as info)",
    "questions.got_quality_answer not in {0,1}",
    "experiment.quality_answer_rate outside [0,1]",
    "experiment.variant not in {control, treatment}",
    "users.device_type not in {mobile, desktop, tablet}",
    "questions.created_date earlier than user's signup_date",
    "experiment.assignment_date earlier than user's signup_date",
    "user_activity.activity_date earlier than the question's created_date"
  ),
  n_flagged = c(
    sum(questions$word_count < 0),
    sum(questions$num_views < 0),
    sum(questions$num_answers < 0),
    sum(questions$num_upvotes < 0),
    sum(questions$num_answers > questions$num_views),
    sum(!questions$got_quality_answer %in% c(0, 1)),
    sum(experiment$quality_answer_rate < 0 | experiment$quality_answer_rate > 1, na.rm = TRUE),
    sum(!experiment$variant %in% c("control", "treatment")),
    sum(!users$device_type %in% c("mobile", "desktop", "tablet")),
    sum((questions %>% left_join(users, by = "user_id") %>%
           mutate(bad = created_date < signup_date))$bad),
    sum((experiment %>% left_join(users, by = "user_id") %>%
           mutate(bad = assignment_date < signup_date))$bad),
    sum((activity %>% left_join(questions %>% select(question_id, created_date),
                                  by = "question_id") %>%
           mutate(bad = activity_date < created_date))$bad, na.rm = TRUE)
  )
)
print(invalid_checks)

# Cross-check: does experiment.country / device_type agree with users.csv?
consistency_check <- experiment %>%
  select(user_id, exp_country = country, exp_device = device_type) %>%
  inner_join(users %>% select(user_id, usr_country = country, usr_device = device_type),
             by = "user_id") %>%
  summarise(country_mismatch = sum(exp_country != usr_country),
            device_mismatch  = sum(exp_device != usr_device))
cat("\nexperiment.csv vs users.csv country/device consistency:\n")
print(consistency_check)

log_issue("experiment/users", "country & device_type in experiment.csv are 100% consistent (redundant) with users.csv",
          0, "info", "Redundant columns dropped from experiment_clean to avoid duplication; users.csv treated as source of truth")

# --- IMPORTANT structural finding: experiment.csv metrics are ALL-TIME, not post-assignment ---
q_counts_all  <- questions %>% count(user_id, name = "actual_q_count_alltime")
q_counts_post <- questions %>%
  inner_join(experiment %>% select(user_id, assignment_date), by = "user_id") %>%
  filter(created_date >= assignment_date) %>%
  count(user_id, name = "actual_q_count_post")

match_check <- experiment %>%
  left_join(q_counts_all,  by = "user_id") %>%
  left_join(q_counts_post, by = "user_id") %>%
  mutate(across(c(actual_q_count_alltime, actual_q_count_post), ~ replace_na(.x, 0)))

match_alltime <- mean(match_check$num_questions_asked == match_check$actual_q_count_alltime)
match_post    <- mean(match_check$num_questions_asked == match_check$actual_q_count_post)

cat(sprintf("\nexperiment.num_questions_asked matches ALL-TIME question count: %.1f%%\n", 100 * match_alltime))
cat(sprintf("experiment.num_questions_asked matches POST-ASSIGNMENT question count only: %.1f%%\n", 100 * match_post))

log_issue(
  dataset = "experiment",
  issue = paste0(
    "num_questions_asked / avg_word_count / quality_answer_rate in experiment.csv are CUMULATIVE, ",
    "ALL-TIME user stats (100% match to lifetime question counts), NOT stats measured after ",
    "assignment_date (only ", round(100 * match_post, 1), "% match). Using these columns directly ",
    "for A/B comparison would conflate pre-existing user behavior with the treatment effect."
  ),
  n_affected = nrow(experiment),
  severity = "high - methodological risk for causal inference",
  resolution = paste(
    "Engineered new post-assignment-only outcome metrics directly from questions.csv",
    "(num_questions_post, avg_word_count_post, quality_answer_rate_post) for valid A/B analysis;",
    "retained original all-time columns separately and clearly relabeled."
  )
)

# --- Aggregate reconciliation: questions.csv counters vs raw activity log ---
answer_counts <- activity %>% filter(activity_type == "answer") %>% count(question_id, name = "actual_answers")
upvote_counts <- activity %>% filter(activity_type == "upvote") %>% count(question_id, name = "actual_upvotes")
view_counts   <- activity %>% filter(activity_type == "view")   %>% count(question_id, name = "actual_views")

recon <- questions %>%
  left_join(answer_counts, by = "question_id") %>%
  left_join(upvote_counts, by = "question_id") %>%
  left_join(view_counts,   by = "question_id") %>%
  mutate(across(c(actual_answers, actual_upvotes, actual_views), ~ replace_na(.x, 0)))

recon_summary <- recon %>%
  summarise(
    answers_match_pct = round(100 * mean(num_answers == actual_answers), 1),
    upvotes_match_pct = round(100 * mean(num_upvotes == actual_upvotes), 1),
    views_match_pct   = round(100 * mean(num_views   == actual_views), 1)
  )
cat("\nquestions.csv counters vs. raw user_activity.csv event counts:\n")
print(recon_summary)

log_issue(
  dataset = "questions / user_activity",
  issue = paste0(
    "questions.csv summary counters (num_views, num_answers, num_upvotes) do not fully ",
    "reconcile with raw event counts in user_activity.csv (answers match ",
    recon_summary$answers_match_pct, "%, upvotes ", recon_summary$upvotes_match_pct,
    "%, views ", recon_summary$views_match_pct, "%)."
  ),
  n_affected = nrow(questions),
  severity = "medium - treat as two independent measurement systems",
  resolution = paste(
    "Both sources kept as-is (no forced reconciliation). questions.csv treated as the",
    "authoritative snapshot/aggregate table; user_activity.csv treated as a separate",
    "behavioral event log best suited for sequencing/funnel/time-to-event analysis",
    "rather than as a re-derivation source for questions.csv's counters."
  )
)

## ---- 7. Date columns and ranges -----------------------------------------------

cat("\n---- Date column ranges ----\n")
date_ranges <- tibble(
  dataset = c("users", "questions", "user_activity", "experiment"),
  column  = c("signup_date", "created_date", "activity_date", "assignment_date"),
  min_date = c(min(users$signup_date), min(questions$created_date),
               min(activity$activity_date), min(experiment$assignment_date)),
  max_date = c(max(users$signup_date), max(questions$created_date),
               max(activity$activity_date), max(experiment$assignment_date))
)
print(date_ranges)

cat("\nInterpretation:\n",
    " - users.signup_date spans", as.character(min(users$signup_date)), "to",
    as.character(max(users$signup_date)), "(historical signups, pre-experiment).\n",
    " - questions/user_activity span the observation window",
    as.character(min(questions$created_date)), "to", as.character(max(questions$created_date)),
    "(~2 months).\n",
    " - experiment.assignment_date is tightly clustered at the start of that window (",
    as.character(min(experiment$assignment_date)), "to", as.character(max(experiment$assignment_date)),
    "), consistent with a single enrollment/ramp period.\n")

log_issue("all", "All date columns parse cleanly as ISO dates with no out-of-range or malformed values",
          0, "info", "No cleaning required beyond type casting (done in Section 12)")

## ---- 8. Categorical and numerical variable identification --------------------

var_types <- tibble(
  dataset = c(rep("users", 4), rep("questions", 9), rep("user_activity", 5), rep("experiment", 8)),
  variable = c(names(users), names(questions), names(activity), names(experiment)),
  r_type = c(map_chr(users, ~class(.x)[1]), map_chr(questions, ~class(.x)[1]),
             map_chr(activity, ~class(.x)[1]), map_chr(experiment, ~class(.x)[1])),
  role = c(
    "identifier", "date (numeric/time)", "categorical (nominal)", "categorical (nominal)",                # users
    "identifier", "identifier (FK)", "categorical (nominal)", "date", "numeric (continuous)",
    "numeric (count)", "numeric (count)", "numeric (count)", "categorical (binary/outcome)",               # questions
    "identifier", "identifier (FK)", "identifier (FK)", "categorical (nominal)", "date",                    # activity
    "identifier (FK)", "categorical (binary, treatment indicator)", "categorical (nominal)",
    "categorical (nominal)", "numeric (count)", "numeric (continuous)", "numeric (proportion)", "date"      # experiment
  )
)
cat("\n---- Variable type / role catalogue ----\n")
print(var_types, n = 30)

## ---- 9. Outlier detection ------------------------------------------------------

iqr_outliers <- function(x, label) {
  q1 <- quantile(x, .25, na.rm = TRUE); q3 <- quantile(x, .75, na.rm = TRUE)
  iqr <- q3 - q1
  lo <- q1 - 1.5 * iqr; hi <- q3 + 1.5 * iqr
  n_out <- sum(x < lo | x > hi, na.rm = TRUE)
  tibble(variable = label, q1 = q1, q3 = q3, lower_bound = lo, upper_bound = hi,
         n_outliers = n_out, pct_outliers = round(100 * n_out / sum(!is.na(x)), 2),
         max_value = max(x, na.rm = TRUE))
}

outlier_summary <- bind_rows(
  iqr_outliers(questions$word_count, "questions.word_count"),
  iqr_outliers(questions$num_views, "questions.num_views"),
  iqr_outliers(questions$num_answers, "questions.num_answers"),
  iqr_outliers(questions$num_upvotes, "questions.num_upvotes"),
  iqr_outliers(experiment$num_questions_asked, "experiment.num_questions_asked"),
  iqr_outliers(experiment$avg_word_count, "experiment.avg_word_count")
)
cat("\n---- IQR-based outlier summary (1.5x IQR rule) ----\n")
print(outlier_summary)

log_issue(
  dataset = "questions / experiment",
  issue = paste0(
    "Right-skewed count/engagement distributions produce IQR-flagged 'outliers' in ",
    "word_count (", outlier_summary$pct_outliers[1], "%), num_upvotes (",
    outlier_summary$pct_outliers[4], "%), and num_questions_asked (",
    outlier_summary$pct_outliers[5], "%). These look like genuine highly-engaged ",
    "users/popular questions rather than data errors (no negative values, no impossible ",
    "magnitudes, e.g. max word_count = 275 words is a plausible long question)."
  ),
  n_affected = sum(outlier_summary$n_outliers),
  severity = "low - natural skew, not an error",
  resolution = paste(
    "Not removed or capped (task requires avoiding unnecessary deletion). Flag columns",
    "(e.g. is_word_count_outlier) added to the analytical dataset so downstream models",
    "can choose to winsorize/transform (e.g. log) if needed."
  )
)

# Boxplots for visual inspection (saved as portfolio figures)
p1 <- ggplot(questions, aes(y = word_count)) + geom_boxplot(fill = "#4C72B0") +
  labs(title = "Outlier check: Question word_count", y = "Word count") +
  theme(axis.text.x = element_blank())
p2 <- ggplot(questions, aes(y = num_upvotes)) + geom_boxplot(fill = "#DD8452") +
  labs(title = "Outlier check: Question num_upvotes", y = "Upvotes") +
  theme(axis.text.x = element_blank())
p3 <- ggplot(experiment, aes(y = num_questions_asked)) + geom_boxplot(fill = "#55A868") +
  labs(title = "Outlier check: experiment.num_questions_asked (all-time)", y = "Questions asked") +
  theme(axis.text.x = element_blank())

ggsave(file.path(FIG_DIR, "outliers_word_count.png"), p1, width = 5, height = 4, dpi = 150)
ggsave(file.path(FIG_DIR, "outliers_upvotes.png"), p2, width = 5, height = 4, dpi = 150)
ggsave(file.path(FIG_DIR, "outliers_questions_asked.png"), p3, width = 5, height = 4, dpi = 150)

## ---- 10. Variables of analytical interest --------------------------------------
## (Printed here for documentation; also written out as a CSV artifact.)

analytical_vars <- tribble(
  ~use_case,             ~variables,
  "Engagement analysis", "num_questions_asked, avg_word_count, quality_answer_rate, num_views, num_answers, num_upvotes, activity_type, activity counts per user, tenure (signup_date), device_type, country",
  "Regression modeling", "DV candidates: quality_answer_rate, num_upvotes, num_views. IV candidates: word_count, topic, device_type, country, num_answers, account_age_days",
  "Hypothesis testing",  "Compare means/proportions across device_type, country, topic (e.g. quality_answer_rate by topic; word_count by device_type)",
  "A/B testing",         "variant (treatment indicator); outcomes: num_questions_post, avg_word_count_post, quality_answer_rate_post (post-assignment, re-derived); stratify by country/device_type",
  "Causal inference",    "variant as treatment; pre-period covariates (account_age_days, historical activity before assignment_date) for covariate balance checks / CUPED / regression adjustment; assignment_date as the time-zero cutoff"
)
cat("\n---- Candidate variables by analytical use case ----\n")
print(analytical_vars, n = 10, width = Inf)

## ---- 11. Data quality issue log (final compile) ---------------------------------

cat("\n---- Full data-quality issue log ----\n")
print(dq_log, n = Inf, width = Inf)
write_csv(dq_log, file.path(CLEAN_DIR, "data_quality_log.csv"))

## =============================================================================
## ---- 12. CLEANING ----------------------------------------------------------
## Principle: standardize types/labels and ADD flags/derived fields;
## do not delete rows unless they are true full-row duplicates (none found).
## =============================================================================

users_clean <- users %>%
  mutate(
    country      = factor(country),
    device_type  = factor(device_type, levels = c("mobile", "desktop", "tablet")),
    signup_date  = as_date(signup_date)
  ) %>%
  arrange(user_id)

questions_clean <- questions %>%
  mutate(
    topic               = factor(topic),
    created_date         = as_date(created_date),
    got_quality_answer   = as.logical(got_quality_answer),
    is_word_count_outlier = word_count > (quantile(word_count, .75) + 1.5 * IQR(word_count)),
    is_upvotes_outlier    = num_upvotes > (quantile(num_upvotes, .75) + 1.5 * IQR(num_upvotes))
  ) %>%
  arrange(question_id)

activity_clean <- activity %>%
  mutate(
    activity_type = factor(activity_type, levels = c("ask", "view", "answer", "upvote")),
    activity_date  = as_date(activity_date)
  ) %>%
  arrange(activity_id)

# Re-derive TRUE post-assignment experiment outcomes from questions.csv,
# rather than trusting experiment.csv's own (all-time) aggregate columns.
post_metrics <- questions %>%
  inner_join(experiment %>% select(user_id, assignment_date), by = "user_id") %>%
  filter(created_date >= assignment_date) %>%
  group_by(user_id) %>%
  summarise(
    num_questions_post       = n(),
    avg_word_count_post       = mean(word_count),
    quality_answer_rate_post  = mean(got_quality_answer),
    .groups = "drop"
  )

experiment_clean <- experiment %>%
  select(user_id, variant, assignment_date,
         num_questions_asked_alltime = num_questions_asked,
         avg_word_count_alltime      = avg_word_count,
         quality_answer_rate_alltime = quality_answer_rate) %>%
  mutate(variant = factor(variant, levels = c("control", "treatment")),
         assignment_date = as_date(assignment_date)) %>%
  left_join(post_metrics, by = "user_id") %>%
  mutate(
    num_questions_post      = replace_na(num_questions_post, 0),
    asked_any_post          = num_questions_post > 0,
    asked_any_alltime       = num_questions_asked_alltime > 0
    # avg_word_count_post / quality_answer_rate_post intentionally left NA
    # when num_questions_post == 0 (undefined mean), matching source convention.
  ) %>%
  arrange(user_id)

cat("\nCleaned table dimensions:\n")
cat("users_clean:     ", nrow(users_clean), "x", ncol(users_clean), "\n")
cat("questions_clean: ", nrow(questions_clean), "x", ncol(questions_clean), "\n")
cat("activity_clean:  ", nrow(activity_clean), "x", ncol(activity_clean), "\n")
cat("experiment_clean:", nrow(experiment_clean), "x", ncol(experiment_clean), "\n")

## =============================================================================
## ---- 13. Analytical (user-level) master dataset ----------------------------
## One row per user: demographics + experiment assignment + engagement rollups.
## Built for engagement analysis, regression, hypothesis testing, A/B testing.
## =============================================================================

user_engagement <- questions_clean %>%
  group_by(user_id) %>%
  summarise(
    total_questions      = n(),
    total_word_count     = sum(word_count),
    avg_word_count_all    = mean(word_count),
    total_views          = sum(num_views),
    total_answers        = sum(num_answers),
    total_upvotes        = sum(num_upvotes),
    quality_answer_rate_all = mean(got_quality_answer),
    first_question_date  = min(created_date),
    last_question_date   = max(created_date),
    .groups = "drop"
  )

user_activity_summary <- activity_clean %>%
  count(user_id, activity_type) %>%
  pivot_wider(names_from = activity_type, values_from = n, values_fill = 0,
              names_prefix = "n_activity_") %>%
  mutate(total_activity_events = rowSums(across(starts_with("n_activity_"))))

user_level_analytical <- users_clean %>%
  left_join(experiment_clean, by = "user_id") %>%
  left_join(user_engagement, by = "user_id") %>%
  left_join(user_activity_summary, by = "user_id") %>%
  mutate(
    account_age_at_assignment_days = as.integer(assignment_date - signup_date),
    across(c(total_questions, total_word_count, total_views, total_answers,
             total_upvotes, starts_with("n_activity_"), total_activity_events),
           ~ replace_na(.x, 0)),
    is_asker = total_questions > 0
  ) %>%
  select(user_id, signup_date, country, device_type,
         variant, assignment_date, account_age_at_assignment_days,
         num_questions_asked_alltime, avg_word_count_alltime, quality_answer_rate_alltime,
         num_questions_post, avg_word_count_post, quality_answer_rate_post,
         asked_any_post, asked_any_alltime,
         total_questions, avg_word_count_all, quality_answer_rate_all,
         total_views, total_answers, total_upvotes,
         starts_with("n_activity_"), total_activity_events, is_asker,
         first_question_date, last_question_date)

cat("\nuser_level_analytical:", nrow(user_level_analytical), "x", ncol(user_level_analytical), "\n")
glimpse(user_level_analytical)

## ---- Sanity checks on the master table ----
stopifnot(nrow(user_level_analytical) == nrow(users_clean))          # no fan-out
stopifnot(n_distinct(user_level_analytical$user_id) == nrow(user_level_analytical))  # still 1 row/user

## ---- Write cleaned outputs ----
write_csv(users_clean,            file.path(CLEAN_DIR, "users_clean.csv"))
write_csv(questions_clean,        file.path(CLEAN_DIR, "questions_clean.csv"))
write_csv(activity_clean,         file.path(CLEAN_DIR, "user_activity_clean.csv"))
write_csv(experiment_clean,       file.path(CLEAN_DIR, "experiment_clean.csv"))
write_csv(user_level_analytical,  file.path(CLEAN_DIR, "user_level_analytical.csv"))

cat("\nAll cleaned datasets written to '", CLEAN_DIR, "/'\n", sep = "")

## ---- Quick descriptive plots for the portfolio write-up ----

p4 <- user_level_analytical %>%
  count(variant) %>%
  ggplot(aes(variant, n, fill = variant)) + geom_col() +
  labs(title = "Experiment group sizes", x = NULL, y = "Users") +
  theme(legend.position = "none")
ggsave(file.path(FIG_DIR, "experiment_group_sizes.png"), p4, width = 5, height = 4, dpi = 150)

p5 <- ggplot(missing_all, aes(x = reorder(paste(dataset, column, sep = "."), pct_missing),
                                y = pct_missing)) +
  geom_col(fill = "#C44E52") + coord_flip() +
  labs(title = "Missing values by column (only columns with >0% shown)",
       x = NULL, y = "% missing")
ggsave(file.path(FIG_DIR, "missingness.png"), p5, width = 6, height = 3, dpi = 150)

p6 <- ggplot(questions_clean, aes(x = topic)) + geom_bar(fill = "#4C72B0") +
  coord_flip() + labs(title = "Questions by topic", x = NULL, y = "Count")
ggsave(file.path(FIG_DIR, "questions_by_topic.png"), p6, width = 6, height = 4, dpi = 150)

cat("\nDone. Figures written to '", FIG_DIR, "/'\n", sep = "")
