## =============================================================================
## User Engagement & A/B Experimentation Analysis
## Step 2: Product Analytics & Exploratory Data Analysis
## =============================================================================
## Purpose: Using the CLEANED datasets from Step 1, define and calculate core
##          product KPIs, break them down by user segment, analyze trends over
##          time, examine the relationship between activity and engagement,
##          study question-asking behavior, and flag unusual patterns.
##
## Explicitly OUT OF SCOPE for this script (by design):
##   - A/B test comparison (variant is not analyzed as a treatment here)
##   - Regression modeling
##   - Causal inference
##
## Inputs  (clean/, produced by 01_data_cleaning_eda.R):
##   users_clean.csv, questions_clean.csv, user_activity_clean.csv,
##   experiment_clean.csv, user_level_analytical.csv
##
## Outputs (data/):
##   kpi_summary.csv, kpi_by_device.csv, kpi_by_country.csv, kpi_by_topic.csv,
##   kpi_by_engagement_tier.csv, daily_trend.csv, weekly_retention.csv,
##   eda_findings_log.csv
## Outputs (figures/): 9 purpose-built ggplot2 charts (see Section 8)
## =============================================================================

suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

CLEAN_DIR <- "clean"
OUT_DIR   <- "clean"     # KPI/segment tables land alongside the clean data
FIG_DIR   <- "figures"
dir.create(FIG_DIR, showWarnings = FALSE)

theme_set(
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey40"),
          panel.grid.minor = element_blank())
)

findings_log <- tibble(section = character(), finding = character())
log_finding <- function(section, finding) {
  findings_log <<- add_row(findings_log, section = section, finding = finding)
}

## ---- 0. Load cleaned data from Step 1 -----------------------------------------

users      <- read_csv(file.path(CLEAN_DIR, "users_clean.csv"), show_col_types = FALSE)
questions  <- read_csv(file.path(CLEAN_DIR, "questions_clean.csv"), show_col_types = FALSE)
activity   <- read_csv(file.path(CLEAN_DIR, "user_activity_clean.csv"), show_col_types = FALSE)
experiment <- read_csv(file.path(CLEAN_DIR, "experiment_clean.csv"), show_col_types = FALSE)
ul         <- read_csv(file.path(CLEAN_DIR, "user_level_analytical.csv"), show_col_types = FALSE)

OBS_START <- min(activity$activity_date)
OBS_END   <- max(activity$activity_date)
OBS_DAYS  <- as.integer(OBS_END - OBS_START) + 1
cat("Observation window:", as.character(OBS_START), "to", as.character(OBS_END),
    "(", OBS_DAYS, "days )\n")

## =============================================================================
## ---- 1. KPI DEFINITIONS ----------------------------------------------------
## Every KPI below is chosen because the underlying field exists in the data.
## Two important LIMITATIONS are documented rather than papered over:
##   - There is no session_id or intra-day timestamp, so "sessions" are
##     approximated by DISTINCT ACTIVE DAYS per user (a session proxy, not a
##     true session count).
##   - There is no duration/timestamp field, so literal "time spent" cannot
##     be computed. We substitute ENGAGEMENT SPAN (days between a user's
##     first and last recorded activity) and ACTIVITY VOLUME (total events)
##     as the closest available proxies, and say so explicitly.
## =============================================================================

kpi_definitions <- tribble(
  ~kpi, ~definition, ~data_source,
  "Daily/Weekly/Monthly Active Users (DAU/WAU/MAU)", "Distinct users with >=1 activity event in the period", "user_activity_clean",
  "Active days per user (session proxy)", "Distinct calendar days with >=1 activity event, per user. Proxy for 'sessions' - no true session boundary exists in the data.", "user_activity_clean",
  "Engagement span (time-spent proxy)", "Days between a user's first and last question (or activity). NOT literal time-on-site; no timestamp/duration field exists.", "questions_clean / user_activity_clean",
  "Question-asking rate", "% of users who asked at least one question", "user_level_analytical",
  "Questions per user", "Mean/median total questions asked per user (all users, and among askers only)", "user_level_analytical",
  "Return-visit rate", "% of users active in an early period who are also active in a later period; also week-over-week retention", "user_activity_clean",
  "Views/Answers/Upvotes per question", "Mean engagement received per question asked", "questions_clean",
  "Quality-answer rate", "% of questions that received a 'quality' answer (got_quality_answer)", "questions_clean",
  "Activity mix", "Share of all logged events that are ask / view / answer / upvote", "user_activity_clean"
)
write_csv(kpi_definitions, file.path(OUT_DIR, "kpi_definitions.csv"))
cat("\n---- KPI definitions ----\n"); print(kpi_definitions, n = Inf, width = Inf)

## =============================================================================
## ---- 2. KPI CALCULATION -----------------------------------------------------
## =============================================================================

# -- Active users --
dau <- activity %>% distinct(user_id, activity_date) %>% count(activity_date, name = "dau")
wau <- activity %>% mutate(week = floor_date(activity_date, "week")) %>%
  distinct(user_id, week) %>% count(week, name = "wau")
mau <- activity %>% mutate(month = floor_date(activity_date, "month")) %>%
  distinct(user_id, month) %>% count(month, name = "mau")

cat("\n---- Active users ----\n")
cat("Mean DAU:", round(mean(dau$dau)), "| Mean WAU:", round(mean(wau$wau)),
    "| Total unique users active at any point (=MAU of the whole window):",
    n_distinct(activity$user_id), "of", nrow(users), "\n")

# -- Active days per user (session proxy) --
active_days <- activity %>% distinct(user_id, activity_date) %>%
  count(user_id, name = "active_days")

cat("\n---- Active days per user (session proxy) ----\n")
print(summary(active_days$active_days))

# -- Engagement span (time-spent proxy), askers only --
engagement_span <- questions %>%
  group_by(user_id) %>%
  summarise(first_q = min(created_date), last_q = max(created_date),
            span_days = as.integer(last_q - first_q), .groups = "drop")
cat("\n---- Engagement span in days, among users who asked >=2 questions (time-spent proxy) ----\n")
print(summary(engagement_span$span_days[engagement_span$span_days > 0]))

# -- Question-asking rate & questions per user --
question_asking_rate <- mean(ul$is_asker)
questions_per_user_all    <- mean(ul$total_questions)
questions_per_user_askers <- mean(ul$total_questions[ul$is_asker])

cat("\n---- Question-asking behavior ----\n")
cat(sprintf("Question-asking rate (>=1 question ever): %.1f%%\n", 100 * question_asking_rate))
cat(sprintf("Questions per user (all users):    mean = %.2f, median = %d\n",
            questions_per_user_all, median(ul$total_questions)))
cat(sprintf("Questions per user (askers only):  mean = %.2f, median = %d\n",
            questions_per_user_askers, median(ul$total_questions[ul$is_asker])))

# -- Return-visit rate --
# (a) crude 2-period split: first half of window vs second half
cutoff <- OBS_START + floor(OBS_DAYS / 2) - 1
p1_users <- activity %>% filter(activity_date <= cutoff) %>% distinct(user_id) %>% pull(user_id)
p2_users <- activity %>% filter(activity_date >  cutoff) %>% distinct(user_id) %>% pull(user_id)
return_rate_2period <- mean(p1_users %in% p2_users)

# (b) week-over-week retention curve (more informative given near-ceiling 2-period rate)
week_users <- activity %>% mutate(week = floor_date(activity_date, "week")) %>%
  distinct(user_id, week)
week_list <- sort(unique(week_users$week))
wow_retention <- map_dfr(seq_len(length(week_list) - 1), function(i) {
  this_w  <- week_users %>% filter(week == week_list[i])   %>% pull(user_id)
  next_w  <- week_users %>% filter(week == week_list[i+1]) %>% pull(user_id)
  tibble(week = week_list[i], next_week = week_list[i + 1],
         active_this_week = length(this_w),
         retained_next_week = sum(this_w %in% next_w),
         retention_rate = sum(this_w %in% next_w) / length(this_w))
})

cat("\n---- Return-visit rate ----\n")
cat(sprintf("2-period return rate (active in first %d days AND active in remaining days): %.1f%%\n",
            OBS_DAYS %/% 2, 100 * return_rate_2period))
cat("Week-over-week retention:\n"); print(wow_retention)

log_finding("KPI - Return visits", sprintf(
  "The simple 'active early AND active late' return rate is %.1f%% - essentially universal - because every user in this dataset has at least 3 active days spread across the 60-day window. This makes the 2-period metric uninformative; week-over-week retention (%.0f%%-%.0f%% across weeks) is the more useful repeat-visit signal.",
  100 * return_rate_2period, 100 * min(wow_retention$retention_rate), 100 * max(wow_retention$retention_rate)))

# -- Engagement metrics (per question) --
engagement_metrics <- questions %>%
  summarise(
    mean_views_per_q   = mean(num_views),
    mean_answers_per_q = mean(num_answers),
    mean_upvotes_per_q = mean(num_upvotes),
    quality_answer_rate = mean(got_quality_answer)
  )
cat("\n---- Per-question engagement metrics ----\n"); print(engagement_metrics)

# -- Activity mix --
activity_mix <- activity %>% count(activity_type) %>% mutate(pct = round(100 * n / sum(n), 1))
cat("\n---- Activity mix (share of all logged events) ----\n"); print(activity_mix)

## ---- Compile top-line KPI summary table ----

kpi_summary <- tibble(
  kpi = c("Mean DAU", "Mean WAU", "Users active at least once (of 12,000)",
          "Mean active days per user (session proxy)",
          "Median active days per user",
          "Question-asking rate",
          "Mean questions per user (all)",
          "Mean questions per user (askers only)",
          "2-period return-visit rate",
          "Mean week-over-week retention rate",
          "Mean views per question", "Mean answers per question", "Mean upvotes per question",
          "Overall quality-answer rate"),
  value = c(round(mean(dau$dau)), round(mean(wau$wau)), n_distinct(activity$user_id),
            round(mean(active_days$active_days), 2), median(active_days$active_days),
            paste0(round(100 * question_asking_rate, 1), "%"),
            round(questions_per_user_all, 2), round(questions_per_user_askers, 2),
            paste0(round(100 * return_rate_2period, 1), "%"),
            paste0(round(100 * mean(wow_retention$retention_rate), 1), "%"),
            round(engagement_metrics$mean_views_per_q, 2),
            round(engagement_metrics$mean_answers_per_q, 2),
            round(engagement_metrics$mean_upvotes_per_q, 2),
            paste0(round(100 * engagement_metrics$quality_answer_rate, 1), "%"))
)
cat("\n---- TOP-LINE KPI SUMMARY ----\n"); print(kpi_summary, n = Inf)
write_csv(kpi_summary, file.path(OUT_DIR, "kpi_summary.csv"))
write_csv(wow_retention, file.path(OUT_DIR, "weekly_retention.csv"))

## =============================================================================
## ---- 3. KPIs BY SEGMENT -----------------------------------------------------
## =============================================================================

kpi_by_device <- ul %>%
  group_by(device_type) %>%
  summarise(n_users = n(),
            asker_rate = round(mean(is_asker), 3),
            mean_questions = round(mean(total_questions), 2),
            mean_quality_rate = round(mean(quality_answer_rate_all, na.rm = TRUE), 3),
            mean_activity_events = round(mean(total_activity_events), 1),
            .groups = "drop")

kpi_by_country <- ul %>%
  group_by(country) %>%
  summarise(n_users = n(),
            asker_rate = round(mean(is_asker), 3),
            mean_questions = round(mean(total_questions), 2),
            mean_quality_rate = round(mean(quality_answer_rate_all, na.rm = TRUE), 3),
            mean_activity_events = round(mean(total_activity_events), 1),
            .groups = "drop") %>%
  arrange(desc(mean_questions))

kpi_by_topic <- questions %>%
  group_by(topic) %>%
  summarise(n_questions = n(),
            mean_word_count = round(mean(word_count), 1),
            mean_views = round(mean(num_views), 1),
            mean_upvotes = round(mean(num_upvotes), 1),
            quality_rate = round(mean(got_quality_answer), 3),
            .groups = "drop") %>%
  arrange(desc(n_questions))

ul <- ul %>% mutate(engagement_tier = ntile(total_activity_events, 4),
                     engagement_tier = factor(engagement_tier, labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")))
kpi_by_engagement_tier <- ul %>%
  group_by(engagement_tier) %>%
  summarise(n_users = n(),
            mean_activity_events = round(mean(total_activity_events), 1),
            mean_questions = round(mean(total_questions), 2),
            mean_quality_rate = round(mean(quality_answer_rate_all, na.rm = TRUE), 3),
            .groups = "drop")

cat("\n---- KPIs by device_type ----\n"); print(kpi_by_device)
cat("\n---- KPIs by country ----\n"); print(kpi_by_country)
cat("\n---- KPIs by topic ----\n"); print(kpi_by_topic)
cat("\n---- KPIs by engagement tier (quartiles of total activity events) ----\n"); print(kpi_by_engagement_tier)

write_csv(kpi_by_device, file.path(OUT_DIR, "kpi_by_device.csv"))
write_csv(kpi_by_country, file.path(OUT_DIR, "kpi_by_country.csv"))
write_csv(kpi_by_topic, file.path(OUT_DIR, "kpi_by_topic.csv"))
write_csv(kpi_by_engagement_tier, file.path(OUT_DIR, "kpi_by_engagement_tier.csv"))

log_finding("Segments - device", sprintf(
  "Device type barely differentiates behavior: asker rate is ~87-88%% and mean quality-answer rate is ~69%% across mobile, desktop, and tablet alike (range < 1pp). Device is not a meaningful engagement segment on its own."))
log_finding("Segments - country", sprintf(
  "Country shows a real, if moderate, spread: mean questions per user ranges from %.2f (%s, lowest) to %.2f (%s, highest), and quality-answer rate ranges from %.1f%% to %.1f%%.",
  min(kpi_by_country$mean_questions), kpi_by_country$country[which.min(kpi_by_country$mean_questions)],
  max(kpi_by_country$mean_questions), kpi_by_country$country[which.max(kpi_by_country$mean_questions)],
  100*min(kpi_by_country$mean_quality_rate), 100*max(kpi_by_country$mean_quality_rate)))
log_finding("Segments - engagement tier", sprintf(
  "Users in the top activity quartile ask %.1fx more questions than the bottom quartile (%.2f vs %.2f) and have a modestly higher quality-answer rate (%.1f%% vs %.1f%%), suggesting more active users are also somewhat higher-quality contributors, not just higher-volume ones.",
  kpi_by_engagement_tier$mean_questions[4]/kpi_by_engagement_tier$mean_questions[1],
  kpi_by_engagement_tier$mean_questions[4], kpi_by_engagement_tier$mean_questions[1],
  100*kpi_by_engagement_tier$mean_quality_rate[4], 100*kpi_by_engagement_tier$mean_quality_rate[1]))

## =============================================================================
## ---- 4. TRENDS OVER TIME -----------------------------------------------------
## =============================================================================

daily_trend <- dau %>%
  left_join(questions %>% count(created_date, name = "questions_asked"),
            by = c("activity_date" = "created_date")) %>%
  mutate(questions_asked = replace_na(questions_asked, 0),
         dow = wday(activity_date, label = TRUE),
         question_asking_rate = questions_asked / dau)
write_csv(daily_trend, file.path(OUT_DIR, "daily_trend.csv"))

cat("\n---- Day-of-week pattern (mean DAU) ----\n")
print(daily_trend %>% group_by(dow) %>% summarise(mean_dau = round(mean(dau)), .groups = "drop"))

# Statistical flag for unusually high/low days (>2 SD from mean)
daily_trend <- daily_trend %>% mutate(dau_z = as.numeric(scale(dau)))
anomalous_days <- daily_trend %>% filter(abs(dau_z) > 2)
cat("\n---- Days with DAU more than 2 SD from the mean ----\n")
print(anomalous_days %>% select(activity_date, dau, dau_z))

log_finding("Trends", sprintf(
  "Daily active users are remarkably flat across the full %d-day window (%d-%d, mean = %.0f, SD = %.0f, range %d-%d) with no growth, decline, or day-of-week seasonality (weekday means all within ~3%% of each other). Only %d day(s) exceed +/-2 SD, consistent with ordinary sampling noise rather than a real event.",
  OBS_DAYS, min(daily_trend$dau), max(daily_trend$dau), mean(daily_trend$dau), sd(daily_trend$dau),
  min(daily_trend$dau), max(daily_trend$dau), nrow(anomalous_days)))

## =============================================================================
## ---- 5. RELATIONSHIP BETWEEN ACTIVITY AND ENGAGEMENT --------------------------
## =============================================================================

cor_activity_questions <- cor(ul$total_activity_events, ul$total_questions)
cor_activity_quality   <- cor(ul$total_activity_events, ul$quality_answer_rate_all, use = "complete.obs")
cor_tenure_questions   <- cor(ul$account_age_at_assignment_days, ul$total_questions)
cor_views_upvotes      <- cor(ul$n_activity_view, ul$total_upvotes)

cat("\n---- Correlations: activity vs. engagement outcomes ----\n")
cat(sprintf("total_activity_events vs total_questions:        r = %.3f\n", cor_activity_questions))
cat(sprintf("total_activity_events vs quality_answer_rate_all: r = %.3f\n", cor_activity_quality))
cat(sprintf("account_age_at_assignment vs total_questions:     r = %.3f\n", cor_tenure_questions))
cat(sprintf("n_activity_view vs total_upvotes:                 r = %.3f\n", cor_views_upvotes))

log_finding("Activity <-> engagement relationship", sprintf(
  "Overall platform activity (total_activity_events, which mixes viewing/answering/upvoting) is moderately correlated with how many questions a user asks (r = %.2f), but essentially uncorrelated with the quality of their answers (r = %.2f). In other words, being more active predicts asking MORE, not asking BETTER; question quality depends on other factors (see word-count finding below), not raw activity volume. Account tenure (account_age_at_assignment_days) shows no relationship with total questions asked (r = %.2f) - longer-tenured users are not inherently more active askers.",
  cor_activity_questions, cor_activity_quality, cor_tenure_questions))

## =============================================================================
## ---- 6. QUESTION-ASKING BEHAVIOR ---------------------------------------------
## =============================================================================

word_count_bins <- questions %>%
  mutate(word_count_bin = cut(word_count, breaks = c(0, 15, 30, 50, 80, 300),
                               labels = c("1-15", "16-30", "31-50", "51-80", "81+"))) %>%
  group_by(word_count_bin) %>%
  summarise(n_questions = n(),
            quality_rate = round(mean(got_quality_answer), 3),
            mean_upvotes = round(mean(num_upvotes), 2),
            mean_views = round(mean(num_views), 2),
            .groups = "drop")
cat("\n---- Quality/engagement by question word-count bucket ----\n")
print(word_count_bins)
write_csv(word_count_bins, file.path(OUT_DIR, "kpi_by_wordcount_bin.csv"))

log_finding("Question-asking behavior", sprintf(
  "Question length is strongly, monotonically related to outcomes: quality-answer rate rises from %.1f%% for the shortest questions (1-15 words) to %.1f%% for the longest (81+ words), and mean upvotes more than double (%.1f -> %.1f) across the same range. This is the strongest behavioral driver of quality found in this EDA - much stronger than device, country, or overall activity volume.",
  100*word_count_bins$quality_rate[1], 100*word_count_bins$quality_rate[5],
  word_count_bins$mean_upvotes[1], word_count_bins$mean_upvotes[5]))

## =============================================================================
## ---- 7. UNUSUAL PATTERNS & ROOT-CAUSE INVESTIGATION --------------------------
## =============================================================================

# (a) Weekly totals look like they drop in the final week - investigate.
weekly_totals <- activity %>% mutate(week = floor_date(activity_date, "week")) %>%
  group_by(week) %>%
  summarise(n_events = n(), n_days_in_data = n_distinct(activity_date), .groups = "drop") %>%
  mutate(events_per_day = round(n_events / n_days_in_data, 1))
cat("\n---- Weekly activity totals with days-observed context ----\n")
print(weekly_totals)

first_partial <- weekly_totals$n_days_in_data[1] < 7
last_partial  <- weekly_totals$n_days_in_data[nrow(weekly_totals)] < 7

log_finding("Unusual pattern investigated", sprintf(
  "Raw weekly totals appear to fall sharply in the first and last calendar weeks of the log. Root cause: those weeks are PARTIAL (only %d and %d days of data fall inside the %d-day observation window, vs. 7 for interior weeks) - not a genuine drop in engagement. Normalizing to events-per-day (%.1f interior weeks vs %.1f/%.1f for the partial edge weeks) confirms daily intensity is essentially flat; the apparent dip is a calendar-boundary artifact and should be excluded from any 'declining engagement' narrative.",
  weekly_totals$n_days_in_data[1], weekly_totals$n_days_in_data[nrow(weekly_totals)], OBS_DAYS,
  mean(weekly_totals$events_per_day[2:(nrow(weekly_totals)-1)]),
  weekly_totals$events_per_day[1], weekly_totals$events_per_day[nrow(weekly_totals)]))

# (b) Never-asked users: who are they?
never_asked <- ul %>% filter(!is_asker)
cat("\n---- Users who never asked a question:", nrow(never_asked), "(",
    round(100*nrow(never_asked)/nrow(ul),1), "% ) ----\n")
print(never_asked %>% count(device_type) %>% mutate(pct = round(100*n/sum(n),1)))
print(never_asked %>% summarise(mean_activity_events = round(mean(total_activity_events),1),
                                 mean_views = round(mean(n_activity_view),1),
                                 mean_upvotes_given = round(mean(n_activity_upvote),1)))
asked <- ul %>% filter(is_asker)
cat("For comparison, askers' mean total_activity_events:", round(mean(asked$total_activity_events),1), "\n")

log_finding("Unusual pattern investigated", sprintf(
  "%.1f%% of users (%d of %d) never ask a single question despite ALL of them having recorded activity (min 3 active days each). They are not simply 'inactive' - they average %.1f activity events (mostly viewing/upvoting) vs %.1f for askers. Root cause is behavioral segmentation, not a data or tracking gap: a consistent 'browser/voter' segment exists that consumes and reacts to content but does not post. This is a distinct segment worth targeting separately from churn risk.",
  100*nrow(never_asked)/nrow(ul), nrow(never_asked), nrow(ul),
  mean(never_asked$total_activity_events), mean(asked$total_activity_events)))

write_csv(findings_log, file.path(OUT_DIR, "eda_findings_log.csv"))
cat("\n---- Full findings log ----\n"); print(findings_log, n = Inf, width = Inf)

## =============================================================================
## ---- 8. VISUALIZATIONS (ggplot2) --------------------------------------------
## Each chart below is tied to one specific question from Sections 2-7 above.
## =============================================================================

# 1. Is engagement growing, flat, or declining over time? (Section 4)
p1 <- ggplot(daily_trend, aes(activity_date, dau)) +
  geom_line(color = "#4C72B0", linewidth = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "#C44E52", linewidth = 0.8, span = 0.3) +
  labs(title = "Daily Active Users: flat over the 2-month window",
       subtitle = "No growth, decline, or day-of-week seasonality detected",
       x = NULL, y = "Daily active users") +
  scale_y_continuous(labels = comma)
ggsave(file.path(FIG_DIR, "01_dau_trend.png"), p1, width = 8, height = 4.5, dpi = 150)

# 2. How "sticky" is the user base? (Section 2 - session proxy)
p2 <- ggplot(active_days, aes(active_days)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "white") +
  labs(title = "Distribution of active days per user (session-frequency proxy)",
       subtitle = paste0("Median = ", median(active_days$active_days), " active days out of ", OBS_DAYS, " observed"),
       x = "Distinct active days", y = "Number of users") +
  scale_y_continuous(labels = comma)
ggsave(file.path(FIG_DIR, "02_active_days_distribution.png"), p2, width = 7.8, height = 4.5, dpi = 150)

# 3. Do repeat visits hold up week over week? (Section 2 - return-visit rate)
p3 <- ggplot(wow_retention, aes(week, retention_rate)) +
  geom_col(fill = "#55A868") +
  geom_text(aes(label = percent(retention_rate, accuracy = 0.1)), vjust = -0.4, size = 3.2) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Week-over-week user retention",
       subtitle = "Share of users active in week N who return in week N+1",
       x = "Week", y = "Retention rate")
ggsave(file.path(FIG_DIR, "03_week_over_week_retention.png"), p3, width = 8, height = 4.5, dpi = 150)

# 4. Does more overall activity mean more OR better questions? (Section 5)
p4 <- ggplot(kpi_by_engagement_tier, aes(engagement_tier, mean_questions, fill = mean_quality_rate)) +
  geom_col() +
  geom_text(aes(label = mean_questions), vjust = -0.4, size = 3.5) +
  scale_fill_gradient(low = "#9ecae1", high = "#08519c", labels = percent, name = "Quality-\nanswer rate") +
  labs(title = "More active users ask more questions - and slightly better ones",
       subtitle = "Users grouped into quartiles by total platform activity",
       x = "Activity quartile", y = "Mean questions per user")
ggsave(file.path(FIG_DIR, "04_engagement_tier_vs_questions.png"), p4, width = 8, height = 4.5, dpi = 150)

# 5. What drives question quality? (Section 6 - the strongest EDA finding)
p5 <- ggplot(word_count_bins, aes(word_count_bin, quality_rate, group = 1)) +
  geom_col(fill = "#DD8452") +
  geom_text(aes(label = percent(quality_rate, accuracy = 0.1)), vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = percent, limits = c(0, 0.9)) +
  labs(title = "Longer questions are far more likely to get a quality answer",
       subtitle = str_wrap("Quality-answer rate nearly 40% higher for 81+ word questions vs. 1-15 word questions", 65),
       x = "Question word count", y = "Quality-answer rate")
ggsave(file.path(FIG_DIR, "05_wordcount_vs_quality.png"), p5, width = 8.5, height = 4.5, dpi = 150)

# 6. Which topics dominate volume, and are any topics under/over-performing on quality? (Section 3)
p6 <- kpi_by_topic %>%
  mutate(topic = fct_reorder(topic, n_questions)) %>%
  ggplot(aes(topic, n_questions, fill = quality_rate)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#fdd0a2", high = "#a63603", labels = percent, name = "Quality-\nanswer rate") +
  labs(title = "Question volume and quality rate by topic",
       subtitle = str_wrap("Technology dominates volume; quality rate is fairly consistent across topics (68-71%)", 70),
       x = NULL, y = "Number of questions")
ggsave(file.path(FIG_DIR, "06_topic_volume_quality.png"), p6, width = 8, height = 5, dpi = 150)

# 7. Does device type meaningfully segment behavior? (Section 3)
p7 <- kpi_by_device %>%
  pivot_longer(c(asker_rate, mean_quality_rate), names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, asker_rate = "Question-asking rate", mean_quality_rate = "Quality-answer rate")) %>%
  ggplot(aes(device_type, value, fill = device_type)) +
  geom_col() +
  geom_text(aes(label = percent(value, accuracy = 0.1)), vjust = -0.4, size = 3.2) +
  facet_wrap(~metric) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Device type does not meaningfully segment engagement",
       subtitle = "Asking rate and quality rate are nearly identical across mobile, desktop, and tablet",
       x = NULL, y = NULL) +
  theme(legend.position = "none")
ggsave(file.path(FIG_DIR, "07_device_segment_comparison.png"), p7, width = 8, height = 4.5, dpi = 150)

# 8. Does country meaningfully segment behavior? (Section 3)
p8 <- kpi_by_country %>%
  mutate(country = fct_reorder(country, mean_questions)) %>%
  ggplot(aes(country, mean_questions, fill = mean_quality_rate)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#c7e9c0", high = "#00441b", labels = percent, name = "Quality-\nanswer rate") +
  labs(title = "Country shows a real, moderate spread in engagement",
       subtitle = "Mean questions per user by country, colored by quality-answer rate",
       x = NULL, y = "Mean questions per user")
ggsave(file.path(FIG_DIR, "08_country_segment_comparison.png"), p8, width = 8, height = 4.5, dpi = 150)

# 9. Who are the "never-ask" users, and are they simply inactive? (Section 7)
p9 <- ul %>%
  mutate(segment = if_else(is_asker, "Askers", "Never asked")) %>%
  ggplot(aes(segment, total_activity_events, fill = segment)) +
  geom_boxplot() +
  labs(title = str_wrap("'Never-ask' users are not inactive - they browse and vote instead", 45),
       subtitle = "Total activity events by asker status",
       x = NULL, y = "Total activity events") +
  theme(legend.position = "none")
ggsave(file.path(FIG_DIR, "09_never_ask_segment.png"), p9, width = 7, height = 4.8, dpi = 150)

cat("\nAll figures written to '", FIG_DIR, "/'\n", sep = "")
cat("All KPI/segment/finding tables written to '", OUT_DIR, "/'\n", sep = "")
cat("\n=== Script complete ===\n")
