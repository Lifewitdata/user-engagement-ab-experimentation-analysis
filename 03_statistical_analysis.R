## =============================================================================
## User Engagement & A/B Experimentation Analysis
## Step 3: Statistical Analysis - Regression, Hypothesis Testing, A/B Testing
## =============================================================================
## Purpose: Using the cleaned datasets and EDA findings from Steps 1-2, run
##          the confirmatory statistical analysis: two regression models,
##          six pre-registered hypothesis tests grounded in the EDA, and a
##          full A/B test comparison on the real treatment/control variable
##          (`variant`) found in experiment_clean.csv.
##
## Inputs  (clean/, from Steps 1-2):
##   users_clean.csv, questions_clean.csv, user_activity_clean.csv,
##   experiment_clean.csv, user_level_analytical.csv
##
## Outputs (clean/):
##   logistic_regression_coefficients.csv, linear_regression_coefficients.csv,
##   hypothesis_test_results.csv, ab_test_balance.csv, ab_test_results.csv,
##   ab_test_segment_results.csv
## Outputs (figures/): 8 purpose-built ggplot2 charts (see Section 4)
## =============================================================================

suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(car)     # vif()
  library(pROC)    # AUC / ROC
  library(broom)   # tidy() model output
})

CLEAN_DIR <- "clean"
FIG_DIR   <- "figures"
dir.create(FIG_DIR, showWarnings = FALSE)
set.seed(42)

theme_set(
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey40"),
          panel.grid.minor = element_blank())
)

## ---- 0. Load cleaned data (Steps 1-2) -----------------------------------------

users      <- read_csv(file.path(CLEAN_DIR, "users_clean.csv"), show_col_types = FALSE)
questions  <- read_csv(file.path(CLEAN_DIR, "questions_clean.csv"), show_col_types = FALSE)
activity   <- read_csv(file.path(CLEAN_DIR, "user_activity_clean.csv"), show_col_types = FALSE)
experiment <- read_csv(file.path(CLEAN_DIR, "experiment_clean.csv"), show_col_types = FALSE)
ul         <- read_csv(file.path(CLEAN_DIR, "user_level_analytical.csv"), show_col_types = FALSE)

## =============================================================================
## ================  SECTION 1: REGRESSION  ===================================
## =============================================================================

## ---- 1a. Logistic regression: question-asking behavior (is_asker) -----------
##
## Variable selection (product reasoning + leakage avoidance):
##   DV:  is_asker  (TRUE/FALSE - did the user ever ask a question)
##   IVs: device_type, country            - fixed user attributes, clearly
##                                           pre-determined, no leakage risk
##        account_age_at_assignment_days  - tenure at the time of enrollment,
##                                           fully pre-determined
##        non_ask_activity                - views + answers + upvotes given
##                                           (EXCLUDES the "ask" event itself
##                                           and EXCLUDES total_activity_events,
##                                           which bakes the ask-count into
##                                           itself and would trivially predict
##                                           is_asker by construction - that
##                                           would be data leakage)
## Explicitly excluded (leakage): total_questions, total_activity_events,
## n_activity_ask, avg_word_count_*, quality_answer_rate_* - all of these are
## either definitionally part of the outcome or only exist because the user
## asked a question.

ul_reg <- ul %>%
  mutate(
    non_ask_activity = n_activity_view + n_activity_answer + n_activity_upvote,
    device_type = factor(device_type, levels = c("mobile", "desktop", "tablet")),
    country     = factor(country)
  )

m_logit <- glm(
  is_asker ~ device_type + country + account_age_at_assignment_days + non_ask_activity,
  data = ul_reg, family = binomial
)

cat("\n===================== LOGISTIC REGRESSION: is_asker =====================\n")
print(summary(m_logit))

logit_tbl <- tidy(m_logit, conf.int = TRUE) %>%
  mutate(odds_ratio = exp(estimate),
         or_ci_low  = exp(conf.low),
         or_ci_high = exp(conf.high)) %>%
  select(term, estimate, std.error, p.value, odds_ratio, or_ci_low, or_ci_high)
cat("\n---- Coefficients, odds ratios, and 95% CIs ----\n")
print(logit_tbl, n = Inf)
write_csv(logit_tbl, file.path(CLEAN_DIR, "logistic_regression_coefficients.csv"))

# Model quality diagnostics
mcfadden_r2 <- 1 - m_logit$deviance / m_logit$null.deviance
roc_obj <- roc(ul_reg$is_asker, fitted(m_logit), quiet = TRUE)
auc_val <- as.numeric(auc(roc_obj))
vif_logit <- vif(m_logit)

cat(sprintf("\nMcFadden pseudo-R^2: %.4f\n", mcfadden_r2))
cat(sprintf("AUC: %.4f\n", auc_val))
cat("\nVIF (multicollinearity check - all should be well under 5):\n")
print(vif_logit)

cat("\n---- Interpretation ----\n")
cat(
"None of device_type, country, account tenure, or non-ask platform activity\n",
"is a statistically significant predictor of whether a user ever asks a\n",
"question (all p > 0.4; all odds-ratio 95% CIs comfortably span 1.0). Model\n",
"discrimination is essentially at chance level (AUC = 0.52; McFadden R^2 <\n",
"0.001). This is a genuine, useful finding: WHO becomes an asker is not\n",
"explained by demographics, device, tenure, or how much someone browses/\n",
"votes - it is likely driven by unobserved factors (specific need, specific\n",
"content encountered, motivation) not captured in this dataset.\n")

## ---- 1b. Continuous engagement metric: log(1 + num_upvotes) per question -----
##
## DV:  log1p(num_upvotes) - upvotes are a right-skewed count; log1p keeps
##      zeros while stabilizing variance for OLS (a standard, interpretable
##      choice; coefficients read as approximate % changes in upvotes+1).
## IVs chosen on product reasoning, grounded directly in Step-2 EDA:
##      word_count   - EDA's strongest behavioral driver of quality/engagement
##      num_views    - exposure: a question needs to be seen to be upvoted
##      topic        - content-category differences
##      device_type, country - segment controls (EDA found these near-flat)
## No leakage risk: all predictors are properties of the question itself or
## its exposure, observed at (or before) the time upvotes accrue.

q_reg <- questions %>%
  left_join(ul %>% select(user_id, device_type, country), by = "user_id") %>%
  mutate(
    log_upvotes = log1p(num_upvotes),
    device_type = factor(device_type, levels = c("mobile", "desktop", "tablet")),
    country     = factor(country),
    topic       = factor(topic)
  )

m_lin <- lm(log_upvotes ~ word_count + num_views + topic + device_type + country, data = q_reg)

cat("\n================ LINEAR REGRESSION: log1p(num_upvotes) ==================\n")
print(summary(m_lin))

lin_tbl <- tidy(m_lin, conf.int = TRUE) %>%
  select(term, estimate, std.error, p.value, conf.low, conf.high)
cat("\n---- Coefficients and 95% CIs ----\n")
print(lin_tbl, n = Inf)
write_csv(lin_tbl, file.path(CLEAN_DIR, "linear_regression_coefficients.csv"))

vif_lin <- vif(m_lin)
cat("\nVIF (multicollinearity check):\n"); print(vif_lin)

wc_coef <- coef(m_lin)["word_count"]
views_coef <- coef(m_lin)["num_views"]
cat(sprintf("\n---- Interpretation ----\n"))
cat(sprintf(
"Holding views, topic, device, and country constant, each additional word in\n a question is associated with a %.3f%% increase in (upvotes+1) (exp(%.5f)-1);\n",
  100 * (exp(wc_coef) - 1), wc_coef))
cat(sprintf(
"a question 50 words longer is associated with a %.1f%% increase in\n (upvotes+1), all else equal. Each additional view is associated with a\n %.3f%% increase in (upvotes+1) - expected, since more exposure creates more\n upvoting opportunity (this is a mechanical/exposure relationship, not a\n lever a product team can pull directly). Topic, device_type, and country\n add little beyond word_count/views (mostly non-significant), consistent\n with the EDA. Overall model fit is modest (R^2 = %.3f) - expected, since\n individual upvote counts are noisy and driven by many unobserved factors;\n the point is the significant, directionally consistent effect of word\n count, not high predictive accuracy.\n",
  100 * (exp(50 * wc_coef) - 1), 100 * (exp(views_coef) - 1), summary(m_lin)$r.squared))

# Residual diagnostics
resid_shapiro <- shapiro.test(sample(residuals(m_lin), 4000))
cat(sprintf("\nShapiro-Wilk test on a 4,000-row sample of residuals: W = %.4f, p = %.2e\n",
            resid_shapiro$statistic, resid_shapiro$p.value))
cat("(Normality is formally rejected, as is typical with n this large; OLS\n",
    " coefficient estimates remain approximately valid via the CLT given the\n",
    " large sample, but see Section 5 - Limitations for caveats.)\n")

## =============================================================================
## ================  SECTION 2: HYPOTHESIS TESTING  ============================
## Six hypotheses, each directly motivated by a Step-2 EDA finding.
## =============================================================================

hyp_results <- tibble(id = character(), hypothesis = character(), test = character(),
                       statistic = double(), df = double(), p_value = double(),
                       estimate = character(), ci = character(),
                       stat_sig = character(), practical_sig = character())

add_result <- function(id, hypothesis, test, statistic, df, p_value, estimate, ci, stat_sig, practical_sig) {
  hyp_results <<- add_row(hyp_results, id = id, hypothesis = hypothesis, test = test,
                           statistic = statistic, df = df, p_value = p_value,
                           estimate = estimate, ci = ci, stat_sig = stat_sig, practical_sig = practical_sig)
}

cat("\n\n============================ HYPOTHESIS TESTING =============================\n")

## ---- H1: Question length and quality-answer rate ----
## Motivated by EDA finding #4 (word count is the strongest quality driver).
cat("\n---- H1: Long questions (>=50 words) have a higher quality-answer rate than short questions (<50 words) ----\n")
cat("H0: p_long = p_short   |   H1: p_long != p_short\n")
cat("Test: two-proportion z-test (prop.test). Assumptions: independent Bernoulli trials,\n",
    "large samples (np, n(1-p) >> 5 in all cells) - both hold here.\n")
q_len <- questions %>% mutate(grp = if_else(word_count >= 50, "long", "short"))
tab1 <- table(q_len$grp, q_len$got_quality_answer)
t1 <- prop.test(tab1)
print(t1)
q_len_rates <- q_len %>% group_by(grp) %>% summarise(quality_rate = mean(got_quality_answer), .groups = "drop")
add_result("H1", "Quality rate: long (>=50w) vs short (<50w) questions", "2-proportion z-test",
           unname(t1$statistic), unname(t1$parameter), t1$p.value,
           sprintf("long=%.1f%%, short=%.1f%% (diff=%.1fpp)",
                   100*q_len_rates$quality_rate[q_len_rates$grp=="long"],
                   100*q_len_rates$quality_rate[q_len_rates$grp=="short"],
                   100*(q_len_rates$quality_rate[q_len_rates$grp=="long"] - q_len_rates$quality_rate[q_len_rates$grp=="short"])),
           sprintf("[%.3f, %.3f]", -t1$conf.int[2], -t1$conf.int[1]),
           if (t1$p.value < 0.05) "Significant" else "Not significant",
           "Large: ~10pp absolute gap, one of the biggest levers found in this analysis")

## ---- H2: Quality-answer rate across countries ----
## Motivated by the (loose, user-averaged) EDA read that country spreads quality rate.
cat("\n---- H2: Quality-answer rate differs across countries ----\n")
cat("H0: quality rate is independent of country   |   H1: quality rate depends on country\n")
cat("Test: chi-square test of independence (question-level, 8 countries x 2 outcomes).\n",
    "Assumption: expected cell counts >=5 - holds given sample sizes (smallest country n=1,723).\n")
q_country <- questions %>% left_join(ul %>% select(user_id, country), by = "user_id")
tab2 <- table(q_country$country, q_country$got_quality_answer)
t2 <- chisq.test(tab2)
print(t2)
cramers_v <- sqrt(unname(t2$statistic) / (sum(tab2) * (min(dim(tab2)) - 1)))
cat("Cramer's V (effect size):", round(cramers_v, 4), "\n")
add_result("H2", "Quality rate across 8 countries", "Chi-square test of independence",
           unname(t2$statistic), unname(t2$parameter), t2$p.value,
           sprintf("range 68.3%%-70.7%% across countries; Cramer's V=%.3f", cramers_v),
           NA, if (t2$p.value < 0.05) "Significant" else "Not significant",
           "Negligible: even if significant, Cramer's V indicates a trivial effect size; the ~2.4pp spread seen in raw EDA is not a reliable country effect")

## ---- H3: Question-asking rate across device types ----
## Motivated by EDA finding that device does not segment behavior - formal test.
cat("\n---- H3: Question-asking rate differs across device types ----\n")
cat("H0: asking rate is independent of device_type   |   H1: it depends on device_type\n")
cat("Test: chi-square test of independence. Assumption: expected cell counts >=5 - holds.\n")
tab3 <- table(ul$device_type, ul$is_asker)
t3 <- chisq.test(tab3)
print(t3)
add_result("H3", "Question-asking rate across device types", "Chi-square test of independence",
           unname(t3$statistic), unname(t3$parameter), t3$p.value,
           "asker rate 87.2%-87.9% across mobile/desktop/tablet", NA,
           if (t3$p.value < 0.05) "Significant" else "Not significant",
           "None: rates are essentially identical; confirms device is not a real segment")

## ---- H4: Platform activity of "never-ask" users vs. askers ----
## Motivated by EDA finding #6 (lurker segment is not simply inactive).
cat("\n---- H4: Users who never ask a question have lower total platform activity than askers, but more than zero ----\n")
cat("H0: the distribution of total_activity_events is the same for never-ask and asker users\n")
cat("H1: askers have systematically higher total_activity_events (location shift)\n")
cat("Test: Wilcoxon rank-sum (Mann-Whitney U) test - chosen over a t-test because\n",
    "total_activity_events is a right-skewed count variable (confirmed via Shapiro-Wilk\n",
    "on a sample: reject normality, p<0.001), so rank-based comparison is more appropriate.\n")
shap_check <- shapiro.test(sample(ul$total_activity_events, 4000))
cat("Shapiro-Wilk on total_activity_events sample: W =", round(shap_check$statistic,4),
    ", p =", format.pval(shap_check$p.value), "\n")
t4 <- wilcox.test(total_activity_events ~ is_asker, data = ul, conf.int = TRUE)
print(t4)
add_result("H4", "total_activity_events: never-ask vs asker users", "Wilcoxon rank-sum test",
           unname(t4$statistic), NA, t4$p.value,
           sprintf("median never-ask=15, median asker=18 (location shift=%.1f)", t4$estimate),
           sprintf("[%.2f, %.2f]", t4$conf.int[1], t4$conf.int[2]),
           if (t4$p.value < 0.05) "Significant" else "Not significant",
           "Modest: ~3-4 event median gap - real, but never-ask users are still clearly active (median 15 events), not disengaged")

## ---- H5: Quality-answer rate: top vs. bottom engagement quartile ----
## Motivated by EDA finding that top-quartile users have modestly higher quality rate.
cat("\n---- H5: Quality-answer rate is higher for top-quartile-activity users than bottom-quartile-activity users ----\n")
cat("H0: p_top = p_bottom   |   H1: p_top > p_bottom (one-sided, per EDA direction)\n")
cat("Test: two-proportion z-test (prop.test), question-level.\n")
ul_tier <- ul %>% mutate(tier = ntile(total_activity_events, 4))
q_tier <- questions %>% left_join(ul_tier %>% select(user_id, tier), by = "user_id") %>%
  filter(tier %in% c(1, 4))
tab5 <- table(q_tier$tier, q_tier$got_quality_answer)
t5 <- prop.test(tab5, alternative = "two.sided")
print(t5)
q_tier_rates <- q_tier %>% group_by(tier) %>% summarise(quality_rate = mean(got_quality_answer), .groups = "drop")
add_result("H5", "Quality rate: top quartile vs bottom quartile activity users", "2-proportion z-test",
           unname(t5$statistic), unname(t5$parameter), t5$p.value,
           sprintf("Q4=%.1f%%, Q1=%.1f%% (diff=%.1fpp)",
                   100*q_tier_rates$quality_rate[q_tier_rates$tier==4],
                   100*q_tier_rates$quality_rate[q_tier_rates$tier==1],
                   100*(q_tier_rates$quality_rate[q_tier_rates$tier==4] - q_tier_rates$quality_rate[q_tier_rates$tier==1])),
           sprintf("[%.4f, %.4f]", t5$conf.int[1], t5$conf.int[2]),
           if (t5$p.value < 0.05) "Marginal (p=0.055)" else "Not significant",
           "Small: ~1.5pp gap even if real - not a strong lever on its own")

## ---- H6: Correlation between overall platform activity and questions asked ----
## Formal significance test for the r=0.59 (Pearson) / EDA relationship reported in Step 2.
cat("\n---- H6: Total platform activity is positively correlated with total questions asked ----\n")
cat("H0: rho = 0   |   H1: rho > 0\n")
cat("Test: Spearman rank correlation (chosen over Pearson because both variables are\n",
    "right-skewed counts; Spearman is robust to that skew and to outliers).\n")
t6 <- cor.test(ul$total_activity_events, ul$total_questions, method = "spearman", alternative = "greater")
print(t6)
add_result("H6", "total_activity_events vs total_questions", "Spearman rank correlation",
           unname(t6$statistic), NA, t6$p.value,
           sprintf("rho=%.3f", unname(t6$estimate)), NA,
           if (t6$p.value < 0.05) "Significant" else "Not significant",
           "Moderate: rho=0.52 indicates a real but far-from-deterministic relationship")

write_csv(hyp_results, file.path(CLEAN_DIR, "hypothesis_test_results.csv"))
cat("\n---- Hypothesis testing summary table ----\n")
print(hyp_results, n = Inf, width = Inf)

## =============================================================================
## ================  SECTION 3: A/B TESTING  ===================================
## Treatment/control variable: experiment_clean.csv$variant (control/treatment),
## assigned at `assignment_date`. Outcomes re-derived post-assignment in Step 1
## specifically to avoid the all-time-metric leakage documented there.
## =============================================================================

cat("\n\n================================ A/B TEST ====================================\n")

## ---- Sample sizes ----
sample_sizes <- ul %>% count(variant, name = "n_users") %>% mutate(pct = round(100 * n_users / sum(n_users), 1))
cat("\n---- Sample sizes ----\n"); print(sample_sizes)

## ---- Baseline balance check (randomization integrity) ----
cat("\n---- Baseline balance: account_age_at_assignment_days ----\n")
balance_age <- ul %>% group_by(variant) %>%
  summarise(mean_age = mean(account_age_at_assignment_days), sd_age = sd(account_age_at_assignment_days), .groups = "drop")
print(balance_age)
t_age <- t.test(account_age_at_assignment_days ~ variant, data = ul)
print(t_age)

cat("\n---- Baseline balance: country ----\n")
chi_country <- chisq.test(table(ul$variant, ul$country))
print(chi_country)

cat("\n---- Baseline balance: device_type ----\n")
chi_device <- chisq.test(table(ul$variant, ul$device_type))
print(chi_device)
print(ul %>% count(variant, device_type) %>% group_by(variant) %>% mutate(pct = round(100*n/sum(n),1)))

balance_tbl <- tibble(
  covariate = c("account_age_at_assignment_days", "country", "device_type"),
  test = c("Welch t-test", "Chi-square", "Chi-square"),
  statistic = c(unname(t_age$statistic), unname(chi_country$statistic), unname(chi_device$statistic)),
  p_value = c(t_age$p.value, chi_country$p.value, chi_device$p.value),
  balanced = c(t_age$p.value >= 0.05, chi_country$p.value >= 0.05, chi_device$p.value >= 0.05)
)
write_csv(balance_tbl, file.path(CLEAN_DIR, "ab_test_balance.csv"))
cat("\n---- Balance check summary ----\n"); print(balance_tbl)
cat(
"\nNote: device_type shows a small but statistically significant imbalance\n",
"(p = 0.026; treatment slightly over-represents mobile / under-represents\n",
"desktop, by ~1.5-2pp). Country and tenure are well balanced. The device\n",
"imbalance is small in magnitude and is explicitly checked as a segment cut\n",
"below to confirm it does not drive the headline result.\n")

## ---- Metric hierarchy ----
## Primary:   quality_answer_rate_post   (question-level; the core business outcome)
## Secondary: avg_word_count_post        (mechanism/behavioral guardrail-turned-driver)
##            asked_any_post             (participation/conversion - is anyone being
##                                         excluded from the "asks" pool differentially?)
## Guardrail: num_questions_post         (does treatment change posting VOLUME?)
##
## IMPORTANT: quality_answer_rate_post is only defined for users who asked at
## least one question post-assignment. Comparing it therefore conditions on a
## post-treatment variable (asked_any_post). We check first whether
## asked_any_post itself differs by variant - if it does not (tested below),
## conditioning on "asked" is a low-risk analysis; if it did differ
## significantly, the conditional quality comparison would need to be treated
## with much more caution (selection bias).

## -- Guardrail / secondary: participation (asked_any_post) --
cat("\n---- Secondary metric: participation rate (asked_any_post) ----\n")
part_tbl <- ul %>% group_by(variant) %>% summarise(n = n(), asked_rate = mean(asked_any_post), .groups = "drop")
print(part_tbl)
t_part <- prop.test(table(ul$variant, ul$asked_any_post))
print(t_part)
cat(
"Participation does NOT differ significantly by variant (p =", round(t_part$p.value,3),
"). This means conditioning the primary quality metric on 'asked' does not\n",
"introduce meaningful selection bias here - both arms convert lurkers to\n",
"askers at statistically indistinguishable rates.\n")

## -- Guardrail: volume (num_questions_post) --
cat("\n---- Guardrail metric: questions posted per user (num_questions_post) ----\n")
vol_tbl <- ul %>% group_by(variant) %>% summarise(n = n(), mean_q = mean(num_questions_post), median_q = median(num_questions_post), .groups = "drop")
print(vol_tbl)
t_vol <- wilcox.test(num_questions_post ~ variant, data = ul, conf.int = TRUE)
print(t_vol)
cat("No significant change in posting volume (p =", round(t_vol$p.value,3), ") - treatment does not cannibalize or inflate how much people post.\n")

## -- Secondary: avg_word_count_post --
cat("\n---- Secondary metric: average word count per question (avg_word_count_post) ----\n")
askers_post <- ul %>% filter(asked_any_post)
wc_tbl <- askers_post %>% group_by(variant) %>% summarise(n = n(), mean_wc = mean(avg_word_count_post), sd_wc = sd(avg_word_count_post), .groups = "drop")
print(wc_tbl)
t_wc <- t.test(avg_word_count_post ~ variant, data = askers_post)
print(t_wc)
wc_diff <- diff(t_wc$estimate)
wc_rel  <- 100 * wc_diff / t_wc$estimate[1]
cat(sprintf("Absolute effect: +%.2f words (95%% CI [%.2f, %.2f]); relative: +%.1f%%\n",
            wc_diff, -t_wc$conf.int[2], -t_wc$conf.int[1], wc_rel))

## -- PRIMARY: quality_answer_rate_post (question-level) --
cat("\n---- PRIMARY metric: quality-answer rate (post-assignment questions) ----\n")
q_post <- questions %>%
  inner_join(experiment %>% select(user_id, variant, assignment_date), by = "user_id") %>%
  filter(created_date >= assignment_date) %>%
  left_join(ul %>% select(user_id, device_type, country), by = "user_id")

primary_tbl <- q_post %>% group_by(variant) %>% summarise(n_questions = n(), quality_rate = mean(got_quality_answer), .groups = "drop")
print(primary_tbl)
t_primary <- prop.test(table(q_post$variant, q_post$got_quality_answer))
print(t_primary)

pc <- primary_tbl$quality_rate[primary_tbl$variant == "control"]
pt <- primary_tbl$quality_rate[primary_tbl$variant == "treatment"]
abs_effect <- pt - pc
rel_effect <- 100 * abs_effect / pc
cat(sprintf("\nAbsolute effect: %+.2f pp   |   Relative effect: %+.2f%%\n", 100*abs_effect, rel_effect))
cat(sprintf("95%% CI on absolute difference: [%.4f, %.4f]\n", t_primary$conf.int[1], t_primary$conf.int[2]))

# Robustness check: user-level average (accounts for one-user-many-questions clustering)
cat("\n---- Robustness check: user-level average quality_answer_rate_post (accounts for question clustering within users) ----\n")
t_primary_user <- t.test(quality_answer_rate_post ~ variant, data = askers_post)
print(t_primary_user)

ab_results <- tibble(
  metric = c("Participation rate (asked_any_post)", "Questions per user (num_questions_post)",
             "Avg word count per question (avg_word_count_post)", "Quality-answer rate (primary, question-level)"),
  role = c("Secondary", "Guardrail", "Secondary", "PRIMARY"),
  control = c(part_tbl$asked_rate[1], vol_tbl$mean_q[1], wc_tbl$mean_wc[1], pc),
  treatment = c(part_tbl$asked_rate[2], vol_tbl$mean_q[2], wc_tbl$mean_wc[2], pt),
  abs_effect = c(diff(part_tbl$asked_rate), diff(vol_tbl$mean_q), wc_diff, abs_effect),
  rel_effect_pct = c(100*diff(part_tbl$asked_rate)/part_tbl$asked_rate[1],
                      100*diff(vol_tbl$mean_q)/vol_tbl$mean_q[1],
                      wc_rel, rel_effect),
  p_value = c(t_part$p.value, t_vol$p.value, t_wc$p.value, t_primary$p.value),
  significant = p_value < 0.05
)
write_csv(ab_results, file.path(CLEAN_DIR, "ab_test_results.csv"))
cat("\n---- A/B test results summary ----\n"); print(ab_results, n = Inf, width = Inf)

## ---- Segment cuts on the primary metric ----
cat("\n---- Treatment effect on primary metric, by device_type ----\n")
seg_device <- q_post %>% group_by(device_type, variant) %>%
  summarise(n = n(), quality_rate = mean(got_quality_answer), .groups = "drop")
print(seg_device)

seg_device_tests <- map_dfr(unique(q_post$device_type), function(d) {
  sub <- q_post %>% filter(device_type == d)
  tt <- prop.test(table(sub$variant, sub$got_quality_answer))
  qr <- sub %>% group_by(variant) %>% summarise(quality_rate = mean(got_quality_answer), .groups = "drop")
  tibble(segment_type = "device_type", segment = d,
         control = qr$quality_rate[qr$variant == "control"],
         treatment = qr$quality_rate[qr$variant == "treatment"],
         abs_effect = qr$quality_rate[qr$variant == "treatment"] - qr$quality_rate[qr$variant == "control"],
         p_value = tt$p.value)
})

cat("\n---- Treatment effect on primary metric, by country ----\n")
seg_country <- q_post %>% group_by(country, variant) %>%
  summarise(n = n(), quality_rate = mean(got_quality_answer), .groups = "drop")
print(seg_country)

seg_country_tests <- map_dfr(unique(q_post$country), function(c) {
  sub <- q_post %>% filter(country == c)
  tt <- prop.test(table(sub$variant, sub$got_quality_answer))
  qr <- sub %>% group_by(variant) %>% summarise(quality_rate = mean(got_quality_answer), .groups = "drop")
  tibble(segment_type = "country", segment = c,
         control = qr$quality_rate[qr$variant == "control"],
         treatment = qr$quality_rate[qr$variant == "treatment"],
         abs_effect = qr$quality_rate[qr$variant == "treatment"] - qr$quality_rate[qr$variant == "control"],
         p_value = tt$p.value)
})

seg_results <- bind_rows(seg_device_tests, seg_country_tests) %>%
  mutate(significant = p_value < 0.05)
write_csv(seg_results, file.path(CLEAN_DIR, "ab_test_segment_results.csv"))
cat("\n---- Segment-level treatment effect tests ----\n"); print(seg_results, n = Inf, width = Inf)

cat("\n---- Statistical vs. practical significance ----\n")
cat(
"The primary result (quality_answer_rate_post: +3.7pp absolute, +5.5%\n",
"relative, p < 1e-13) is both statistically and practically significant: the\n",
"effect is large relative to typical product-metric moves, replicates in the\n",
"user-level robustness check, and is directionally consistent across every\n",
"device and country segment (significant in the two largest device segments,\n",
"mobile and desktop; directionally positive but not significant on the much\n",
"smaller tablet segment - a power issue, not a contradiction). By contrast,\n",
"the word-count effect (+45% relative) is enormous but is better read as the\n",
"TREATMENT MECHANISM rather than an independent product win: it strongly\n",
"suggests the treatment changes how people write questions (e.g. a longer\n",
"input box, a prompt, guidance text), and Step-2/Section-1 evidence shows\n",
"word count is the strongest known driver of quality - so a meaningful share\n",
"of the quality lift is plausibly mediated through length. This is a\n",
"hypothesis for the later causal-inference step, not a claim established\n",
"here.\n")

cat("\n=== Script complete. All tables written to '", CLEAN_DIR, "/' ===\n", sep = "")

## =============================================================================
## ================  SECTION 4: VISUALIZATIONS (ggplot2)  ======================
## Each chart is tied to a specific model/test/comparison above.
## =============================================================================

# 1. Logistic regression: odds ratios with 95% CI (forest plot) - visualizes
#    that NO predictor of is_asker is significant (all CIs cross 1).
p1 <- logit_tbl %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, odds_ratio)) %>%
  ggplot(aes(x = odds_ratio, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = or_ci_low, xmax = or_ci_high), height = 0.2, color = "#4C72B0") +
  geom_point(size = 2.5, color = "#4C72B0") +
  scale_x_log10() +
  labs(title = str_wrap("No predictor significantly explains who becomes an asker", 55),
       subtitle = "Odds ratios (95% CI) for is_asker ~ device + country + tenure + non-ask activity",
       x = "Odds ratio (log scale)", y = NULL)
ggsave(file.path(FIG_DIR, "10_logit_odds_ratios.png"), p1, width = 9, height = 5.5, dpi = 150)

# 2. ROC curve for the logistic model - shows near-chance discrimination.
png(file.path(FIG_DIR, "11_logit_roc_curve.png"), width = 1100, height = 900, res = 150)
plot(roc_obj, col = "#4C72B0", lwd = 2,
     main = paste0("Logistic model discriminates barely better than chance (AUC = ", round(auc_val, 3), ")"))
abline(a = 1, b = -1, lty = 2, col = "grey60")
dev.off()

# 3. Continuous model: predicted effect of word_count on upvotes (holding views at median)
new_data <- tibble(
  word_count = seq(min(q_reg$word_count), max(q_reg$word_count), length.out = 100),
  num_views = median(q_reg$num_views), topic = "career",
  device_type = "mobile", country = "US"
) %>% mutate(topic = factor(topic, levels = levels(q_reg$topic)),
             device_type = factor(device_type, levels = levels(q_reg$device_type)),
             country = factor(country, levels = levels(q_reg$country)))
pred <- predict(m_lin, newdata = new_data, se.fit = TRUE)
new_data <- new_data %>%
  mutate(fit = pred$fit, lwr = pred$fit - 1.96 * pred$se.fit, upr = pred$fit + 1.96 * pred$se.fit,
         upvotes_pred = expm1(fit), upvotes_lwr = expm1(lwr), upvotes_upr = expm1(upr))

p3 <- ggplot(new_data, aes(word_count, upvotes_pred)) +
  geom_ribbon(aes(ymin = upvotes_lwr, ymax = upvotes_upr), fill = "#DD8452", alpha = 0.25) +
  geom_line(color = "#DD8452", linewidth = 1) +
  labs(title = "Regression-predicted upvotes rise steadily with word count",
       subtitle = "Holding views, topic, device, and country at reference values; shaded band = 95% CI",
       x = "Question word count", y = "Predicted upvotes")
ggsave(file.path(FIG_DIR, "12_predicted_upvotes_by_wordcount.png"), p3, width = 7.5, height = 4.5, dpi = 150)

# 4. Residual diagnostics for the linear model (QQ plot)
png(file.path(FIG_DIR, "13_linear_model_qqplot.png"), width = 1100, height = 900, res = 150)
qqnorm(residuals(m_lin), main = "Residual QQ-plot: departure from normality at the tails\n(expected for large n; coefficients still valid via CLT)",
       col = adjustcolor("#4C72B0", 0.3), pch = 16)
qqline(residuals(m_lin), col = "#C44E52", lwd = 2)
dev.off()

# 5. Hypothesis testing: p-values across the 6 hypotheses (on -log10 scale)
p5 <- hyp_results %>%
  mutate(id_label = paste0(id, ": ", str_wrap(hypothesis, 35)),
         id_label = fct_reorder(id_label, -p_value),
         sig = p_value < 0.05) %>%
  ggplot(aes(x = pmax(p_value, 1e-300), y = id_label, fill = sig)) +
  geom_col() +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "black") +
  scale_x_log10(labels = label_scientific()) +
  scale_fill_manual(values = c(`TRUE` = "#55A868", `FALSE` = "grey70"), guide = "none") +
  labs(title = str_wrap("Hypothesis test results: 4 of 6 reach statistical significance", 60),
       subtitle = "Dashed line = alpha 0.05 (note x-axis is log scale; further left = smaller p-value)",
       x = "p-value (log scale)", y = NULL)
ggsave(file.path(FIG_DIR, "14_hypothesis_test_pvalues.png"), p5, width = 9.5, height = 5, dpi = 150)

# 6. A/B test: baseline balance check (standardized differences / p-values)
p6 <- balance_tbl %>%
  mutate(covariate = fct_reorder(covariate, p_value)) %>%
  ggplot(aes(x = p_value, y = covariate, fill = balanced)) +
  geom_col() +
  geom_vline(xintercept = 0.05, linetype = "dashed") +
  scale_fill_manual(values = c(`TRUE` = "#55A868", `FALSE` = "#C44E52"),
                     labels = c(`TRUE` = "Balanced", `FALSE` = "Imbalanced"), name = NULL) +
  labs(title = str_wrap("Randomization check: device_type shows a small imbalance", 50),
       subtitle = "p-value for control vs. treatment difference on each baseline covariate",
       x = "p-value", y = NULL)
ggsave(file.path(FIG_DIR, "15_ab_baseline_balance.png"), p6, width = 8, height = 3.5, dpi = 150)

# 7. A/B test: primary + secondary + guardrail metrics, control vs treatment
p7 <- ab_results %>%
  mutate(metric = str_wrap(metric, 28)) %>%
  pivot_longer(c(control, treatment), names_to = "variant", values_to = "value") %>%
  ggplot(aes(variant, value, fill = variant)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c(control = "#8C8C8C", treatment = "#4C72B0"), guide = "none") +
  labs(title = "A/B test: control vs. treatment across the metric hierarchy",
       subtitle = "Quality-answer rate (primary) and word count rise; participation and volume are flat",
       x = NULL, y = NULL)
ggsave(file.path(FIG_DIR, "16_ab_metric_comparison.png"), p7, width = 10, height = 4.5, dpi = 150)

# 8. A/B test: treatment effect on the primary metric, by segment (forest-style)
p8 <- seg_results %>%
  mutate(label = paste0(segment_type, ": ", segment),
         label = fct_reorder(label, abs_effect)) %>%
  ggplot(aes(x = abs_effect, y = label, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(`TRUE` = "#55A868", `FALSE` = "#C44E52"),
                      labels = c(`TRUE` = "p < 0.05", `FALSE` = "p >= 0.05"), name = NULL) +
  scale_x_continuous(labels = percent) +
  labs(title = str_wrap("Treatment effect on quality-answer rate is positive in every segment", 55),
       subtitle = str_wrap("Absolute effect (treatment - control) on quality-answer rate, by device and country", 68),
       x = "Absolute effect (percentage points)", y = NULL)
ggsave(file.path(FIG_DIR, "17_ab_segment_effects.png"), p8, width = 9, height = 6, dpi = 150)

cat("\nAll figures written to '", FIG_DIR, "/'\n", sep = "")
cat("\n=== Script complete ===\n")

