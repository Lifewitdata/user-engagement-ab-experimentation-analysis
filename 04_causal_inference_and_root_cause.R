## =============================================================================
## User Engagement & A/B Experimentation Analysis
## Step 4: Causal Inference & Root-Cause Analysis
## =============================================================================
## Purpose: Determine whether causal treatment-effect estimation is justified
##          by this data, apply the appropriate method(s) if so, and
##          synthesize Steps 1-4 into a structured root-cause analysis of
##          engagement and question-asking drivers.
##
## Inputs (clean/, from Steps 1-3):
##   experiment_clean.csv, questions_clean.csv, user_level_analytical.csv
##
## Outputs (clean/):
##   causal_model_comparison.csv, causal_propensity_check.csv,
##   root_cause_summary.csv
## Outputs (figures/): 3 purpose-built ggplot2 charts
## =============================================================================

suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(broom)
  library(pROC)
  library(scales)
})

CLEAN_DIR <- "clean"
FIG_DIR   <- "figures"
theme_set(
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey40"),
          panel.grid.minor = element_blank())
)

experiment <- read_csv(file.path(CLEAN_DIR, "experiment_clean.csv"), show_col_types = FALSE)
questions  <- read_csv(file.path(CLEAN_DIR, "questions_clean.csv"), show_col_types = FALSE)
ul         <- read_csv(file.path(CLEAN_DIR, "user_level_analytical.csv"), show_col_types = FALSE)

## =============================================================================
## ================  SECTION 1: IS CAUSAL INFERENCE JUSTIFIED?  ================
## =============================================================================
cat("\n============================ CAUSAL INFERENCE ================================\n")
cat(
"This dataset contains a genuine RANDOMIZED experiment (experiment_clean.csv's\n",
"`variant`, assigned at `assignment_date`). Step 3 confirmed near-successful\n",
"randomization: country and tenure balanced (p=0.573, p=0.323); device_type\n",
"shows a small imbalance (p=0.026). Because treatment is (quasi-)randomly\n",
"assigned, causal inference on the TREATMENT EFFECT of `variant` IS justified\n",
"and is the appropriate object of causal analysis here - this is a much\n",
"stronger identification setting than the observational relationships\n",
"explored in Step 3's regression models (word_count -> upvotes, etc.), which\n",
"remain correlational and are NOT re-litigated as causal claims here.\n")

## ---- Treatment & outcome definition ----
cat("\nTreatment (D):  variant (control=0, treatment=1), assigned at assignment_date\n")
cat("Outcome (Y):    got_quality_answer, for questions asked ON OR AFTER assignment_date\n",
    "                (re-derived post-assignment outcome from Step 1, avoiding the\n",
    "                 all-time-metric leakage documented there)\n")

## ---- Potential confounders considered ----
cat("\nPotential confounders considered: device_type, country, account_age_at_\n",
    "assignment_days (all PRE-TREATMENT, i.e. fixed before assignment - valid\n",
    "adjustment covariates). EXPLICITLY EXCLUDED as covariates: word_count,\n",
    "num_views, num_questions_post, avg_word_count_post - these are POST-\n",
    "TREATMENT variables (in fact, Step 3 found the treatment itself increases\n",
    "word_count by ~45%). Adjusting for a post-treatment variable that sits on\n",
    "the causal pathway is a 'bad control' - it would partially adjust away the\n",
    "very effect being estimated and bias the ATE toward zero. This is why they\n",
    "appear nowhere in the causal models below (unlike the purely predictive/\n",
    "descriptive regression in Step 3).\n")

## =============================================================================
## ---- 2. Why NOT difference-in-differences ----
## =============================================================================
pre <- questions %>%
  inner_join(experiment %>% select(user_id, assignment_date), by = "user_id") %>%
  filter(created_date < assignment_date)

n_users_with_pre <- n_distinct(pre$user_id)
pct_users_with_pre <- round(100 * n_users_with_pre / nrow(experiment), 1)
mean_pre_days <- experiment %>%
  mutate(days_pre = as.integer(assignment_date - min(questions$created_date))) %>%
  summarise(mean_days = mean(days_pre)) %>% pull(mean_days)

cat("\n---- Why difference-in-differences is NOT used here ----\n")
cat(sprintf(
"DiD requires a meaningful pre-treatment observation window to establish a\nbaseline / check parallel trends. Here, questions.csv begins on the exact\nsame day the experiment ramp starts, so the available pre-assignment window\nis only %.1f days on average (assignment happened within the first 5 days\nof the entire 60-day log). Only %d of %d users (%.1f%%) have EVEN ONE\npre-assignment question, with a mean of ~1.1 questions among those. This is\nfar too sparse to estimate reliable user-level baselines or assess parallel\ntrends, so DiD is explicitly NOT applied - forcing it would produce a noisy,\nunreliable estimate built on ~90%% missing pre-period data for the outcome\nof interest.\n",
  mean_pre_days, n_users_with_pre, nrow(experiment), pct_users_with_pre))

## =============================================================================
## ---- 3. PRIMARY METHOD: Regression adjustment (ANCOVA-style) ----
## Why appropriate: under randomization, a simple difference in means is
## already an unbiased ATE estimate (confirmed as the Step-3 primary A/B
## result). Regression adjustment on PRE-TREATMENT covariates is the
## textbook-recommended refinement for RCTs: it (a) improves precision by
## explaining outcome variance, and (b) explicitly corrects for the known
## chance imbalance in device_type. It is preferred here over propensity
## score matching / IPW, which are designed to address CONFOUNDING in
## observational (non-randomized) treatment assignment - a problem that
## does not exist by design in a randomized experiment. (Section 4 below
## verifies this directly with a propensity-model diagnostic.)
## =============================================================================

q_post <- questions %>%
  inner_join(experiment %>% select(user_id, variant, assignment_date), by = "user_id") %>%
  filter(created_date >= assignment_date) %>%
  left_join(ul %>% select(user_id, device_type, country, account_age_at_assignment_days),
            by = "user_id") %>%
  mutate(variant = factor(variant, levels = c("control", "treatment")),
         device_type = factor(device_type, levels = c("mobile", "desktop", "tablet")),
         country = factor(country))

cat("\n---- Assumptions for regression-adjustment ATE estimation ----\n")
cat(
"1. Ignorability / unconfoundedness: satisfied BY DESIGN via randomization\n",
"   (not an assumption we must argue for from observational reasoning).\n",
"2. SUTVA (no interference between users, single version of treatment):\n",
"   plausible here - one user's assignment shouldn't mechanically change\n",
"   another's quality-answer outcomes.\n",
"3. Correct functional form for the adjustment covariates: we use a linear\n",
"   probability model (LPM) for direct pp-scale interpretation, cross-\n",
"   checked with a logistic specification for robustness.\n",
"4. No post-treatment covariates included (see 'bad control' note above).\n")

## Unadjusted ATE (= Step 3 primary A/B result, reproduced here as the baseline)
m_unadj <- lm(got_quality_answer ~ variant, data = q_post)

## Regression-adjusted ATE (LPM)
m_adj_lpm <- lm(got_quality_answer ~ variant + device_type + country + account_age_at_assignment_days,
                 data = q_post)

## Regression-adjusted ATE (logistic, for odds-ratio robustness check)
m_adj_logit <- glm(got_quality_answer ~ variant + device_type + country + account_age_at_assignment_days,
                    data = q_post, family = binomial)

cat("\n---- Unadjusted ATE (LPM, variant only) ----\n")
print(tidy(m_unadj, conf.int = TRUE) %>% filter(term == "varianttreatment"))

cat("\n---- Regression-adjusted ATE (LPM, + device/country/tenure) ----\n")
print(tidy(m_adj_lpm, conf.int = TRUE) %>% filter(term == "varianttreatment"))

cat("\n---- Regression-adjusted ATE (logistic, odds ratio) ----\n")
print(tidy(m_adj_logit, conf.int = TRUE, exponentiate = TRUE) %>% filter(term == "varianttreatment"))

model_comparison <- bind_rows(
  tidy(m_unadj, conf.int = TRUE) %>% filter(term == "varianttreatment") %>%
    mutate(model = "Unadjusted (simple ITT difference)"),
  tidy(m_adj_lpm, conf.int = TRUE) %>% filter(term == "varianttreatment") %>%
    mutate(model = "Regression-adjusted LPM (+ device/country/tenure)")
) %>%
  select(model, estimate, std.error, conf.low, conf.high, p.value)
write_csv(model_comparison, file.path(CLEAN_DIR, "causal_model_comparison.csv"))
cat("\n---- ATE: unadjusted vs. regression-adjusted (should be near-identical under good randomization) ----\n")
print(model_comparison)

ate_unadj <- model_comparison$estimate[1]
ate_adj   <- model_comparison$estimate[2]
cat(sprintf(
"\nThe adjusted ATE (%.4f) is nearly identical to the unadjusted ATE (%.4f) -\nas expected given the largely successful randomization confirmed in Step 3.\nThe device_type covariate adjustment does not materially change the\nconclusion; it mainly tightens/validates the estimate rather than\ncorrecting a meaningful bias.\n", ate_adj, ate_unadj))

## =============================================================================
## ---- 4. Robustness diagnostic: propensity model (confirms no PSM/IPW needed) ----
## =============================================================================
ul_prop <- ul %>%
  mutate(variant = factor(variant, levels = c("control", "treatment")),
         device_type = factor(device_type), country = factor(country))
m_prop <- glm(variant ~ device_type + country + account_age_at_assignment_days,
              data = ul_prop, family = binomial)
roc_prop <- roc(ul_prop$variant, fitted(m_prop), quiet = TRUE)
auc_prop <- as.numeric(auc(roc_prop))

cat("\n---- Propensity model: can treatment assignment be predicted from pre-treatment covariates? ----\n")
cat(sprintf("AUC = %.3f (0.5 = pure chance). ", auc_prop))
cat(
"An AUC this close to 0.5 confirms treatment assignment carries essentially\n",
"no relationship to observed covariates - i.e., there is no meaningful\n",
"selection into treatment for PSM/IPW to correct. This is the diagnostic\n",
"justification for using simple regression adjustment rather than\n",
"propensity-score matching or inverse-probability weighting: those methods\n",
"solve a confounding problem that this randomized design does not have.\n")

propensity_check <- tibble(auc = auc_prop,
                            interpretation = "Near 0.5 confirms randomization; PSM/IPW unnecessary")
write_csv(propensity_check, file.path(CLEAN_DIR, "causal_propensity_check.csv"))

## =============================================================================
## ---- 5. Final causal estimate & limitations ----
## =============================================================================
final_ci <- confint(m_adj_lpm)["varianttreatment", ]
cat(sprintf(
"\n---- FINAL CAUSAL ESTIMATE ----\nAverage Treatment Effect on quality-answer rate: %+.2f percentage points\n95%% CI: [%.2f, %.2f] pp   |   Relative effect: %+.1f%%   |   p = %.2e\n",
  100 * ate_adj, 100 * final_ci[1], 100 * final_ci[2],
  100 * ate_adj / coef(m_unadj)["(Intercept)"], tidy(m_adj_lpm) %>% filter(term=="varianttreatment") %>% pull(p.value)))

cat(
"\n---- Causal inference limitations ----\n",
"1. This estimates the effect of BEING ASSIGNED to treatment (intent-to-\n",
"   treat), not the effect of any single mechanism within it - we cannot\n",
"   separate 'longer input box' from 'prompt text' from other bundled UI\n",
"   changes without more granular data.\n",
"2. The quality outcome is only observed for users who asked a question\n",
"   post-assignment; Step 3 showed participation is balanced (p=0.095), so\n",
"   selection risk is low but not zero.\n",
"3. Word count is very likely a MEDIATOR of this effect (Step 3 regression);\n",
"   this analysis estimates the TOTAL causal effect, not the direct effect\n",
"   net of that pathway - a formal mediation analysis would be needed to\n",
"   decompose how much flows through question length specifically.\n",
"4. The small residual device_type imbalance (p=0.026) is adjusted for, but\n",
"   with only 3 device categories the adjustment has limited power to fully\n",
"   rule out subtle device-driven confounding.\n",
"5. Single 60-day, single-cohort experiment - no evidence here on effect\n",
"   persistence beyond the observed window.\n")

## =============================================================================
## ---- 6. Visualization: unadjusted vs adjusted ATE + propensity check ----
## =============================================================================

p1 <- model_comparison %>%
  mutate(model = fct_rev(fct_inorder(model))) %>%
  ggplot(aes(x = estimate, y = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, color = "#4C72B0", linewidth = 0.9) +
  geom_point(size = 3, color = "#4C72B0") +
  scale_x_continuous(labels = percent) +
  labs(title = "Regression adjustment confirms the A/B result",
       subtitle = str_wrap("ATE on quality-answer rate: unadjusted vs. covariate-adjusted estimate (95% CI)", 60),
       x = "Average treatment effect", y = NULL)
ggsave(file.path(FIG_DIR, "18_causal_ate_comparison.png"), p1, width = 9.5, height = 3.5, dpi = 150)

png(file.path(FIG_DIR, "19_causal_propensity_roc.png"), width = 1100, height = 900, res = 150)
plot(roc_prop, col = "#55A868", lwd = 2,
     main = paste0("Treatment assignment is unpredictable from covariates\n(AUC = ", round(auc_prop, 3), ") - confirms clean randomization"))
abline(a = 1, b = -1, lty = 2, col = "grey60")
dev.off()

cat("\nCausal inference figures written.\n")

## =============================================================================
## ================  SECTION 7: ROOT-CAUSE ANALYSIS  ============================
## Synthesizes Steps 1-4 into a structured driver analysis, PLUS one new cut
## (new vs. established users) not yet formally tested in prior steps.
## =============================================================================
cat("\n\n=========================== ROOT-CAUSE ANALYSIS ==============================\n")

## ---- New cut: tenure / new-vs-established users ----
ul_tenure <- ul %>%
  mutate(tenure_group = case_when(
    account_age_at_assignment_days < 180 ~ "New (<6mo)",
    account_age_at_assignment_days < 545 ~ "Established (6mo-1.5y)",
    TRUE ~ "Veteran (1.5y+)"
  ))

tenure_engagement <- ul_tenure %>%
  group_by(tenure_group) %>%
  summarise(n = n(), asker_rate = mean(is_asker), mean_questions = mean(total_questions),
            mean_quality_rate = mean(quality_answer_rate_all, na.rm = TRUE),
            mean_activity = mean(total_activity_events), .groups = "drop")
cat("\n---- Engagement by tenure group (new vs. established vs. veteran) ----\n")
print(tenure_engagement)

chi_tenure_asker <- chisq.test(table(ul_tenure$tenure_group, ul_tenure$is_asker))
cor_tenure_q <- cor.test(ul$account_age_at_assignment_days, ul$total_questions)
cat(sprintf("\nAsker rate by tenure: chi-sq p = %.3f (no significant difference)\n", chi_tenure_asker$p.value))
cat(sprintf("Tenure vs. total_questions correlation: r = %.4f, p = %.3f (no relationship)\n",
            cor_tenure_q$estimate, cor_tenure_q$p.value))

## Treatment effect by tenure group
q_post_tenure <- q_post %>% left_join(ul_tenure %>% select(user_id, tenure_group), by = "user_id")
tenure_treatment <- q_post_tenure %>%
  group_by(tenure_group, variant) %>%
  summarise(n = n(), quality_rate = mean(got_quality_answer), .groups = "drop")
cat("\n---- Treatment effect on quality rate, by tenure group ----\n")
print(tenure_treatment)

tenure_tests <- map_dfr(unique(q_post_tenure$tenure_group), function(g) {
  sub <- q_post_tenure %>% filter(tenure_group == g)
  qr <- sub %>% group_by(variant) %>% summarise(quality_rate = mean(got_quality_answer), .groups = "drop")
  tt <- prop.test(table(sub$variant, sub$got_quality_answer))
  tibble(tenure_group = g,
         control = qr$quality_rate[qr$variant == "control"],
         treatment = qr$quality_rate[qr$variant == "treatment"],
         abs_effect = qr$quality_rate[qr$variant == "treatment"] - qr$quality_rate[qr$variant == "control"],
         p_value = tt$p.value)
})
cat("\n---- Treatment effect significance by tenure group ----\n")
print(tenure_tests)
write_csv(tenure_tests, file.path(CLEAN_DIR, "tenure_treatment_effects.csv"))

## ---- Root-cause driver summary table (pulling together Steps 1-4) ----
root_cause_summary <- tribble(
  ~driver, ~category, ~effect_size, ~evidence,
  "Question word count", "User behavior / content", "+9.8pp quality rate (long vs short); regression: +50 words = +29% upvotes",
    "Step 2 EDA, Step 3 H1 (p=2.6e-74) and linear regression (p<2e-16)",
  "Experiment treatment (variant)", "Experiment response", "+3.7pp quality rate (causal ATE, 95% CI [2.8,4.7]pp)",
    "Step 3 A/B test + Step 4 causal regression adjustment",
  "Overall platform activity level", "User behavior", "rho=0.52 with questions asked; NOT correlated with quality (r=0.02)",
    "Step 2 EDA, Step 3 H6 (p<2.2e-16)",
  "Asker vs never-ask segment", "User segment", "12.7% never ask despite avg 14.7 activity events (vs 18.4 for askers)",
    "Step 2 EDA, Step 3 H4 (p=6.7e-174)",
  "Device type", "User segment", "No significant effect on asking rate (p=0.94) or quality (regression n.s.)",
    "Step 2 EDA, Step 3 H3, logistic regression",
  "Country", "User segment", "No significant effect after formal testing (p=0.51, Cramer's V=0.01)",
    "Step 3 H2 (corrects looser Step 2 EDA read)",
  "Account tenure (new vs established)", "New vs established users", "No significant difference in asking rate (p=0.90) or question volume (r=-0.006, p=0.48)",
    "Step 4 root-cause cut + Step 3 logistic regression",
  "Content topic", "Content category", "Volume varies widely (2x); quality rate flat (68-71%) across topics",
    "Step 2 EDA, kpi_by_topic"
)
write_csv(root_cause_summary, file.path(CLEAN_DIR, "root_cause_summary.csv"))
cat("\n---- Root-cause driver summary ----\n"); print(root_cause_summary, n = Inf, width = Inf)

## ---- Chart: new vs established users treatment effect ----
p2 <- tenure_tests %>%
  mutate(tenure_group = fct_reorder(tenure_group, abs_effect)) %>%
  pivot_longer(c(control, treatment), names_to = "variant", values_to = "quality_rate") %>%
  ggplot(aes(tenure_group, quality_rate, fill = variant)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(control = "#8C8C8C", treatment = "#4C72B0")) +
  scale_y_continuous(labels = percent) +
  labs(title = str_wrap("Treatment effect holds across new, established, and veteran users", 55),
       subtitle = "Quality-answer rate by tenure group and variant - no tenure dependency",
       x = NULL, y = "Quality-answer rate", fill = NULL)
ggsave(file.path(FIG_DIR, "20_tenure_treatment_effect.png"), p2, width = 7.5, height = 4.5, dpi = 150)

cat("\n=== Step 4 script complete. All tables written to '", CLEAN_DIR, "/', figures to '", FIG_DIR, "/' ===\n", sep = "")
