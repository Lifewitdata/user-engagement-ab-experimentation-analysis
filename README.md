# User Engagement & A/B Experimentation Analysis
### A Product Analytics Case Study on a Quora-style Q&A Platform | R

**Author's note:** This is an end-to-end product analytics case study built entirely in R, covering data cleaning, exploratory analysis, statistical inference, A/B testing, and causal inference on a Q&A / knowledge-sharing platform's engagement data. Every number in this document is pulled directly from the scripts in this repository — nothing is invented.

---

## 1. Project Overview

This project analyzes user engagement and a live product experiment on a Q&A platform (a Quora-style application where users ask questions, view, answer, and upvote content). It was executed in four stages, each with its own reproducible R script:

| Stage | Script | Focus |
|---|---|---|
| 1 | `01_data_cleaning_eda.R` | Data inspection, quality audit, cleaning |
| 2 | `02_product_analytics_eda.R` | Product KPIs, segments, trends, exploratory analysis |
| 3 | `03_statistical_analysis.R` | Regression, hypothesis testing, A/B testing |
| 4 | `04_causal_inference_and_root_cause.R` | Causal inference, root-cause synthesis |

All four stages read/write from a shared `clean/` and `figures/` folder, so the pipeline runs end-to-end with `Rscript 0X_....R` in sequence.

---

## 2. Business Problem

The platform wants to know:
1. **How engaged are users**, and what does that engagement actually look like given the data available?
2. **What drives users to ask questions**, and what drives the quality of the answers those questions receive?
3. **Does a live product experiment** (`variant`: control vs. treatment) actually improve outcomes — and is that effect *causal*, not just correlational?
4. **Where should the product team focus next** — which levers are real, and which apparent patterns are noise?

---

## 3. Analytical Questions

- What are the core engagement KPIs, and how do they break down by user segment?
- Are there meaningful trends, anomalies, or seasonality in engagement over time?
- What predicts whether a user asks a question, and what predicts the quality of the answers they receive?
- Do specific, EDA-motivated hypotheses about engagement hold up under formal statistical testing?
- Does the platform's A/B experiment produce a statistically **and** practically significant effect?
- Is that effect *causal*, and if so, by how much — with what assumptions and limitations?
- What are the true root-cause drivers of engagement, and which commonly-assumed drivers (device, country) turn out not to matter?

---

## 4. Dataset

Four raw CSVs, provided as-is (`raw/`), covering a **60-day observation window (2026-06-01 to 2026-07-30)**:

| File | Rows | Grain | Key fields |
|---|---|---|---|
| `users.csv` | 12,000 | 1 row / user | `user_id`, `signup_date`, `country`, `device_type` |
| `questions.csv` | 37,719 | 1 row / question | `question_id`, `user_id`, `topic`, `created_date`, `word_count`, `num_views`, `num_answers`, `num_upvotes`, `got_quality_answer` |
| `user_activity.csv` | 215,370 | 1 row / event | `activity_id`, `user_id`, `question_id`, `activity_type` (ask/view/answer/upvote), `activity_date` |
| `experiment.csv` | 12,000 | 1 row / user | `user_id`, `variant` (control/treatment), `country`, `device_type`, `num_questions_asked`, `avg_word_count`, `quality_answer_rate`, `assignment_date` |

---

## 5. Data Quality

The data is structurally very clean: **100% referential integrity** across all four tables, **zero full-row or key duplicates**, no negative/impossible values, and all dates parse correctly. The real issues were **methodological**, not row-level garbage — and finding them mattered a lot for the rest of the analysis:

1. **`experiment.csv`'s summary columns (`num_questions_asked`, `avg_word_count`, `quality_answer_rate`) are cumulative, ALL-TIME user stats — not post-assignment outcomes.** They match each user's lifetime question count 100% of the time but only match the post-`assignment_date` count 90.4% of the time. Using them directly for the A/B analysis would have leaked pre-experiment behavior into the treatment effect. **Fix:** re-derived true post-assignment metrics (`num_questions_post`, `avg_word_count_post`, `quality_answer_rate_post`) directly from `questions.csv`.
2. **1,529 rows in `experiment.csv` have structurally missing `avg_word_count`/`quality_answer_rate`** — exactly the users with zero questions asked (mean of zero is undefined). Left as `NA`, not imputed to 0.
3. **`questions.csv`'s engagement counters don't fully reconcile with the raw `user_activity.csv` event log** (answers match only 31.9%, upvotes 14.6%, views 0%) — treated as two independent measurement systems rather than force-reconciled.
4. **`experiment.csv`'s `country`/`device_type` are 100% redundant** with `users.csv` — dropped to avoid duplication.
5. Outliers in `word_count`, `num_upvotes`, and `num_questions_asked` were flagged (IQR method), not deleted — they reflect natural right-skew, not data errors.

Full log: [`data/data_quality_log.csv`](data/data_quality_log.csv).

---

## 6. Methodology

**Stack:** R, tidyverse (dplyr/tidyr/readr/purrr), lubridate, ggplot2, broom, car (VIF), pROC (AUC/ROC), scales.

**Approach:**
- Stage 1: profile → clean (standardize types, add flags, never delete rows unnecessarily) → build a **user-level master table** (`user_level_analytical.csv`) joining demographics, experiment assignment, and engagement rollups.
- Stage 2: define KPIs *only* where the data supports them (explicitly flagging where it doesn't — see Limitations), segment, trend, and correlate.
- Stage 3: two regression models (leakage-checked), six hypotheses derived directly from Stage-2 findings, and a full A/B test with balance checks, a metric hierarchy, and segment cuts.
- Stage 4: assess whether causal inference is justified (it is — this is a randomized experiment), apply regression adjustment with an explicit "bad control" check, and synthesize all four stages into a root-cause driver table.

---

## 7. Product KPIs

*(60-day window, 12,000 users; full definitions in [`data/kpi_definitions.csv`](data/kpi_definitions.csv))*

| KPI | Value |
|---|---|
| Mean daily active users (DAU) | ~3,116 |
| Mean weekly active users (WAU) | ~7,150 |
| Mean active days per user *(session proxy — no session ID in the data)* | 15.58 |
| Question-asking rate (≥1 question ever) | 87.3% |
| Mean questions per user (all users) | 3.14 |
| Mean week-over-week retention rate | 86.4% |
| Mean views / answers / upvotes per question | 23.35 / 1.38 / 6.32 |
| Overall quality-answer rate | 69.7% |

**Measurement honesty note:** there is no session ID, intra-day timestamp, or duration field in this data. "Sessions" are approximated as **distinct active days**, and literal **"time spent" is not computed** — it isn't measurable from what's available, and a fabricated number would be worse than none. This is stated explicitly rather than papered over.

Full table: [`data/kpi_summary.csv`](data/kpi_summary.csv).

---

## 8. Exploratory Analysis

- **Engagement is flat**, not growing or declining: DAU ranges 2,899–3,360 across 60 days with no trend and no day-of-week seasonality. Apparent dips at the start/end of the log are a **partial-calendar-week artifact**, confirmed by normalizing to events/day. *(`figures/01_dau_trend.png`)*
- **Word count is the strongest behavioral driver of quality found in the entire analysis**: quality-answer rate climbs from 55.8% (1–15 words) to 78.2% (81+ words). *(`figures/05_wordcount_vs_quality.png`)*
- **Activity volume predicts how much a user posts, not how good it is** (r=0.59 with questions asked, r=0.02 with quality rate).
- **Device type is not a real segment** (asker rate/quality rate within ~1pp across mobile/desktop/tablet); **country and engagement tier show real gradients** in the raw EDA (later tested formally — see Section 10).
- **12.7% of users are consistent "lurkers"** — active (avg. 14.7 events) but never post. *(`figures/09_never_ask_segment.png`)*

Full findings log: [`data/eda_findings_log.csv`](data/eda_findings_log.csv).

---

## 9. Regression Analysis

**Logistic regression — who becomes an asker (`is_asker`)?**
Predictors: `device_type`, `country`, `account_age_at_assignment_days`, `non_ask_activity` (leakage-free: excludes `total_activity_events` and any ask-derived field).
**Result: no predictor is significant.** AUC = **0.519**, McFadden pseudo-R² = 0.0006 — essentially chance-level. Who asks is not explained by demographics or non-posting activity. *(`figures/10_logit_odds_ratios.png`, `figures/11_logit_roc_curve.png`)*

**Linear regression — question upvotes (`log1p(num_upvotes)`)**
Predictors: `word_count`, `num_views`, `topic`, `device_type`, `country`.
- `word_count`: β=0.00508, p<2e-16, 95% CI [0.0047, 0.0055] → +50 words ≈ **+29% more upvotes**, controlling for exposure and topic.
- `num_views`: β=0.01752, p<2e-16 → mechanical exposure effect.
- `topic`/`device_type`/`country`: mostly non-significant. R²=0.038 (individual upvotes are noisy; the point is the significant, controlled word-count effect). *(`figures/12_predicted_upvotes_by_wordcount.png`)*

Full coefficients: [`data/logistic_regression_coefficients.csv`](data/logistic_regression_coefficients.csv), [`data/linear_regression_coefficients.csv`](data/linear_regression_coefficients.csv).

---

## 10. Hypothesis Testing

Six hypotheses, each motivated directly by an EDA finding:

| ID | Hypothesis | Test | Result | p-value |
|---|---|---|---|---|
| H1 | Quality rate: long (≥50w) vs. short questions | 2-proportion z-test | +9.8pp | **2.6e-74** |
| H2 | Quality rate across 8 countries | Chi-square | Cramér's V=0.013 (trivial) | 0.514 (n.s.) |
| H3 | Asking rate across device types | Chi-square | 87.2–87.9% (flat) | 0.94 (n.s.) |
| H4 | Activity: never-ask vs. asker | Wilcoxon rank-sum | median 15 vs 18 | **6.7e-174** |
| H5 | Quality rate: top vs. bottom activity quartile | 2-proportion z-test | +1.5pp | 0.055 (marginal) |
| H6 | Activity volume vs. questions asked | Spearman correlation | ρ=0.520 | **<2.2e-16** |

**Notable correction:** the Stage-2 EDA's loose, user-averaged read suggested a real country effect on quality. The **formal question-level test (H2) found this is not significant** (trivial effect size) — a clean example of confirmatory testing correcting an exploratory impression. *(`figures/14_hypothesis_test_pvalues.png`)*

Full results: [`data/hypothesis_test_results.csv`](data/hypothesis_test_results.csv).

---

## 11. A/B Testing

A genuine, live experiment: `variant` (control n=5,937 vs. treatment n=6,063), randomized at `assignment_date`.

**Baseline balance:** country (p=0.573 ✅) and tenure (p=0.323 ✅) balanced; **device_type shows a small imbalance (p=0.026)** — flagged and checked as a segment cut. *(`figures/15_ab_baseline_balance.png`)*

**Metric hierarchy & results:**

| Metric | Role | Control | Treatment | Effect | p-value |
|---|---|---|---|---|---|
| **Quality-answer rate** | **Primary** | 67.8% | 71.5% | **+3.7pp (+5.5% rel.)** | **1.1e-14** |
| Avg. word count / question | Secondary (mechanism) | 33.4 | 48.3 | +14.9 words (+44.7%) | <2.2e-16 |
| Participation rate | Secondary | 87.2% | 86.1% | −1.1pp | 0.095 (n.s.) |
| Questions per user | Guardrail | 3.03 | 3.05 | +0.02 | 0.64 (n.s.) |

*(`figures/16_ab_metric_comparison.png`)*

**Segment cuts:** the effect is **positive in every device and country segment tested**, significant in the two largest device segments (mobile +4.0pp, desktop +3.5pp). *(`figures/17_ab_segment_effects.png`)*

**Statistical vs. practical significance:** the primary result is both — a 5.5% relative quality lift with no cost to participation or volume is a meaningful product win. The word-count effect (+44.7%) is very likely the **mechanism**, not an independent effect (see Section 12).

---

## 12. Causal Inference

**Is causal inference justified? Yes** — `variant` is genuinely randomized (Section 11's balance checks), making this a much stronger identification setting than the correlational regressions in Section 9.

- **Treatment:** `variant` (control/treatment). **Outcome:** `got_quality_answer` on post-assignment questions.
- **Confounders considered:** `device_type`, `country`, `account_age_at_assignment_days` — all pre-treatment. **Explicitly excluded:** `word_count`, `num_views`, `num_questions_post` — these are **post-treatment mediators** (the treatment itself changes word count by +45%); adjusting for them would be a "bad control" that biases the effect toward zero.
- **Why not difference-in-differences:** the pre-treatment window is only 0–4 days on average, and just **9.6% of users (1,149 of 12,000)** have even one pre-assignment question. Far too sparse for a reliable baseline or parallel-trends check — DiD was explicitly not forced.
- **Method used: regression adjustment (ANCOVA-style).** Appropriate because randomization already provides ignorability by design; adjustment improves precision and explicitly corrects for the small device_type imbalance. A propensity-score model (`variant ~ device_type + country + tenure`) confirms treatment is **unpredictable from covariates (AUC=0.518)** — validating that PSM/IPW would solve a confounding problem this randomized design doesn't have. *(`figures/19_causal_propensity_roc.png`)*

**Final causal estimate:**
> **Average Treatment Effect on quality-answer rate: +3.72 percentage points, 95% CI [2.78, 4.66]pp, p=1.18e-14 (+5.5% relative)**

The adjusted estimate (3.72pp) is nearly identical to the unadjusted one (3.73pp) — expected under successful randomization. *(`figures/18_causal_ate_comparison.png`)*

**Limitations:** this is an intent-to-treat estimate (the bundled treatment, not a specific mechanism); quality is only observed for the ~87% who asked post-assignment (low but non-zero selection risk); word count is very likely a mediator, so this is a *total* effect estimate, not a mediation decomposition; single 60-day cohort, no evidence on persistence.

Full tables: [`data/causal_model_comparison.csv`](data/causal_model_comparison.csv), [`data/causal_propensity_check.csv`](data/causal_propensity_check.csv).

---

## 13. Key Findings

1. **Question length is the single strongest lever for quality found anywhere in this analysis** — both in raw EDA (+9.8pp), formal hypothesis testing (p=2.6e-74), and controlled regression (+50 words ≈ +29% upvotes).
2. **The A/B treatment causally improves quality-answer rate by 3.72pp (+5.5% relative)**, robust to covariate adjustment, positive across every segment, with no cost to participation or volume — and it works *at least as well* for brand-new users as for veterans.
3. **The treatment's likely mechanism is word count**: it increases average question length by 44.7%, and word count independently predicts quality — making this an important, evidence-based hypothesis for future mediation work, not an established fact.
4. **~12.7% of users are a stable "lurker" segment** — active but non-posting — and this segment cannot be explained by device, country, or tenure (logistic regression AUC=0.52). Conversion needs a behavioral trigger, not demographic targeting.
5. **Device type and country do not meaningfully affect engagement or quality** once formally tested — an EDA-level impression about country was specifically corrected by hypothesis testing (Section 10, H2).
6. **New users behave statistically identically to established/veteran users** (p=0.90 for asker rate, r=-0.006 for tenure vs. questions) — tenure is not a meaningful driver of engagement on this platform.
7. **Overall platform engagement is flat** across the full observation window — no organic growth, no decline, no real anomalies (apparent dips were calendar-boundary artifacts).

---

## 14. Product Recommendations

*(Format: Finding → Evidence → Product Implication → Recommended Action → Metric to Monitor. Full table: [`data/product_recommendations.csv`](data/product_recommendations.csv))*

**1. The treatment causally increases quality-answer rate.**
Evidence: Causal ATE +3.72pp (95% CI [2.78, 4.66]pp), p=1.18e-14; robust to adjustment; positive in every tested segment.
→ *Implication:* the tested change produces a real, generalizable quality improvement.
→ *Action:* roll out to 100% of users; first confirm the exact UI/copy mechanism so it's preserved in the final design.
→ *Monitor:* `quality_answer_rate_post` (primary), `avg_word_count_post` and `num_questions_post` (guardrails).

**2. Question length is the strongest standalone quality driver.**
Evidence: +9.8pp quality rate for long vs. short questions (p=2.6e-74); regression +50 words = +29% more upvotes.
→ *Implication:* encouraging detail is a generalizable lever, independent of the specific tested treatment.
→ *Action:* add composition-time guidance (soft length nudges, example placeholder text) to the question-asking flow platform-wide.
→ *Monitor:* average `word_count` per question; `quality_answer_rate`.

**3. ~12.7% of users are lurkers who cannot be explained by demographics.**
Evidence: Logistic regression AUC=0.52 (no significant predictors); lurkers average 14.7 activity events vs. 18.4 for askers (p=6.7e-174) — active, not disengaged.
→ *Implication:* demographic targeting won't convert lurkers; the trigger is contextual.
→ *Action:* test in-context prompts triggered by browsing behavior (e.g., "ask about this") instead of broad demographic campaigns.
→ *Monitor:* `asked_any_post` (lurker-to-asker conversion rate).

**4. Device and country do not meaningfully affect these outcomes.**
Evidence: asking rate by device p=0.94; quality rate by country p=0.51 (Cramér's V=0.013).
→ *Implication:* device/country-specific engagement initiatives for these outcomes are not evidence-based.
→ *Action:* deprioritize device/country-specific roadmap items for asking/quality; re-test only after major UI or localization changes.
→ *Monitor:* periodic re-check of `asker_rate`/`quality_answer_rate` by segment.

**5. New users engage identically to established users, and the treatment works at least as well for them.**
Evidence: asker rate by tenure p=0.90; treatment effect New +5.6pp (p=2.6e-5) vs. Established/Veteran ~+3.4pp.
→ *Implication:* the treatment is safe and effective from day one — no onboarding-specific risk.
→ *Action:* build the treatment experience into new-user onboarding by default, not only as a post-signup test arm.
→ *Monitor:* `quality_answer_rate_post` by tenure segment; new-user activation rate.

**6. Platform activity predicts volume, not quality.**
Evidence: activity vs. questions asked ρ=0.52 (p<2.2e-16); activity vs. quality rate r=0.02 (n.s.).
→ *Implication:* engagement/gamification features will grow volume but won't improve quality alone, and could dilute it.
→ *Action:* pair any future volume-boosting feature (streaks, notifications) with a quality lever like the word-count nudge above.
→ *Monitor:* `quality_answer_rate` alongside any volume-feature launch.

**7. Engagement is flat, with no organic growth over the observed window.**
Evidence: mean DAU ~3,116, range 2,899–3,360 across 60 days, no trend or seasonality.
→ *Implication:* this period is a clean baseline, but growth won't come from existing-user engagement alone.
→ *Action:* use this window's KPI averages as the reference baseline for judging future launches; treat growth as an acquisition problem.
→ *Monitor:* DAU/WAU trend vs. this baseline after future launches.

---

## 15. Limitations

- **No session ID or timestamp field** — "sessions" are approximated as active days; **no duration field** — "time spent" is not computed at all (explicitly, rather than fabricated).
- **`questions.csv` engagement counters don't fully reconcile** with the raw `user_activity.csv` event log (treated as two separate measurement systems).
- **Regression models (Section 9) are correlational**, not causal — word_count/views/upvotes are simultaneously determined, so reverse causality and omitted-variable bias cannot be ruled out.
- **Residual non-normality** in the linear model is expected given n=37,719 and count-like skew; coefficients remain valid via the CLT, but a Poisson/negative-binomial model would be more textbook-correct.
- **Question-level clustering** (many questions per user) isn't explicitly modeled in the question-level tests; a user-level robustness check was run for the primary A/B metric but a full mixed-effects approach would be more rigorous.
- **The causal estimate is intent-to-treat**, not a decomposition of the specific mechanism; word count is very likely a mediator, and a formal mediation analysis was out of scope here.
- **Small device_type imbalance (p=0.026)** in the A/B test, adjusted for but not fully eliminated by a 3-category covariate.
- **No multiple-comparison correction** was applied across the 6 hypotheses / ~19 segment tests; the headline results are far below any reasonable corrected threshold, but marginal ones (H5, individual country cuts) should be read with that caveat.
- **Single 60-day, single-cohort window** — no evidence on seasonality beyond this window or on effect persistence over time.

---

## 16. Tech Stack

`R` · `tidyverse` (dplyr, tidyr, readr, purrr, stringr, forcats) · `ggplot2` · `lubridate` · `broom` · `car` (VIF) · `pROC` (AUC/ROC) · `scales`

**Methods applied:** Data cleaning & validation · Exploratory data analysis · KPI design · Logistic & linear regression · Hypothesis testing (2-proportion z-test, chi-square, Wilcoxon rank-sum, Spearman correlation) · A/B testing (balance checks, metric hierarchies, segment analysis) · Causal inference (regression adjustment / ANCOVA, propensity-score diagnostics) · Root-cause analysis

---

## 17. Project Structure

```
.
├── README.md                                  <- this file (the only README - covers all 4 stages)
├── 01_data_cleaning_eda.R                     <- Stage 1: cleaning & data-quality audit
├── 02_product_analytics_eda.R                 <- Stage 2: KPIs, segments, trends, EDA
├── 03_statistical_analysis.R                  <- Stage 3: regression, hypothesis tests, A/B test
├── 04_causal_inference_and_root_cause.R       <- Stage 4: causal inference, root-cause synthesis
│
├── raw/                                       <- original, unmodified input CSVs
│   ├── users.csv
│   ├── questions.csv
│   ├── user_activity.csv
│   └── experiment.csv
│
├── data/                                      <- all cleaned data + analysis output tables
│   ├── users_clean.csv, questions_clean.csv, user_activity_clean.csv, experiment_clean.csv
│   ├── user_level_analytical.csv              <- master one-row-per-user analytical table
│   ├── data_quality_log.csv, eda_findings_log.csv
│   ├── kpi_summary.csv, kpi_definitions.csv, kpi_by_*.csv, daily_trend.csv, weekly_retention.csv
│   ├── logistic_regression_coefficients.csv, linear_regression_coefficients.csv
│   ├── hypothesis_test_results.csv
│   ├── ab_test_balance.csv, ab_test_results.csv, ab_test_segment_results.csv
│   ├── causal_model_comparison.csv, causal_propensity_check.csv
│   ├── root_cause_summary.csv, tenure_treatment_effects.csv
│   └── product_recommendations.csv
│
└── figures/                                   <- 23 purpose-built ggplot2 / diagnostic charts
    ├── 01-09_*.png                            <- Stage 2: KPI, trend, and segment charts
    ├── 10-17_*.png                            <- Stage 3: regression, hypothesis, A/B charts
    └── 18-20_*.png                            <- Stage 4: causal inference & root-cause charts
```

**To reproduce:** run the four scripts in order from the project root (`Rscript 01_data_cleaning_eda.R`, then `02_...`, `03_...`, `04_...`) — each reads the previous stage's `clean/` outputs and writes its own additions back into `clean/` and `figures/`.
