# ============================================================
# Analysis: Visual vs Auditory Feedback on Performance
# DVs: Total Errors, Total Time to Complete
# IV: Feedback Type (Visual / Auditory)
# ============================================================

# --- 1. LOAD AND PREPARE DATA ---

data <- read.csv("stress_test_data.csv", header = TRUE)


colnames(data) <- c("name", "round_type", "round_number", 
                     "time_to_complete", "errors", "date")


head(data)
str(data)

# --- 2. AGGREGATE: Sum errors and time per participant per condition ---

library(dplyr)

totals <- data %>%
  group_by(name, round_type) %>%
  summarise(
    total_errors = sum(errors, na.rm = TRUE),
    total_time   = sum(time_to_complete, na.rm = TRUE),
    .groups = "drop"
  )

print(totals)

# Reshape to wide format so each row = one participant
# with columns for each condition
wide_errors <- totals %>%
  select(name, round_type, total_errors) %>%
  tidyr::pivot_wider(names_from = round_type, values_from = total_errors)

wide_time <- totals %>%
  select(name, round_type, total_time) %>%
  tidyr::pivot_wider(names_from = round_type, values_from = total_time)

# Compute differences (Auditory - Visual)
wide_errors$diff <- wide_errors$Auditory - wide_errors$Visual
wide_time$diff   <- wide_time$Auditory - wide_time$Visual

cat("\n--- Error Totals (wide) ---\n")
print(wide_errors)
cat("\n--- Time Totals (wide) ---\n")
print(wide_time)


# ============================================================
# --- 3. DESCRIPTIVE STATISTICS ---
# ============================================================

cat("\n========== DESCRIPTIVE STATISTICS ==========\n")

cat("\n--- Total Errors per Condition ---\n")
totals %>%
  group_by(round_type) %>%
  summarise(
    mean   = mean(total_errors),
    sd     = sd(total_errors),
    median = median(total_errors),
    min    = min(total_errors),
    max    = max(total_errors),
    n      = n()
  ) %>%
  print()

cat("\n--- Total Time per Condition ---\n")
totals %>%
  group_by(round_type) %>%
  summarise(
    mean   = mean(total_time),
    sd     = sd(total_time),
    median = median(total_time),
    min    = min(total_time),
    max    = max(total_time),
    n      = n()
  ) %>%
  print()


# ============================================================
# --- 4. DISTRIBUTION PLOTS WITH MEAN AND SD MARKED ---
# ============================================================

# --- 4a. Error Distribution by Condition (bar plot with Poisson overlay) ---
par(mar = c(5, 5, 5, 6))
par(mfrow = c(1, 1))  # 2x2 grid of plots

# Histogram of errors per condition
for (cond in c("Auditory", "Visual")) {
  err_vals <- totals$total_errors[totals$round_type == cond]
  m <- mean(err_vals)
  s <- sd(err_vals)
  
  hist(err_vals,
       breaks = seq(-0.5, max(err_vals) + 1.5, by = 1),
       freq = FALSE,
       ylim = c(0, 0.20),
       main = paste("Total Errors -", cond),
       xlab = "Total Errors",
       ylab = "Density",
       col = ifelse(cond == "Auditory", "lightblue", "lightyellow"),
       border = "white")
  
  # Overlay Poisson PMF
  x_range <- 0:max(err_vals + 3)
  poisson_probs <- dpois(x_range, lambda = m)
  points(x_range, poisson_probs, col = "red", pch = 19, cex = 1.2)
  lines(x_range, poisson_probs, col = "red", lwd = 2)
  
  # Mark mean and +/- 1 SD
  abline(v = m, col = "blue", lwd = 2, lty = 1)
  abline(v = m - s, col = "blue", lwd = 1, lty = 2)
  abline(v = m + s, col = "blue", lwd = 1, lty = 2)
  
  legend("topright", 
         legend = c(paste("Mean =", round(m, 2)),
                    paste("SD =", round(s, 2)),
                    "Poisson fit"),
         col = c("blue", "blue", "red"),
         lty = c(1, 2, 1),
         lwd = c(2, 1, 2),
         cex = 0.8)
}

# --- 4b. Time Distribution by Condition ---

for (cond in c("Auditory", "Visual")) {
  time_vals <- totals$total_time[totals$round_type == cond]
  m <- mean(time_vals)
  s <- sd(time_vals)
  
  hist(time_vals,
       breaks = 10,
       freq = FALSE,
       main = paste("Total Time -", cond),
       xlab = "Total Time (s)",
       ylab = "Density",
       col = ifelse(cond == "Auditory", "lightblue", "lightyellow"),
       border = "white")
  
  # Overlay normal curve
  x_seq <- seq(min(time_vals) - s, max(time_vals) + s, length.out = 100)
  lines(x_seq, dnorm(x_seq, mean = m, sd = s), col = "red", lwd = 2)
  
  # Mark mean and +/- 1 SD
  abline(v = m, col = "blue", lwd = 2, lty = 1)
  abline(v = m - s, col = "blue", lwd = 1, lty = 2)
  abline(v = m + s, col = "blue", lwd = 1, lty = 2)
  
  legend("topright",
         legend = c(paste("Mean =", round(m, 2)),
                    paste("SD =", round(s, 2)),
                    "Normal fit"),
         col = c("blue", "blue", "red"),
         lty = c(1, 2, 1),
         lwd = c(2, 1, 2),
         cex = 0.8)
}


# ============================================================
# --- 5. NORMALITY CHECK ON DIFFERENCES ---
# ============================================================

cat("\n========== NORMALITY CHECKS ==========\n")

# Shapiro-Wilk test on the difference scores
# p > 0.05 means we CANNOT reject normality (which is what we want)

cat("\n--- Shapiro-Wilk: Error Differences ---\n")
shapiro_err <- shapiro.test(wide_errors$diff)
print(shapiro_err)

cat("\n--- Shapiro-Wilk: Time Differences ---\n")
shapiro_time <- shapiro.test(wide_time$diff)
print(shapiro_time)

# Histograms of differences

par(mfrow = c(1, 1))

hist(wide_errors$diff,
     breaks = 10,
     main = "Differences: Errors\n(Auditory - Visual)",
     xlab = "Difference in Total Errors",
     col = "lightgreen",
     border = "white")
abline(v = 0, col = "red", lwd = 2, lty = 2)
abline(v = mean(wide_errors$diff), col = "blue", lwd = 2)

hist(wide_time$diff,
     breaks = 10,
     main = "Differences: Time\n(Auditory - Visual)",
     xlab = "Difference in Total Time (s)",
     col = "lightgreen",
     border = "white")
abline(v = 0, col = "red", lwd = 2, lty = 2)
abline(v = mean(wide_time$diff), col = "blue", lwd = 2)

# QQ PLOT OF DIFFERENCES 
qqnorm(wide_errors$diff, main = "QQ Plot: Error Differences")
qqline(wide_errors$diff, col = "red", lwd = 2)

qqnorm(wide_time$diff, main = "QQ Plot: Time Differences")
qqline(wide_time$diff, col = "red", lwd = 2)
# ============================================================
# --- 6. PAIRED T-TESTS ---
# ============================================================

# Hypotheses (two-tailed):
#   H0: mean difference = 0 (no effect of feedback type)
#   H1: mean difference ≠ 0 (feedback type affects performance)

cat("\n========== PAIRED T-TESTS ==========\n")

cat("\n--- Errors: Paired t-test ---\n")
t_err <- t.test(wide_errors$Auditory, wide_errors$Visual, 
                paired = TRUE, 
                alternative = "two.sided")
print(t_err)

cat("\n--- Time: Paired t-test ---\n")
t_time <- t.test(wide_time$Auditory, wide_time$Visual, 
                 paired = TRUE, 
                 alternative = "two.sided")
print(t_time)


# ============================================================
# --- 7. EFFECT SIZE: COHEN'S D (PAIRED) ---
# ============================================================

# Cohen's d for paired data = mean(differences) / sd(differences)
# Interpretation:
#   |d| < 0.2  = negligible
#   |d| ~ 0.2  = small
#   |d| ~ 0.5  = medium
#   |d| ~ 0.8+ = large

cat("\n========== EFFECT SIZES ==========\n")

cohens_d_errors <- mean(wide_errors$diff) / sd(wide_errors$diff)
cat("Cohen's d (Errors):", round(cohens_d_errors, 3), "\n")

cohens_d_time <- mean(wide_time$diff) / sd(wide_time$diff)
cat("Cohen's d (Time):", round(cohens_d_time, 3), "\n")


# ============================================================
# --- 8. SUMMARY TABLE ---
# ============================================================

cat("\n========== RESULTS SUMMARY ==========\n")

summary_table <- data.frame(
  DV = c("Total Errors", "Total Time"),
  Mean_Auditory = c(mean(wide_errors$Auditory), mean(wide_time$Auditory)),
  SD_Auditory   = c(sd(wide_errors$Auditory), sd(wide_time$Auditory)),
  Mean_Visual   = c(mean(wide_errors$Visual), mean(wide_time$Visual)),
  SD_Visual     = c(sd(wide_errors$Visual), sd(wide_time$Visual)),
  Mean_Diff     = c(mean(wide_errors$diff), mean(wide_time$diff)),
  t_stat        = c(t_err$statistic, t_time$statistic),
  df            = c(t_err$parameter, t_time$parameter),
  p_value       = c(t_err$p.value, t_time$p.value),
  Cohens_d      = c(cohens_d_errors, cohens_d_time),
  Shapiro_p     = c(shapiro_err$p.value, shapiro_time$p.value)
)

print(summary_table, digits = 4)

cat("\nNote: Shapiro p > 0.05 indicates normality assumption is met.\n")
cat("Note: p_value < 0.05 would indicate a significant difference.\n")


# ---------------------
# Correlation on the Speed vs Accuracy trade-off
#------------

# Speed-Accuracy Trade-off: Correlation
cor_test <- cor.test(totals$total_time, totals$total_errors, method = "pearson")
print(cor_test)

# Scatterplot
plot(totals$total_time, totals$total_errors,
     main = "Speed-Accuracy Trade-off",
     xlab = "Total Time (s)",
     ylab = "Total Errors",
     pch = 19,
     col = ifelse(totals$round_type == "Auditory", "blue", "orange"))
abline(lm(total_errors ~ total_time, data = totals), col = "red", lwd = 2)
legend("topright", legend = c("Auditory", "Visual"), col = c("blue", "orange"), pch = 19)