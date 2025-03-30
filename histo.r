## AI gen R code to check for fit

# Observed data
observed_counts <- c(
  "1" = 892887,
  "2" = 72,
  "3" = 3,
  "4" = 0,
  "5" = 0,
  "6" = 0,
  "7" = 0,
  "8" = 0,
  "9" = 0
)

# Remove zero counts (they contribute 0 to log-likelihood)
nonzero_counts <- observed_counts[observed_counts > 0]
k_values <- as.numeric(names(nonzero_counts))
counts <- as.numeric(nonzero_counts)

# Given power law distribution function
power_law_prob <- function(k, C = 1.00008399719, alpha = 13.54) {
  C / (k^alpha)
}

# Calculate probabilities for each k (unnormalized)
probs <- power_law_prob(k_values)

# Normalize probabilities to sum to 1
probs <- probs / sum(probs)

# Calculate log-likelihood
log_likelihood <- sum(counts * log(probs))

# Calculate AIC and BIC (for model comparison)
n_params <- 2  # C and alpha in the power law
n_obs <- sum(counts)
aic <- -2 * log_likelihood + 2 * n_params
bic <- -2 * log_likelihood + n_params * log(n_obs)

# For comparison, let's calculate log-likelihood for empirical distribution
empirical_probs <- counts / sum(counts)
empirical_log_lik <- sum(counts * log(empirical_probs))

# Results
cat("Power Law Model:\n")
cat("Log-Likelihood:", log_likelihood, "\n")
cat("AIC:", aic, "\n")
cat("BIC:", bic, "\n\n")

cat("Empirical Distribution (Saturated Model):\n")
cat("Log-Likelihood:", empirical_log_lik, "\n\n")

# Likelihood ratio test
lr_stat <- 2 * (empirical_log_lik - log_likelihood)
df <- length(counts) - n_params
p_value <- 1 - pchisq(lr_stat, df)

cat("Likelihood Ratio Test:\n")
cat("Test Statistic:", lr_stat, "\n")
cat("Degrees of Freedom:", df, "\n")
cat("p-value:", p_value, "\n")

# Plot observed vs fitted on log-log scale
fitted_counts <- probs * sum(counts)

plot_data <- data.frame(
  k = k_values,
  observed = counts,
  fitted = fitted_counts
)

library(ggplot2)
ggplot(plot_data, aes(x = log(k), y = log(observed + 1))) +
  geom_point(aes(color = "Observed"), size = 3) +
  geom_line(aes(y = log(fitted + 1), color = "Fitted"), size = 1) +
  scale_color_manual(values = c("Observed" = "blue", "Fitted" = "red")) +
  labs(title = "Log-Log Plot of Observed vs Fitted Counts",
       x = "log(k)",
       y = "log(count + 1)",
       color = "") +
  theme_minimal()