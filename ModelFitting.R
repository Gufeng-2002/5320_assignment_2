library(ggplot2)
library(tidyr)
library(tidyverse)
library(xtable)

# read data
anscombe

# Reshape the data for visualization
anscombe_long <- anscombe |>
  pivot_longer(cols = everything(),
  names_to = c(".value", "set"),
  names_pattern = "(.)(.)")

# Visualize data
ggplot(anscombe_long, aes(x = x, y = y)) +
    geom_point() +
    facet_wrap(~ set, scales = "free") +
    labs(title = "Anscombe's Quartet", x = "X values", y = "Y values") + 

ggsave(filename = "results/anscombe_quartet.png", width = 10, height = 6)


# Fit a linear model to each dataset
lm1 <- lm(y1 ~ x1, data = anscombe)
lm2 <- lm(y2 ~ x2, data = anscombe)
lm3 <- lm(y3 ~ x3, data = anscombe)
lm4 <- lm(y4 ~ x4, data = anscombe)

# Visualize fitted models
ggplot(anscombe_long, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    facet_wrap(~ set, scales = "free") +
    labs(title = "Anscombe's Quartet with Fitted Models", x = "X values", y = "Y values")

ggsave(filename = "results/anscombe_quartet_fitted.png", width = 10, height = 6)

# visualize the residuals against the fitted values
# Create a data frame with residuals and fitted values
residuals_df <- data.frame(
  fitted = c(fitted(lm1), fitted(lm2), fitted(lm3), fitted(lm4)),
  residuals = c(residuals(lm1), residuals(lm2), residuals(lm3), residuals(lm4)),
  set = rep(c("data 1", "data 2", "data 3", "data 4"), each = nrow(anscombe))
)

# Calculate the R-squared value for each dataset
r_squared <- c(summary(lm1)$r.squared, summary(lm2)$r.squared, summary(lm3)$r.squared, summary(lm4)$r.squared)
r_squared

# Visualize residuals against fitted values with improved layout and save the figure
ggplot(residuals_df, aes(x = fitted, y = residuals)) + 
  geom_point() +
  facet_wrap(~ set, scales = "free", ncol = 2) +
  labs(title = "Residuals vs Fitted Values", x = "Fitted values", y = "Residuals") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(data = data.frame(set = c("data 1", "data 2", "data 3", "data 4"), 
                              r_squared = r_squared), 
            aes(x = Inf, y = Inf, label = paste("R^2 =", round(r_squared, 2))), 
            hjust = 1.2, vjust = 2, size = 5, inherit.aes = FALSE)

# Save the figure to the 'result' directory
ggsave(filename = "/Users/gufeng/2025_winter/STAT_5320/assignments/5320_assignment_2/results/residuals_vs_fitted.png", width = 10, height = 6)

# plot histgram of residuals
# Combine histogram and QQ-plots into one figure with 8 subplots
library(gridExtra)

# Histogram of residuals
histogram_plot <- ggplot(residuals_df, aes(x = residuals)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  facet_wrap(~ set, scales = "free", ncol = 2) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Save the combined figure
ggsave(filename = "results/residuals_histogram.png", plot = histogram_plot, width = 10, height = 6)


# QQ-plots of residuals
qq_plot <- ggplot(residuals_df, aes(sample = residuals)) +
  stat_qq(size = 2) +
  stat_qq_line() +
  facet_wrap(~ set, scales = "free", ncol = 2) +
  labs(title = "QQ-Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Save the combined figure
ggsave(filename = "results/residuals_qq.png", plot = qq_plot, width = 10, height = 6)



# organize the results information for models, including coefficients, standard errors, t-values, and p-values
# Organize the results information for models, including coefficients, standard errors, t-values, and p-values
results <- data.frame(
  model = rep(c("data 1", "data 2", "data 3", "data 4"), each = 2),
  term = rep(c("(Intercept)", "x"), times = 4),
  coefficients = c(coef(lm1), coef(lm2), coef(lm3), coef(lm4)),
  std_errors = c(summary(lm1)$coefficients[, "Std. Error"], summary(lm2)$coefficients[, "Std. Error"],
                 summary(lm3)$coefficients[, "Std. Error"], summary(lm4)$coefficients[, "Std. Error"]),
  t_values = c(summary(lm1)$coefficients[, "t value"], summary(lm2)$coefficients[, "t value"],
               summary(lm3)$coefficients[, "t value"], summary(lm4)$coefficients[, "t value"]),
  p_values = c(summary(lm1)$coefficients[, "Pr(>|t|)"], summary(lm2)$coefficients[, "Pr(>|t|)"],
               summary(lm3)$coefficients[, "Pr(>|t|)"], summary(lm4)$coefficients[, "Pr(>|t|)"])
)

# Convert the results dataframe to a LaTeX table
talbe_to_latex <-function(table){
  latex_table <- print(
  xtable(table, caption = "Model Fitting Results", label = "tab:model_fitting_results"),
  include.rownames = FALSE,
  print.results = FALSE
)
  return(latex_table)
}

# Function to copy LaTeX table to clipboard
copy_to_clipboard <- function(text) {
  clip <- pipe("pbcopy", "w")
  writeLines(text, clip)
  close(clip)
}

# Convert the LaTeX table to a string and copy to clipboard
copy_to_clipboard(talbe_to_latex(results))
