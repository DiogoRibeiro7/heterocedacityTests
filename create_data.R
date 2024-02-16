# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate independent variable
x <- 1:100

# Generate heteroscedastic error terms
error <- rnorm(100, mean = 0, sd = 0.1 * x)

# Generate dependent variable with a relationship to x and varying variance
y <- 2 * x + error

# Create a data frame
data <- data.frame(x = x, y = y)

# Optional: Plot the data to visualize heteroscedasticity
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  ggtitle("Heteroscedastic Data Example")

# Save the data frame to an RData file
save(data, file = "data/heteroscedastic_data.RData")
