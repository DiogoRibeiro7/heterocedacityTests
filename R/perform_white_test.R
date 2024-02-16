#' White Test for Heteroscedasticity
#'
#' Performs the White test for heteroscedasticity on a linear model object.
#'
#' @param model An object of class \code{lm}, representing a fitted linear model.
#' @param data A dataframe used to fit the \code{lm} model. It should include all variables used in the model.
#' @return A list containing the test statistic, its p-value, and degrees of freedom.
#' @export
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + qsec, data = mtcars)
#' performWhiteTest(model, mtcars)


library(assertthat)

performWhiteTest <- function(model, data) {
  # Ensure input types are correct
  assertthat::assert_that(is.list(model), is.data.frame(data))

  # Documentation:
  # This function performs the White test for heteroscedasticity on a linear model.
  #
  # Args:
  #   model: An object of class 'lm', representing a fitted linear model.
  #   data: A data frame used to fit the 'lm' model. It should include all variables used in the model.
  #
  # Returns:
  #   A list containing the test statistic, its p-value, and degrees of freedom.

  # Extract the model's formula to identify the dependent variable
  model_formula <- formula(model)
  dependent_var <- all.vars(model_formula)[1]

  # Calculate squared residuals
  squared_residuals <- residuals(model)^2

  # Construct the model matrix for the auxiliary regression
  # Exclude the dependent variable from the model matrix
  independent_vars <- model.matrix(model_formula, data = data)
  independent_var_names <- colnames(independent_vars)[-1]  # Exclude intercept

  # Create auxiliary data excluding the dependent variable
  aux_data <- data.frame(independent_vars[, -1, drop = FALSE])
  names(aux_data) <- independent_var_names

  # Add squared terms and possibly interactions for auxiliary regression
  for (var in independent_var_names) {
    aux_data[[paste0(var, "_squared")]] <- aux_data[[var]]^2
  }

  # Fit the auxiliary regression model using squared residuals as the dependent variable
  aux_model <- lm(squared_residuals ~ ., data = aux_data)

  # Calculate the test statistic and p-value
  n <- nrow(data)
  test_statistic <- summary(aux_model)$r.squared * n
  df <- length(aux_model$coefficients) - 1  # Degrees of freedom
  p_value <- 1 - pchisq(test_statistic, df)

  # Return the test results
  list(test_statistic = test_statistic, p_value = p_value, degrees_of_freedom = df)
}
