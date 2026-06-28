meanList = function(inputList, nrow, ncol) {
    m = 0
    count = 0
    for (i in seq_len(length(inputList))) {
        if (!is.na(inputList[[i]][[ncol]][nrow])) {
            m = m + inputList[[i]][[ncol]][nrow]
            count = count + 1
        }
    }
    if (count == 0) {m = NA}
    return(m / count)
}

sdList = function(inputList, nrow, ncol) {
    m = meanList(inputList, nrow, ncol)
    if (!is.na(m)) {
        sd = 0
        count = 0
        for (i in seq_len(length(inputList))) {
            if (!is.na(inputList[[i]][[ncol]][nrow])) {
                sd = sd + (m - inputList[[i]][[ncol]][nrow])**2
                count = count + 1
            }
        }
        if (count == 0) {sd = NA}
        sd = (sd / count)**0.5
    } else {
        sd = NA
    }
    return(sd)
}


library(limSolve)
#' Fit a Monotonic Second-Degree Polynomial using Linear Equality/Inequality Solvers
#'
#' @param data A data frame containing your variables
#' @param x_name Name of the independent variable (string, e.g., "raw_signal")
#' @param y_name Name of the dependent variable (string, e.g., "reference_signal")
#' @param direction Either "increasing" (slope >= 0) or "decreasing" (slope <= 0)
#'
#' @return A list containing the fitted coefficients, the model object, and predicted values
fit_monotonic_poly <- function(data, x_name, y_name, direction = "increasing") {
  
  # Remove any rows with missing values for the regression setup
  clean_data <- data[!is.na(data[[x_name]]) & !is.na(data[[y_name]]), ]
  
  X_val <- clean_data[[x_name]]
  Y_val <- clean_data[[y_name]]
  
  # 1. Build the Design Matrix (Intercept, x, x^2)
  A_mat <- cbind(1, X_val, X_val^2)
  B_vec <- Y_val
  
  # Find boundaries of x range to apply the slope constraints
  x_min <- min(X_val)
  x_max <- max(X_val)
  
  # 2. Formulate the Inequality Constraints (G * Beta >= H)
  # The derivative (slope) of (b0 + b1*x + b2*x^2) is: b1 + 2*b2*x
  if (direction == "increasing") {
    # b1 + 2*b2*x_min >= 0  AND  b1 + 2*b2*x_max >= 0
    G_mat <- matrix(c(
      0, 1, 2 * x_min,
      0, 1, 2 * x_max
    ), nrow = 2, byrow = TRUE)
    H_vec <- c(0, 0)
    
  } else if (direction == "decreasing") {
    # b1 + 2*b2*x_min <= 0  ==>  -b1 - 2*b2*x_min >= 0
    G_mat <- matrix(c(
      0, -1, -2 * x_min,
      0, -1, -2 * x_max
    ), nrow = 2, byrow = TRUE)
    H_vec <- c(0, 0)
    
  } else {
    stop("Direction must be either 'increasing' or 'decreasing'")
  }
  
  # 3. Solve the Least Squares problem with Inequality Constraints
  # lsei computes ||A*X - B||^2 subject to G*X >= H
  fit <- lsei(A = A_mat, B = B_vec, G = G_mat, H = H_vec)
  
  # Extract the clean coefficients
  coefs <- fit$X
  names(coefs) <- c("Intercept", "Beta_1", "Beta_2")
  
  # 4. Generate predictions across the full original data vector (including NAs)
  # This makes it easy to add back to your original data frame
  full_X <- data[[x_name]]
  predictions <- coefs["Intercept"] + (coefs["Beta_1"] * full_X) + (coefs["Beta_2"] * (full_X^2))
  
  return(list(
    coefficients = coefs,
    fitted_values = predictions,
    raw_model = fit
  ))
}


fit_poly <- function(data, x_name, y_name, x_min, x_max, direction = "increasing") {
  
  # Remove any rows with missing values for the regression setup
  clean_data <- data[!is.na(data[[x_name]]) & !is.na(data[[y_name]]), ]
  
  X_val <- clean_data[[x_name]]
  Y_val <- clean_data[[y_name]]
  
  # 1. Build the Design Matrix (Intercept, x, x^2)
  A_mat <- cbind(1, X_val, X_val^2)
  B_vec <- Y_val
     
  # 2. Formulate the Inequality Constraints (G * Beta >= H)
  # The derivative (slope) of (b0 + b1*x + b2*x^2) is: b1 + 2*b2*x
  if (direction == "increasing") {
    # b1 + 2*b2*x_min >= 0  AND  b1 + 2*b2*x_max >= 0
    G_mat <- matrix(c(
      0, 1, 2 * x_min,
      0, 1, 2 * x_max
    ), nrow = 2, byrow = TRUE)
    H_vec <- c(0, 0)
    
  } else if (direction == "decreasing") {
    # b1 + 2*b2*x_min <= 0  ==>  -b1 - 2*b2*x_min >= 0
    G_mat <- matrix(c(
      0, -1, -2 * x_min,
      0, -1, -2 * x_max
    ), nrow = 2, byrow = TRUE)
    H_vec <- c(0, 0)
    
  } else {
    stop("Direction must be either 'increasing' or 'decreasing'")
  }
  
  # 3. Solve the Least Squares problem with Inequality Constraints
  # lsei computes ||A*X - B||^2 subject to G*X >= H
  fit <- lsei(A = A_mat, B = B_vec, G = G_mat, H = H_vec)
  
  # Extract the clean coefficients
  coefs <- fit$X
  names(coefs) <- c("Intercept", "Beta_1", "Beta_2")
  
  # 4. Generate predictions across the full original data vector (including NAs)
  # This makes it easy to add back to your original data frame
  full_X <- data[[x_name]]
  predictions <- coefs["Intercept"] + (coefs["Beta_1"] * full_X) + (coefs["Beta_2"] * (full_X^2))
  
  return(list(
    coefficients = coefs,
    fitted_values = predictions,
    raw_model = fit
  ))
}