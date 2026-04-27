library(quantmod)

getSymbols("RELIANCE.NS", src = "yahoo",
           from = "2025-01-01", to = "2025-12-30")

df <- data.frame(
  date = index(`RELIANCE.NS`),
  price = as.numeric(Cl(`RELIANCE.NS`))
)

df$y <- log(df$price)
df$dy <- c(NA, diff(df$y))

simulate_model <- function(params, df) {
  
  a <- params[1]
  k <- params[2]
  c <- params[3]
  
  n <- nrow(df)
  dt <- 0.5
  
  P <- numeric(n)
  M <- numeric(n)
  
  P[1] <- df$y[1]
  M[1] <- 0
  
  for(i in 1:(n-1)) {
    
    k1_P <- dt * M[i]
    k1_M <- dt * (c + a * P[i] - k * M[i])
    
    k2_P <- dt * (M[i] + k1_M/2)
    k2_M <- dt * (c + a * (P[i] + k1_P/2) - k * (M[i] + k1_M/2))
    
    k3_P <- dt * (M[i] + k2_M/2)
    k3_M <- dt * (c + a * (P[i] + k2_P/2) - k * (M[i] + k2_M/2))
    
    k4_P <- dt * (M[i] + k3_M)
    k4_M <- dt * (c + a * (P[i] + k3_P) - k * (M[i] + k3_M))
    
    P[i+1] <- P[i] + (k1_P + 2*k2_P + 2*k3_P + k4_P)/6
    M[i+1] <- M[i] + (k1_M + 2*k2_M + 2*k3_M + k4_M)/6
    
    if (!is.finite(P[i+1]) || !is.finite(M[i+1])) {
      return(NULL)
    }
  }
  
  return(list(P = P, M = M))
}

loss_function <- function(params, df) {
  
  a <- params[1]
  k <- params[2]
  c <- params[3]
  
  if (a < 0 || k < 0) return(1e10)
  
  sim <- tryCatch({
    simulate_model(params, df)
  }, error = function(e) return(NULL))
  
  if (is.null(sim)) return(1e10)
  
  P_pred <- sim$P
  M_pred <- sim$M
  
  if (any(!is.finite(P_pred)) || any(!is.finite(M_pred))) {
    return(1e10)
  }
  
  P_actual <- df$y
  M_actual <- df$dy
  
  valid <- which(!is.na(M_actual))
  
  error_P <- mean((P_actual[valid] - P_pred[valid])^2)
  error_M <- mean((M_actual[valid] - M_pred[valid])^2)
  
  loss <- error_P + 0.1 * error_M
  
  if (!is.finite(loss)) return(1e10)
  
  return(loss)
}

init_params <- c(0.001, 0.5, 0)

fit <- optim(
  par = init_params,
  fn = loss_function,
  df = df,
  method = "L-BFGS-B",
  lower = c(0, 0, -1),
  upper = c(1, 5, 1)
)

# Extract parameters
a_hat <- fit$par[1]
k_hat <- fit$par[2]
c_hat <- fit$par[3]

cat("Estimated Parameters:\n")
cat("a =", a_hat, "\n")
cat("k =", k_hat, "\n")
cat("c =", c_hat, "\n")

sim_final <- simulate_model(c(a_hat, k_hat, c_hat), df)

P_log_pred <- sim_final$P
P_pred <- exp(P_log_pred)

plot(df$date, df$price, type="l", col="blue", lwd=2,
     main="Actual vs Improved ODE Model",
     xlab="Time", ylab="Price")

lines(df$date, P_pred, col="red", lwd=2)

legend("topleft",
       legend=c("Actual", "ODE Fit"),
       col=c("blue", "red"),
       lwd=2)

residuals <- df$price - P_pred

plot(df$date, residuals, type="l", col="purple",
     main="Residuals",
     xlab="Time", ylab="Error")

abline(h=0, col="red")

# MSE
mse <- mean(residuals^2)
cat("MSE =", mse, "\n")