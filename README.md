# ODE-Based Stock Price Modeling (Reliance)

This project models stock price dynamics using a second-order Ordinary Differential Equation (ODE) incorporating momentum, damping, and drift.

## 📊 Overview
- Data: Reliance Industries (RELIANCE.NS)
- Method: Log-transformation + ODE modeling
- Numerical Solution: RK4
- Parameter Estimation: Optimization (L-BFGS-B)

## 📈 Key Features
- Captures long-term trend using deterministic dynamics
- Uses momentum-based second-order system
- Demonstrates limitations of deterministic models in finance

## 🧠 Model
dP/dt = M  
dM/dt = c + aP - kM  

## 📁 Structure

## ▶️ How to Run
1. Open `model.R`
2. Run the script in R
3. Plots will be generated automatically

## 📌 Results
- Model captures trend well  
- Cannot capture volatility (stochastic nature)

## 🔗 Report
Full report available in `report`


**Author:** Dipendu
