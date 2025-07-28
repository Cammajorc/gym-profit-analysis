# Load required libraries
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("scales")
install.packages("formattable")
library(tidyverse)
library(ggplot2)
library(scales)
library(formattable)

# Gym Expansion Feasibility Study – Phase 2

# --- Step 1: Key Metrics ---
revenue <- 140000
expenses <- 96900
net_profit <- revenue - expenses
profit_margin <- net_profit / revenue * 100
expansion_cost <- 150000
savings_gap <- expansion_cost - net_profit

# Output base metrics
cat("Net Profit: $", net_profit, "\n")
cat("Profit Margin: ", round(profit_margin, 2), "%\n")
cat("Savings Gap: $", savings_gap, "\n\n")

# --- Step 2: CAGR & Projected Revenue ---
required_annual_profit <- expansion_cost / 3
cagr <- (expansion_cost / net_profit)^(1/3) - 1
projected_revenue <- revenue * (1 + cagr)^3

# Output growth calculations
cat("Required Annual Profit: $", required_annual_profit, "\n")
cat("Compound Annual Growth Rate (CAGR):", round(cagr * 100, 2), "%\n")
cat("Projected Revenue in 3 years (at CAGR): $", round(projected_revenue, 2), "\n\n")

# --- Step 3: 3-Year Forecast Table ---
forecast_years <- c("Y1", "Y2", "Y3")

forecast_revenue <- c(
  revenue * 1.10,                    # Y1: +10% price
  revenue * 1.10 * 1.15,             # Y2: +15% members
  revenue * 1.10 * 1.15              # Y3: same as Y2
)

forecast_expenses <- c(
  expenses,                          # Y1: same expenses
  expenses * 1.10,                   # Y2: +10% growth costs
  expenses * 1.10 * 0.95             # Y3: -5% cost savings
)

forecast_net_profit <- forecast_revenue - forecast_expenses

forecast_notes <- c(
  "Raise prices by 10%",
  "Add 15% more members",
  "Cut costs by 5%"
)

forecast_df <- data.frame(
  Year = forecast_years,
  Revenue = round(forecast_revenue, 2),
  Expenses = round(forecast_expenses, 2),
  Net_Profit = round(forecast_net_profit, 2),
  Notes = forecast_notes
)

cat("3-Year Forecast:\n")
print(forecast_df)
cat("\n")

# --- Step 4: 25% Annual Profit Growth Projection ---
growth_rate <- 0.25
years <- 0:6

profit_projection <- net_profit * (1 + growth_rate)^years

profit_df <- data.frame(
  Year = years,
  Projected_Profit = round(profit_projection, 2)
)

cat("7-Year Profit Projection at 25% Growth:\n")
print(profit_df)

## Visuals
### Line Plot: Net Profit vs. Expansion Cost
ggplot(forecast_df, aes(x = Year, y = Net_Profit, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred", size = 3) +
  geom_hline(yintercept = expansion_cost, linetype = "dashed", color = "gray40") +
  labs(
    title = "3-Year Forecast: Net Profit vs. Expansion Cost",
    y = "Net Profit ($)",
    x = "Year"
  ) +
  theme_minimal()

### Bar Plot: Revenue, Expenses, and Net Profit
library(tidyr)

forecast_long <- forecast_df %>%
  pivot_longer(cols = c("Revenue", "Expenses", "Net_Profit"),
               names_to = "Metric", values_to = "Amount")

ggplot(forecast_long, aes(x = Year, y = Amount, fill = Metric)) +
  geom_col(position = "dodge", width = 0.5) +
  labs(
    title = "3-Year Financial Forecast: Revenue, Expenses, and Net Profit",
    y = "Amount ($)",
    x = "Year",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9))

### Line Plot: 25% Annual Net Profit Growth with Red Points < Year 6
ggplot(profit_df, aes(x = Year, y = Projected_Profit)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(aes(color = Year < 6), size = 3) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "darkgreen")) +
  labs(
    title = "Projected Profit Growth at 25% Annual Rate",
    x = "Year",
    y = "Projected Profit ($)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
