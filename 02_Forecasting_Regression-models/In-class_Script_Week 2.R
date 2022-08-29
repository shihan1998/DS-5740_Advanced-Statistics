#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### DS-5740 | Week 2 Script ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load {fpp3}
library(fpp3)

# Load US Consumption data
data("us_change")

# Length of time series
ts_length <- nrow(us_change)

# Remove last five years (we'll make a prediction later) 
us_prediction <- us_change[
  -c((ts_length - 19):ts_length), # remove last 5 years
]

# Save last five years (we'll compare with prediction)
us_actual <- us_change[
  c((ts_length - 19):ts_length), # keeps last 5 years
]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit linear model
fit_us_lm <- us_prediction %>% # our data
  model( # model for time series
    tslm = TSLM( # time series linear model
      Consumption ~ Income + Production + Savings + Unemployment
    )
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Report fit
report(fit_us_lm)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot model
augment(fit_us_lm) %>%
  # Plot quarter on x-axis
  ggplot(aes(x = Quarter)) +
  # Plot actual values
  geom_line(aes(y = Consumption, colour = "Data")) +
  # Plot fit values
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(
    # No y-axis label
    y = NULL, 
    # Change title
    title = "Percent change in US consumption expenditure"
  ) +
  # Change colors
  scale_colour_manual(
    values = c(
      Data = "black", # Make data line black
      Fitted = "orange" # Make fitted line orange
    )
  ) +
  # No title for legend
  guides(colour = guide_legend(title = NULL))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forecast
fc <- forecast(fit_us_lm, new_data = us_actual)

# Plot forecast
us_change %>%
  # Plot quarter on x-axis
  ggplot(aes(x = Quarter)) +
  # Plot actual values
  geom_line(aes(y = Consumption, colour = "Data")) +
  # Plot predicted values
  geom_line(
    data = fc,
    aes(y = .mean, colour = "Fitted"),
    size = 1
  ) +
  labs(
    # No y-axis label
    y = NULL, 
    # Change title
    title = "Percent change in US consumption expenditure"
  ) +
  # Change colors
  scale_colour_manual(
    values = c(
      Data = "black", # Make data line black
      Fitted = "orange" # Make fitted line orange
    )
  ) +
  # No title for legend
  guides(colour = guide_legend(title = NULL))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# R-squared
cor(fc$.mean, us_actual$Consumption)^2

# MAE
mean(abs(fc$.mean - us_actual$Consumption))

# RMSE
sqrt(mean((fc$.mean - us_actual$Consumption)^2))

# MBE
mean(fc$.mean - us_actual$Consumption)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Check residuals
gg_tsresiduals(fit_us_lm)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Future scenarios
future_scenarios <- scenarios( # Create future scenarios
  increase_income = new_data( # Create new data
    us_prediction,  # Original data
    nrow(us_actual) # Number of new data
  ) %>%
    mutate(
      Income = mean(us_prediction$Income) + # Add to mean Income
        seq(0, 1, length = nrow(us_actual)), # Increase from 0 to 1
      # with a length equal to the number of actual data
      Production = mean(us_prediction$Production) + 
        rep(0, nrow(us_actual)), # No increase/decrease
      # Repeat 0 with a length equal to the number of actual data
      Savings = mean(us_prediction$Savings) + 
        rep(0, nrow(us_actual)),
      Unemployment = mean(us_prediction$Unemployment) +
        rep(0, nrow(us_actual))
    ),
  decrease_income = new_data(
    us_prediction, nrow(us_actual)
  ) %>%
    mutate(
      Income = mean(us_prediction$Income) + 
        seq(0, -1, length = nrow(us_actual)),
      Production = mean(us_prediction$Production) + 
        rep(0, nrow(us_actual)),
      Savings = mean(us_prediction$Savings) + 
        rep(0, nrow(us_actual)),
      Unemployment = mean(us_prediction$Unemployment) +
        rep(0, nrow(us_actual))
    )
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forecast
fc_us <- fit_us_lm %>% 
  forecast(new_data = future_scenarios)

# Plot
autoplot(us_prediction, Consumption) +
  autolayer(fc_us)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit linear model with trend
fit_us_trend <- us_prediction %>%
  model( # model for time series
    tslm = TSLM( # time series linear model
      Consumption ~ trend() # trend component
    )
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Report fit
report(fit_us_trend)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit linear model with trend and season
fit_us_season <- us_prediction %>%
  model( # model for time series
    tslm = TSLM( # time series linear model
      Consumption ~ trend() + # trend component
        season() # season component
    )
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Report fit
report(fit_us_season)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Australian beer production
recent_production <- aus_production %>% filter(year(Quarter) >= 1992)
recent_production %>% autoplot(Beer) +
  labs(y="Megalitres",title ="Australian quarterly beer production")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit model
fit_beer <- recent_production %>% model(TSLM(Beer ~ trend() + season()))
fit_beer %>% report()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Residuals
fit_beer %>%
  gg_tsresiduals()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot fitted model
augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y="Megalitres",title ="Australian quarterly beer production") +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Examining seasonality
augment(fit_beer) %>%
  ggplot(aes(x=Beer, y=.fitted, colour=factor(quarter(Quarter)))) +
  geom_point() +
  labs(y="Fitted", x="Actual values", title = "Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forecasting prediction
fc <- fit_beer %>% forecast
# Plot forecast
fc %>% autoplot(recent_production)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Report fit measures
glance(fit_beer) %>%
  select(
    adj_r_squared, CV, AIC, AICc, BIC
  )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Harmonic regression
fourier_beer <- recent_production %>%
  model( # model for time series
    tslm = TSLM( # time series linear model
      Beer ~ trend() + # trend component
        fourier(K = 2) # harmonic regression
    )
  )

# Report fit
report(fourier_beer)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit multiple models
fit <- recent_production %>%
  model(
    K1 = TSLM(Beer ~ trend() + fourier(K = 1)),
    K2 = TSLM(Beer ~ trend() + fourier(K = 2)),
    K3 = TSLM(Beer ~ trend() + fourier(K = 3)),
    K4 = TSLM(Beer ~ trend() + fourier(K = 4)),
    K5 = TSLM(Beer ~ trend() + fourier(K = 5)),
    K6 = TSLM(Beer ~ trend() + fourier(K = 6))
  )

# Check fit
glance(fit) %>% 
  select(.model, r_squared, adj_r_squared, AICc)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot fitted model
augment(fourier_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y="Megalitres",title ="Australian quarterly beer production") +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Plot fitted model
fourier_beer %>%
  gg_tsresiduals()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


