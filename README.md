# Bike Rental Analysis: Marginal Effects and Critical Points

This repository contains an R script used to analyze bike rental data and estimate key marginal effects and critical points using linear regression. The analysis evaluates how various factors, such as humidity, temperature, wind speed, and seasonal effects, influence the number of registered bike rentals.

## Features of the Analysis
The script covers the following steps:

1. **Linear Regression Model Construction**
   - Builds a comprehensive regression model using interaction terms and polynomial transformations to capture non-linear relationships.
   - Example terms:
     - `workingday:season`
     - `I(difatemp^2)` (quadratic transformation of "difference of feel-like and actual temperature")
     - `I(mnth^2)` (quadratic month effects)

2. **Coefficient Extraction**
   - Extracts coefficients from the linear model for further analysis.
   - Coefficients are visualized and used to calculate marginal effects.

3. **Marginal Effects Calculation**
   - Computes the marginal effects for key variables:
     - Month (`mnth`)
     - Difference in temperature (`difatemp`)
     - Wind speed (`windspeed_centered`)
     - Humidity (`hum_centered`)
   - The marginal effects show how small changes in each variable influence the number of rentals.

4. **Critical Point Estimation**
   - Identifies critical points where the effect of a variable shifts (e.g., where the marginal effect changes from positive to negative).

5. **Visualizations**
   - Generates insightful plots to visualize:
     - Data points and regression lines
     - Marginal effects as functions of key predictors
     - Critical point locations

## Example Insights
- **Humidity:** A 1-unit increase in centered humidity results in 156 additional bike rentals from registered users when humidity is below the critical point. Beyond the critical point, the effect becomes negative.
- **Windspeed:** A 1 km/h increase in centered windspeed is associated with 22 additional rentals.
- **Temperature:** Quadratic temperature effects reveal a nuanced relationship between perceived and actual temperatures on rentals.

## Visual Outputs
The following types of plots are included:
- Regression line overlaid on scatterplots of actual data.
- Marginal effects visualized as a function of the predictor variable.
- Highlighted critical points using horizontal reference lines.

## Repository Contents
- `bike_rental_analysis.R`: The main R script containing the entire analysis and visualizations.
- `README.md`: Documentation for the repository.
- `.gitignore`: Excludes unnecessary files (e.g., `.Rproj.user`, `.Rhistory`) from Git tracking.

## Getting Started

### Prerequisites
To run the analysis, you will need the following R packages:
- `tidyverse`
- `broom`

Install them with:
```R
install.packages(c("tidyverse", "broom"))
