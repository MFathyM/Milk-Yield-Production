# Milk Yield Production Forecasting App

This project is an interactive Shiny dashboard designed to visualize and forecast milk yield production in Malaysian farms using the ARIMA time-series forecasting model. It also incorporates geospatial visualization using `leaflet` and `rgdal`.

**Note:** This project was developed during an internship at **Devan & Company** in **Kuala Lumpur, Malaysia**, and completed in **October 2019**. The dataset included in this repository is synthetic and was anonymized for demonstration purposes only.

**Live App:** [Shiny Demo](https://mfmgouda.shinyapps.io/Shiny)

## Features
- ARIMA-based forecasting of milk yield
- Interactive filtering by time and location
- Manual data modification and scenario simulation
- Accuracy evaluation against actuals
- Geospatial drill-down visualization (state → city → farm)

## Technologies Used
- R, Shiny, Shinydashboard
- Plotly, Leaflet, Forecast, rgdal
- ArcGIS shapefiles
