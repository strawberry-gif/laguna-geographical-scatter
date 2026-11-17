# Laguna GIS Analytics Dashboard

**R + Shiny + Leaflet + sf + geodata + plotly + DT**

This is an advanced GIS and data analytics dashboard for Laguna Province, Philippines, built as a student project. It visualizes synthetic property/observation data with interactive maps, charts, and filtering.

## Features
- Interactive Leaflet map with multiple layers:
  - Municipality boundaries
  - Scatter points (size/color by value)
  - Marker clustering
  - Heatmap (density)
  - Choropleth (aggregated metrics)
- Advanced filtering: Municipality, Category, Status, Value, Risk, Demand, Date, Year
- Real-time KPI cards
- Municipality click analysis
- Interactive charts (plotly): Records by Muni, Avg Value, Category, Status, Time Series, Risk vs Demand
- Ranking table and filtered data table (DT)
- CSV export of filtered data

## Installation
Run in R console:
```r
install.packages(c("shiny", "leaflet", "sf", "dplyr", "ggplot2", "geodata", "leaflet.extras", "plotly", "DT", "shinyWidgets", "lubridate", "tidyr", "scales"))