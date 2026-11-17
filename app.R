
library(shiny)
library(shinyWidgets)          
library(leaflet)
library(leaflet.extras)       
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)                
library(DT)                    
library(lubridate)             
library(scales)                
library(geodata)               


options(shiny.maxRequestSize = 10 * 1024^2)
DISCLAIMER <- "Synthetic Sample Data — Geographic boundaries represent Laguna, Philippines; observation values are generated for demonstration and are not real‑world statistics."


load_geodata <- function() {
  if (file.exists("data/laguna_municipalities.geojson")) {
    st_read("data/laguna_municipalities.geojson", quiet = TRUE)
  } else {
   
    phl <- gadm(country = "Philippines", level = 2, path = tempdir())
    phl_sf <- st_as_sf(phl)
    phl_sf[phl_sf$NAME_1 == "Laguna", ]
  }
}

laguna <- load_geodata()
muni_names <- sort(unique(laguna$NAME_2))


set.seed(2026)


n_per_muni <- sample(50:120, length(muni_names), replace = TRUE)
names(n_per_muni) <- muni_names

points_list <- list()
for (mun in muni_names) {
  poly <- laguna[laguna$NAME_2 == mun, ]
  n <- n_per_muni[mun]
  pts <- st_sample(poly, size = n, type = "random")
  coords <- st_coordinates(pts)
  
  
  base_val <- runif(1, 500000, 3000000)
  values <- round(runif(n, 0.5, 1.5) * base_val, 0)
  
  df <- data.frame(
    ID = seq_len(n),
    Municipality = mun,
    Latitude = coords[, "Y"],
    Longitude = coords[, "X"],
    Value = values,
    Category = sample(c("Residential", "Commercial", "Industrial", "Educational"),
                      n, replace = TRUE, prob = c(0.55, 0.25, 0.10, 0.10)),
    Status = sample(c("Available", "Active", "Pending", "Maintenance"),
                    n, replace = TRUE, prob = c(0.35, 0.35, 0.20, 0.10)),
    Date = sample(seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by = "day"),
                  n, replace = TRUE),
    RiskScore = round(runif(n, 1, 100), 0),
    DemandScore = round(runif(n, 1, 100), 0)
  )
  points_list[[mun]] <- df
}

synthetic_data <- bind_rows(points_list) %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE)
  )


if (nrow(synthetic_data) == 0) stop("No synthetic data generated.")
cat("Loaded", nrow(synthetic_data), "synthetic records across", length(muni_names), "municipalities.\n")


compute_kpis <- function(data) {
  list(
    total = nrow(data),
    total_value = sum(data$Value),
    avg_value = mean(data$Value),
    median_value = median(data$Value),
    avg_risk = mean(data$RiskScore),
    avg_demand = mean(data$DemandScore),
    highest_muni = if (nrow(data) > 0) {
      data %>% group_by(Municipality) %>%
        summarise(avg = mean(Value)) %>%
        arrange(desc(avg)) %>%
        pull(Municipality) %>% first()
    } else "N/A"
  )
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .kpi-box { background: #f8f9fa; border-radius: 5px; padding: 12px; text-align: center; margin-bottom: 5px; }
      .kpi-value { font-size: 22px; font-weight: bold; }
      .kpi-label { color: #6c757d; font-size: 14px; }
      .disclaimer { background: #fff3cd; padding: 8px; border-radius: 4px; color: #856404; margin-bottom: 10px; }
    "))
  ),
  
  div(class = "disclaimer", DISCLAIMER),
  
  titlePanel(
    div(
      style = "text-align: center; margin-bottom: 5px;",
      h1("Laguna GIS Analytics Dashboard"),
      h5("Lalawigan ng Laguna, Pilipinas • Synthetic Sample Data")
    )
  ),
  
  
  fluidRow(
    column(2, div(class = "kpi-box", div(class = "kpi-value", textOutput("kpi_total", inline = TRUE)), div(class = "kpi-label", "Total Records"))),
       column(2, div(class = "kpi-box", div(class = "kpi-value", textOutput("kpi_visible", inline = TRUE)), div(class = "kpi-label", "Visible Records"))),
    column(2, div(class = "kpi-box", div(class = "kpi-value", textOutput("kpi_avg_value", inline = TRUE)), div(class = "kpi-label", "Average Value"))),
          column(2, div(class = "kpi-box", div(class = "kpi-value", textOutput("kpi_median_value", inline = TRUE)), div(class = "kpi-label", "Median Value"))),
           column(2, div(class = "kpi-box", div(class = "kpi-value", textOutput("kpi_avg_risk", inline = TRUE)), div(class = "kpi-label", "Avg Risk Score"))),
    column(2, div(class = "kpi-box", div(class = "kpi-value", textOutput("kpi_avg_demand", inline = TRUE)), div(class = "kpi-label", "Avg Demand Score")))
  ),
  
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filters"),
      
      pickerInput("muni_filter", "Municipality",
                  choices = c("All", muni_names),
                  selected = "All",
                  options = list(`actions-box` = TRUE, size = 10),
                  multiple = TRUE),
      
      pickerInput("cat_filter", "Category",
                  choices = c("All", unique(synthetic_data$Category)),
                  selected = "All",
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      
      pickerInput("status_filter", "Status",
                  choices = c("All", unique(synthetic_data$Status)),
                  selected = "All",
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      
      sliderInput("value_filter", "Value Range (₱)",
                  min = min(synthetic_data$Value),
                  max = max(synthetic_data$Value),
                  value = c(min(synthetic_data$Value), max(synthetic_data$Value)),
                  step = 100000,
                  pre = "₱",
                  sep = ","),
      
      sliderInput("risk_filter", "Risk Score",
                  min = 0, max = 100, value = c(0, 100)),
      
      sliderInput("demand_filter", "Demand Score",
                  min = 0, max = 100, value = c(0, 100)),
      
      dateRangeInput("date_filter", "Date Range",
                     start = min(synthetic_data$Date),
                     end = max(synthetic_data$Date)),
      
      pickerInput("year_filter", "Year",
                  choices = sort(unique(synthetic_data$Year)),
                  selected = sort(unique(synthetic_data$Year)),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),
      
      br(),
      actionButton("reset_filters", "Reset All Filters", icon = icon("undo"), class = "btn-primary btn-block"),
      actionButton("reset_view", "↺ Reset Map View", icon = icon("refresh"), class = "btn-secondary btn-block"),
      
      hr(),
      downloadButton("export_csv", "Export Filtered Data (CSV)", class = "btn-success btn-block")
    ),
    
    mainPanel(
      width = 9,
      
      leafletOutput("map", height = "550px"),
      
      
      fluidRow(
        column(3, checkboxInput("show_boundaries", "Municipality Boundaries", value = TRUE)),
        column(3, checkboxInput("show_scatter", "Scatter Points", value = TRUE)),
        column(3, checkboxInput("show_clusters", "Marker Clusters", value = FALSE)),
        column(3, checkboxInput("show_heatmap", "Heatmap", value = FALSE))
      ),
      fluidRow(
        column(6, selectInput("choropleth_metric", "Choropleth Metric",
                              choices = c("Records", "Average Value", "Total Value", "Avg Risk", "Avg Demand"),
                              selected = "Records")),
        column(6, checkboxInput("show_choropleth", "Show Choropleth", value = FALSE))
      ),
      
      br(),
      
      div(style = "background: #e9ecef; padding: 12px; border-radius: 5px; margin-bottom: 10px;",
          h4("Selected Municipality Analysis"),
          htmlOutput("muni_analysis")
      ),
      
      
      fluidRow(
        column(6, plotlyOutput("chart_records_muni", height = "300px")),
        column(6, plotlyOutput("chart_avg_value_muni", height = "300px"))
      ),
      fluidRow(
        column(6, plotlyOutput("chart_category", height = "250px")),
        column(6, plotlyOutput("chart_status", height = "250px"))
      ),
      fluidRow(
        column(6, plotlyOutput("chart_time_series", height = "300px")),
        column(6, plotlyOutput("chart_risk_demand", height = "300px"))
      ),
      
     
      h4("Municipality Ranking"),
      DTOutput("ranking_table"),
      
     
      h4("Filtered Data"),
      DTOutput("data_table")
    )
  )
)


server <- function(input, output, session) {
  
  
  filtered_data <- reactive({
    data <- synthetic_data
    
   
    if (!("All" %in% input$muni_filter) && length(input$muni_filter) > 0) {
      data <- data %>% filter(Municipality %in% input$muni_filter)
    }
    
    if (!("All" %in% input$cat_filter) && length(input$cat_filter) > 0) {
      data <- data %>% filter(Category %in% input$cat_filter)
    }
   
    if (!("All" %in% input$status_filter) && length(input$status_filter) > 0) {
      data <- data %>% filter(Status %in% input$status_filter)
    }
    
    data <- data %>% filter(Value >= input$value_filter[1] & Value <= input$value_filter[2])
    
    data <- data %>% filter(RiskScore >= input$risk_filter[1] & RiskScore <= input$risk_filter[2])
    
    data <- data %>% filter(DemandScore >= input$demand_filter[1] & DemandScore <= input$demand_filter[2])
   
    data <- data %>% filter(Date >= input$date_filter[1] & Date <= input$date_filter[2])
    # Year
    if (length(input$year_filter) > 0) {
      data <- data %>% filter(Year %in% input$year_filter)
    }
    data
  })
  
  #KPI Outputs 
  kpis <- reactive({ compute_kpis(filtered_data()) })
  
  output$kpi_total <- renderText({ kpis()$total })
  output$kpi_visible <- renderText({ nrow(filtered_data()) })
  output$kpi_avg_value <- renderText({ 
    if (kpis()$total == 0) "N/A" else paste0("₱", format(round(kpis()$avg_value, 0), big.mark = ","))
  })
  output$kpi_median_value <- renderText({
    if (kpis()$total == 0) "N/A" else paste0("₱", format(round(kpis()$median_value, 0), big.mark = ","))
  })
  output$kpi_avg_risk <- renderText({
    if (kpis()$total == 0) "N/A" else round(kpis()$avg_risk, 1)
  })
  output$kpi_avg_demand <- renderText({
    if (kpis()$total == 0) "N/A" else round(kpis()$avg_demand, 1)
  })
  
  #Map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 121.3, lat = 14.2, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  # Selected municipality for highlighting
  selected_muni <- reactiveVal(NULL)
  
  # Map layers observer
  observe({
    df <- filtered_data()
    if (nrow(df) == 0) {
      # Clear everything and show empty message
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addControl(html = "<div style='background:white; padding:15px; border-radius:5px;'>No records match the selected filters.</div>",
                   position = "topright")
      return()
    }
    
    # Base map update
    proxy <- leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% clearControls()
    
    #Boundaries
    if (input$show_boundaries) {
      poly_colors <- ifelse(laguna$NAME_2 %in% selected_muni(), "#ffd700", "#4d4d4d")
      poly_opacity <- ifelse(laguna$NAME_2 %in% selected_muni(), 0.7, 0.3)
      proxy <- proxy %>%
        addPolygons(
          data = laguna,
          layerId = ~NAME_2,
          fillColor = poly_colors,
          fillOpacity = poly_opacity,
          weight = 1.5,
          color = "#333333",
          highlightOptions = highlightOptions(weight = 3, color = "#ff0000", bringToFront = TRUE, fillOpacity = 0.5),
          label = ~NAME_2,
          labelOptions = labelOptions(style = list("font-weight" = "bold"))
        )
    }
    
    # 2. Choropleth
    if (input$show_choropleth) {
      metric <- input$choropleth_metric
      # Aggregate per municipality
      muni_stats <- df %>%
        group_by(Municipality) %>%
        summarise(
          Records = n(),
          AvgValue = mean(Value),
          TotalValue = sum(Value),
          AvgRisk = mean(RiskScore),
          AvgDemand = mean(DemandScore)
        )
      # Merge with laguna polygons
      laguna_merged <- laguna %>% left_join(muni_stats, by = c("NAME_2" = "Municipality"))
      # Choose column for coloring
      col_vals <- switch(metric,
                         "Records" = laguna_merged$Records,
                         "Average Value" = laguna_merged$AvgValue,
                         "Total Value" = laguna_merged$TotalValue,
                         "Avg Risk" = laguna_merged$AvgRisk,
                         "Avg Demand" = laguna_merged$AvgDemand)
      # Replace NA with 0
      col_vals[is.na(col_vals)] <- 0
      
      pal <- colorNumeric(palette = "YlOrRd", domain = col_vals, na.color = "grey")
      proxy <- proxy %>%
        addPolygons(
          data = laguna_merged,
          layerId = ~NAME_2,
          fillColor = ~pal(col_vals),
                  fillOpacity = 0.7,
              weight = 1.5,
          color = "#333333",
          label = ~paste0(NAME_2, ": ", format(col_vals, big.mark = ",")),
          labelOptions = labelOptions(style = list("font-weight" = "bold"))
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = col_vals, title = metric, labFormat = labelFormat(prefix = ifelse(metric %in% c("Average Value", "Total Value"), "₱", ""), big.mark = ","))
    }
    
    # 3. Scatter points
    if (input$show_scatter && !input$show_clusters) {
      proxy <- proxy %>%
        addCircleMarkers(
              data = df,
                 lng = ~Longitude,
          lat = ~Latitude,
          radius = ~ sqrt(Value / 100000),
             color = ~ colorNumeric(palette = c("green", "yellow", "red"), domain = Value)(Value),
          fillOpacity = 0.7,
              stroke = FALSE,
          layerId = ~ID,
               label = ~paste0(Municipality, " (", Category, ") - ₱", format(Value, big.mark = ",")),
          labelOptions = labelOptions(noHide = FALSE, direction = "auto", textsize = "12px"),
          popup = ~paste0(
            "<b>ID:</b> ", ID, "<br>",
            "<b>Municipality:</b> ", Municipality, "<br>",
            "<b>Value:</b> ₱", format(Value, big.mark = ","), "<br>",
            "<b>Category:</b> ", Category, "<br>",
            "<b>Status:</b> ", Status, "<br>",
            "<b>Date:</b> ", Date, "<br>",
            "<b>Risk:</b> ", RiskScore, "<br>",
            "<b>Demand:</b> ", DemandScore, "<br>",
            "<b>Lat/Long:</b> ", round(Latitude, 4), ", ", round(Longitude, 4)
          )
        ) %>%
        addLegend(position = "bottomright", pal = colorNumeric(palette = c("green", "yellow", "red"), domain = df$Value),
                  values = df$Value, title = "Value", labFormat = labelFormat(prefix = "₱", big.mark = ","))
    }
    
    #Clusters 
    if (input$show_clusters) {
      proxy <- proxy %>%
        addMarkers(
          data = df,
          lng = ~Longitude,
          lat = ~Latitude,
          clusterOptions = markerClusterOptions(),
          label = ~paste0(Municipality, " - ₱", format(Value, big.mark = ",")),
          popup = ~paste0(
            "<b>ID:</b> ", ID, "<br>",
            "<b>Municipality:</b> ", Municipality, "<br>",
                "<b>Value:</b> ₱", format(Value, big.mark = ","), "<br>",
            "<b>Category:</b> ", Category, "<br>",
            "<b>Status:</b> ", Status, "<br>",
            "<b>Date:</b> ", Date, "<br>",
            "<b>Risk:</b> ", RiskScore, "<br>",
            "<b>Demand:</b> ", DemandScore
          )
        )
    }
    
    #Heatmap 
    if (input$show_heatmap) {
      proxy <- proxy %>%
        addHeatmap(
          data = df,
          lng = ~Longitude,
          lat = ~Latitude,
          intensity = ~Value,
          blur = 20,
          max = 1,
          radius = 10
        )
    }
    
    # show empty message
    if (!input$show_scatter && !input$show_clusters && !input$show_heatmap && !input$show_boundaries && !input$show_choropleth) {
      proxy <- proxy %>%
        addControl(html = "<div style='background:white; padding:15px;'>Please enable at least one layer.</div>",
                   position = "topright")
    }
  })
  
  #  Municipality Click 
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id) && click$id %in% laguna$NAME_2) {
      selected_muni(click$id)
      # Update analysis panel
      output$muni_analysis <- renderUI({
        muni <- click$id
        df_muni <- synthetic_data %>% filter(Municipality == muni)
        if (nrow(df_muni) == 0) {
          return(HTML("<p>No data for this municipality.</p>"))
        }
        stats <- compute_kpis(df_muni)
        HTML(paste0(
          "<b>", muni, "</b><br>",
          "Records: ", nrow(df_muni), " | ",
          "Avg Value: ₱", format(round(stats$avg_value, 0), big.mark = ","), " | ",
                 "Median: ₱", format(round(stats$median_value, 0), big.mark = ","), " | ",
          "Total: ₱", format(stats$total_value, big.mark = ","), "<br>",
                 "Active: ", sum(df_muni$Status == "Active"), " | ",
          "Available: ", sum(df_muni$Status == "Available"), " | ",
          "Pending: ", sum(df_muni$Status == "Pending"), " | ",
          "Maintenance: ", sum(df_muni$Status == "Maintenance"), "<br>",
          "Avg Risk: ", round(stats$avg_risk, 1), " | ",
          "Avg Demand: ", round(stats$avg_demand, 1)
        ))
      })
    } else {
      selected_muni(NULL)
      output$muni_analysis <- renderUI({ HTML("<p>Click a municipality on the map to see detailed analysis.</p>") })
    }
  })
  
  # Charts 
  output$chart_records_muni <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers", height = 300))
    df_sum <- df %>% group_by(Municipality) %>% summarise(Count = n())
    plot_ly(df_sum, x = ~Count, y = ~reorder(Municipality, Count), type = "bar", orientation = 'h',
            marker = list(color = ~Count, colorscale = "Blues")) %>%
      layout(title = "Records by Municipality", xaxis = list(title = ""), yaxis = list(title = ""), height = 300)
  })
  
  output$chart_avg_value_muni <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers", height = 300))
    df_sum <- df %>% group_by(Municipality) %>% summarise(Avg = mean(Value))
    plot_ly(df_sum, x = ~Avg, y = ~reorder(Municipality, Avg), type = "bar", orientation = 'h',
            marker = list(color = ~Avg, colorscale = "Reds")) %>%
      layout(title = "Average Value by Municipality", xaxis = list(title = ""), yaxis = list(title = ""), height = 300)
  })
  
  output$chart_category <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(plotly_empty(type = "pie", height = 250))
    df_sum <- df %>% group_by(Category) %>% summarise(Count = n())
    plot_ly(df_sum, labels = ~Category, values = ~Count, type = "pie", hole = 0.4) %>%
      layout(title = "Category Distribution", height = 250)
  })
  
  output$chart_status <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(plotly_empty(type = "pie", height = 250))
          df_sum <- df %>% group_by(Status) %>% summarise(Count = n())
    plot_ly(df_sum, labels = ~Status, values = ~Count, type = "pie") %>%
      layout(title = "Status Distribution", height = 250)
  })
  
  output$chart_time_series <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "lines", height = 300))
    df_ts <- df %>% group_by(Year) %>% summarise(AvgValue = mean(Value))
    plot_ly(df_ts, x = ~Year, y = ~AvgValue, type = "scatter", mode = "lines+markers",
            line = list(color = "blue"), marker = list(color = "blue")) %>%
      layout(title = "Average Value over Time", xaxis = list(title = ""), yaxis = list(title = "Avg Value"), height = 300)
  })
  
  output$chart_risk_demand <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(plotly_empty(type = "scatter", mode = "markers", height = 300))
    # Classify
    df <- df %>%
      mutate(RiskClass = ifelse(RiskScore > median(RiskScore), "High Risk", "Low Risk"),
             DemandClass = ifelse(DemandScore > median(DemandScore), "High Demand", "Low Demand"),
             Quadrant = paste(RiskClass, DemandClass, sep = " / "))
    plot_ly(df, x = ~RiskScore, y = ~DemandScore, color = ~Quadrant, type = "scatter", mode = "markers",
            marker = list(size = 8), colors = "Set1") %>%
      layout(title = "Risk vs Demand", xaxis = list(title = "Risk Score"), yaxis = list(title = "Demand Score"), height = 300)
  })
  
  # Ranking Table
  output$ranking_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No data")))
    }
    rank_df <- df %>%
      group_by(Municipality) %>%
      summarise(
        Records = n(),
        AvgValue = mean(Value),
        TotalValue = sum(Value),
        AvgRisk = mean(RiskScore),
        AvgDemand = mean(DemandScore)
      ) %>%
      arrange(desc(Records))
    datatable(rank_df, options = list(pageLength = 10, dom = 'Bfrtip', 
                                      columnDefs = list(
                                        list(targets = c(2,3), render = JS("function(data) { return '₱' + data.toLocaleString(); }"))
                                      )),
              rownames = FALSE) %>%
      formatRound(columns = c("AvgValue", "TotalValue", "AvgRisk", "AvgDemand"), digits = 1)
  })
  
  #  Data Table 
  output$data_table <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No records match filters")))
    }
    datatable(df %>% select(ID, Municipality, Category, Status, Value, RiskScore, DemandScore, Date, Latitude, Longitude),
              options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip'),
              rownames = FALSE) %>%
      formatCurrency(columns = "Value", currency = "₱", interval = 3, mark = ",")
  })
  
  #  Export CSV
  output$export_csv <- downloadHandler(
    filename = function() { paste0("laguna_data_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  #  Reset Filters 
  observeEvent(input$reset_filters, {
    updatePickerInput(session, "muni_filter", selected = "All")
             updatePickerInput(session, "cat_filter", selected = "All")
              updatePickerInput(session, "status_filter", selected = "All")
    updateSliderInput(session, "value_filter", value = c(min(synthetic_data$Value), max(synthetic_data$Value)))
                 updateSliderInput(session, "risk_filter", value = c(0, 100))
    updateSliderInput(session, "demand_filter", value = c(0, 100))
             updateDateRangeInput(session, "date_filter", start = min(synthetic_data$Date), end = max(synthetic_data$Date))
    updatePickerInput(session, "year_filter", selected = sort(unique(synthetic_data$Year)))
    selected_muni(NULL)
    output$muni_analysis <- renderUI({ HTML("<p>Click a municipality on the map to see detailed analysis.</p>") })
  })
  
  observeEvent(input$reset_view, {
    leafletProxy("map") %>% setView(lng = 121.3, lat = 14.2, zoom = 10)
  })
}

shinyApp(ui, server)
