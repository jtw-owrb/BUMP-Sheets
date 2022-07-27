# BUMP Data Sheet Creator
# version 2.0.1
# Created by Justin Wright, 2022
# Oklahoma Water Resources Board

library(tidyverse)
library(dplyr)
library(shiny)
library(DT)

#-----
# Pull data from GitHub
data <- readRDS(gzcon(url("https://github.com/jtw-owrb/BUMP-Sheets/raw/main/BUMPPOR.rds")))
#-----
#Begin Shiny app

ui <- fluidPage(
  selectInput(inputId = "site", 
              label = "Choose a Site",
              choices = unique(data$Site_Name),
              selected = NULL,
              selectize = FALSE
              ),
              dataTableOutput("table")
   )

server <- function(input, output) {

#-----
#Functions for sheet creation
  site <- reactive({
    as.data.frame(subset(data, Site_Name == input$site))
  })
  
Parameter <- c("Water Temperature (C)", 
               "Turbidity (NTU)",
               "pH",
               "Dissolved Oxygen (mg/L)",
               "Hardness (mg/L)",
               "Total Dissolved Solids (mg/L)",
               "Specific Conductance (uS/cm)",
               "Chloride (mg/L)",
               "Sulfate (mg/L)",
               "Total Phosphorus (mg/L)",
               "Total Nitrogen (mg/L)",
               "Nitrate/Nitrite (mg/L)",
               "Chlorophyll a (mg/m3)")

n <- reactive({
  n <- c(sum(!is.na(as.vector(site()$Temp_C))), 
              sum(!is.na(as.vector(site()$Turbidity_NTU))),
              sum(!is.na(as.vector(site()$pH))), 
              sum(!is.na(as.vector(site()$DO_mgL))), 
              sum(!is.na(as.vector(site()$Hardness_mgL))), 
              sum(!is.na(as.vector(site()$TDS_mgL))), 
              sum(!is.na(as.vector(site()$SpC_uScm))), 
              sum(!is.na(as.vector(site()$Cl_mgL))),
              sum(!is.na(as.vector(site()$SO4_mgL))), 
              sum(!is.na(as.vector(site()$TP_mgL))), 
              sum(!is.na(as.vector(site()$TN_mgL))), 
              sum(!is.na(as.vector(site()$NO2NO3_mgL))),
              sum(!is.na(as.vector(site()$Chlorophyll_mgm3))))
})

Mean <- reactive({
  Mean <- as.vector(c(round(mean(as.vector(site()$Temp_C), na.rm = TRUE), digits = 1), 
              round(mean(as.vector(site()$Turbidity_NTU), na.rm = TRUE), digits = 0),
              round(mean(as.vector(site()$pH), na.rm = TRUE), digits = 2),
              round(mean(as.vector(site()$DO_mgL), na.rm = TRUE), digits = 2),
              round(mean(as.vector(site()$Hardness_mgL), na.rm = TRUE), digits = 2),
              round(mean(as.vector(site()$TDS_mgL), na.rm = TRUE), digits = 0),
              round(mean(as.vector(site()$SpC_uScm), na.rm = TRUE), digits = 0),
              round(mean(as.vector(site()$Cl_mgL), na.rm = TRUE), digits = 0),
              round(mean(as.vector(site()$SO4_mgL), na.rm = TRUE), digits = 0),
              round(mean(as.vector(site()$TP_mgL), na.rm = TRUE), digits = 3),
              round(mean(as.vector(site()$TN_mgL), na.rm = TRUE), digits = 2),
              round(mean(as.vector(site()$NO2NO3_mgL), na.rm = TRUE), digits = 2),
              round(mean(as.vector(site()$Chlorophyll_mgm3), na.rm = TRUE), digits = 1)))
})

Median <- reactive({
  Median <- as.vector(c(round(median(as.vector(site()$Temp_C), na.rm = TRUE), digits = 1), 
              round(median(as.vector(site()$Turbidity_NTU), na.rm = TRUE), digits = 0),
              round(median(as.vector(site()$pH), na.rm = TRUE), digits = 2),
              round(median(as.vector(site()$DO_mgL), na.rm = TRUE), digits = 2),
              round(median(as.vector(site()$Hardness_mgL), na.rm = TRUE), digits = 0),
              round(median(as.vector(site()$TDS_mgL), na.rm = TRUE), digits = 0),
              round(median(as.vector(site()$SpC_uScm), na.rm = TRUE), digits = 0),
              round(median(as.vector(site()$Cl_mgL), na.rm = TRUE), digits = 0),
              round(median(as.vector(site()$SO4_mgL), na.rm = TRUE), digits = 0),
              round(median(as.vector(site()$TP_mgL), na.rm = TRUE), digits = 3),
              round(median(as.vector(site()$TN_mgL), na.rm = TRUE), digits = 2),
              round(median(as.vector(site()$NO2NO3_mgL), na.rm = TRUE), digits = 2),
              round(median(as.vector(site()$Chlorophyll_mgm3), na.rm = TRUE), digits = 1)))
})

p25 <- reactive({
  Percentile_25th <- as.vector(c(round(quantile(as.vector(site()$Temp_C), probs = 0.25, na.rm = TRUE), digits = 1),
              round(quantile(as.vector(site()$Turbidity_NTU), probs = 0.25, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$pH), probs = 0.25, na.rm = TRUE), digits = 2),
              round(quantile(as.vector(site()$DO_mgL), probs = 0.25, na.rm = TRUE), digits = 2),
              round(quantile(as.vector(site()$Hardness_mgL), probs = 0.25, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$TDS_mgL), probs = 0.25, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$SpC_uScm), probs = 0.25, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$Cl_mgL), probs = 0.25, na.rm = TRUE),digits = 0),
              round(quantile(as.vector(site()$SO4_mgL), probs = 0.25, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$TP_mgL), probs = 0.25, na.rm = TRUE), digits = 3),
              round(quantile(as.vector(site()$TN_mgL), probs = 0.25, na.rm = TRUE), digits = 2),
              round(quantile(as.vector(site()$NO2NO3_mgL), probs = 0.25, na.rm = TRUE),digits = 2),
              round(quantile(as.vector(site()$Chlorophyll_mgm3), probs = 0.25, na.rm = TRUE), digits = 1)))
})

p75 <- reactive({
  Percentile_75th <- as.vector(c(round(quantile(as.vector(site()$Temp_C), probs = 0.75, na.rm = TRUE), digits = 1),
              round(quantile(as.vector(site()$Turbidity_NTU), probs = 0.75, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$pH), probs = 0.75, na.rm = TRUE), digits = 2),
              round(quantile(as.vector(site()$DO_mgL), probs = 0.75, na.rm = TRUE), digits = 2),
              round(quantile(as.vector(site()$Hardness_mgL), probs = 0.75, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$TDS_mgL), probs = 0.75, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$SpC_uScm), probs = 0.75, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$Cl_mgL), probs = 0.75, na.rm = TRUE),digits = 0),
              round(quantile(as.vector(site()$SO4_mgL), probs = 0.75, na.rm = TRUE), digits = 0),
              round(quantile(as.vector(site()$TP_mgL), probs = 0.75, na.rm = TRUE), digits = 3),
              round(quantile(as.vector(site()$TN_mgL), probs = 0.75, na.rm = TRUE), digits = 2),
              round(quantile(as.vector(site()$NO2NO3_mgL), probs = 0.75, na.rm = TRUE),digits = 2),
              round(quantile(as.vector(site()$Chlorophyll_mgm3), probs = 0.75, na.rm = TRUE), digits = 1)))
})

Min <- reactive({
  Minimum <- as.vector(c(round(min(as.vector(site()$Temp_C), na.rm = TRUE), digits = 1), 
              round(min(as.vector(site()$Turbidity_NTU), na.rm = TRUE), digits = 0),
              round(min(as.vector(site()$pH), na.rm = TRUE), digits = 2),
              round(min(as.vector(site()$DO_mgL), na.rm = TRUE), digits = 2),
              round(min(as.vector(site()$Hardness_mgL), na.rm = TRUE), digits = 0),
              round(min(as.vector(site()$TDS_mgL), na.rm = TRUE), digits = 0),
              round(min(as.vector(site()$SpC_uScm), na.rm = TRUE), digits = 0),
              round(min(as.vector(site()$Cl_mgL), na.rm = TRUE), digits = 0),
              round(min(as.vector(site()$SO4_mgL), na.rm = TRUE), digits = 0),
              round(min(as.vector(site()$TP_mgL), na.rm = TRUE), digits = 3),
              round(min(as.vector(site()$TN_mgL), na.rm = TRUE), digits = 2),
              round(min(as.vector(site()$NO2NO3_mgL), na.rm = TRUE), digits = 2),
              round(min(as.vector(site()$Chlorophyll_mgm3), na.rm = TRUE), digits = 1)))
})

Max <- reactive({
  Maximum <- as.vector(c(round(max(as.vector(site()$Temp_C), na.rm = TRUE), digits = 1), 
              round(max(as.vector(site()$Turbidity_NTU), na.rm = TRUE), digits = 0),
              round(max(as.vector(site()$pH), na.rm = TRUE), digits = 2),
              round(max(as.vector(site()$DO_mgL), na.rm = TRUE), digits = 2),
              round(max(as.vector(site()$Hardness_mgL), na.rm = TRUE), digits = 0),
              round(max(as.vector(site()$TDS_mgL), na.rm = TRUE), digits = 0),
              round(max(as.vector(site()$SpC_uScm), na.rm = TRUE), digits = 0),
              round(max(as.vector(site()$Cl_mgL), na.rm = TRUE), digits = 0),
              round(max(as.vector(site()$SO4_mgL), na.rm = TRUE), digits = 0),
              round(max(as.vector(site()$TP_mgL), na.rm = TRUE), digits = 3),
              round(max(as.vector(site()$TN_mgL), na.rm = TRUE), digits = 2),
              round(max(as.vector(site()$NO2NO3_mgL), na.rm = TRUE), digits = 2),
              round(max(as.vector(site()$Chlorophyll_mgm3), na.rm = TRUE), digits = 1)))
})



sheeter <- reactive({
  sheet <- data.frame(Parameter = as.vector(Parameter), n = as.vector(n()), 
                 Mean = as.vector(Mean()), Median = as.vector(Median()), 
                 Minimum = as.vector(Min()), Maximum = as.vector(Max()), 
                 Percentile_25th = as.vector(p25()), Percentile_75th = as.vector(p75()))
})

output$table <- renderDataTable(sheeter(), options = list(pageLength = 13, info = FALSE, lengthMenu = list(c(13, -1), c("13", "All")) ))                    

}

shinyApp(ui = ui, server = server)

