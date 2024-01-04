
if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("DT")) install.packages("DT")
if (!require("leaflet")) install.packages("leaflet")
if (!require("sp")) install.packages("sp")
if (!require("sf")) install.packages("sf")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("leaflet.extras")) install.packages("leaflet.extras")

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(DT)
library(leaflet)
library(sp)
library(sf)
library(RColorBrewer)
library(htmlwidgets)

#DATA PREPROCESSING

accident_data <- read.csv("C:\\Users\\afont\\Documents\\RStudio WD\\Shiny Tutorial Assignment -  Group 12\\Assignment\\2023_Accidentalidad.csv", sep=";")
street_data=read.csv("C:\\Users\\afont\\Documents\\RStudio WD\\Shiny Tutorial Assignment -  Group 12\\Assignment\\calles_tranquilas.csv", sep=",")
street_data<-na.omit(street_data)
ciclostreet_data=read.csv("C:\\Users\\afont\\Documents\\RStudio WD\\Shiny Tutorial Assignment -  Group 12\\Assignment\\ciclocarriles.csv", sep=",")
ciclostreet_data<-na.omit(ciclostreet_data)
# Reclassify 'rango_edad' into simplified age categories
accident_data <- accident_data %>%
  mutate(simplified_age_range = case_when(
    str_detect(rango_edad, "Menor de 5 años|De 6 a 9 años|De 10 a 14 años|De 15 a
17 años") ~ "<18",
    str_detect(rango_edad, "De 18 a 20 años|De 21 a 24 años|De 25 a 29 años|De 30 a
34 años|De 35 a 39 años") ~ "18-40",
    str_detect(rango_edad, "De 40 a 44 años|De 45 a 49 años|De 50 a 54 años|De 55 a
59 años|De 60 a 64 años") ~ "40-65",
    str_detect(rango_edad, "De 65 a 69 años|De 70 a 74 años|Más de 74 años") ~
      ">65",
    TRUE ~ "Desconocido"
  )) %>%
  mutate(simplified_age_range = factor(simplified_age_range, levels = c("<18", "18-40",
                                                                        "40-65", ">65", "Desconocido")))

#merge all of the "se desconoce" into NULLS
accident_data <- accident_data %>%
  mutate(estado_meteorológico = ifelse(estado_meteorológico == "Se desconoce",
                                       "NULL",
                                       estado_meteorológico))

#Modify hour column
# Function to extract hour from "hora"
extract_hour <- function(time_str) {
  parts <- strsplit(time_str, ":")[[1]]
  as.numeric(parts[1])
}

# Apply the function to create a new variable "hour"
accident_data$hora <- sapply(accident_data$hora, extract_hour)


# Adjusting the coordinates
accident_data$coordenada_x_utm <- accident_data$coordenada_x_utm / 1000
accident_data$coordenada_y_utm <- accident_data$coordenada_y_utm / 1000
# Convert the data frame to an sf object and set ETRS89 CRS (EPSG:4258)
accident_data_sf <- st_as_sf(accident_data, coords = c("coordenada_x_utm",
                                                       "coordenada_y_utm"), crs = 32630)

# Transform to WGS84 (EPSG:4326)
accident_data_sf <- st_transform(accident_data_sf, crs = 4326)
# Extract the longitude and latitude
coords <- st_coordinates(accident_data_sf)
accident_data$longitude <- coords[,1]
accident_data$latitude <- coords[,2]

accident_data$fecha <- as.Date(accident_data$fecha, format = "%d/%m/%Y")
#create colors for each district
num_districts <- 20
paleqe <- brewer.pal(min(num_districts, 9), "Set1") # 'Set1' has max 9 distinct colors
colors <- rep(paleqe, length.out = num_districts)
names(colors) <- as.character(1:num_districts)

#UI

ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("Madrid Traffic Accident Analysis"),
  tabsetPanel(
    tabPanel("Visualization",
             sidebarLayout(
                   sidebarPanel(
                       dateInput("startDate", "Select Initial Date:",  value = as.Date("2023-01-01")),
                       dateInput("endDate", "Select Last Date:", Sys.Date()),
                     selectInput("xVariable", "Select X-axis Variable:",
                                 choices = c("District" = "distrito",
                                             "Hora" = "hora",
                                             "Fecha" ="fecha")),
                     selectInput("yVariable", "Select Y-axis Variable:",
                                 choices = c("Weather" = "estado_meteorológico",
                                             "Type of Accident" = "tipo_accidente",
                                             "Type of Vehicle" = "tipo_vehiculo",
                                             "Type of Person" = "tipo_persona",
                                             "Severity" = "lesividad",
                                             "Age Range" = "simplified_age_range")),
                         conditionalPanel(
                           condition = "input.yVariable == 'estado_meteorológico'",
                           checkboxInput("proportional","Normalize")
                         )
                   ),
               mainPanel(
                 plotOutput("districtPlot")
               )
             )
             
    ),
    
    tabPanel("Map View",
            fluidRow(
               column(3,
                      tabsetPanel(
                        tabPanel("Group 1",
                                  dateInput("startDate1", "Select Initial Date:",  value = as.Date("2023-01-01")),
                                  dateInput("endDate1", "Select Last Date:", Sys.Date()),
                                  selectInput("filterVariable1", "Select Variable to Filter By",
                                              choices = c("Type of Accident" = "tipo_accidente",
                                                          "Type of Vehicle" = "tipo_vehiculo",
                                                          "Type of Person" = "tipo_persona",
                                                          "Severity" = "lesividad",
                                                          "Age Range" = "simplified_age_range"),
                                              selected = "tipo_accidente"),
                                  
                                  uiOutput("checkboxGroup1")
                        ),
                        tabPanel("Group 2",
                                 checkboxInput("enableGroup2", "Enable Group 2", value = FALSE),tags$hr(), 
                                 conditionalPanel(
                                   condition = "input.enableGroup2 == true",
                                   dateInput("startDate2", "Select Initial Date:", value = as.Date("2023-01-01")),
                                   dateInput("endDate2", "Select Last Date:", Sys.Date()),
                                   selectInput("filterVariable2", "Select Variable to Filter By",
                                               choices = c("Type of Accident" = "tipo_accidente",
                                                           "Type of Vehicle" = "tipo_vehiculo",
                                                           "Type of Person" = "tipo_persona",
                                                           "Severity" = "lesividad",
                                                           "Age Range" = "simplified_age_range"),
                                               selected = "tipo_accidente"),
                                   uiOutput("checkboxGroup2")
                                 )
                        )
                      )
                     ),
                column(9,
                       
                       leafletOutput("map", height = "600px"),
                       tags$hr(style = "border-top: 2px solid #333;"),  # Styled horizontal line
                       fluidRow(
                         column(3,
                                actionButton("refreshMap", "Refresh Map")),
                         column(3,checkboxInput("enableStreets", "View Quiet Streets", value = FALSE)),
                         column(3,checkboxInput("enableCicloStreets", "View Ciclocarriles", value = FALSE))
                       )
                )
            )
    )
    ,tabPanel("Data View",
               DTOutput("dataTable")
    )
  )
)


#SERVER 

server <- function(input, output,session) {
  
  #output for checkbox selector
  output$checkboxGroup1 <- renderUI({
    variable <- input$filterVariable1
    
    checkboxGroupInput("features1", paste("Select", variable),
                       choices = unique(accident_data[[variable]]),
                       selected = unique(accident_data[[variable]])
    )
  })
  
  output$checkboxGroup2 <- renderUI({
    variable <- input$filterVariable2
    
    checkboxGroupInput("features2", paste("Select", variable),
                       choices = unique(accident_data[[variable]]),
                       selected = unique(accident_data[[variable]])
    )
  })
  
  
  # Existing server logic for plots and data table
  # Plotting logic
  # Convert the input string to a symbol
  yVariable <- reactive(sym(input$yVariable))
  xVariable <- reactive(sym(input$xVariable))
  # Group and summarize data based on the selected y-axis variable
  grouped_data <- reactive({
    accident_data %>%
      filter(fecha >= input$startDate & fecha <= input$endDate) %>%
      group_by(!!xVariable(), !!yVariable()) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  grouped_data_proportional <- reactive({
    proportional <- input$proportional
    if (proportional && yVariable() == "estado_meteorológico") {
      result <- grouped_data() %>%
        mutate(count = case_when(
          estado_meteorológico == "Despejado" ~ count / 228*365,
          estado_meteorológico == "LLuvia intensa" ~ count / 14*365,
          estado_meteorológico == "Lluvia débil" ~ count / 75*365,
          estado_meteorológico == "Nublado" ~ count / 45*365,
          estado_meteorológico == "Granizando" ~ count / 3*365,
          TRUE ~ count  # Keep other cases unchanged
        ))
    } else {
      result <- grouped_data()
    }
    
    return(result)
    
  })
  output$districtPlot <- renderPlot({
    # Extract the data from the reactive expression
    data_to_plot <- grouped_data_proportional()
    
    if (xVariable()=="fecha"){
      
      ggplot(data_to_plot, aes(x = !!xVariable(), y = count, color = !!yVariable())) +
        geom_line() +
        stat_summary(aes(group = 1), fun = sum, geom = "line", color = "black") +
        labs(x = "Fecha",
             y = "Number of Accidents") +
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
        theme_minimal()
      
    }else{
      # Use the extracted data for plotting
      ggplot(data_to_plot, aes(x = !!xVariable(), y = count, fill = !!yVariable())) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = "Number of Accidents", x = xVariable())
    }
  })

  refreshed_map <- eventReactive(input$refreshMap, {
    filtered_data <- accident_data %>%
      filter(accident_data[[input$filterVariable1]] %in% input$features1) %>%
      filter(fecha >= input$startDate1 & fecha <= input$endDate1) 
    
    filtered_data2 <- accident_data %>%
      filter(accident_data[[input$filterVariable2]] %in% input$features2) %>%
      filter(fecha >= input$startDate2 & fecha <= input$endDate2)
    
    
    
    map <- leaflet(data = filtered_data) %>%
      addTiles() %>%
      setView(lng = -3.70379, lat = 40.41678, zoom = 12) %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude, weight = 1,
                       color = "blue",
                       fillColor = "blue",
                       fillOpacity = 0.5, radius = 5,
                       clusterOptions = markerClusterOptions(
                         spiderfyOnMaxZoom = FALSE,
                         disableClusteringAtZoom = 15
                       ))
    if(input$enableStreets==TRUE){
      for (i in 1:nrow(street_data)) {
        map <- map %>%
          addPolylines(
            lng = c(street_data$lon1[i], street_data$lon2[i]),
            lat = c(street_data$lat1[i], street_data$lat2[i]),
            color = "green"
          )
      }
    }
    if(input$enableCicloStreets==TRUE){
      for (i in 1:nrow(ciclostreet_data)) {
        map <- map %>%
          addPolylines(
            lng = c(ciclostreet_data$lon1[i], ciclostreet_data$lon2[i]),
            lat = c(ciclostreet_data$lat1[i], ciclostreet_data$lat2[i]),
            color = "cyan"
          )
      }
    }
    if(input$enableGroup2==TRUE){
      map <- map %>%
      addCircleMarkers(
        data = filtered_data2,
        lng = ~longitude, lat = ~latitude,
        weight = 1, color = "red", fillColor = "red",
        fillOpacity = 0.5, radius = 5,
        clusterOptions = markerClusterOptions(
          spiderfyOnMaxZoom = FALSE,
          disableClusteringAtZoom = 15
        ))
    }
    
    map
  })
  
  # Render the map
  output$map <- renderLeaflet({
    refreshed_map()
  })
  
  
  # Data table logic
  output$dataTable <- renderDT({
    accident_data %>%
      arrange(fecha, hora) # Assuming 'fecha' and 'hora' are your date and time columns
  }, options = list(pageLength = 10)) # Set number of rows per page
}
# Run the application
shinyApp(ui = ui, server = server)
