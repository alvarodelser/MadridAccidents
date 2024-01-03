library(sf)

kml_file_path <- "C:\\Users\\afont\\Documents\\RStudio WD\\Shiny Tutorial Assignment -  Group 12\\Assignment\\calles_tranquilas.kml"

# Attempt to read the KML file using sf
tryCatch({
  sf_data <- st_read(kml_file_path)
  print(head(sf_data))  # Print the first few rows of the data
}, error = function(e) {
  stop("Error reading KML file. Make sure the file is a valid KML file.")
})