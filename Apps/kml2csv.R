
kml_content <- readLines("C:\\Users\\afont\\Documents\\RStudio WD\\Shiny Tutorial Assignment -  Group 12\\Assignment\\ciclocarriles.kml", warn = FALSE)

# Combine the lines into a single character string
kml_text <- paste(kml_content, collapse = " ")

# Use regular expression to extract coordinate strings
coordinates <- regmatches(kml_text, gregexpr("<coordinates>(.*?)</coordinates>", kml_text, perl = TRUE))[[1]]

# Print the list of coordinate strings
print(coordinates)

# Function to process coordinates and create dataframe
process_coordinates <- function(coord_str) {
  coord_str <- gsub("\\t", "", coord_str, fixed = TRUE)  # Remove tabs
  coords <- strsplit(coord_str, "\\s+|,")[[1]]  # Split by spaces or commas
  df <- data.frame(
    lon1 = as.numeric(coords[2]),
    lat1 = as.numeric(coords[3]),
    lon2 = as.numeric(coords[5]),
    lat2 = as.numeric(coords[6])
  )
  return(df)
}

# Apply the function to each entry in coordinates
result_df <- lapply(coordinates, process_coordinates)
# Combine the list of dataframes into a single dataframe
final_df <- do.call(rbind, result_df)

# Print the final dataframe
print(final_df)
write.csv(final_df, "C:\\Users\\afont\\Documents\\RStudio WD\\Shiny Tutorial Assignment -  Group 12\\Assignment\\ciclocarriles.csv", row.names = FALSE)


street_data=read.csv("C:\\Users\\afont\\Documents\\RStudio WD\\Shiny Tutorial Assignment -  Group 12\\Assignment\\calles_tranquilas.csv", sep=",")
print(head(street_data))