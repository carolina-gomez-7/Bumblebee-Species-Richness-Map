# ---------------------------------------------
# Bumblebee Species Richness Mapping Tool
# ---------------------------------------------
# GOALS:
# 1. Interactive map with pie charts showing species richness per location
# 2. Export map to PDF and HTML
# 3. Save coordinates and site IDs to CSV
# ---------------------------------------------

# Load required libraries
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(png)
library(grid)
library(htmltools)
library(webshot2)
library(mapview)
library(htmlwidgets)

# ---------------------------------------------
# STEP 1: Create species-specific data frames
# ---------------------------------------------

# Create species-specific data frames 
df_B_hortorum <- data.frame(
  lon = c(-1.576277, -1.5751182, -1.5719642, -1.5772777, -1.5596666, -1.565666, -1.5646606, -1.5600888, -1.5600944, -1.5522758, -1.5279676),
  lat = c(54.761424, 54.7620558, 54.764135, 54.7665833, 54.7708333, 54.7762778, 54.7769370, 54.7781604, 54.7799444, 54.7820572, 54.8086426),
  richness = c(1, 6, 1, 1, 3, 1, 1, 2, 2, 1, 1),
  species = "B_hortorum"
)

df_B_hypnorum <- data.frame(
  lon = c(-1.576277, -1.5751182, -1.5733442, -1.5719642,  -1.5704870,  -1.5703611, -1.5692777, -1.5740621, -1.5681740, -1.5583888, -1.565666, -1.5656666, -1.5522758),
  lat = c(54.761424, 54.7620558, 54.7639349, 54.764135, 54.7645481, 54.7647778, 54.7673611, 54.7686715, 54.7688479, 54.7718611, 54.7762778, 54.7762778, 54.7820572),
  richness = c(1, 3, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1),
  species = "B_hypnorum"
)

df_B_lapidarius <- data.frame(
  lon = c(-1.576277, -1.5732222, -1.6044652, -1.5719642, -1.5703611, -1.5839970, -1.5704870, -1.57141666,  -1.57197222, -1.5700046, -1.5701440, -1.5583888, -1.5564084, -1.565666,  -1.5600944, -1.5974077),
  lat = c(54.761424, 54.7636513, 54.7640868, 54.764135, 54.7647778, 54.7669422, 54.7645481, 54.7675278, 54.7676389, 54.7682667, 54.7687814, 54.7718611, 54.7732966, 54.7762778, 54.7799444, 54.7894340),
  richness = c(1, 17, 5, 5, 1, 18, 2, 3, 4, 5, 2, 1, 3, 1, 1, 1),
  species = "B_lapidarius"
)

df_B_lucorum <- data.frame(
  lon = c(-1.576277, -1.5732222,-1.5733442, -1.5719642, -1.5703611, -1.5839970, -1.5692777, -1.5719217, -1.5700046, -1.5740621, -1.5701440, -1.565666, -1.5646606),
  lat = c(54.761424, 54.7636513, 54.7639349, 54.764135, 54.7647778, 54.7669422, 54.7673611, 54.7676108, 54.7682667, 54.7686715, 54.7687814, 54.7762778, 54.7769370),
  richness = c(11, 5, 1, 3, 2, 8, 1, 1, 2, 1, 3, 1, 1),
  species = "B_lucorum"
)

df_B_pascuorum <- data.frame(
  lon = c(-1.576277, -1.5751182, -1.5732222, -1.5733442, -1.6044652, -1.5719642, -1.5704870, -1.5772777, -1.5839970, -1.57141666, -1.5719217, -1.5700046, -1.5701440, -1.5596666, -1.5583888, -1.5564084, -1.565666, -1.5656666,  -1.56566666, -1.5646606, -1.5600944, -1.5619807, -1.5522758, -1.5974077, -1.5244402),
  lat = c(54.761424, 54.7620558, 54.7636513, 54.7639349, 54.7640868, 54.764135, 54.7645481, 54.7665833, 54.7669422, 54.7675278, 54.7676108, 54.7682667, 54.7687814, 54.7708333, 54.7718611, 54.7732966, 54.7762778, 54.7762778, 54.7762778, 54.7769370, 54.7799444, 54.7804274, 54.7820572, 54.7894340, 54.8046718),
  richness = c(6, 4, 11, 4, 2, 12, 11, 1, 17, 3, 4, 1, 1, 6, 9, 1, 2, 4, 4, 1, 1, 2, 3, 3, 4),
  species = "B_pascuorum"
)

df_B_pratorum <- data.frame(
  lon = c(-1.576277, -1.5751182, -1.5732222, -1.5733442, -1.5719642, -1.5703611, -1.5692777, -1.5700046, -1.5740621, -1.5681740, -1.5596666, -1.5656666, -1.5646606, -1.5279676),
  lat = c(54.761424, 54.7620558, 54.7636513, 54.7639349, 54.764135, 54.7647778, 54.7673611, 54.7682667, 54.7686715, 54.7688479, 54.7708333, 54.7762778, 54.7769370, 54.8086426),
  richness = c(4, 4, 2, 4, 10, 1, 6, 3, 2, 7, 1, 2, 1, 3),
  species = "B_pratorum"
)

df_B_rupestris <- data.frame(
  lon = c(-1.5746575, -1.6044652, -1.5704870, -1.57141666, -1.5700046),
  lat = c(54.7626921, 54.7640868, 54.7645481, 54.7675278, 54.7682667),
  richness = c(1, 2, 1, 1, 1),
  species = "B_rupestris"
)

df_B_terrestris <- data.frame(
  lon = c(-1.576277,-1.5751182, -1.5746575, -1.5732222, -1.5733442, -1.6044652, -1.5719642, -1.5704870, -1.5703611,  -1.5772777, -1.5839970, -1.5692777, -1.57141666, -1.5719217,  -1.57197222, -1.5700046, -1.5740621, -1.5701440, -1.5681740, -1.5583888, -1.5564084, -1.56566666, -1.5646606, -1.5600944, -1.5619807,  -1.5522758, -1.5974077),
  lat = c(54.761424, 54.7620558, 54.7626921, 54.7636513, 54.7639349, 54.7640868, 54.764135, 54.7645481, 54.7647778, 54.7665833, 54.7669422, 54.7673611, 54.7675278, 54.7676108, 54.7676389, 54.7682667, 54.7686715, 54.7687814, 54.7688479, 54.7718611, 54.7732966, 54.7762778, 54.7769370, 54.7799444, 54.7804274, 54.7820572, 54.7894340),
  richness = c(4, 2, 1, 6, 5, 2, 5, 3, 1, 10, 16, 2, 3, 3, 1, 3, 1, 3, 1, 7, 1, 1, 1, 8, 1, 3, 1),
  species = "B_terrestris"
)

df_B_vestalis <- data.frame(
  lon = c(-1.5719642, -1.57141666),
  lat = c(54.764135, 54.7675278),
  richness = c(2, 1),
  species = "B_vestalis"
)

df_B_barbutellus <- data.frame(
  lon = c(-1.4948615),
  lat = c(54.8314907),
  richness = c(1),
  species = "B_barbutellus"
)


# Combine all species into one dataset
all_data <- bind_rows(
  df_B_hortorum, df_B_hypnorum, df_B_lapidarius, df_B_lucorum,
  df_B_pascuorum, df_B_pratorum, df_B_rupestris, df_B_terrestris, 
  df_B_vestalis, df_B_barbutellus
) %>%
  mutate(lat = round(lat, 5), lon = round(lon, 5))

# ---------------------------------------------
# STEP 2: Aggregate data by species and location
# ---------------------------------------------

richness_by_location <- all_data %>%
  group_by(lat, lon, species) %>%
  summarise(richness = sum(richness), .groups = "drop")

wide_data <- richness_by_location %>%
  pivot_wider(names_from = species, values_from = richness, values_fill = 0) %>%
  mutate(location_id = row_number()) %>%
  select(location_id, everything())

# ---------------------------------------------
# STEP 3: Define color palette
# ---------------------------------------------

species_colors <- c(
  B_hortorum = "#4d9de0",
  B_hypnorum = "#7776bc",
  B_lapidarius = "#ffd900",
  B_lucorum = "#71d6ca",
  B_pascuorum = "#f050ae",
  B_pratorum = "#57c78b",
  B_terrestris = "#ff595e",
  B_rupestris = "#ff924c",  
  B_vestalis  = "#7a0045",   
  B_barbutellus  = "#35e95f" 
)

# Create pie charts and save as PNGs
temp_dir <- tempdir()
icon_paths <- list()

for (i in 1:nrow(wide_data)) {
  row <- wide_data[i, ]
  species_counts <- row %>%
    select(-lat, -lon) %>%
    pivot_longer(cols = everything(), names_to = "species", values_to = "count") %>%
    filter(count > 0)
  
  # Make pie chart
  p <- ggplot(species_counts, aes(x = "", y = count, fill = species)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position = "none")
  
  # Save as PNG
  img_path <- file.path(temp_dir, paste0("pie_", i, ".png"))
  ggsave(img_path, plot = p, width = 2, height = 2, dpi = 72, bg = "transparent")
  icon_paths[[i]] <- img_path
}

# Create leaflet map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -1.5732, lat = 54.7768, zoom = 13)

# Add pie chart markers
for (i in 1:nrow(wide_data)) {
  row <- wide_data[i, ]
  icon <- makeIcon(
    iconUrl = icon_paths[[i]],
    iconWidth = 40, iconHeight = 40,
    iconAnchorX = 20, iconAnchorY = 20
  )
  map <- map %>%
    addMarkers(
      lng = row$lon, lat = row$lat,
      icon = icon,
      popup = paste("Location:", row$lat, row$lon)
    )
}

# Show map
map


# ---------------------------------------------
# Add scale bar and species legend to map
# ---------------------------------------------

# Add scale bar
map <- map %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 100, metric = TRUE))

# Define HTML for species color legend
legend_html <- HTML(paste0(
  "<div style='background:white;padding:10px;border-radius:8px;box-shadow:0 0 10px rgba(0,0,0,0.3);font-size:14px;'>",
  "<b>Species Legend</b><br>",
  paste(
    sprintf(
      "<i style='background:%s;width:12px;height:12px;display:inline-block;margin-right:5px;border-radius:50%%;'></i> %s",
      species_colors, names(species_colors)
    ),
    collapse = "<br>"
  ),
  "</div>"
))

# Add the legend to the bottom right
map <- map %>%
  addControl(legend_html, position = "bottomright")


map


# ---------------------------------------------
# Save the interactive map as an HTML file
# ---------------------------------------------
saveWidget(map, file = "species_richness_map_june26_2.html", selfcontained = TRUE)


# ---------------------------------------------
# Save site coordinates and IDs as CSV
# ---------------------------------------------
coords <- wide_data %>% select(location_id, lat, lon)
write.csv(coords, "species_locations.csv", row.names = FALSE)

