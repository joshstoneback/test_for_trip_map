library(leaflet)
library(htmlwidgets)
library(dplyr)

df_locations <- read.csv("50 50 - Locations.csv")

# sequence of states found in my most recent optimization
state_order <- data.frame(
  Day = 1:48,
  State = c("Kansas","Nebraska","Colorado","New Mexico","Texas","Oklahoma","Arkansas","Missouri",
            "Illinois","Indiana","Ohio","Kentucky","Tennessee","North Carolina","South Carolina",
            "Georgia","Alabama","Louisiana","Mississippi","Florida","Virginia","West Virginia",
            "Maryland","Delaware","Pennsylvania","New Jersey","New York","Connecticut","Rhode Island",
            "Massachusetts","New Hampshire","Vermont","Maine","Michigan","Minnesota","Wisconsin",
            "Iowa","South Dakota","North Dakota","Montana","Wyoming","Utah","Arizona","Nevada",
            "California","Idaho","Washington","Oregon"),
  First_Stop = c("Food","Trail","Trail","Trail","Trail","Food","Food","Trail","Trail","Trail",
                 "Trail","Trail","Food","Food","Trail","Food","Trail","Food","Trail","Food",
                 "Trail","Food","Trail","Food","Food","Trail","Food","Food","Trail","Trail",
                 "Trail","Trail","Trail","Trail","Trail","Trail","Trail","Food","Food","Trail",
                 "Trail","Food","Food","Food","Food","Food","Food","Food")
)

# merge state sequence with locations
df_map <- df_locations %>%
  inner_join(state_order, by = "State") %>%
  arrange(Day, desc(Type == First_Stop))

# orange for Food, green for Trails
pal <- colorFactor(palette = c("#E65100", "#2E7D32"), domain = c("Food", "Trail"))

# build interactive leaflet map
map <- leaflet(df_map) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>% # one of the few free options
  
  # connect stops with a smooth route polyline
  addPolylines(
    lng = ~Longitude, lat = ~Latitude,
    color = "#1565C0", weight = 3, opacity = 0.7, dashArray = "5, 5"
  ) %>%
  
  # add markers for each location
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,
    radius = 6,
    color = ~pal(Type),
    stroke = TRUE, fillOpacity = 0.9,
    popup = ~paste0(
      "<b>Day ", Day, ": ", State, "</b><br>",
      "<b>Type:</b> ", Type, "<br>",
      "<b>Name:</b> ", Location.Name
    )
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors = c("#E65100", "#2E7D32", "#1565C0"),
    labels = c("Food Stop", "Trailhead", "Route Line"),
    title = "48-Day Road Trip"
  )

# save
saveWidget(map, file = "index.html", selfcontained = TRUE)