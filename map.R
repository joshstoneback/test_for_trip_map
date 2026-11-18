library(readxl)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmltools)

# Load itinerary csv and locations excel sheet
itinerary <- read.csv("Itinerary.csv", stringsAsFactors = FALSE)
locs_df   <- read_excel("50 50.xlsx", sheet = "Locations")

# Clean up location names
locs_clean <- locs_df %>%
  mutate(
    Clean_Name = sub("^[^:]+:\\s*", "", `Location Name`),
    Open_Str   = ifelse(is.na(`Food Open`), "", format(as.POSIXct(`Food Open`), "%H:%M")),
    Close_Str  = ifelse(is.na(`Food Close`), "", format(as.POSIXct(`Food Close`), "%H:%M"))
  )

# Expand the 48-day itinerary into 96 ordered stops
ordered_stops <- list()
seq_num <- 1

for (i in 1:nrow(itinerary)) {
  row <- itinerary[i, ]
  st  <- row$State
  
  t_loc <- locs_clean %>% filter(State == st, Type == "Trail")
  f_loc <- locs_clean %>% filter(State == st, Type == "Food")
  
  # Determine chronological sequence for this day
  if (row$Order == "Trail_First") {
    first_type  <- "Trail"; first_df  <- t_loc; first_win  <- row$Hike_Time
    second_type <- "Food";  second_df <- f_loc; second_win <- row$Food_Time
  } else {
    first_type  <- "Food";  first_df  <- f_loc; first_win  <- row$Food_Time
    second_type <- "Trail"; second_df <- t_loc; second_win <- row$Hike_Time
  }
  
  # Stop 1 of Day d
  ordered_stops[[seq_num]] <- data.frame(
    Seq          = seq_num,
    Day          = row$Day,
    Date         = row$Date,
    DOW          = row$DOW,
    State        = st,
    Day_Order    = row$Order,
    Stop_Num     = 1,
    Type         = first_type,
    Name         = first_df$Clean_Name,
    Lat          = first_df$Latitude,
    Lon          = first_df$Longitude,
    Window       = first_win,
    Sun_Window   = row$Sun_Window,
    Trail_Hrs    = ifelse(first_type == "Trail", first_df$`Trail Length (hours)`, NA),
    Food_Hours   = ifelse(first_type == "Food", paste0(first_df$Open_Str, " - ", first_df$Close_Str), NA),
    Morning_Mins = row$Morning_Drive,
    Intra_Mins   = row$Intra_Drive,
    Night_Mins   = row$Night_Drive,
    Sleep_Hrs    = row$Sleep_Hrs,
    Penalty      = row$Penalty,
    stringsAsFactors = FALSE
  )
  seq_num <- seq_num + 1
  
  # Stop 2 of Day d
  ordered_stops[[seq_num]] <- data.frame(
    Seq          = seq_num,
    Day          = row$Day,
    Date         = row$Date,
    DOW          = row$DOW,
    State        = st,
    Day_Order    = row$Order,
    Stop_Num     = 2,
    Type         = second_type,
    Name         = second_df$Clean_Name,
    Lat          = second_df$Latitude,
    Lon          = second_df$Longitude,
    Window       = second_win,
    Sun_Window   = row$Sun_Window,
    Trail_Hrs    = ifelse(second_type == "Trail", second_df$`Trail Length (hours)`, NA),
    Food_Hours   = ifelse(second_type == "Food", paste0(second_df$Open_Str, " - ", second_df$Close_Str), NA),
    Morning_Mins = row$Morning_Drive,
    Intra_Mins   = row$Intra_Drive,
    Night_Mins   = row$Night_Drive,
    Sleep_Hrs    = row$Sleep_Hrs,
    Penalty      = row$Penalty,
    stringsAsFactors = FALSE
  )
  seq_num <- seq_num + 1
}

stops_df <- bind_rows(ordered_stops)

# Build html popups and hover labels
stops_df <- stops_df %>%
  mutate(
    Search_Label = sprintf("Day %d - %s (%s): %s", Day, State, Type, Name),
    Hover_HTML = sprintf(
      "<b>Day %d (%s) — %s</b><br/>%s: %s<br/><i>Scheduled: %s</i>",
      Day, DOW, State, Type, Name, Window
    ),
    Detail_Spec = ifelse(
      Type == "Trail",
      sprintf("<b>Trail Duration:</b> %.2f hrs<br/><b>Local Daylight:</b> %s", Trail_Hrs, Sun_Window),
      sprintf("<b>Restaurant Hours:</b> %s", Food_Hours)
    ),
    Popup_HTML = sprintf(
      "<div style='font-family: Arial, sans-serif; min-width: 230px; line-height: 1.4;'>
        <div style='background:%s; color:white; padding:6px 10px; border-radius:4px 4px 0 0; font-weight:bold;'>
          Day %d (%s, %s) &bull; %s
        </div>
        <div style='padding:8px 10px; border:1px solid #ddd; border-top:none; border-radius:0 0 4px 4px;'>
          <div style='font-size:14px; font-weight:bold; margin-bottom:4px;'>%s</div>
          <span style='background:#f0f2f5; padding:2px 6px; border-radius:3px; font-size:11px;'>
            Stop #%d of Day (%s)
          </span>
          <hr style='margin:6px 0; border:0; border-top:1px solid #eee;'/>
          <b>Scheduled Window:</b> %s<br/>
          %s<br/>
          <hr style='margin:6px 0; border:0; border-top:1px solid #eee;'/>
          <b>Day Drive Breakdown:</b><br/>
          &bull; Morning Transit In: %d mins<br/>
          &bull; Intra-State Drive: %d mins<br/>
          &bull; Night Transit Out: %d mins<br/>
          <b>Overnight Sleep After:</b> %s hrs
        </div>
      </div>",
      ifelse(Type == "Trail", "#2e7d32", "#d84315"),
      Day, DOW, Date, State,
      Name,
      Stop_Num, gsub("_", " ", Day_Order),
      Window,
      Detail_Spec,
      Morning_Mins, Intra_Mins, Night_Mins,
      ifelse(is.na(Sleep_Hrs), "Finish!", as.character(Sleep_Hrs))
    )
  )

# Create icons
trail_icons <- awesomeIcons(
  icon = "tree", library = "fa",
  markerColor = "green", iconColor = "#ffffff"
)

food_icons <- awesomeIcons(
  icon = "cutlery", library = "fa",
  markerColor = "orange", iconColor = "#ffffff"
)

start_finish_df <- stops_df %>% filter(Seq %in% c(1, nrow(stops_df)))
sf_icons <- awesomeIcons(
  icon = c("play", "flag-checkered"), library = "fa",
  markerColor = c("blue", "red"), iconColor = "#ffffff"
)

trails_only <- stops_df %>% filter(Type == "Trail")
foods_only  <- stops_df %>% filter(Type == "Food")

# Initialize leaflet map
m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World Street Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri Satellite") %>%
  setView(lng = -96.5, lat = 39.5, zoom = 4)

# Draw route segments
for (k in 1:(nrow(stops_df) - 1)) {
  p1 <- stops_df[k, ]
  p2 <- stops_df[k + 1, ]
  is_intra <- (p1$State == p2$State)
  
  seg_label <- if (is_intra) {
    sprintf("<b>Day %d (%s) Intra-State Drive:</b> %d mins (%s &rarr; %s)",
            p1$Day, p1$State, p1$Intra_Mins, p1$Type, p2$Type)
  } else {
    sprintf("<b>Cross-Border Transit (Day %d %s &rarr; Day %d %s):</b> %d mins night + %d mins morning",
            p1$Day, p1$State, p2$Day, p2$State, p1$Night_Mins, p2$Morning_Mins)
  }
  
  m <- m %>%
    addPolylines(
      lng = c(p1$Lon, p2$Lon),
      lat = c(p1$Lat, p2$Lat),
      color = ifelse(is_intra, "#1565c0", "#e65100"),
      weight = ifelse(is_intra, 3.5, 2.5),
      opacity = 0.85,
      dashArray = if (is_intra) NULL else "6, 6",
      label = lapply(seg_label, HTML),
      group = ifelse(is_intra, "Intra-State Drives", "Inter-State Transits")
    )
}

# Add markers, pins, and badges
m <- m %>%
  addAwesomeMarkers(
    data = trails_only,
    lng = ~Lon, lat = ~Lat,
    icon = trail_icons,
    label = ~lapply(Hover_HTML, HTML),
    popup = ~Popup_HTML,
    group = "Trails (48)"
  ) %>%
  addAwesomeMarkers(
    data = foods_only,
    lng = ~Lon, lat = ~Lat,
    icon = food_icons,
    label = ~lapply(Hover_HTML, HTML),
    popup = ~Popup_HTML,
    group = "Food Stops (48)"
  ) %>%
  addAwesomeMarkers(
    data = start_finish_df,
    lng = ~Lon, lat = ~Lat,
    icon = sf_icons,
    label = ~lapply(Hover_HTML, HTML),
    popup = ~Popup_HTML,
    group = "Start & Finish Pins"
  ) %>%
  # Invisible circle markers for the search bar
  addCircleMarkers(
    data = stops_df,
    lng = ~Lon, lat = ~Lat,
    radius = 0, opacity = 0, fillOpacity = 0,
    label = ~Search_Label,
    popup = ~Popup_HTML,
    group = "SearchIndex"
  ) %>%
  # Add day numbers
  addLabelOnlyMarkers(
    data = stops_df %>% filter(Stop_Num == 1),
    lng = ~Lon, lat = ~Lat,
    label = ~paste0("D", Day),
    labelOptions = labelOptions(
      noHide = TRUE, direction = "top", textOnly = TRUE,
      style = list(
        "color" = "#1a237e",
        "font-weight" = "bold",
        "font-size" = "10px",
        "text-shadow" = "1px 1px 2px #ffffff, -1px -1px 2px #ffffff"
      )
    ),
    group = "Day Number Labels"
  )

# Add the interactive components
m <- m %>%
  addSearchFeatures(
    targetGroups = "SearchIndex",
    options = searchFeaturesOptions(
      zoom = 10, openPopup = TRUE, firstTipSubmit = TRUE,
      autoCollapse = TRUE, hideMarkerOnCollapse = TRUE,
      textPlaceholder = "Search Day, State, Trail, or Food..."
    )
  ) %>%
  addLayersControl(
    baseGroups = c("Esri World Street Map", "Esri Satellite"),
    overlayGroups = c(
      "Trails (48)",
      "Food Stops (48)",
      "Start & Finish Pins",
      "Intra-State Drives",
      "Inter-State Transits",
      "Day Number Labels"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "miles",
    primaryAreaUnit = "sqmiles",
    activeColor = "#1565c0",
    completedColor = "#2e7d32"
  ) %>%
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE,
    position = "bottomright"
  ) %>%
  addScaleBar(position = "bottomleft") %>%
  addLegend(
    position = "topright",
    colors = c("#2e7d32", "#d84315", "#1565c0", "#e65100"),
    labels = c(
      "Trailhead (48)",
      "Food Stop (48)",
      "Intra-State Drive (Solid)",
      "Inter-State Transit (Dashed)"
    ),
    title = "50-50-50-50 Map",
    opacity = 0.9
  )

m
htmlwidgets::saveWidget(m, "index.html", selfcontained = TRUE)
