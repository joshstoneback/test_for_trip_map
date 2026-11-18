library(readxl)
library(writexl)
library(httr)    
library(jsonlite)
library(dplyr)

# Load in locations from excel document containing info including lat and long
locs <- read_excel("50 50.xlsx", sheet = "Locations") %>%
  mutate(Label = paste0(State, " (", Type, ")"))

n <- nrow(locs)
distances <- matrix(NA_real_, nrow = n, ncol = n)
rownames(distances) <- locs$Label
colnames(distances) <- locs$Label

# Queries OSRM Table API for a block of source & destination indices
fetch_osrm_block <- function(src_idx, dst_idx, df) {
  all_idx <- unique(c(src_idx, dst_idx))
  coords_str <- paste(sprintf("%.6f,%.6f", df$Longitude[all_idx], df$Latitude[all_idx]), collapse = ";")
  
  # Map src_idx and dst_idx to 0-based positions in all_idx
  src_pos <- paste(match(src_idx, all_idx) - 1, collapse = ";")
  dst_pos <- paste(match(dst_idx, all_idx) - 1, collapse = ";")
  
  url <- sprintf(
    "http://router.project-osrm.org/table/v1/driving/%s?sources=%s&destinations=%s&annotations=duration",
    coords_str, src_pos, dst_pos
  )
  
  res <- GET(url, timeout(30))
  stop_for_status(res)
  parsed <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  if (parsed$code != "Ok") stop("OSRM Routing Error: ", parsed$code)
  
  # Convert seconds to rounded minutes
  return(round(parsed$durations / 60))
}

# Query 96x96 matrix in four 48x48 quadrants (avoids URL length limits)
idx_1 <- 1:48
idx_2 <- 49:96

cat("Fetching Quadrant 1/4...\n"); distances[idx_1, idx_1] <- fetch_osrm_block(idx_1, idx_1, locs); Sys.sleep(1)
cat("Fetching Quadrant 2/4...\n"); distances[idx_1, idx_2] <- fetch_osrm_block(idx_1, idx_2, locs); Sys.sleep(1)
cat("Fetching Quadrant 3/4...\n"); distances[idx_2, idx_1] <- fetch_osrm_block(idx_2, idx_1, locs); Sys.sleep(1)
cat("Fetching Quadrant 4/4...\n"); distances[idx_2, idx_2] <- fetch_osrm_block(idx_2, idx_2, locs)

diag(distances) <- 0

print(distances)
