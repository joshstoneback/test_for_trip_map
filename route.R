library(readxl)
library(dplyr)
library(lubridate)
library(suncalc) 
library(lutz) 

# Load Data 
excel_file <- "50 50.xlsx"
locs_df <- read_excel(excel_file, sheet = "Locations")
dist_df <- read_excel(excel_file, sheet = "Distance Matrix (minutes)")

mat_data <- as.matrix(dist_df[, -1])
rownames(mat_data) <- dist_df[[1]]
colnames(mat_data) <- colnames(dist_df)[-1]

# Converts time to minutes
time_to_mins <- function(t_col) {
  sapply(t_col, function(x) {
    if (is.na(x)) return(NA_real_)
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
      return(hour(x) * 60 + minute(x))
    }
    parts <- as.numeric(strsplit(as.character(x), ":")[[1]])
    return(parts[1] * 60 + parts[2])
  })
}

locs_df <- locs_df %>%
  mutate(
    Open_Mins  = time_to_mins(`Food Open`),
    Close_Mins = time_to_mins(`Food Close`),
    Hike_Mins  = round(`Trail Length (hours)` * 60)
  )

trails <- locs_df %>% filter(Type == "Trail") %>% arrange(State)
foods  <- locs_df %>% filter(Type == "Food")  %>% arrange(State)
states <- trails$State
n_states <- length(states)

# Precompute timezone for each trailhead
trails$TZ <- tz_lookup_coords(trails$Latitude, trails$Longitude, method = "fast", warn = FALSE)

# Precompute exact local sunrise & sunset minutes
# Using June 13, 2027 (Sunday) as Day 0
# start_offset (0..6) shifts start day of week
base_sunday <- as.Date("2027-06-13")
days_of_week <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")

get_local_sun_mins <- function(lat, lon, tz_str, date_val) {
  st <- getSunlightTimes(date = date_val, lat = lat, lon = lon, keep = c("sunrise", "sunset"), tz = "UTC")
  sr_local <- with_tz(st$sunrise, tzone = tz_str)
  ss_local <- with_tz(st$sunset,  tzone = tz_str)
  c(
    sunrise = hour(sr_local) * 60 + minute(sr_local),
    sunset  = hour(ss_local) * 60 + minute(ss_local)
  )
}

is_closed_on_day <- function(closed_str, dow_str) {
  if (is.na(closed_str) || closed_str == "") return(FALSE)
  closed_vec <- trimws(strsplit(closed_str, ",")[[1]])
  return(dow_str %in% closed_vec)
}

# Map state names to integer indices 1..48
state_idx_map <- setNames(seq_along(states), states)

# Precompute 48x55 matrices of exact local sunrise & sunset minutes
# Doing this ahead of time significantly helps with compute time
max_days_span <- 55 # 48 trip days + 7 start-day offsets
sunrise_mat <- matrix(0, nrow = n_states, ncol = max_days_span)
sunset_mat  <- matrix(0, nrow = n_states, ncol = max_days_span)

for (i in 1:n_states) {
  for (day_col in 1:max_days_span) {
    d_val <- base_sunday + (day_col - 1)
    sun_b <- get_local_sun_mins(trails$Latitude[i], trails$Longitude[i], trails$TZ[i], d_val)
    sunrise_mat[i, day_col] <- sun_b["sunrise"]
    sunset_mat[i, day_col]  <- sun_b["sunset"]
  }
}

# Precompute 48x7 boolean matrix of closed days (State x Days in week)
closed_mat <- matrix(FALSE, nrow = n_states, ncol = 7)
for (i in 1:n_states) {
  for (w in 1:7) {
    closed_mat[i, w] <- is_closed_on_day(foods$`Food Close Days`[i], days_of_week[w])
  }
}

hike_mins_vec  <- trails$Hike_Mins
open_mins_vec  <- foods$Open_Mins
close_mins_vec <- foods$Close_Mins

# Precompute 48x7 matrix of closing times (State x Day of week: 1=Sun, ..., 7=Sat)
close_mins_mat <- matrix(rep(foods$Close_Mins, times = 7), nrow = n_states, ncol = 7)

# Override Texas closing time to 22:00 (1320 mins) on Fridays (col 6) and Saturdays (col 7)
# This is the longest state for drive time so doing this helps performance a lot
tx_idx <- state_idx_map["Texas"]
close_mins_mat[tx_idx, c(6, 7)] <- 22 * 60

# Precompute 48x48 intra-state drive vector and 48x48x2x2 transit array
intra_drive_vec <- numeric(n_states)
transit_arr <- array(0, dim = c(n_states, n_states, 2, 2))

for (i in 1:n_states) {
  st_i <- states[i]
  intra_drive_vec[i] <- mat_data[paste0(st_i, " (Trail)"), paste0(st_i, " (Food)")]
  for (j in 1:n_states) {
    st_j <- states[j]
    transit_arr[i, j, 1, 1] <- mat_data[paste0(st_i, " (Trail)"), paste0(st_j, " (Trail)")]
    transit_arr[i, j, 1, 2] <- mat_data[paste0(st_i, " (Trail)"), paste0(st_j, " (Food)")]
    transit_arr[i, j, 2, 1] <- mat_data[paste0(st_i, " (Food)"),  paste0(st_j, " (Trail)")]
    transit_arr[i, j, 2, 2] <- mat_data[paste0(st_i, " (Food)"),  paste0(st_j, " (Food)")]
  }
}

# Function to evaluate a route, returning a score
# Lower scores are better
evaluate_route_fast <- function(state_idx_order, start_offset = 2) {
  EAT_DURATION      <- 45
  TARGET_SLEEP_MINS <- 420  # 7.0 hrs ideal rest
  HARD_MIN_SLEEP    <- 300  # 5.0 hrs minimum sleep floor
  
  curr_time        <- 0
  prev_idx         <- 0
  prev_last_type   <- 1 # 1 = Trail, 2 = Food
  night_drive_done <- 0
  
  total_drive_mins <- 0
  total_penalty    <- 0
  
  for (d in 1:n_states) {
    idx     <- state_idx_order[d]
    day_col <- start_offset + d
    dow_idx <- ((day_col - 1) %% 7) + 1
    
    # Expand solar daylight window by +/- 30 mins for civil twilight
    sunrise_mins <- sunrise_mat[idx, day_col] - 30
    sunset_mins  <- sunset_mat[idx, day_col] + 30
    
    intra_drive  <- intra_drive_vec[idx]
    hike_len     <- hike_mins_vec[idx]
    f_open       <- open_mins_vec[idx]
    f_close      <- close_mins_mat[idx, dow_idx]
    base_pen     <- if (closed_mat[idx, dow_idx]) 100000 else 0
    
    # Trail First (first_type = 1, last_type = 2)
    p1 <- base_pen
    sim1 <- if (d == 1) {
      sunrise_mins
    } else {
      curr_time + max(0, transit_arr[prev_idx, idx, prev_last_type, 1] - night_drive_done)
    }
    h_start1 <- max(sim1, sunrise_mins)
    h_end1   <- h_start1 + hike_len
    if (h_end1 > sunset_mins) p1 <- p1 + (h_end1 - sunset_mins) * 80
    
    f_arr1 <- max(h_end1 + intra_drive, f_open)
    f_end1 <- f_arr1 + EAT_DURATION
    if (f_end1 > f_close) p1 <- p1 + (f_end1 - f_close) * 120
    if (d == n_states && f_end1 > 780) p1 <- p1 + (f_end1 - 780) * 200
    score1 <- p1 + f_end1
    
    # Food First (first_type = 2, last_type = 1)
    p2 <- base_pen
    sim2 <- if (d == 1) {
      max(sunrise_mins, f_open)
    } else {
      curr_time + max(0, transit_arr[prev_idx, idx, prev_last_type, 2] - night_drive_done)
    }
    f_arr2 <- max(sim2, f_open)
    f_end2 <- f_arr2 + EAT_DURATION
    if (f_end2 > f_close) p2 <- p2 + (f_end2 - f_close) * 120
    
    h_start2 <- max(f_end2 + intra_drive, sunrise_mins)
    h_end2   <- h_start2 + hike_len
    if (h_end2 > sunset_mins) p2 <- p2 + (h_end2 - sunset_mins) * 80
    if (d == n_states && h_end2 > 780) p2 <- p2 + (h_end2 - 780) * 200
    score2 <- p2 + h_end2
    
    if (score1 <= score2) {
      total_penalty <- total_penalty + p1
      end_time      <- f_end1
      last_type     <- 2
    } else {
      total_penalty <- total_penalty + p2
      end_time      <- h_end2
      last_type     <- 1
    }
    
    if (d < n_states) {
      next_idx <- state_idx_order[d + 1]
      est_next_drive <- min(transit_arr[idx, next_idx, last_type, 1],
                            transit_arr[idx, next_idx, last_type, 2])
      
      # Flexible night driving up to 23:30 (1410 mins) when needed to protect tomorrow's windows
      avail_tonight    <- max(0, 1410 - end_time)
      night_drive_done <- min(est_next_drive, avail_tonight)
      bed_time         <- end_time + night_drive_done
      
      rem_drive_est <- max(0, est_next_drive - night_drive_done)
      earliest_goal <- min(sunrise_mat[next_idx, day_col + 1] - 30, open_mins_vec[next_idx])
      ideal_wake    <- max(300, earliest_goal - rem_drive_est) # Earliest wake 05:00 AM (300)
      avail_sleep   <- (1440 - bed_time) + ideal_wake
      
      if (avail_sleep >= TARGET_SLEEP_MINS) {
        wake_time <- ideal_wake
      } else if (avail_sleep >= HARD_MIN_SLEEP) {
        wake_time     <- ideal_wake
        total_penalty <- total_penalty + (TARGET_SLEEP_MINS - avail_sleep) * 12
      } else {
        wake_time     <- (bed_time + HARD_MIN_SLEEP) - 1440
        total_penalty <- total_penalty + (TARGET_SLEEP_MINS - HARD_MIN_SLEEP) * 12
      }
      
      curr_time        <- wake_time
      prev_idx         <- idx
      prev_last_type   <- last_type
      total_drive_mins <- total_drive_mins + intra_drive + est_next_drive
    } else {
      total_drive_mins <- total_drive_mins + intra_drive
    }
  }
  
  return(total_drive_mins + total_penalty)
}

set.seed(42)

# Find integer indices for Oregon and Washington
or_idx <- state_idx_map["Oregon"]
wa_idx <- state_idx_map["Washington"]
all_idx <- 1:n_states

global_best_cost  <- Inf
global_best_idx   <- NULL
global_best_start <- 0

# Search across all 7 days of the week
for (start_day_idx in 0:6) {
  for (run in 1:5) { # 5 random restarts per start day (35 total runs)
    end_idx    <- sample(c(or_idx, wa_idx), 1)
    other_idx  <- sample(setdiff(all_idx, end_idx))
    curr_route <- c(other_idx, end_idx)
    curr_cost  <- evaluate_route_fast(curr_route, start_offset = start_day_idx)
    
    temp <- 4000
    for (iter in 1:50000) {
      cand_route <- curr_route
      
      # 5% of the time, swap the Day 48 finish between Oregon and Washington
      if (runif(1) < 0.05) {
        alt_end <- ifelse(cand_route[n_states] == or_idx, wa_idx, or_idx)
        alt_pos <- which(cand_route == alt_end)
        cand_route[c(alt_pos, n_states)] <- cand_route[c(n_states, alt_pos)]
      } else {
        # Otherwise, do a 2-opt segment reversal or pairwise swap on Days 1..47
        swap_pos <- sort(sample(1:(n_states - 1), 2))
        if (runif(1) < 0.7) {
          cand_route[swap_pos[1]:swap_pos[2]] <- rev(cand_route[swap_pos[1]:swap_pos[2]])
        } else {
          cand_route[swap_pos] <- cand_route[rev(swap_pos)]
        }
      }
      
      cand_cost <- evaluate_route_fast(cand_route, start_offset = start_day_idx)
      delta <- cand_cost - curr_cost
      
      if (delta < 0 || runif(1) < exp(-delta / temp)) {
        curr_route <- cand_route
        curr_cost  <- cand_cost
        if (curr_cost < global_best_cost) {
          global_best_cost  <- curr_cost
          global_best_idx   <- curr_route
          global_best_start <- start_day_idx
        }
      }
      temp <- temp * 0.9993
    }
  }
  cat(sprintf("Checked Start Day: %-5s | Best Global Cost so far: %.1f\n",
              days_of_week[start_day_idx + 1], global_best_cost))
}

# Generate schedule using the best route found above
generate_schedule_table <- function(state_idx_order, start_offset) {
  EAT_DURATION      <- 45
  TARGET_SLEEP_MINS <- 420  # 7.0 hrs ideal rest
  HARD_MIN_SLEEP    <- 300  # 5.0 hrs minimum sleep floor
  
  trip_start_date  <- base_sunday + start_offset
  curr_time        <- 0
  prev_idx         <- 0
  prev_last_type   <- 1
  night_drive_done <- 0
  
  fmt_t <- function(m) sprintf("%02d:%02d", floor((m %% 1440) / 60), round(m %% 60))
  schedule_log <- vector("list", n_states)
  
  for (d in 1:n_states) {
    idx       <- state_idx_order[d]
    st        <- states[idx]
    curr_date <- trip_start_date + (d - 1)
    day_col   <- start_offset + d
    dow_idx   <- ((day_col - 1) %% 7) + 1
    dow       <- days_of_week[dow_idx]
    
    # Raw solar times for display, plus +/- 30 min civil twilight buffer for scheduling
    raw_sunrise  <- sunrise_mat[idx, day_col]
    raw_sunset   <- sunset_mat[idx, day_col]
    sunrise_mins <- raw_sunrise - 30
    sunset_mins  <- raw_sunset + 30
    
    intra_drive  <- intra_drive_vec[idx]
    hike_len     <- hike_mins_vec[idx]
    f_open       <- open_mins_vec[idx]
    f_close      <- close_mins_mat[idx, dow_idx]
    closed_today <- closed_mat[idx, dow_idx]
    base_pen     <- if (closed_today) 100000 else 0
    
    # Trail first
    p1 <- base_pen
    m_drive1 <- if (d == 1) 0 else max(0, transit_arr[prev_idx, idx, prev_last_type, 1] - night_drive_done)
    sim1     <- if (d == 1) sunrise_mins else curr_time + m_drive1
    h_start1 <- max(sim1, sunrise_mins)
    h_end1   <- h_start1 + hike_len
    if (h_end1 > sunset_mins) p1 <- p1 + (h_end1 - sunset_mins) * 80
    
    f_arr1 <- max(h_end1 + intra_drive, f_open)
    f_end1 <- f_arr1 + EAT_DURATION
    if (f_end1 > f_close) p1 <- p1 + (f_end1 - f_close) * 120
    if (d == n_states && f_end1 > 780) p1 <- p1 + (f_end1 - 780) * 200
    score1 <- p1 + f_end1
    
    # Food first
    p2 <- base_pen
    m_drive2 <- if (d == 1) 0 else max(0, transit_arr[prev_idx, idx, prev_last_type, 2] - night_drive_done)
    sim2     <- if (d == 1) max(sunrise_mins, f_open) else curr_time + m_drive2
    f_arr2   <- max(sim2, f_open)
    f_end2   <- f_arr2 + EAT_DURATION
    if (f_end2 > f_close) p2 <- p2 + (f_end2 - f_close) * 120
    
    h_start2 <- max(f_end2 + intra_drive, sunrise_mins)
    h_end2   <- h_start2 + hike_len
    if (h_end2 > sunset_mins) p2 <- p2 + (h_end2 - sunset_mins) * 80
    if (d == n_states && h_end2 > 780) p2 <- p2 + (h_end2 - 780) * 200
    score2 <- p2 + h_end2
    
    if (score1 <= score2) {
      mode       <- "Trail_First"
      day_pen    <- p1
      m_drive    <- m_drive1
      hike_start <- h_start1; hike_end <- h_end1
      food_arr   <- f_arr1;   food_end <- f_end1
      end_time   <- f_end1;   last_type <- 2
    } else {
      mode       <- "Food_First"
      day_pen    <- p2
      m_drive    <- m_drive2
      hike_start <- h_start2; hike_end <- h_end2
      food_arr   <- f_arr2;   food_end <- f_end2
      end_time   <- h_end2;   last_type <- 1
    }
    
    # Flexible night driving and elastic sleep
    if (d < n_states) {
      next_idx <- state_idx_order[d + 1]
      est_next_drive <- min(transit_arr[idx, next_idx, last_type, 1],
                            transit_arr[idx, next_idx, last_type, 2])
      
      avail_tonight    <- max(0, 1410 - end_time)
      night_drive_done <- min(est_next_drive, avail_tonight)
      bed_time         <- end_time + night_drive_done
      
      rem_drive_est <- max(0, est_next_drive - night_drive_done)
      earliest_goal <- min(sunrise_mat[next_idx, day_col + 1] - 30, open_mins_vec[next_idx])
      ideal_wake    <- max(300, earliest_goal - rem_drive_est) # Earliest wake 05:00 AM (300)
      avail_sleep   <- (1440 - bed_time) + ideal_wake
      
      if (avail_sleep >= TARGET_SLEEP_MINS) {
        actual_sleep <- avail_sleep
        wake_time    <- ideal_wake
      } else if (avail_sleep >= HARD_MIN_SLEEP) {
        actual_sleep <- avail_sleep
        wake_time    <- ideal_wake
        day_pen      <- day_pen + (TARGET_SLEEP_MINS - avail_sleep) * 12
      } else {
        actual_sleep <- HARD_MIN_SLEEP
        wake_time    <- (bed_time + HARD_MIN_SLEEP) - 1440
        day_pen      <- day_pen + (TARGET_SLEEP_MINS - HARD_MIN_SLEEP) * 12
      }
      
      curr_time      <- wake_time
      prev_idx       <- idx
      prev_last_type <- last_type
    } else {
      night_drive_done <- 0
      actual_sleep     <- NA
    }
    
    schedule_log[[d]] <- data.frame(
      Day             = d,
      Date            = as.character(curr_date),
      DOW             = dow,
      State           = st,
      Order           = mode,
      Morning_Drive   = round(m_drive),
      Intra_Drive     = round(intra_drive),
      Night_Drive     = round(night_drive_done),
      Sun_Window      = paste0(fmt_t(raw_sunrise), "-", fmt_t(raw_sunset)),
      Hike_Time       = paste0(fmt_t(hike_start), "-", fmt_t(hike_end)),
      Food_Time       = paste0(fmt_t(food_arr), "-", fmt_t(food_end)),
      Sleep_Hrs       = ifelse(is.na(actual_sleep), NA, round(actual_sleep / 60, 1)),
      Closed_Conflict = closed_today,
      Penalty         = round(day_pen),
      stringsAsFactors = FALSE
    )
  }
  
  bind_rows(schedule_log)
}

# Run on best route (global_best_idx)
final_itinerary <- generate_schedule_table(global_best_idx, global_best_start)
print(final_itinerary)
write.csv(final_itinerary, "Itinerary.csv", row.names = FALSE)