library(readxl)
library(dplyr)
library(TSP)

# load excel travel matrix file
matrix_file <- "Continental_US_96_Locations_Travel_Matrix.xlsx"

time_matrix_df <- read_excel(matrix_file, sheet = "Travel Time (Minutes)")
loc_directory  <- read_excel(matrix_file, sheet = "Locations Directory")

# convert to matrix
mat_data <- as.matrix(time_matrix_df[, -1])
rownames(mat_data) <- time_matrix_df[[1]]
colnames(mat_data) <- colnames(time_matrix_df)[-1]

# get unique 48 continental states
continental_states <- unique(loc_directory$State)
n_states <- length(continental_states)

# build asymmetric state transition matrix
state_cost_matrix <- matrix(0, nrow = n_states, ncol = n_states)
rownames(state_cost_matrix) <- continental_states
colnames(state_cost_matrix) <- continental_states

for(i in 1:n_states) {
  state_A <- continental_states[i]
  A_trail <- paste0(state_A, " (Trail)")
  A_food  <- paste0(state_A, " (Food)")
  intra_A <- mat_data[A_trail, A_food]
  
  for(j in 1:n_states) {
    if(i == j) {
      state_cost_matrix[i, j] <- 0
    } else {
      state_B <- continental_states[j]
      B_trail <- paste0(state_B, " (Trail)")
      B_food  <- paste0(state_B, " (Food)")
      
      # 4 possible transit options from A to B
      d1 <- intra_A + mat_data[A_food, B_trail]
      d2 <- intra_A + mat_data[A_food, B_food]
      d3 <- intra_A + mat_data[A_trail, B_trail]
      d4 <- intra_A + mat_data[A_trail, B_food]
      
      state_cost_matrix[i, j] <- min(d1, d2, d3, d4)
    }
  }
}

# solve asymmetric TSP tour
atsp_prob <- ATSP(state_cost_matrix)

set.seed(42)
tour <- solve_TSP(atsp_prob, method = "repetitive_nn")

tour_indices <- as.integer(tour)
ordered_states <- continental_states[tour_indices]

cat("Optimal Route Found!\n")
cat("Total Continental Driving Time:", round(tour_length(tour) / 60, 1), "Hours\n\n")

# build itinerary table
itinerary <- data.frame(
  Day = paste("Day", 1:n_states),
  State = ordered_states,
  First_Stop = character(n_states),
  Intra_State_Drive_Mins = numeric(n_states),
  Drive_To_Next_State_Mins = numeric(n_states),
  Total_Drive_Hours = numeric(n_states),
  stringsAsFactors = FALSE
)

for(d in 1:n_states) {
  curr_state <- ordered_states[d]
  next_state <- ifelse(d < n_states, ordered_states[d + 1], ordered_states[1])
  
  curr_trail <- paste0(curr_state, " (Trail)")
  curr_food  <- paste0(curr_state, " (Food)")
  next_trail <- paste0(next_state, " (Trail)")
  next_food  <- paste0(next_state, " (Food)")
  
  intra_mins <- mat_data[curr_trail, curr_food]
  
  drive_if_trail_first <- mat_data[curr_food, next_trail]
  drive_if_food_first  <- mat_data[curr_trail, next_trail]
  
  if(drive_if_trail_first <= drive_if_food_first) {
    itinerary$First_Stop[d] <- "Trail First"
    itinerary$Drive_To_Next_State_Mins[d] <- drive_if_trail_first
  } else {
    itinerary$First_Stop[d] <- "Food First"
    itinerary$Drive_To_Next_State_Mins[d] <- drive_if_food_first
  }
  
  itinerary$Intra_State_Drive_Mins[d] <- intra_mins
  itinerary$Total_Drive_Hours[d] <- round((intra_mins + itinerary$Drive_To_Next_State_Mins[d]) / 60, 1)
}

print(head(itinerary, 10))

write.csv(itinerary, "Optimal_48_Day_Continental_Itinerary.csv", row.names = FALSE)
