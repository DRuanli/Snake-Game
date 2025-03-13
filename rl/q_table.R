# Q-table management functions

# Initialize an empty Q-table
initialize_q_table <- function() {
  return(list())
}

# Get Q-value for a state-action pair
get_q_value <- function(q_table, state, action, default_value = 0) {
  # If state not in q_table, add it with default values
  if (!state %in% names(q_table)) {
    q_table[[state]] <- setNames(
      rep(default_value, 4),
      c("UP", "DOWN", "LEFT", "RIGHT")
    )
  }
  
  return(q_table[[state]][[action]])
}

# Update Q-value using Q-learning formula
update_q_value <- function(q_table, state, action, reward, next_state, config_rl) {
  # Current Q-value
  current_q <- get_q_value(q_table, state, action)
  
  # If next state is in the q-table, get the maximum Q-value for the next state
  max_next_q <- 0
  if (next_state %in% names(q_table)) {
    max_next_q <- max(unlist(q_table[[next_state]]))
  }
  
  # Q-learning formula: Q(s,a) = Q(s,a) + α * [R + γ * max(Q(s',a')) - Q(s,a)]
  new_q <- current_q + config_rl$alpha * (reward + config_rl$gamma * max_next_q - current_q)
  
  # Update the Q-table
  if (!state %in% names(q_table)) {
    q_table[[state]] <- setNames(
      rep(0, 4),
      c("UP", "DOWN", "LEFT", "RIGHT")
    )
  }
  
  q_table[[state]][[action]] <- new_q
  
  return(q_table)
}

# Save Q-table to file
save_q_table <- function(q_table, filename) {
  saveRDS(q_table, filename)
}

# Load Q-table from file
load_q_table <- function(filename) {
  if (file.exists(filename)) {
    return(readRDS(filename))
  } else {
    return(initialize_q_table())
  }
}