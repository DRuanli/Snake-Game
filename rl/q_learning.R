# Core Q-learning implementation

# Source all RL components
source("rl/state_encoding.R")
source("rl/reward_function.R")
source("rl/action_selection.R")
source("rl/q_table.R")

# Perform a single Q-learning update
q_learning_update <- function(old_state, action, reward, new_state, q_table, config_rl) {
  # Update Q-table using Q-learning formula
  q_table <- update_q_value(q_table, old_state, action, reward, new_state, config_rl)
  return(q_table)
}

# Get valid actions based on current direction
get_valid_actions <- function(current_direction) {
  # Define opposites
  opposites <- list(
    "UP" = "DOWN",
    "DOWN" = "UP",
    "LEFT" = "RIGHT",
    "RIGHT" = "LEFT"
  )
  
  # Valid actions are all except the opposite of current direction
  valid_actions <- c("UP", "DOWN", "LEFT", "RIGHT")
  valid_actions <- valid_actions[valid_actions != opposites[[current_direction]]]
  
  return(valid_actions)
}