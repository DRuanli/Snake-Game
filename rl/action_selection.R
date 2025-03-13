# Action selection functions for RL

# Select action using epsilon-greedy strategy
select_action <- function(state, q_table, epsilon, available_actions = NULL) {
  # Default available actions if not specified
  if (is.null(available_actions)) {
    available_actions <- c("UP", "DOWN", "LEFT", "RIGHT")
  }
  
  # With probability epsilon, select a random action (explore)
  if (runif(1) < epsilon) {
    return(sample(available_actions, 1))
  }
  
  # Otherwise, select the best action from Q-table (exploit)
  return(get_best_action(state, q_table, available_actions))
}

# Get the action with the highest Q-value for a given state
get_best_action <- function(state, q_table, available_actions) {
  # If state not in Q-table, return random action
  if (!state %in% names(q_table)) {
    return(sample(available_actions, 1))
  }
  
  # Get Q-values for all actions in this state
  state_q_values <- q_table[[state]]
  
  # Filter to available actions
  available_q_values <- state_q_values[names(state_q_values) %in% available_actions]
  
  # Find action with maximum Q-value
  # In case of ties, randomly select among the best actions
  max_value <- max(available_q_values)
  best_actions <- names(available_q_values)[available_q_values == max_value]
  
  return(sample(best_actions, 1))
}

# Update epsilon based on decay rate
update_epsilon <- function(epsilon, config_rl) {
  return(max(config_rl$epsilon_min, epsilon * config_rl$epsilon_decay))
}