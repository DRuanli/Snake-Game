# Main entry point for RL training
library(ggplot2)

# Source all necessary files
source("utils/constants.R")
source("utils/helpers.R")
source("game/config.R")
source("config_rl.R")
source("rl/q_learning.R")
source("game_rl/rl_environment.R")
source("game_rl/training_loop.R")
source("visualization/renderer.R")
source("visualization/learning_metrics.R")
source("evaluation/performance_metrics.R")

# Main function to train the RL agent
train_rl_agent <- function(render = TRUE, render_interval = 100) {
  cat("Starting RL training with", config_rl$num_episodes, "episodes\n")
  
  # Train the agent
  result <- train_agent(config, config_rl, render, render_interval)
  q_table <- result$q_table
  metrics <- result$metrics
  
  # Plot learning curves
  plots <- plot_learning_curve(metrics)
  print(plots$reward_plot)
  
  # Save plots
  save_learning_plots(metrics)
  
  cat("Training completed!\n")
  
  return(q_table)
}

# Function to run the trained agent
run_trained_agent <- function(q_table_file = NULL, num_episodes = 10) {
  # Load Q-table
  q_table <- NULL
  if (is.null(q_table_file)) {
    q_table_file <- config_rl$q_table_file
  }
  
  if (file.exists(q_table_file)) {
    q_table <- load_q_table(q_table_file)
    cat("Loaded Q-table from", q_table_file, "\n")
  } else {
    cat("No Q-table found. Please train the agent first.\n")
    return(NULL)
  }
  
  # Evaluate the agent
  cat("Evaluating agent for", num_episodes, "episodes\n")
  metrics <- evaluate_agent(q_table, config, config_rl, num_episodes)
  
  # Print summary
  print_evaluation_summary(metrics)
  
  return(metrics)
}

# Function to get a single move from the trained agent
get_rl_agent_move <- function(game, q_table_file = NULL) {
  # Load Q-table if not already loaded
  if (is.null(q_table) || !exists("q_table")) {
    if (is.null(q_table_file)) {
      q_table_file <- config_rl$q_table_file
    }
    
    if (file.exists(q_table_file)) {
      q_table <<- load_q_table(q_table_file)
    } else {
      cat("No Q-table found. Using random actions.\n")
      return(sample(c("UP", "DOWN", "LEFT", "RIGHT"), 1))
    }
  }
  
  # Encode current state
  state <- encode_state(game, config_rl)
  
  # Get valid actions
  valid_actions <- get_valid_actions(game$snake$direction)
  
  # Select best action
  action <- get_best_action(state, q_table, valid_actions)
  
  return(action)
}

# Display menu for selecting action
cat("Snake Game with Q-Learning - Phase 2\n")
cat("Available functions:\n")
cat("1. train_rl_agent(render = TRUE, render_interval = 100) - Train the RL agent\n")
cat("2. run_trained_agent(q_table_file = NULL, num_episodes = 10) - Run the trained agent\n")

# Initialize global q_table
q_table <- NULL