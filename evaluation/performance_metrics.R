# Evaluation metrics for the trained agent

# Evaluate the trained agent
evaluate_agent <- function(q_table, config, config_rl, num_episodes = 100, render = TRUE) {
  # Initialize metrics
  metrics <- list(
    scores = numeric(num_episodes),
    steps = numeric(num_episodes),
    win_rate = 0
  )
  
  for (episode in 1:num_episodes) {
    # Reset environment
    env <- rl_reset(config)
    game <- env$game
    state <- env$state
    
    steps <- 0
    
    # Run episode
    for (step in 1:1000) {  # Limit to 1000 steps to prevent infinite loops
      # Get valid actions
      valid_actions <- get_valid_actions(game$snake$direction)
      
      # Select best action (no exploration)
      action <- get_best_action(state, q_table, valid_actions)
      
      # Take action
      result <- rl_step(game, action, config_rl)
      game <- result$game
      state <- result$state
      done <- result$done
      
      steps <- steps + 1
      
      # Render game if specified
      if (render && episode <= 5) {  # Only render first 5 episodes
        render_game(game)
        Sys.sleep(0.1)
      }
      
      # Break if done
      if (done) {
        break
      }
    }
    
    # Store metrics
    metrics$scores[episode] <- game$score
    metrics$steps[episode] <- steps
    
    # Print progress
    if (episode %% 10 == 0) {
      cat(sprintf("Evaluation Episode: %d, Score: %d, Steps: %d\n", 
                  episode, game$score, steps))
    }
  }
  
  # Calculate summary statistics
  metrics$avg_score <- mean(metrics$scores)
  metrics$max_score <- max(metrics$scores)
  metrics$avg_steps <- mean(metrics$steps)
  
  return(metrics)
}

# Print evaluation summary
print_evaluation_summary <- function(metrics) {
  cat("Evaluation Summary:\n")
  cat(sprintf("Average Score: %.2f\n", metrics$avg_score))
  cat(sprintf("Maximum Score: %d\n", metrics$max_score))
  cat(sprintf("Average Steps: %.2f\n", metrics$avg_steps))
}