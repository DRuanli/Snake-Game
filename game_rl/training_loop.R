# Training loop for the RL agent

# Train the RL agent
train_agent <- function(config, config_rl, render = TRUE, render_interval = 100) {
  # Initialize or load Q-table
  q_table <- initialize_q_table()
  if (config_rl$save_q_table && file.exists(config_rl$q_table_file)) {
    q_table <- load_q_table(config_rl$q_table_file)
    cat("Loaded existing Q-table\n")
  }
  
  # Initialize epsilon for exploration
  epsilon <- config_rl$epsilon_start
  
  # Initialize metrics
  metrics <- list(
    episode_rewards = numeric(config_rl$num_episodes),
    episode_lengths = numeric(config_rl$num_episodes),
    epsilon_values = numeric(config_rl$num_episodes)
  )
  
  # Main training loop
  for (episode in 1:config_rl$num_episodes) {
    # Reset environment
    env <- rl_reset(config)
    game <- env$game
    state <- env$state
    
    total_reward <- 0
    steps <- 0
    
    # Episode loop
    for (step in 1:config_rl$max_steps_per_episode) {
      # Get valid actions based on current direction
      valid_actions <- get_valid_actions(game$snake$direction)
      
      # Select action using epsilon-greedy
      action <- select_action(state, q_table, epsilon, valid_actions)
      
      # Take action and get new state and reward
      result <- rl_step(game, action, config_rl)
      new_state <- result$state
      reward <- result$reward
      done <- result$done
      game <- result$game
      
      # Update total reward
      total_reward <- total_reward + reward
      steps <- steps + 1
      
      # Update Q-table
      q_table <- q_learning_update(state, action, reward, new_state, q_table, config_rl)
      
      # Move to next state
      state <- new_state
      
      # Render game if specified
      if (render && episode %% render_interval == 0) {
        render_game(game)
        Sys.sleep(0.1)  # Slow down to make it visible
      }
      
      # Break if done
      if (done) {
        break
      }
    }
    
    # Update epsilon
    epsilon <- update_epsilon(epsilon, config_rl)
    
    # Store metrics
    metrics$episode_rewards[episode] <- total_reward
    metrics$episode_lengths[episode] <- steps
    metrics$epsilon_values[episode] <- epsilon
    
    # Print progress
    if (episode %% 10 == 0) {
      cat(sprintf("Episode: %d, Score: %d, Steps: %d, Epsilon: %.2f\n", 
                  episode, game$score, steps, epsilon))
    }
    
    # Save Q-table periodically
    if (config_rl$save_q_table && episode %% 100 == 0) {
      save_q_table(q_table, config_rl$q_table_file)
      cat("Saved Q-table\n")
    }
  }
  
  # Save final Q-table
  if (config_rl$save_q_table) {
    save_q_table(q_table, config_rl$q_table_file)
    cat("Saved final Q-table\n")
  }
  
  return(list(
    q_table = q_table,
    metrics = metrics
  ))
}