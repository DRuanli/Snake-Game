# Reward function for the RL agent

# Calculate the reward based on the game state and action
calculate_reward <- function(old_game, new_game, config_rl) {
  # Default step penalty
  reward <- config_rl$reward_step
  
  # Check if game is over (collision)
  if (new_game$status == STATUS$GAME_OVER) {
    return(config_rl$reward_collision)
  }
  
  # Check if snake ate food (score increased)
  if (new_game$score > old_game$score) {
    reward <- config_rl$reward_food
  }
  
  return(reward)
}