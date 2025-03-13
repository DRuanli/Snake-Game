# Interface between the game and RL system

# Source all required game files
source("utils/constants.R")
source("utils/helpers.R")
source("game/config.R")
source("game/grid.R")
source("game/snake.R")
source("game/food.R")
source("game/collision.R")
source("game/game_environment.R")

# Process a single step with the RL agent
rl_step <- function(game, action, config_rl) {
  # Save old game state for reward calculation
  old_game <- game
  
  # Process the game step with the given action
  game <- process_game_step(game, action)
  
  # Calculate reward
  reward <- calculate_reward(old_game, game, config_rl)
  
  # Encode the new state
  state <- encode_state(game, config_rl)
  
  # Check if episode is done
  done <- (game$status == STATUS$GAME_OVER)
  
  # Return the new state, reward, and done flag
  return(list(
    game = game,
    state = state,
    reward = reward,
    done = done
  ))
}

# Reset the environment for a new episode
rl_reset <- function(config) {
  game <- initialize_game(config)
  state <- encode_state(game, config_rl)
  
  return(list(
    game = game,
    state = state
  ))
}