# Main entry point script
library(ggplot2)

# Source all necessary files
source("utils/constants.R")
source("utils/helpers.R")
source("game/config.R")
source("game/grid.R")
source("game/snake.R")
source("game/food.R")
source("game/collision.R")
source("game/game_environment.R")
source("visualization/renderer.R")

# Global game state
game <- initialize_game(config)

# Interactive mode - Use console commands to move
play_interactive <- function() {
  # Render initial state
  render_game(game)
  
  # Function to make a move
  make_move <- function(direction) {
    # Validate direction
    valid_dirs <- c("UP", "DOWN", "LEFT", "RIGHT")
    if (!(direction %in% valid_dirs)) {
      cat("Invalid direction! Use: UP, DOWN, LEFT, RIGHT\n")
      return(FALSE)
    }
    
    # Process game step with the given direction
    game <<- process_game_step(game, direction)
    
    # Render the game
    render_game(game)
    
    # Check if game is over
    if (game$status == STATUS$GAME_OVER) {
      cat("Game Over! Final Score:", game$score, "\n")
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  # Export the make_move function to the global environment
  assign("move", make_move, envir = .GlobalEnv)
  
  cat("Game initialized in interactive mode!\n")
  cat("Use move('DIRECTION') to play. Example: move('RIGHT')\n")
  cat("Directions: UP, DOWN, LEFT, RIGHT\n")
}

# Demo mode - Snake moves automatically with simple AI
play_demo <- function(steps = 100, delay = 0.2) {
  # Reset game
  game <<- initialize_game(config)
  
  # Render initial state
  render_game(game)
  
  for (i in 1:steps) {
    if (game$status == STATUS$GAME_OVER) {
      cat("Game Over! Final Score:", game$score, "\n")
      break
    }
    
    # Get current direction
    current_dir <- game$snake$direction
    
    # Calculate next head position
    head_pos <- get_head_position(game$snake)
    dir_vector <- direction_to_vector(current_dir)
    next_pos <- add_positions(head_pos, dir_vector)
    
    # Check if next position would result in collision
    would_collide <- get_cell(game$grid, next_pos) == CELL_TYPES$WALL ||
                     is_position_in_snake_body(game$snake, next_pos)
    
    # If the snake would collide, try to change direction
    if (would_collide) {
      # Try each direction to find a safe one
      possible_dirs <- c("UP", "DOWN", "LEFT", "RIGHT")
      
      # Remove the opposite of current direction
      opposite <- list(
        "UP" = "DOWN",
        "DOWN" = "UP",
        "LEFT" = "RIGHT",
        "RIGHT" = "LEFT"
      )
      possible_dirs <- possible_dirs[possible_dirs != opposite[[current_dir]]]
      
      # Shuffle directions for randomness
      possible_dirs <- sample(possible_dirs)
      
      # Try each possible direction
      found_safe <- FALSE
      for (new_dir in possible_dirs) {
        dir_vector <- direction_to_vector(new_dir)
        next_pos <- add_positions(head_pos, dir_vector)
        
        # Check if this direction is safe
        if (get_cell(game$grid, next_pos) != CELL_TYPES$WALL &&
            !is_position_in_snake_body(game$snake, next_pos)) {
          current_dir <- new_dir
          found_safe <- TRUE
          break
        }
      }
      
      # If no safe direction, continue with current (will likely result in game over)
      if (!found_safe) {
        cat("No safe direction found! Game will likely end.\n")
      }
    } else {
      # If no collision ahead, try to move toward food if it exists
      if (!is.null(game$food_position)) {
        # Calculate direction to food
        food_x_diff <- game$food_position[1] - head_pos[1]
        food_y_diff <- game$food_position[2] - head_pos[2]
        
        new_dir <- current_dir  # Default to current direction
        
        # Try to move toward food
        if (abs(food_x_diff) > abs(food_y_diff)) {
          # Prioritize horizontal movement
          if (food_x_diff > 0 && current_dir != "LEFT") {
            new_dir <- "RIGHT"
          } else if (food_x_diff < 0 && current_dir != "RIGHT") {
            new_dir <- "LEFT"
          } else if (food_y_diff > 0 && current_dir != "UP") {
            new_dir <- "DOWN"
          } else if (food_y_diff < 0 && current_dir != "DOWN") {
            new_dir <- "UP"
          }
        } else {
          # Prioritize vertical movement
          if (food_y_diff > 0 && current_dir != "UP") {
            new_dir <- "DOWN"
          } else if (food_y_diff < 0 && current_dir != "DOWN") {
            new_dir <- "UP"
          } else if (food_x_diff > 0 && current_dir != "LEFT") {
            new_dir <- "RIGHT"
          } else if (food_x_diff < 0 && current_dir != "RIGHT") {
            new_dir <- "LEFT"
          }
        }
        
        # Check if this new direction is valid
        dir_vector <- direction_to_vector(new_dir)
        next_pos <- add_positions(head_pos, dir_vector)
        
        if (get_cell(game$grid, next_pos) == CELL_TYPES$WALL ||
            is_position_in_snake_body(game$snake, next_pos)) {
          # If new direction would cause collision, stick with current direction
          new_dir <- current_dir
        }
        
        current_dir <- new_dir
      }
    }
    
    # Process the game step with the chosen direction
    game <<- process_game_step(game, current_dir)
    
    # Render the game
    render_game(game)
    
    # Pause to view the game
    Sys.sleep(delay)
  }
}

# Random mode - Snake moves completely randomly
play_random <- function(steps = 100, delay = 0.2) {
  # Reset game
  game <<- initialize_game(config)
  
  # Render initial state
  render_game(game)
  
  for (i in 1:steps) {
    if (game$status == STATUS$GAME_OVER) {
      cat("Game Over! Final Score:", game$score, "\n")
      break
    }
    
    # Choose a random direction that's not opposite of current
    current_dir <- game$snake$direction
    opposite <- list(
      "UP" = "DOWN",
      "DOWN" = "UP",
      "LEFT" = "RIGHT",
      "RIGHT" = "LEFT"
    )
    possible_dirs <- c("UP", "DOWN", "LEFT", "RIGHT")
    possible_dirs <- possible_dirs[possible_dirs != opposite[[current_dir]]]
    new_dir <- sample(possible_dirs, 1)
    
    # Process the game step
    game <<- process_game_step(game, new_dir)
    
    # Render the game
    render_game(game)
    
    # Pause to view the game
    Sys.sleep(delay)
  }
}

# Reset the game
reset <- function() {
  game <<- initialize_game(config)
  render_game(game)
  cat("Game reset!\n")
}

# Display menu for selecting game mode
cat("Snake Game - Phase 1\n")
cat("Available functions:\n")
cat("1. play_interactive() - Play using commands\n")
cat("2. play_demo(steps = 100, delay = 0.2) - Watch AI play\n")
cat("3. play_random(steps = 100, delay = 0.2) - Watch random movements\n")
cat("4. reset() - Reset the game\n")
cat("\nIn interactive mode, use move('DIRECTION') to control the snake.\n")