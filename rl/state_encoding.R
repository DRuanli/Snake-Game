# State encoding functions

# Convert the game state into a state representation for RL
# Returns a string representing the state
encode_state <- function(game, config_rl) {
  snake <- game$snake
  head_pos <- get_head_position(snake)
  food_pos <- game$food_position
  grid <- game$grid
  
  # 1. Danger detection - Check if moving in each direction would cause collision
  # We encode this as a binary number (0 = safe, 1 = danger)
  
  # Get current direction vector
  curr_dir <- snake$direction
  
  # Get relative directions based on current direction
  # For example, if snake is moving RIGHT:
  # forward = RIGHT, left = UP, right = DOWN
  fwd_dir <- curr_dir
  relative_dirs <- list()
  
  # Calculate relative directions based on current direction
  if (curr_dir == "UP") {
    relative_dirs$left <- "LEFT"
    relative_dirs$right <- "RIGHT"
  } else if (curr_dir == "DOWN") {
    relative_dirs$left <- "RIGHT"
    relative_dirs$right <- "LEFT"
  } else if (curr_dir == "LEFT") {
    relative_dirs$left <- "DOWN"
    relative_dirs$right <- "UP"
  } else if (curr_dir == "RIGHT") {
    relative_dirs$left <- "UP"
    relative_dirs$right <- "DOWN"
  }
  
  # Check for danger in each relative direction
  danger <- list()
  
  # Forward danger
  fwd_vector <- direction_to_vector(fwd_dir)
  fwd_pos <- add_positions(head_pos, fwd_vector)
  danger$forward <- is_danger(grid, snake, fwd_pos)
  
  # Left danger
  left_vector <- direction_to_vector(relative_dirs$left)
  left_pos <- add_positions(head_pos, left_vector)
  danger$left <- is_danger(grid, snake, left_pos)
  
  # Right danger
  right_vector <- direction_to_vector(relative_dirs$right)
  right_pos <- add_positions(head_pos, right_vector)
  danger$right <- is_danger(grid, snake, right_pos)
  
  # 2. Food direction - Where is the food relative to the snake head?
  food_dir <- list(
    x = sign(food_pos[1] - head_pos[1]),  # -1, 0, or 1 for left, same column, right
    y = sign(food_pos[2] - head_pos[2])   # -1, 0, or 1 for up, same row, down
  )
  
  # Determine food's relative position based on current direction
  food_relative <- list()
  
  if (curr_dir == "UP") {
    food_relative$left <- food_dir$x < 0
    food_relative$front <- food_dir$y < 0
    food_relative$right <- food_dir$x > 0
  } else if (curr_dir == "DOWN") {
    food_relative$left <- food_dir$x > 0
    food_relative$front <- food_dir$y > 0
    food_relative$right <- food_dir$x < 0
  } else if (curr_dir == "LEFT") {
    food_relative$left <- food_dir$y > 0
    food_relative$front <- food_dir$x < 0
    food_relative$right <- food_dir$y < 0
  } else if (curr_dir == "RIGHT") {
    food_relative$left <- food_dir$y < 0
    food_relative$front <- food_dir$x > 0
    food_relative$right <- food_dir$y > 0
  }
  
  # 3. Combine danger and food information into a single state string
  # Format: "DANGER_LEFT,DANGER_FRONT,DANGER_RIGHT,FOOD_LEFT,FOOD_FRONT,FOOD_RIGHT,CURRENT_DIR"
  state <- paste(
    as.integer(danger$left),
    as.integer(danger$forward),
    as.integer(danger$right),
    as.integer(food_relative$left),
    as.integer(food_relative$front),
    as.integer(food_relative$right),
    curr_dir,
    sep = ","
  )
  
  return(state)
}

# Helper to check if a position is dangerous (wall or snake body)
is_danger <- function(grid, snake, position) {
  # Out of bounds or wall
  if (position[1] < 1 || position[1] > ncol(grid) || 
      position[2] < 1 || position[2] > nrow(grid) ||
      get_cell(grid, position) == CELL_TYPES$WALL) {
    return(TRUE)
  }
  
  # Snake body (not including the tail which will move)
  return(is_position_in_snake_body(snake, position))
}