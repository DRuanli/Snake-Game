# Main game environment

# Initialize the game environment
initialize_game <- function(config) {
  # Create the game state
  game <- list()
  
  # Initialize the grid
  game$grid <- create_grid(config$grid_width, config$grid_height, config$use_walls)
  
  # Initialize the snake in the middle of the grid
  start_x <- floor(config$grid_width / 2)
  start_y <- floor(config$grid_height / 2)
  game$snake <- create_snake(c(start_x, start_y), config$initial_snake_length, config$initial_direction)
  
  # Initialize the game status
  game$status <- STATUS$RUNNING
  
  # Initialize score
  game$score <- 0
  
  # Generate initial food
  food_position <- generate_food(game$grid, game$snake)
  game$food_position <- food_position
  game$grid <- place_food(game$grid, food_position)
  
  # Update the grid with snake
  game$grid <- update_grid_with_snake(game$grid, game$snake)
  
  return(game)
}

# Update the grid with the snake's current position
update_grid_with_snake <- function(grid, snake) {
  # Save the food position first
  food_pos <- NULL
  for (y in 1:nrow(grid)) {
    for (x in 1:ncol(grid)) {
      if (grid[y, x] == CELL_TYPES$FOOD) {
        food_pos <- c(x, y)
        break
      }
    }
    if (!is.null(food_pos)) break
  }
  
  # Reset the grid (keeping walls)
  grid <- reset_grid(grid)
  
  # Place food back (if it exists)
  if (!is.null(food_pos)) {
    grid <- set_cell(grid, food_pos, CELL_TYPES$FOOD)
  }
  
  # Add the snake body segments
  segments <- snake$segments
  for (i in length(segments):2) {
    grid <- set_cell(grid, segments[[i]], CELL_TYPES$SNAKE_BODY)
  }
  
  # Add the snake head
  grid <- set_cell(grid, segments[[1]], CELL_TYPES$SNAKE_HEAD)
  
  return(grid)
}

# Process a single game step
process_game_step <- function(game, action = NULL) {
  # If game is over, don't process
  if (game$status == STATUS$GAME_OVER) {
    return(game)
  }
  
  # Process action (direction change)
  if (!is.null(action)) {
    game$snake <- change_direction(game$snake, action)
  }
  
  # Calculate where the head will be after moving
  head_pos <- get_head_position(game$snake)
  dir_vector <- direction_to_vector(game$snake$direction)
  new_head_pos <- add_positions(head_pos, dir_vector)
  
  # Check for wall collision
  if (get_cell(game$grid, new_head_pos) == CELL_TYPES$WALL) {
    game$status <- STATUS$GAME_OVER
    return(game)
  }
  
  # Check if the new head position will collide with snake body
  # Skip the tail since it will move away unless we're growing
  for (i in 1:(length(game$snake$segments) - 1)) {
    if (positions_equal(new_head_pos, game$snake$segments[[i]])) {
      game$status <- STATUS$GAME_OVER
      return(game)
    }
  }
  
  # Check if we'll eat food
  grow <- FALSE
  if (!is.null(game$food_position) && positions_equal(new_head_pos, game$food_position)) {
    grow <- TRUE
    game$score <- game$score + 1
    
    # Generate new food
    game$food_position <- generate_food(game$grid, game$snake)
    game$grid <- place_food(game$grid, game$food_position)
  }
  
  # Move the snake
  game$snake <- move_snake(game$snake, grow)
  
  # Update the grid
  game$grid <- update_grid_with_snake(game$grid, game$snake)
  
  return(game)
}

# Reset the game
reset_game <- function(game, config) {
  return(initialize_game(config))
}