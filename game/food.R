# Food generation logic

# Generate a new food position that's not on the snake or walls
generate_food <- function(grid, snake) {
  width <- ncol(grid)
  height <- nrow(grid)
  
  # Get all valid positions (not wall, not snake)
  valid_positions <- list()
  pos_idx <- 1
  
  for (y in 1:height) {
    for (x in 1:width) {
      position <- c(x, y)
      if (get_cell(grid, position) != CELL_TYPES$WALL && 
          !is_position_in_snake(snake, position)) {
        valid_positions[[pos_idx]] <- position
        pos_idx <- pos_idx + 1
      }
    }
  }
  
  # Randomly select one of the valid positions
  if (length(valid_positions) > 0) {
    return(valid_positions[[sample(1:length(valid_positions), 1)]])
  } else {
    # If no valid positions, return a default (shouldn't happen unless snake fills the grid)
    return(c(2, 2))  # Return a position that's likely valid
  }
}

# Place food on the grid
place_food <- function(grid, food_position) {
  if (!is.null(food_position)) {
    grid <- set_cell(grid, food_position, CELL_TYPES$FOOD)
  }
  return(grid)
}