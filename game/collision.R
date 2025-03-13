# Collision detection

# Check if the snake has collided with a wall
check_wall_collision <- function(grid, snake) {
  head_pos <- get_head_position(snake)
  return(get_cell(grid, head_pos) == CELL_TYPES$WALL)
}

# Check if the snake has collided with itself
check_self_collision <- function(snake) {
  head_pos <- get_head_position(snake)
  return(is_position_in_snake_body(snake, head_pos))
}

# Check if the snake has eaten food
check_food_collision <- function(grid, snake) {
  head_pos <- get_head_position(snake)
  return(get_cell(grid, head_pos) == CELL_TYPES$FOOD)
}