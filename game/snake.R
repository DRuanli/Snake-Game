# Snake implementation

# Create a new snake with given position and length
create_snake <- function(start_position, length = 2, direction = "RIGHT") {
  # Initialize an empty list for the snake
  snake <- list()
  
  # Set the initial direction
  snake$direction <- direction
  
  # Create the snake segments
  snake$segments <- list()
  
  # Add head
  snake$segments[[1]] <- start_position
  
  # Add body segments (they start in the opposite direction of the initial direction)
  dir_vector <- direction_to_vector(direction)
  for (i in 2:length) {
    # Add segments in the opposite direction of movement
    prev_position <- snake$segments[[i-1]]
    new_position <- c(prev_position[1] - dir_vector[1], prev_position[2] - dir_vector[2])
    snake$segments[[i]] <- new_position
  }
  
  return(snake)
}

# Move the snake in its current direction
move_snake <- function(snake, grow = FALSE) {
  # Get the direction vector
  dir_vector <- direction_to_vector(snake$direction)
  
  # Calculate new head position
  old_head <- snake$segments[[1]]
  new_head <- add_positions(old_head, dir_vector)
  
  # Create a new segments list
  new_segments <- c(list(new_head), snake$segments)
  
  # Remove the tail if not growing
  if (!grow) {
    new_segments <- new_segments[-length(new_segments)]
  }
  
  snake$segments <- new_segments
  return(snake)
}

# Change the snake's direction
change_direction <- function(snake, new_direction) {
  # Check if the direction change is valid
  if (is_valid_direction_change(snake$direction, new_direction)) {
    snake$direction <- new_direction
  }
  
  return(snake)
}

# Get the current head position
get_head_position <- function(snake) {
  return(snake$segments[[1]])
}

# Get all the snake's positions
get_all_positions <- function(snake) {
  return(snake$segments)
}

# Check if a position is part of the snake
is_position_in_snake <- function(snake, position) {
  for (segment in snake$segments) {
    if (positions_equal(segment, position)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Check if a position is in the snake's body (not head)
is_position_in_snake_body <- function(snake, position) {
  for (i in 2:length(snake$segments)) {
    if (positions_equal(snake$segments[[i]], position)) {
      return(TRUE)
    }
  }
  return(FALSE)
}