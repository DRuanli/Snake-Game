# Helper functions

# Check if two positions are equal
positions_equal <- function(pos1, pos2) {
  return(pos1[1] == pos2[1] && pos1[2] == pos2[2])
}

# Add two position vectors
add_positions <- function(pos1, pos2) {
  return(c(pos1[1] + pos2[1], pos1[2] + pos2[2]))
}

# Get a random position within grid bounds
get_random_position <- function(width, height) {
  x <- sample(2:(width-1), 1)  # Avoid walls
  y <- sample(2:(height-1), 1) # Avoid walls
  return(c(x, y))
}

# Convert direction string to vector
direction_to_vector <- function(direction) {
  return(DIRECTIONS[[direction]])
}

# Check if direction change is valid (can't go directly backwards)
is_valid_direction_change <- function(current_dir, new_dir) {
  curr_vec <- direction_to_vector(current_dir)
  new_vec <- direction_to_vector(new_dir)
  
  # If the sum of the vectors is zero, it means we're trying to go in the opposite direction
  return(!all(curr_vec + new_vec == c(0, 0)))
}