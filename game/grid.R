# Grid implementation

# Create a new grid with given dimensions
create_grid <- function(width, height, use_walls = TRUE) {
  # Create an empty grid filled with zeros (empty cells)
  grid <- matrix(CELL_TYPES$EMPTY, nrow = height, ncol = width)
  
  # Add walls if needed
  if (use_walls) {
    # Top and bottom walls
    grid[1, ] <- CELL_TYPES$WALL
    grid[height, ] <- CELL_TYPES$WALL
    
    # Left and right walls
    grid[, 1] <- CELL_TYPES$WALL
    grid[, width] <- CELL_TYPES$WALL
  }
  
  return(grid)
}

# Get the cell type at a specific position
get_cell <- function(grid, position) {
  # Check if position is within grid bounds
  if (position[1] < 1 || position[1] > ncol(grid) || 
      position[2] < 1 || position[2] > nrow(grid)) {
    return(CELL_TYPES$WALL)  # Consider out-of-bounds as walls
  }
  
  return(grid[position[2], position[1]])
}

# Set the cell type at a specific position
set_cell <- function(grid, position, cell_type) {
  # Check if position is within grid bounds
  if (position[1] < 1 || position[1] > ncol(grid) || 
      position[2] < 1 || position[2] > nrow(grid)) {
    return(grid)  # Can't set out-of-bounds cells
  }
  
  grid[position[2], position[1]] <- cell_type
  return(grid)
}

# Reset the grid to empty (keeping walls)
reset_grid <- function(grid) {
  # Keep walls, set everything else to empty
  grid[grid != CELL_TYPES$WALL] <- CELL_TYPES$EMPTY
  return(grid)
}