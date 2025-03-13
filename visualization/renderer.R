# Visualization using ggplot2
library(ggplot2)
library(grid)

# Convert game grid to a data frame for plotting
grid_to_df <- function(game) {
  width <- ncol(game$grid)
  height <- nrow(game$grid)
  
  # Initialize an empty data frame
  df <- data.frame(
    x = integer(0),
    y = integer(0),
    cell_type = integer(0)
  )
  
  # Fill the data frame
  for (y in 1:height) {
    for (x in 1:width) {
      df <- rbind(df, data.frame(
        x = x,
        y = y,
        cell_type = game$grid[y, x]
      ))
    }
  }
  
  return(df)
}

# Render the game state using ggplot2
render_game <- function(game) {
  # Convert grid to data frame
  grid_df <- grid_to_df(game)
  
  # Define colors for cell types
  cell_colors <- c(
    "0" = "white",      # EMPTY
    "1" = "darkgreen",  # SNAKE_HEAD
    "2" = "green",      # SNAKE_BODY
    "3" = "red",        # FOOD
    "4" = "black"       # WALL
  )
  
  # Create the plot
  p <- ggplot(grid_df, aes(x = x, y = y, fill = as.factor(cell_type))) +
    geom_tile(color = "lightgray", size = 0.5) +
    scale_fill_manual(values = cell_colors) +
    scale_y_reverse() +  # Flip y-axis to match traditional grid coordinates
    theme_void() +
    theme(legend.position = "none") +
    coord_equal() +
    ggtitle(paste("Score:", game$score, "- Status:", game$status))
  
  # Print the plot
  print(p)
}