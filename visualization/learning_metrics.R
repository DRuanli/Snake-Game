# Visualization of learning progress
library(ggplot2)

# Plot the learning curve (rewards over episodes)
plot_learning_curve <- function(metrics) {
  # Create a data frame for plotting
  df <- data.frame(
    episode = 1:length(metrics$episode_rewards),
    reward = metrics$episode_rewards,
    steps = metrics$episode_lengths,
    epsilon = metrics$epsilon_values
  )
  
  # Calculate moving averages for smoother curves
  window_size <- min(100, length(metrics$episode_rewards))
  df$reward_ma <- stats::filter(df$reward, rep(1/window_size, window_size), sides = 1)
  df$steps_ma <- stats::filter(df$steps, rep(1/window_size, window_size), sides = 1)
  
  # Replace NA values with original values
  df$reward_ma[is.na(df$reward_ma)] <- df$reward[is.na(df$reward_ma)]
  df$steps_ma[is.na(df$steps_ma)] <- df$steps[is.na(df$steps_ma)]
  
  # Plot rewards
  p1 <- ggplot(df, aes(x = episode, y = reward_ma)) +
    geom_line(color = "blue") +
    geom_smooth(method = "loess", span = 0.1, color = "red", se = FALSE) +
    labs(title = "Learning Curve: Rewards per Episode",
         x = "Episode", y = "Reward (Moving Average)") +
    theme_minimal()
  
  # Plot episode lengths
  p2 <- ggplot(df, aes(x = episode, y = steps_ma)) +
    geom_line(color = "green") +
    geom_smooth(method = "loess", span = 0.1, color = "red", se = FALSE) +
    labs(title = "Learning Curve: Steps per Episode",
         x = "Episode", y = "Steps (Moving Average)") +
    theme_minimal()
  
  # Plot epsilon decay
  p3 <- ggplot(df, aes(x = episode, y = epsilon)) +
    geom_line(color = "purple") +
    labs(title = "Epsilon Decay",
         x = "Episode", y = "Epsilon") +
    theme_minimal()
  
  # Return plots in a list
  return(list(reward_plot = p1, steps_plot = p2, epsilon_plot = p3))
}

# Save plots to files
save_learning_plots <- function(metrics, prefix = "learning_curve") {
  plots <- plot_learning_curve(metrics)
  
  # Save each plot to a file
  ggsave(paste0(prefix, "_rewards.png"), plots$reward_plot, width = 8, height = 6)
  ggsave(paste0(prefix, "_steps.png"), plots$steps_plot, width = 8, height = 6)
  ggsave(paste0(prefix, "_epsilon.png"), plots$epsilon_plot, width = 8, height = 6)
  
  cat("Saved plots to files\n")
}