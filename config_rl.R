# Q-learning configuration parameters
config_rl <- list(
  # Learning parameters
  alpha = 0.1,              # Learning rate
  gamma = 0.9,              # Discount factor
  epsilon_start = 1.0,      # Initial exploration rate
  epsilon_min = 0.1,        # Minimum exploration rate
  epsilon_decay = 0.995,    # Decay rate for epsilon
  
  # Training parameters
  num_episodes = 1000,      # Number of episodes to train
  max_steps_per_episode = 500,  # Maximum steps per episode
  
  # Reward structure
  reward_food = 10,         # Reward for eating food
  reward_collision = -10,   # Penalty for collision
  reward_step = -0.1,       # Small penalty for each step
  
  # State encoding - number of vision cells in each direction
  vision_cells = 1,         # How many cells the snake can "see" in each direction
  
  # Save & load
  save_q_table = TRUE,      # Whether to save the Q-table
  q_table_file = "q_table.rds"  # Filename for saved Q-table
)