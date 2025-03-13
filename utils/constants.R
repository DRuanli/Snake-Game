# Game constants
DIRECTIONS <- list(
  UP = c(0, -1),
  DOWN = c(0, 1),
  LEFT = c(-1, 0),
  RIGHT = c(1, 0)
)

# Game status
STATUS <- list(
  RUNNING = "running",
  GAME_OVER = "game_over"
)

# Cell types
CELL_TYPES <- list(
  EMPTY = 0,
  SNAKE_HEAD = 1,
  SNAKE_BODY = 2,
  FOOD = 3,
  WALL = 4
)