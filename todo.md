# To do
Automate DDC R output for historical performance for the current financial year, table export still needs manual entry into Weekly Drilling report. Maybe export photo of tables to directly copy and paste in?

recommended from windsurfer
```
library(ggplot2)
library(gridExtra)

# Load the leaderboard data
leaderboard_for <- read.csv("leaderboard_for.csv")
leaderboard_tld <- read.csv("leaderboard_tld.csv")

# Combine the data into a single data frame
leaderboard <- rbind(leaderboard_for, leaderboard_tld)

# Create a table for each rig ID
tables <- lapply(unique(leaderboard$rig_id), function(rig_id) {
  df <- leaderboard[leaderboard$rig_id == rig_id, ]
  table <- ggplot(df, aes(x = bhid, y = distance)) +
    geom_point() +
    labs(title = paste("Rig ID:", rig_id)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(table)
})

# Arrange the tables in a grid
grid <- grid.arrange(grobs = tables, ncol = 2, main = "Leaderboard")

# Export the grid as an image
ggsave("leaderboard.png", plot = grid, width = 10, height = 6, dpi = 300)
```