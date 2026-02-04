library(jsonlite)
library(tidyverse)

process_instagram_data <- function(followers_path, following_path) {
  
  # Read Uploaded Files
  ig_follower <- fromJSON(followers_path)
  ig_data <- fromJSON(following_path)
  
  # Clean Following Data
  following_clean <- ig_data$relationships_following %>%
    unnest_wider(string_list_data) %>%
    select(value, href, timestamp) %>%
    rename(username = value) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))
  
  # Clean Followers Data
  followers_clean <- ig_follower %>%
    unnest_wider(string_list_data) %>%
    select(value, href, timestamp) %>%
    rename(username = value) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"))
  
  # Compute/Analyse Data Cleaned
  foes <- setdiff(following_clean$username, followers_clean$username)
  fans <- setdiff(followers_clean$username, following_clean$username)
  friends <- intersect(followers_clean$username, following_clean$username)
  
  # Results
  list(
    followers_clean = followers_clean,
    following_clean = following_clean,
    friends = friends,
    foes = foes,
    fans = fans
  )
}

# Tibble
create_summary <- function(processed_data) {
  tibble(
    category = c("friends", "foes", "fans"),
    count = c(
      length(processed_data$friends),
      length(processed_data$foes),
      length(processed_data$fans)
    )
  )
}

# Save/Load
save_uploaded_files <- function(followers_file, following_file, data_dir = "data") {
  
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  
  file.copy(
    from = followers_file$datapath,
    to = file.path(data_dir, "followers_1.json"),
    overwrite = TRUE
  )
  
  file.copy(
    from = following_file$datapath,
    to = file.path(data_dir, "following.json"),
    overwrite = TRUE
  )
  
  return(TRUE)
}

load_saved_files <- function(data_dir = "data") {
  
  followers_path <- file.path(data_dir, "followers_1.json")
  following_path <- file.path(data_dir, "following.json")
  
  # Check if both files exist
  if (file.exists(followers_path) && file.exists(following_path)) {
    
    process_instagram_data(followers_path, following_path)
    
  } else {
    return(NULL)
  }
}


