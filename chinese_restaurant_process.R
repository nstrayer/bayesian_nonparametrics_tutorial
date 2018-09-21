library(MASS)
library(tidyverse)
# Chinese Restaurant Process Demo

n_init_clusts <- 4

# Our sample data has 4 true clusters of 2d normals
Sigma <- matrix(c(1,0,0,1),2,2)

sample_data <- list(
  c(-3,3),
  c( 3,3),
  c(-2,-2),
  c(0,0)
) %>% 
  purrr::map_df(~as_tibble(mvrnorm(n = 50, mu = ., Sigma = Sigma))) %>% 
  mutate(id = 1:n()) %>% 
  mutate(cluster = sample(x = 1:n_init_clusts, size = n(), replace = TRUE))

get_cluster_centers <- . %>% 
  group_by(cluster) %>% 
  summarise(
    mean_V1 = mean(V1),
    mean_v2 = mean(V2)
  )
  

ggplot(sample_data, aes(x = V1, y = V2)) +
  geom_point(aes(color = cluster))

# Initialize Clusters
clusters <- sample_data %>% get_cluster_centers()

# Randomly assign points to clusters

# For each point, get pairwise distances between point and cluster centers
current_id <- 1

  # Calculate probabilities of joining each cluster or making a new one

  # Draw and record which cluster was joined

  # Repeat for each datapoint. 
