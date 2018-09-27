library(tidyverse)
library(purrr)
library(glue)
library(mvtnorm)
# library(patchwork)

# Implementing Algorithm 8: Aka MH + Gibs + Auxiliary parameters from Neal 2000. 

# Start with our data. 
Sigma <- matrix(c(0.5,0,0,0.5),2,2)*0.5
data <- list(
  c(-3,3),
  c( 3,3),
  c(-3,-3),
  c(3,-3)
) %>% 
  map_df(~as_tibble(rmvnorm(n = 50, mean = ., sigma = Sigma)))

# Plot to see our nice and separated clusters
ggplot(data, aes(x = V1, y = V2)) + geom_point()

# The prior for our cluster locations which is a normal centered at the origin with a large variance. 
cluster_center_prior <- function(){
  rmvnorm(n = 1, mean = c(0,0), sigma = Sigma*5) 
}

  
num_initial_clusts <- 20

# Start by drawing some number of intial clusters from our prior
clusters <- 1:num_initial_clusts %>% 
  map_df(~as_tibble(cluster_center_prior())) %>% 
  mutate(id = 1:n())

# We start by randomly assigning points to a given starting cluster. 
data_cluster_membership <- data %>% 
  mutate(cluster = sample(1:num_initial_clusts, size = n(), replace = TRUE))


i <- 1


# Need to add M more clusters to the clusters list before merging with the data list.
# Then need to decide which cluster to join based on the acceptance algorithm 
# Wrap up in a purrr function to make it fast and clean. 

# take out the point we're looking at 
current_point <- data_cluster_membership[1,c('V1', 'V2')]

# Look at data omitting the currently looked at point. 
(data_cluster_membership[-1,]) %>% 
  group_by(cluster) %>% 
  summarise(n_in = n()) %>% 
  inner_join(clusters, by = c('cluster' = 'id')) %>% 
  mutate(
    likelihood_from_cluster = map2_dbl(V1, V2, ~dmvnorm(current_point, c(.x,.y)))
  )




# First we will initialize a cluster dataframe that contains info on each cluster, its center, and 

# Dataframe that will hold each 

# Start by randomly assigning 