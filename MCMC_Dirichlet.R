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



n <- nrow(data)  
num_initial_clusts <- 20
# How many proposal clusters drawn from the prior do we have for each iteration. 
proposal_clusters <- 3
# Concentration parameter
alpha <- 1.2
scalar_mult_denom <- (n-1) + alpha



# Function to draw random cluster centers from our prior. 
draw_clusters <- function(num_clusts, id_start = 0){
  1:num_clusts %>% 
  map_df(~as_tibble(cluster_center_prior())) %>% 
  mutate(id = id_start + (1:n()))
}

# Start by drawing some number of intial clusters from our prior
clusters <- draw_clusters(num_initial_clusts) 

# We start by randomly assigning points to a given starting cluster. 
data_cluster_membership <- data %>% 
  mutate(cluster = sample(1:num_initial_clusts, size = n(), replace = TRUE))


i <- 1


# Then need to decide which cluster to join based on the acceptance algorithm 
# Wrap up in a purrr function to make it fast and clean. 

# take out the point we're looking at 
current_point <- data_cluster_membership[i,c('V1', 'V2')]

# Add the newly drawn clusters to cluster object
new_clusters <- clusters %>% 
  bind_rows(
    draw_clusters(proposal_clusters, id_start = max(.$id))
  )

# Look at data omitting the currently point. 
cluster_likelihoods <- (data_cluster_membership[-1,]) %>% 
  group_by(cluster) %>% 
  summarise(n_in = n()) %>% 
  full_join(new_clusters, by = c('cluster' = 'id')) %>% 
  mutate(
    scaler_mult = ifelse(is.na(n_in), (alpha/proposal_clusters), n_in)/scalar_mult_denom,
    likelihood_from_cluster = map2_dbl(V1, V2, ~dmvnorm(current_point, c(.x,.y))),
    un_normed_prob_of_choosing = scaler_mult * likelihood_from_cluster,
    normed_prob_of_choosing = un_normed_prob_of_choosing / sum(un_normed_prob_of_choosing)
  )

# Plot of cluster likelihoods
cluster_likelihoods %>% 
  ggplot(aes(x = cluster, y = normed_prob_of_choosing)) +
  geom_point()

# cluster
cluster_choice <- sample(
  cluster_likelihoods$cluster, 
  size = 1, 
  prob = cluster_likelihoods$normed_prob_of_choosing )

# Return a simple dataframe with the ID of the point and its cluster assignment. 
tibble(
  id = i, 
  cluster = cluster_choice
)


# Mutate the cluster membership dataframe with this new value. 
data_cluster_membership[i,'cluster'] <- cluster_choice

# First we will initialize a cluster dataframe that contains info on each cluster, its center, and 

# Dataframe that will hold each 

# Start by randomly assigning 