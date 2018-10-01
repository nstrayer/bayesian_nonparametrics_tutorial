library(tidyverse)
library(purrr)
library(glue)
library(mvtnorm)
library(patchwork)

# Implementing Algorithm 8: Aka MH + Gibs + Auxiliary parameters from Neal 2000. 

# Start with our data. 
Sigma <- matrix(c(0.5,0,0,0.5),2,2)*0.5
data <- list(
  c(-3,3),
  c( 3,3),
  c(-3,-3),
  c(3,-3)
) %>% 
  map_df(~as_tibble(rmvnorm(n = 50, mean = ., sigma = Sigma))) %>% 
  mutate(obs_id = 1:n())

# Plot to see our nice and separated clusters
ggplot(data, aes(x = V1, y = V2)) + geom_point()

# The prior for our cluster locations which is a normal centered at the origin with a large variance. 
cluster_center_prior <- function(){
  rmvnorm(n = 1, mean = c(0,0), sigma = Sigma*5) 
}



n <- nrow(data)  
num_initial_clusts <- 10
# How many proposal clusters drawn from the prior do we have for each iteration. 
proposal_clusters <- 3
# Concentration parameter
alpha <- 1.2
scalar_mult_denom <- (n-1) + alpha



# Function to draw random cluster centers from our prior. 
draw_clusters <- function(num_clusts, id_start = 0){
  1:num_clusts %>% 
  map_df(~as_tibble(cluster_center_prior())) %>% 
  mutate(cluster_id = id_start + (1:n()))
}

# Start by drawing some number of intial clusters from our prior
clusters <- draw_clusters(num_initial_clusts) 

# We start by randomly assigning points to a given starting cluster. 
data_cluster_membership <- data %>% 
  mutate(cluster_id = sample(1:num_initial_clusts, size = n(), replace = TRUE))


draw_cluster_assignment <- function(i, clusters, data_cluster_membership){

  # take out the point we're looking at 
  current_point <- data_cluster_membership[i,c('V1', 'V2')]
  
  # Add the newly drawn clusters to cluster object
  new_clusters <- clusters %>% 
    bind_rows(
      draw_clusters(proposal_clusters, id_start = max(.$cluster_id))
    )
  
  # Look at data omitting the current point. 
  cluster_likelihoods <- (data_cluster_membership[-i,]) %>% 
    group_by(cluster_id) %>% 
    summarise(n_in = n()) %>% 
    full_join(new_clusters, by = 'cluster_id') %>% 
    mutate(
      scaler_mult = ifelse(is.na(n_in), (alpha/proposal_clusters), n_in)/scalar_mult_denom,
      likelihood_from_cluster = map2_dbl(V1, V2, ~dmvnorm(current_point, c(.x,.y))),
      un_normed_prob_of_choosing = scaler_mult * likelihood_from_cluster,
      normed_prob_of_choosing = un_normed_prob_of_choosing / sum(un_normed_prob_of_choosing)
    )

  
  # draw cluster with a weighted draw
  cluster_choice <- sample(
    cluster_likelihoods$cluster_id, 
    size = 1, 
    prob = cluster_likelihoods$normed_prob_of_choosing )
  
  # Return a simple dataframe with the ID of the point and its cluster assignment. 
  tibble(
    obs_id = i, 
    cluster_id = cluster_choice
  )
}


num_iterations <- 25

for(iteration in 1:num_iterations){
  print(glue("Running step {iteration}"))
  # Update the points with new cluster memberships
  data_cluster_membership <- 1:n %>% 
    map_df(draw_cluster_assignment, clusters, data_cluster_membership) %>% 
    full_join(data, by = 'obs_id')
  
  # Update the clusters with their new sizes and locations.
  clusters <- data_cluster_membership %>% 
    group_by(cluster_id) %>% 
    summarise(
      size = n(),
      V1 = mean(V1),
      V2 = mean(V2)
    )
  
  points_plot <- data_cluster_membership  %>% 
    ggplot(aes(x = V1, y = V2)) +
    geom_point(aes(color = as.character(cluster_id))) +
    geom_text(data = clusters, aes(color = as.character(cluster_id), label = cluster_id)) +
    ggtitle(glue('Iteration {iteration}'))
  
  cluster_counts_plot <- ggplot(clusters, aes(x = reorder(as.character(cluster_id), -size), y = size, fill = as.character(cluster_id))) +
    geom_col() +
    scale_y_continuous(expand = c(0,0)) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(x = '')
  
  current_plot <- (points_plot + cluster_counts_plot + plot_layout(ncol = 2, widths = c(3, 1)))*guides(fill = FALSE, color = FALSE)
  
  ggsave(glue('animation/step{iteration}.png'), current_plot)
}

1:num_iterations %>% 
  sprintf('animation/step%i.png', .) %>% 
  gifski::gifski(gif_file = 'animated_mcmc.gif')
