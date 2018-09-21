library(MASS)
library(tidyverse)
library(magrittr)
library(glue)
library(patchwork)
# Chinese Restaurant Process Demo

n_init_clusts <- 2
# Stickiness hyperparameter. Larger this is the higher the expected number of clusters under prior
alpha <- 1

# Our sample data has 4 true clusters of 2d normals
Sigma <- matrix(c(0.5,0,0,0.5),2,2)*0.5

sample_data <- list(
  c(-3,3),
  c( 3,3),
  c(-3,-3),
  c(3,-3)
) %>% 
  purrr::map_df(~as_tibble(mvrnorm(n = 50, mu = ., Sigma = Sigma)))
  
ggplot(sample_data, aes(x = V1, y = V2)) +
  geom_point()

data <- sample_data %>%
  mutate(
    id = 1:n(),
    cluster = sample(x = 1:n_init_clusts, size = n(), replace = TRUE)
  )

N <- nrow(data)
prob_new_cluster <- alpha/(N - 1 + alpha)

get_cluster_centers <- . %>% 
  group_by(cluster) %>% 
  summarise(
    mean_V1 = mean(V1),
    mean_V2 = mean(V2),
    size = n()
  )

# Initialize Clusters
# clusters <- data %>% get_cluster_centers()

# For each point, get pairwise distances between point and cluster centers 

for (run in 1:150) {
  print(glue('Running epoch number {run}'))
  
  for (current_id in 1:N) {
    point_v1 <- data[current_id,'V1'] %>% as.numeric()
    point_v2 <- data[current_id,'V2'] %>% as.numeric()
    current_cluster <- data[current_id, 'cluster']
    
    clusters <- data %>% 
      filter(id != current_id) %>%
      get_cluster_centers()
    
    # if the current datum is in its own cluster we need to remove that cluster 
    # compute normalized probabilities of joining existing cluster or deciding on a new one.
    cluster_probs <- clusters %>% 
      mutate(
        sqr_distance = (mean_V1 - point_v1)^2 + (mean_V2 - point_v2)^2,
        scale_factor = size/((N - 1)*alpha),
        prob_in_cluster = scale_factor/sqr_distance,
      ) %>%
      select(cluster, prob_in_cluster) %>% {
        this <- .
        highest_cluster <- max(.$cluster)
        bind_rows(
          this, 
          tibble(cluster = highest_cluster + 1, prob_in_cluster = prob_new_cluster)
        )
      } %>% 
      mutate( normed_probs = prob_in_cluster/sum(prob_in_cluster))
    
    new_cluster <- cluster_probs %$%
      sample( cluster, size = 1, prob = normed_probs)
    
    # Update the data with the new cluster assignment
    data[current_id,'cluster'] <- new_cluster
  }
}


(
  ggplot(data, aes(x = V1, y = V2)) +
    geom_point(aes(color = as.factor(cluster)), alpha = 0.5) +
    guides(color = FALSE)
) /
(
  clusters %>% 
    mutate(cluster = as.character(cluster)) %>% 
    ggplot(aes(x = reorder(cluster, -size), y = size)) +
    geom_point()
)

