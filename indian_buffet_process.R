library(tidyverse)
set.seed(42)
# simple algorithm of the indian buffet process

## constants

# Number of customers/data points
N <- 15

# Stickiness hyperparameter. Larger this is the higher the expected number of latent variables
alpha <- 4

# Average number of new dishes selected by each customer. 
lambda <- N/alpha

# First customer enters and decides to sample some number of dishes...
num_to_sample <- function() rpois(1, lambda)
num_dishes <- num_to_sample()

# We give our first customer his plate, aka a vector of length num_dishes with ones.
customer_one_plate <- rep(1, num_dishes)


# start a leger of the plates
plate_history <- tibble(
  plate = 1:num_dishes,
  id = 1L
)

# Next customer comes in and desides to sample first num_dishes plates
# with an even probability. 
customer_number <- 2L
plate_probs <- plate_history %>% 
  group_by(plate) %>% 
  summarise(
    times_chosen =  n(),
    prob_of_selection = times_chosen/2*(customer_number - 1)
  )

new_selection <- rbinom(n = max(plate_history$plate), size = 1, prob = plate_probs$prob_of_selection)
extra_plates <- num_to_sample()
customer_plates <- c(new_selection, rep(1, extra_plates))

plate_history <- tibble(
  plate = 1:length(customer_plates),
  id = customer_number
)

dishes <- tibble(
  dish = 1,
  times_taken = 0
)
