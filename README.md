# Simulation-0
Confirmation Bias Model
# Load necessary libraries
library(ggplot2)
library(gganimate)
library(dplyr)

# Define the ConfirmationBiasModel class
ConfirmationBiasModel <- setRefClass(
  "ConfirmationBiasModel",
  
  fields = list(
    population_size = "numeric",
    initial_belief_strength = "numeric",
    information_exposure_rate = "numeric",
    beliefs = "numeric"
  ),
  
  methods = list(
    # Initialize the model with the given parameters
    initialize = function(population_size, initial_belief_strength, information_exposure_rate) {
      population_size <<- population_size
      initial_belief_strength <<- initial_belief_strength
      information_exposure_rate <<- information_exposure_rate
      beliefs <<- rep(initial_belief_strength, population_size)
    },
    
    # Run the simulation, tracking belief polarization at each iteration
    run_simulation_with_tracking = function(iterations) {
      # Create a data frame to store polarization at each iteration
      polarization_tracking <- data.frame(iteration = integer(), belief_polarization = numeric(), stringsAsFactors = FALSE)
      
      for (iter in 1:iterations) {
        # Update beliefs based on exposure rate and some confirmation bias logic
        beliefs <<- beliefs + rnorm(population_size, 0, 0.05) * information_exposure_rate
        
        # Calculate belief polarization (e.g., standard deviation)
        belief_polarization <- sd(beliefs)
        
        # Track the polarization at this iteration
        polarization_tracking <- rbind(polarization_tracking, data.frame(iteration = iter, belief_polarization = belief_polarization))
      }
      
      return(polarization_tracking)
    }
  )
)

# Simulate the experiment with tracking over iterations
run_bias_experiments_with_tracking <- function(exposure_rates = c(0.3, 0.5, 0.7, 0.9), replications = 100, iterations = 10) {
  results <- lapply(exposure_rates, function(rate) {
    rep_results <- lapply(1:replications, function(x) {
      set.seed(x)  # Ensure reproducibility
      model <- ConfirmationBiasModel$new(
        population_size = 1000,
        initial_belief_strength = 0.5,
        information_exposure_rate = rate
      )
      
      # Run the simulation and track belief polarization at each iteration
      model_output <- model$run_simulation_with_tracking(iterations = iterations)
      
      # Add exposure rate and replication for tracking
      model_output$exposure_rate <- rate
      model_output$replication <- x
      return(model_output)
    })
    
    # Combine results from all replications for this exposure rate
    do.call(rbind, rep_results)
  })
  
  # Combine results from all exposure rates
  final_results <- do.call(rbind, results)
  return(as.data.frame(final_results))
}

# Run the simulation with tracking
experiment_results_with_tracking <- run_bias_experiments_with_tracking()

# Check the structure of the data to ensure it has the right columns
str(experiment_results_with_tracking)

# Create an animated plot: showing belief polarization over time
p <- ggplot(experiment_results_with_tracking, aes(x = exposure_rate, y = belief_polarization, color = factor(replication))) +
  geom_boxplot(fill = "blue", alpha = 0.5) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linewidth = 1) +  # Trend line
  labs(
    title = "Impact of Information Exposure Rate on Belief Polarization",
    subtitle = 'Iteration: {frame_time}',  # Display iteration number in subtitle
    x = "Information Exposure Rate",
    y = "Belief Polarization (Standard Deviation)",
    color = "Replication"
  ) +
  theme_minimal() +
  transition_time(iteration) +  # Transition based on the iteration number
  ease_aes('linear')  # Smooth transitions between frames

# Render the animation
animate(p, nframes = 100, fps = 10, width = 800, height = 600)

# Save the animation as a gif
anim_save("belief_simulation.gif", animation = last_animation())
