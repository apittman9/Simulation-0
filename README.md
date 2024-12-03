# Library Imports
# ---------------
# These libraries are crucial for the simulation and visualization:
# - ggplot2: Powerful graphics library for creating complex visualizations
# - gganimate: Extends ggplot2 to create animated graphics
# - dplyr: Data manipulation library for efficient data processing

library(ggplot2)
library(gganimate)
library(dplyr)

# Confirmation Bias Model Definition
# -----------------------------------
# This Reference Class creates a computational model of belief polarization
# Key components simulate how individuals' beliefs might change with information exposure
ConfirmationBiasModel <- setRefClass(
  "ConfirmationBiasModel",
  
  # Model Fields (Attributes)
  # ------------------------
  # Define the core characteristics of the simulation:
  # - population_size: Total number of individuals in the model
  # - initial_belief_strength: Starting point of belief distribution
  # - information_exposure_rate: How much new information impacts beliefs
  # - beliefs: Array representing individual belief strengths
  fields = list(
    population_size = "numeric",
    initial_belief_strength = "numeric",
    information_exposure_rate = "numeric",
    beliefs = "numeric"
  ),
  
  # Model Methods
  # -------------
  methods = list(
    # Initialization Method
    # --------------------
    # Sets up the initial state of the simulation
    # Parameters:
    # - population_size: Number of individuals
    # - initial_belief_strength: Starting belief intensity
    # - information_exposure_rate: Rate of belief modification
    initialize = function(population_size, initial_belief_strength, information_exposure_rate) {
      # Store input parameters as class fields
      population_size <<- population_size
      initial_belief_strength <<- initial_belief_strength
      information_exposure_rate <<- information_exposure_rate
      
      # Create initial belief array
      # Starts with uniform belief strength across population
      beliefs <<- rep(initial_belief_strength, population_size)
    },
    
    # Simulation Runner with Tracking
    # -------------------------------
    # Runs the simulation and tracks belief polarization over iterations
    # Key aspects:
    # - Introduces randomness to simulate information exposure
    # - Calculates polarization via standard deviation
    # - Tracks changes over time
    run_simulation_with_tracking = function(iterations) {
      # Initialize tracking dataframe to store polarization data
      polarization_tracking <- data.frame(
        iteration = integer(), 
        belief_polarization = numeric(), 
        stringsAsFactors = FALSE
      )
      
      # Iterative simulation
      for (iter in 1:iterations) {
        # Belief Update Mechanism
        # ----------------------
        # Modifies beliefs using:
        # - Random noise (rnorm): Simulates variability in information processing
        # - Information exposure rate: Scales the impact of new information
        beliefs <<- beliefs + rnorm(population_size, 0, 0.05) * information_exposure_rate
        
        # Polarization Calculation
        # -----------------------
        # Measures divergence of beliefs using standard deviation
        # Higher SD indicates more polarized beliefs
        belief_polarization <- sd(beliefs)
        
        # Track Polarization
        # -----------------
        # Accumulate polarization data for each iteration
        polarization_tracking <- rbind(
          polarization_tracking, 
          data.frame(iteration = iter, belief_polarization = belief_polarization)
        )
      }
      
      return(polarization_tracking)
    }
  )
)

# Experiment Runner
# -----------------
# Orchestrates multiple simulation runs to explore different scenarios
run_bias_experiments_with_tracking <- function(
  exposure_rates = c(0.3, 0.5, 0.7, 0.9), 
  replications = 100, 
  iterations = 10
) {
  # Experiment Design
  # ----------------
  # - Tests multiple information exposure rates
  # - Runs multiple replications for each rate
  # - Ensures statistical robustness through repeated trials
  
  results <- lapply(exposure_rates, function(rate) {
    rep_results <- lapply(1:replications, function(x) {
      # Reproducibility Mechanism
      # -----------------------
      # Sets seed to ensure consistent random number generation
      set.seed(x)
      
      # Model Instantiation
      # ------------------
      # Creates a new model for each replication
      model <- ConfirmationBiasModel$new(
        population_size = 1000,
        initial_belief_strength = 0.5,
        information_exposure_rate = rate
      )
      
      # Run Simulation
      # --------------
      # Captures polarization data for this specific model run
      model_output <- model$run_simulation_with_tracking(iterations = iterations)
      
      # Metadata Addition
      # ----------------
      # Adds context to the results for later analysis
      model_output$exposure_rate <- rate
      model_output$replication <- x
      return(model_output)
    })
    
    # Combine Replication Results
    # --------------------------
    # Aggregates results across all replications for a given exposure rate
    do.call(rbind, rep_results)
  })
  
  # Final Results Compilation
  # ------------------------
  # Combines results across all exposure rates
  final_results <- do.call(rbind, results)
  return(as.data.frame(final_results))
}

# Simulation Execution
# -------------------
# Runs the full experiment and generates results
experiment_results_with_tracking <- run_bias_experiments_with_tracking()

# Data Verification
# -----------------
# Checks the structure of the generated results
str(experiment_results_with_tracking)

# Visualization Configuration
# --------------------------
# Creates an animated plot to visualize belief polarization dynamics
p <- ggplot(experiment_results_with_tracking, 
            aes(x = exposure_rate, 
                y = belief_polarization, 
                color = factor(replication))) +
  # Boxplot Layer: Shows distribution of polarization
  geom_boxplot(fill = "blue", alpha = 0.5) +
  
  # Trend Line: Highlights average polarization
  stat_summary(fun = mean, 
               geom = "line", 
               aes(group = 1), 
               color = "red", 
               linewidth = 1) +
  
  # Plot Labeling
  # -------------
  labs(
    title = "Impact of Information Exposure Rate on Belief Polarization",
    subtitle = 'Iteration: {frame_time}',
    x = "Information Exposure Rate",
    y = "Belief Polarization (Standard Deviation)",
    color = "Replication"
  ) +
  
  # Aesthetic Styling
  theme_minimal() +
  
  # Animation Transition
  # -------------------
  # Enables frame-by-frame animation based on iterations
  transition_time(iteration) +
  ease_aes('linear')

# Animation Rendering
# ------------------
# Generates the animated visualization
animate(p, nframes = 100, fps = 10, width = 800, height = 600)

# Animation Saving
# ---------------
# Exports the animation as a gif for sharing/analysis
anim_save("belief_simulation.gif", animation = last_animation())
