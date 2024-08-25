############################
##### LOADING PACKAGES #####
############################

# Load and manage project environment
library(renv)
renv::activate() # Activate the renv environment for this project
renv::restore() # Restore the renv environment from the lockfile

library(fixest) # Two-way fixed effects models

# Install the 'sf' package manually, as it may not be properly installed using renv.
renv::install("sf")
