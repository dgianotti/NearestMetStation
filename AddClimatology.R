# Get climatology data, add to Master plus met data, and save as new csv file:

# We'll just work with the point data right now
# Load the data:
master_plus_met <- read.csv("Master_Plus_Met.csv")

# For each site in master+met, download the full data record:
for (SITE in master_plus_met$Met_Station_ID) {
  # Determine if GHCN or GSOD:
  
  ## Download all years of data:
    
}


## Now set up a date frame to store this in:
# Copy master_plus_met to clim_data:

# Define met variables:

# Set all met variables to NA:


# For each row, aggregate the climate data and store it in clim_data:


# Rename the met variables so that they have clim in their names:


# Merge master+met and clim together so that it has twice as many met variables:


# Determine met anomalies:
