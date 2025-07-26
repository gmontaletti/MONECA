# Test script for segment.membership.dataframe function

# Load necessary libraries
library(devtools)

# Load the package
devtools::load_all()

# Generate synthetic data
mx <- generate_mobility_data(n_classes = 5, immobility_strength = 1.5, seed = 123)

# Run MONECA analysis
seg <- moneca_fast(mx, segment.levels = 3)

# Print segment list structure for debugging
cat("Segment list structure:\n")
str(seg$segment.list)

# Generate membership dataframe
membership_df <- segment.membership.dataframe(seg)

# Display results
cat("\n\nMembership dataframe:\n")
print(membership_df)

# Check structure
cat("\n\nDataframe structure:\n")
str(membership_df)

# Save results for inspection
write.csv(membership_df, "membership_dataframe_output.csv", row.names = FALSE)
cat("\nResults saved to membership_dataframe_output.csv\n")