# Load required package
library(data.table)

# Step 1: List all individual station RDS files
files <- list.files("temps_tmp", pattern = "\\.RDS$", full.names = TRUE)

cat("Reading and combining", length(files), "files...\n")

# Step 2: Efficiently read and combine all station data
dt_list <- lapply(files, function(f) {
  dt <- readRDS(f)
  as.data.table(dt)  # convert to data.table for fast binding
})

# Step 3: Combine all into a single big data.table
temperatures_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

cat("✅ Combined temperature data has", nrow(temperatures_dt), "rows.\n")

# Optional: Convert to data.frame if needed for compatibility
temperatures_df <- as.data.frame(temperatures_dt)

# Step 4: Save results to disk
save(temperatures_dt, temperatures_df, file = "temperatures.RData")

cat("✅ Saved to temperatures_fast.RData\n")
