# Compatibility notes
# works_with_R("3.1.1", dplyr="0.3.0.2")

load("stations.RData")  # loads 'stations' with columns: Number, Lat, Long, etc.

## Prepare the 5°×5° grid matching CRUTEM data description
long.mid <- seq(-177.5, 177.5, by = 5)
lat.mid  <- seq(-87.5, 87.5, by = 5)

crutem.grid <- expand.grid(
  Long.mid = long.mid,
  Lat.mid  = lat.mid
) %>%
  mutate(
    Long.min = Long.mid - 2.5,
    Long.max = Long.mid + 2.5,
    Lat.min  = Lat.mid  - 2.5,
    Lat.max  = Lat.mid  + 2.5
  )

# Assign a unique square ID
crutem.grid$square <- seq_len(nrow(crutem.grid))

## Associate each station with the grid square it falls into
stations.grid.list <- list()

for (sq in seq_len(nrow(crutem.grid))) {
  G <- crutem.grid[sq, ]
  cat(sprintf("Processing square %4d / %4d\n", sq, nrow(crutem.grid)))
  
  some <- stations %>%
    filter(
      Lat > G$Lat.min, Lat <= G$Lat.max,
      Long > G$Long.min, Long <= G$Long.max
    )
  
  if (nrow(some) > 0) {
    some$square <- sq
    stations.grid.list[[paste0("square_", sq)]] <- some
  }
}

stations.grid <- do.call(rbind, stations.grid.list)
rownames(stations.grid) <- stations.grid$Number

cat("Total stations:", nrow(stations), "\n")
cat("Stations in grid:", nrow(stations.grid), "\n")
cat("Stations not matched:",
    nrow(stations %>% filter(!Number %in% stations.grid$Number)), "\n")

save(crutem.grid, stations.grid, file = "crutem.grid.RData")
