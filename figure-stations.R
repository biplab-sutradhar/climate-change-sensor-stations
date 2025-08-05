library(ggplot2)
library(animint2)
library(dplyr)
library(data.table)

load("crutem.grid.RData")
load("temperatures.RData")

stations.grid$Number <- as.character(stations.grid$Number)
temperatures <- temperatures_df
temperatures$station <- as.character(temperatures$station)

Lat.min  <- min(stations.grid$Lat,  na.rm = TRUE)
Lat.max  <- max(stations.grid$Lat,  na.rm = TRUE)
Long.min <- min(stations.grid$Long, na.rm = TRUE)
Long.max <- max(stations.grid$Long, na.rm = TRUE)

station.disp <- stations.grid %>%
  inner_join(crutem.grid, by = "square") %>%
  arrange(StartYear, EndYear) %>%
  mutate(
    station = factor(Number, Number),
    name = sub("-+$", "", Name),
    elevation.meters = ifelse(Height == -999, NA, Height),
    Lat.norm = Lat - Lat.min,
    Long.norm = Long - Long.min
  )

station.counts <- station.disp %>%
  group_by(square) %>%
  summarise(stations = n(), .groups = "drop")

grid.counts <- inner_join(station.counts, crutem.grid, by = "square")

temperatures$square <- stations.grid[
  match(as.character(temperatures$station), stations.grid$Number),
  "square"
]
temperatures$square <- as.character(temperatures$square)

temp.extra <- temperatures %>%
  filter(celsius < 50) %>%
  group_by(square, station) %>%
  mutate(
    year.num = as.numeric(strftime(date, "%Y")),
    month.num = as.numeric(strftime(date, "%m")),
    date.num = year.num + month.num / 12,
    date.diff = c(0, 1/12 - diff(date.num)),
    diff.thresh = ifelse(abs(date.diff) < 1e-5, 0, date.diff),
    line.id = cumsum(diff.thresh != 0)
  ) %>%
  group_by(square, station, line.id) %>%
  mutate(line.data = n()) %>%
  ungroup()

temp.lines  <- temp.extra %>% filter(line.data > 1)
temp.points <- temp.extra %>% filter(line.data == 1)

available.squares <- unique(temp.extra$square)
if (length(available.squares) == 0) stop("No data after filtering.")

first.square <- available.squares[1]
available.stations <- unique(temp.extra$station[temp.extra$square == first.square])
if (length(available.stations) == 0) stop(paste("No stations for square", first.square))

first.station <- available.stations[1]

viz <- list(
  title = "CRUTEM4 Temperature Sensor Stations",
  source = 'https://github.com/biplab-sutradhar/climate-change-sensor-stations/blob/main/figure-stations.R',
  stationMap = ggplot() +
    ggtitle("Map of all stations – click a square to zoom") +
    theme_animint(width = 720) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    geom_point(
      aes(-Long, Lat, fill = elevation.meters),
      data = station.disp,
      pch = 21,
      clickSelects = "station"
    ) +
    geom_rect(
      aes(xmin = -Long.max, xmax = -Long.min,
          ymin = Lat.min, ymax = Lat.max),
      data = grid.counts,
      alpha = 0.5,
      clickSelects = "square"
    ) +
    guides(fill = "none") +
    scale_fill_gradient2(),

  squareMap = ggplot() +
  ggtitle("Zoomed square – click station for time series") +
  theme_bw() +
  theme(
    axis.line       = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    axis.title      = element_blank(),
    panel.border    = element_blank(),
    panel.grid.major= element_blank(),
    panel.grid.minor= element_blank()
  ) +
  geom_text(
    aes(x = -2.5, y = 5,
        label = paste0(stations, " station", ifelse(stations == 1, "", "s"))),
    data = station.counts,
    showSelected = "square"
  ) +
  geom_point(
    aes(x = -Long.norm, y = Lat.norm,
        fill = elevation.meters,
        tooltip = paste0(name, " ", Country, " Elev=", elevation.meters)),
    data = station.disp,
    pch          = 21,
    size         = 4,
    showSelected = "square",
    clickSelects = "station"
  ) +
  geom_text(aes(-Long.norm, Lat.norm,
          label = paste(name, Country)),
          data = station.disp,
          showSelected = c("square", "station"),
          clickSelects = "station")
   +
  scale_fill_gradient2(),

  timeSeries = ggplot() +
  ggtitle("Temperature time series for selected square") +
  theme_bw() +
  theme_animint(width = 1245, height = 600) +  
  geom_line(
    aes(x = date, y = celsius, group = interaction(station, line.id)),
    data = temp.lines,
    color = "red",       
    color_off = "gray30",
    alpha = 0.2,         
    alpha_off = 0.1,    
    size = 1,            
    showSelected = "square",
    clickSelects = "station",
    chunk_vars = c("square")
  ),

  selector.types = list(station = "multiple"),

  first = list(
    square = first.square,
    station = first.station
  ),

  options = list(
    title = "CRUTEM4 Temperature Sensor Stations"
  )
)


animint2dir(viz, out.dir = "figurestations")
