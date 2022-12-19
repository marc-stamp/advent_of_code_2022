
### day 15

### read file in

file <- 'local_files/example_files/day_15_example.txt'

sensor_beacons <- readLines(file)

library(tidyverse)

# ==== Part 1 ====

# find all positions (within beacon range) where beacon can not exist

row_to_check <- 10

# create grid, make larger for now

min_col = -5
max_col = 30
min_row = -2
max_row = 25

beacon_grid <- matrix('', nrow = (max_row - min_row)+1, ncol = (max_col - min_col)+1)
rownames(beacon_grid) <- as.character(min_row:max_row)
colnames(beacon_grid) <- as.character(min_col:max_col)

add_sensor_and_beacon <- function(matrix, sensor_beacon_string) {

  string_split <- strsplit(sensor_beacon_string, ': ')

  sensor <- string_split[[1]][1]
  beacon <- string_split[[1]][2]

  sensor_coords <- strsplit(sensor, ' at ')[[1]][2] %>%
                    paste0("list(",., ')') %>%
                    parse(text = .) %>%
                    eval()

  beacon_coords <- strsplit(beacon, ' at ')[[1]][2] %>%
                    paste0("list(",., ')') %>%
                    parse(text = .) %>%
                    eval()

  matrix[as.character(sensor_coords$y), as.character(sensor_coords$x)] <- 'S'
  matrix[as.character(beacon_coords$y), as.character(beacon_coords$x)] <- 'B'

  return(list(matrix, sensor_coords, beacon_coords))
}

all_sensor_coords <- data.frame()
all_beacon_coords <- data.frame()

for (row in sensor_beacons) {

  beacon_grid_with_coords <- add_sensor_and_beacon(beacon_grid, row)
  beacon_grid <- beacon_grid_with_coords[[1]]

  all_sensor_coords <- rbind(all_sensor_coords, beacon_grid_with_coords[[2]])
  all_beacon_coords <- rbind(all_beacon_coords, beacon_grid_with_coords[[3]])
}

# remove all cols and rows which don't contain anything

add_external_beacon_areas <- function(matrix, coords) {

  found_beacon <- F
  if (matrix[as.character((coords$y)-1), as.character(coords$x)] == 'B') {
    found_beacon <- T
  } else {
    matrix[as.character((coords$y)-1), as.character(coords$x)] <- '#'
  }
  if (matrix[as.character((coords$y)+1), as.character(coords$x)]) {

  } else {
    matrix[as.character((coords$y)+1), as.character(coords$x)] <- '#'
  }
  matrix[as.character(coords$y), as.character((coords$x)-1)] <- '#'
  matrix[as.character(coords$y), as.character((coords$x)+1)] <- '#'

  return(list(matrix, found_beacon))
}

fill_in_non_beacon_areas <- function(matrix, sensor_coords) {

  beacon_not_met_indicator <- T

  most_northern_coord <- sensor_coords
  most_southern_coord <- sensor_coords
  most_western_coord <- sensor_coords
  most_eastern_coord <- sensor_coords

  while(beacon_not_met_indicator) {

    # check most northern section - if b there in future
    if (matrix[as.character((most_northern_coord$y)+1), as.character(most_northern_coord$x)] == 'B' |
        matrix[as.character(most_northern_coord$y), as.character((most_northern_coord$x)+1)] == 'B' |
        matrix[as.character(most_northern_coord$y), as.character((most_northern_coord$x)-1)] == 'B') {
      beacon_not_met_indicator = F
    }

  }

}

fill_in_non_beacon_areas(beacon_grid, all_sensor_coords[7, ])
