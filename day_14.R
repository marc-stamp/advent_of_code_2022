
### day 13

### read file in

file <- 'local_files/day_14.txt'

rock_positions <- readLines(file)

library(tidyverse)
library(reshape2)

# ==== part 1 ====

# look at how many rocks fall

list_position <- function(rock_string) {

  output <- strsplit(rock_string, ' -> ') %>%
    sapply(function(x) { strsplit(x = x, split = ',')}) %>%
    do.call(cbind, .) %>%
    t(.) %>%
    as.data.frame(.) %>%
    rename(x = V1,
           y = V2
    )
  return(output)
}

add_rocks <- function(matrix, first_rock_position, last_rock_position) {

  # if x equals, loop through y
  if (first_rock_position[1] == last_rock_position[1]) {

    matrix[(first_rock_position[1,2]+1):(last_rock_position[1,2]+1), first_rock_position[1,1]] <- '#'

  }
  # if y equals, loop through x
  if (first_rock_position[2] == last_rock_position[2]) {

    matrix[(first_rock_position[1,2]+1),first_rock_position[1,1]:last_rock_position[1,1]] <- '#'

  }

  return(matrix)
}

add_all_rocks_in_section <- function(matrix, rocks_list) {

  for (index in 1:(nrow(rocks_list)-1)) {

    first_rock_position <- rocks_list[index,]
    last_rock_position <- rocks_list[index+1,]

    matrix <- add_rocks(matrix, first_rock_position, last_rock_position)

  }
  return(matrix)
}

# create grid - make much larger for now

sand_rock_grid <- matrix('', nrow = 200, ncol = 800)
colnames(sand_rock_grid) <- 1:ncol(sand_rock_grid)
rownames(sand_rock_grid) <- 1:nrow(sand_rock_grid)

# find max positions of rocks so to cut down
min_x <- 10000
max_x <- 0
min_y <- 10000
max_y <- 0

for (rocks in rock_positions) {

  print(rocks)

  rock_split <- list_position(rocks)
  rock_split$x <- as.numeric(rock_split$x)
  rock_split$y <- as.numeric(rock_split$y)

  sand_rock_grid <- add_all_rocks_in_section(sand_rock_grid, rock_split)

  # check if min/max index of rocks has been found
  rock_split_min_x <- min(rock_split$x)
  rock_split_max_x <- max(rock_split$x)
  rock_split_min_y <- min(rock_split$y)
  rock_split_max_y <- max(rock_split$y)

  if (rock_split_min_x < min_x) {
    min_x <- rock_split_min_x
  }
  if (rock_split_max_x > max_x) {
    max_x <- rock_split_max_x
  }
  if (rock_split_min_y < min_y) {
    min_y <- rock_split_min_y
  }
  if (rock_split_max_y > max_y) {
    max_y <- rock_split_max_y
  }

}

# cut down matrix, so only one row lower and 2 rows either side

sand_rock_grid <- sand_rock_grid[1:(max_y+1), ]

original_sand_position <- c(500,1)

check_sand_movement <- function(matrix, sand_position) {
  if (matrix[sand_position[2]+1, sand_position[1]] == '') {
    return('down')
  } else if (matrix[sand_position[2]+1, sand_position[1]-1] == '') {
    return('down-left')
  } else if  (matrix[sand_position[2]+1, sand_position[1]+1] == '') {
    return('down-right')
  } else {
  return('rest')
  }
}

sand_counter <- 0

while(T) {
  sand_position <- original_sand_position

  while(T) {
    sand_movement <- check_sand_movement(sand_rock_grid, sand_position)
    if (sand_movement == 'down') {
      sand_position[2] <- sand_position[2] + 1
    }
    if (sand_movement == 'down-left') {
      sand_position[1] <- sand_position[1] - 1
      sand_position[2] <- sand_position[2] + 1
    }
    if (sand_movement == 'down-right') {
      sand_position[1] <- sand_position[1] + 1
      sand_position[2] <- sand_position[2] + 1
    }
    if (sand_movement == 'rest') {
      sand_rock_grid[sand_position[2], sand_position[1]] <- 'o'
      sand_counter <- sand_counter + 1
      print(paste0("Current grains of sand ", sand_counter))
      break
    }

    print(paste0("Sand Position: ", paste0(sand_position, collapse = ',')))

    if (sand_position[2] == nrow(sand_rock_grid)) {
      break
    }
  }

  if (sand_position[2] == nrow(sand_rock_grid)) {
    print("Sand has reached bottom of cave")
    break
  }

}

print(paste0("Total grains of sand: ", sand_counter))

# ==== Part 2 ====

# there is actually floor, how many grains of sand get up to 500,1
sand_rock_grid_with_floor <- matrix('', nrow = 200, ncol = 800)
colnames(sand_rock_grid_with_floor) <- 1:ncol(sand_rock_grid_with_floor)
rownames(sand_rock_grid_with_floor) <- 1:nrow(sand_rock_grid_with_floor)

# find max positions of rocks so to cut down
min_x <- 10000
max_x <- 0
min_y <- 10000
max_y <- 0

for (rocks in rock_positions) {

  print(rocks)

  rock_split <- list_position(rocks)
  rock_split$x <- as.numeric(rock_split$x)
  rock_split$y <- as.numeric(rock_split$y)

  sand_rock_grid_with_floor <- add_all_rocks_in_section(sand_rock_grid_with_floor, rock_split)

  # check if min/max index of rocks has been found
  rock_split_min_x <- min(rock_split$x)
  rock_split_max_x <- max(rock_split$x)
  rock_split_min_y <- min(rock_split$y)
  rock_split_max_y <- max(rock_split$y)

  if (rock_split_min_x < min_x) {
    min_x <- rock_split_min_x
  }
  if (rock_split_max_x > max_x) {
    max_x <- rock_split_max_x
  }
  if (rock_split_min_y < min_y) {
    min_y <- rock_split_min_y
  }
  if (rock_split_max_y > max_y) {
    max_y <- rock_split_max_y
  }

}

# make floor
sand_rock_grid_with_floor[(max_y+2+1), ] = '#'

sand_counter <- 0

while(T) {
  sand_position <- original_sand_position

  while(T) {
    sand_movement <- check_sand_movement(sand_rock_grid_with_floor, sand_position)
    if (sand_movement == 'down') {
      sand_position[2] <- sand_position[2] + 1
    }
    if (sand_movement == 'down-left') {
      sand_position[1] <- sand_position[1] - 1
      sand_position[2] <- sand_position[2] + 1
    }
    if (sand_movement == 'down-right') {
      sand_position[1] <- sand_position[1] + 1
      sand_position[2] <- sand_position[2] + 1
    }
    if (sand_movement == 'rest') {
      sand_rock_grid_with_floor[sand_position[2], sand_position[1]] <- 'o'
      sand_counter <- sand_counter + 1
      print(paste0("Current grains of sand ", sand_counter))
      break
    }

    print(paste0("Sand Position: ", paste0(sand_position, collapse = ',')))

  }

  if (all(sand_position == original_sand_position)) {
    print("resting sand reached original position")
    break
  }

}

print(paste0("Total sand grains needed to reach top: ",sand_counter))

