## day 7

### read file in

file <- 'local_files/day_9.txt'

rope_movements <- readLines(file)

library(mefa)

# === Part 1 ===

# View all the instances where tails has visited

# set initial states
head_position <- c(0, 0)

tail_position <- c(0, 0)

# log all tail positions that have been, make into 2 cols
all_tail_positions <- data.frame(row_position = tail_position[1], col_position = tail_position[2])

# have function to check how the tail positions should change
tail_position_change <- function(head_position, tail_position) {

  row_head_difference <- head_position[1] - tail_position[1]
  col_head_difference <- head_position[2] - tail_position[2]

  distance <- sqrt((row_head_difference)^2 + (col_head_difference)^2)

  # if less than 1.5 (~sqrt(2)) - then no movement needed
  if (distance < 1.5) {
    return(tail_position)
  }

  # move in direction based upon sign of difference, if both have differences then this counts as diagonal
  tail_position[1] <- tail_position[1] + sign(row_head_difference)
  tail_position[2] <- tail_position[2] + sign(col_head_difference)

  return(tail_position)

}

movement_reference = list('U' = c(1,0), 'D' = c(-1, 0), 'L' = c(0, -1), 'R' = c(0, 1))

for (movement in rope_movements) {

  movement_split <- strsplit(movement, ' ')[[1]]

  print(movement_split)

  direction <- movement_split[1]

  number <- as.numeric(movement_split[2])

  for (n_move in 1:number) {

    moves_to_make <- movement_reference[[direction]]

    head_position <- head_position + moves_to_make

    tail_position <- tail_position_change(head_position, tail_position)

    print(paste0("Latest Head Position: ", paste0(head_position, collapse = ', ')))
    print(paste0("Latest Tail Position: ", paste0(tail_position, collapse = ', ')))

    # check if this tail position is new
    past_row_positions <- unique(all_tail_positions$row_position)

    if (nrow(all_tail_positions) != nrow(unique(rbind(all_tail_positions, tail_position)))) {

      print(paste0("New Tail position: ", paste0(tail_position, collapse = ', ')))

      all_tail_positions <- rbind(all_tail_positions, tail_position)

    }
  }
}


print(paste0("Total Positions that tail visits: ", nrow(all_tail_positions)))


# === Part 2 ===

# how many location does the end knot visit

total_knots <- 10

initial_position <- c(0,0)

all_knot_positions <- mefa:::rep.data.frame(data.frame(row_position = initial_position[1], col_position = initial_position[2]), total_knots)

all_tail_positions <- data.frame(row_position = initial_position[1], col_position = initial_position[2])

for (movement in rope_movements) {

  movement_split <- strsplit(movement, ' ')[[1]]

  print(movement_split)

  direction <- movement_split[1]

  number <- as.numeric(movement_split[2])

  for (n_move in 1:number) {

    moves_to_make <- movement_reference[[direction]]

    all_knot_positions[1,] <- all_knot_positions[1,] + moves_to_make

    for (knot in 2:total_knots) {

      all_knot_positions[knot,] <- tail_position_change(all_knot_positions[knot-1,], all_knot_positions[knot,])

    }

    head_position <- all_knot_positions[1,]
    tail_position <- all_knot_positions[total_knots,]

    print(paste0("Latest Head Position: ", paste0(head_position, collapse = ', ')))
    print(paste0("Latest Tail Position: ", paste0(tail_position, collapse = ', ')))

    # check if this tail position is new
    if (nrow(all_tail_positions) != nrow(unique(rbind(all_tail_positions, tail_position)))) {

      print(paste0("New Tail position: ", paste0(tail_position, collapse = ', ')))

      all_tail_positions <- rbind(all_tail_positions, tail_position)

    }
  }
}

print(paste0("Total Positions that tail visits: ", nrow(all_tail_positions)))
