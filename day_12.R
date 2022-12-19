
### day 12

### read file in

file <- 'local_files/day_12.txt'

elevations <- readLines(file)

# ==== Part 1 ====

# Look at minimum number of steps to get up to part E

split_elevations <- sapply(elevations, function(x) {strsplit(x, '')[[1]]})

elevations_matrix <- t(as.matrix(split_elevations))
rownames(elevations_matrix) <- NULL

# find position which equals S
start_position <- which(elevations_matrix == 'S', T)

# find position which equals E
end_position <- which(elevations_matrix == 'E', T)

# change letters, with S as 0 and E higher than z

elevations_number_matrix <- elevations_matrix

for (letter in letters) {

  elevations_number_matrix[elevations_number_matrix == letter] <- as.numeric(match(letter, letters))

}

elevations_number_matrix[start_position[1], start_position[2]] <- -1
elevations_number_matrix[end_position[1], end_position[2]] <- 26

storage.mode(elevations_number_matrix) <- "numeric"

