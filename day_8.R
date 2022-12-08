
### Day 8

### read file in

file <- 'local_files/day_8.txt'

trees <- readLines(file)

split_trees <- sapply(trees, function(x) {as.numeric(strsplit(x, '')[[1]])})

split_trees_matrix <- as.matrix(split_trees)

colnames(split_trees_matrix) <- NULL

row_size <- nrow(split_trees_matrix)
col_size <- ncol(split_trees_matrix)

# === Part 1 ===

# find all trees

total_visible_trees <- 0

for (row in 1:nrow(split_trees_matrix)) {
  for (col in 1:nrow(split_trees_matrix)) {

    if ((col == 1) | (row == 1)) {

      print(paste("Position 1 at position:", row, col))

      total_visible_trees <- total_visible_trees + 1

      next
    }

    if ((col == col_size) | (row == row_size)) {

      print(paste("End of trees at position:", row, col))

      total_visible_trees <- total_visible_trees + 1

      next
    }

    value_in_position <- split_trees_matrix[row, col]

    # check that all values up, down, left and right are lower

    # up
    up_check <- 0
    for (up_row in 1:(row-1)) {

      up_check <- up_check + 1*(split_trees_matrix[up_row, col] >= value_in_position)

    }

    if (up_check == 0) {

      print(paste0("Position: ", row, col, " has all trees above it that are lower."))

      total_visible_trees <- total_visible_trees + 1

      next

    }

    # down
    down_check <- 0
    for (down_row in (row+1):row_size) {

      down_check <- down_check + 1*(split_trees_matrix[down_row, col] >= value_in_position)

      if (down_check > 0) {
        break
      }

    }

    if (down_check == 0) {

      print(paste0("Position: ", row, col, " has all trees below it that are lower."))

      total_visible_trees <- total_visible_trees + 1

      next

    }

    # left
    left_check <- 0
    for (left_col in 1:(col-1)) {

      left_check <- left_check + 1*(split_trees_matrix[row, left_col] >= value_in_position)

      if (left_check > 0) {
        break
      }

    }

    if (left_check == 0) {

      print(paste0("Position: ", row, col, " has all trees left of it that are lower."))

      total_visible_trees <- total_visible_trees + 1

      next

    }

    # right
    right_check <- 0
    for (right_col in (col+1):col_size) {

      right_check <- right_check + 1*(split_trees_matrix[row, right_col] >= value_in_position)

      if (right_check > 0) {
        break
      }

    }

    if (right_check == 0) {

      print(paste0("Position: ", row, col, " has all trees left of it that are lower."))

      total_visible_trees <- total_visible_trees + 1

      next

    }

    print(paste("Position", row, ",", col, "-", value_in_position, "not visible."))
  }
}

print(paste0("Total Visible Trees: ", total_visible_trees))

# ==== Part 2 ====

# find max scenic score
max_scenic_score <- 0

for (row in 1:nrow(split_trees_matrix)) {
  for (col in 1:nrow(split_trees_matrix)) {

    # assumption - if on edge then multiplies to zero, so won't be selected
    if ((col %in% c(1, col_size)) | (row %in% c(1, row_size))) {

      print(paste("Position:", row, col, "won't be selected as on edge."))

      total_visible_trees <- total_visible_trees + 1

      next
    }

    # check number of trees that can be seen in each position
    value_in_position <- split_trees_matrix[row, col]

    # up
    up_tree_count <- 0
    for (up_row in (row-1):1) {

      up_tree_count <- up_tree_count + 1
      if (split_trees_matrix[up_row, col] >= value_in_position) {
        break
      }
    }

    # down
    down_tree_count <- 0
    for (down_row in (row+1):row_size) {

      down_tree_count <- down_tree_count + 1
      if (split_trees_matrix[down_row, col] >= value_in_position) {
        break
      }
    }

    # left
    left_tree_count <- 0
    for (left_col in (col-1):1) {

      left_tree_count <- left_tree_count + 1
      if (split_trees_matrix[row, left_col] >= value_in_position) {
        break
      }

    }

    # right
    right_tree_count <- 0
    for (right_col in (col+1):col_size) {

        right_tree_count <- right_tree_count + 1
        if (split_trees_matrix[row, right_col] >= value_in_position) {
          break
        }

    }

    position_score <- up_tree_count * down_tree_count * left_tree_count * right_tree_count
    print(paste("Position", row, ",", col, "had scores of:", up_tree_count, down_tree_count, left_tree_count, right_tree_count, "with total score of:", position_score))

    if (position_score > max_scenic_score) {

      print(paste("Position", row, ",", col, "has achieved a greater score than", max_scenic_score))

      max_scenic_score <- position_score

    }
  }
}

print(paste0("Max Scenic Score: ", max_scenic_score))
