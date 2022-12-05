
### day 5

library(tidyverse)

## get data in

local_file_moves = 'local_files/day_5_moves.txt'
local_file_initial_setup = 'local_files/day_5_initial_setup.txt'

moves <- read.table(local_file_moves, header = F, col.names = c('move', 'number', 'from', 'from_stack', 'to', 'to_stack'))

moves <- moves[, c('number', 'from_stack', 'to_stack')]

initial_setup <- read.csv(local_file_initial_setup, header = F)

initial_setup$stacks <- gsub(' \\[|\\]', '', initial_setup$V1)

initial_setup <- separate(initial_setup, 'stacks', into = as.character(1:9), sep = '', remove = F)

split_setup <- data.frame(t(data.frame(sapply(initial_setup$stacks, function(x) {strsplit(x, '')}))))

colnames(split_setup) <- 1:9

split_setup <- remove_rownames(split_setup)

# remove 1 to 9, add extra blank rows above as might be needed

split_setup <- split_setup[1:8, ]

additional_empty_rows <- data.frame(matrix(' ', nrow = 50, ncol = ncol(split_setup)))

colnames(additional_empty_rows) <- 1:9

stacks <- rbind(additional_empty_rows, split_setup)

# === Part 1 ===

latest_stacks <- stacks

for (nmove in 1:nrow(moves)) {

  print(paste0("Number Move: ", nmove))

  move <- moves[nmove,]

  # get columns that have to move
  from_column <- move$from_stack[1]
  to_column <- move$to_stack[1]
  number_of_moves <- move$number[1]

  print(paste0("From column: ", from_column))
  print(paste0("To column: ", to_column))

  # get number of iteration that have to do
  for (number in 1:number_of_moves) {
    print(paste0("number: ", number))

    top_of_from_check <- ''
    first_blank_in_to_check <- ''

    for (row_check in 1:nrow(latest_stacks)) {

      if (top_of_from_check != '' & first_blank_in_to_check != '') {

        break

      }

      if (top_of_from_check == '') {
        if(latest_stacks[row_check, from_column] != ' ') {
          top_of_from_check <- row_check
        }
      }

      if (first_blank_in_to_check == '') {
        if(latest_stacks[row_check, to_column] != ' ') {
          first_blank_in_to_check <- row_check - 1
        }

      }

    }

    if (first_blank_in_to_check == '') {
      first_blank_in_to_check <- nrow(latest_stacks)
    }

    print(paste0("Position to move from in from column: ", nrow(latest_stacks) - top_of_from_check + 1))
    print(paste0("Position to move to in to column: ", nrow(latest_stacks) - first_blank_in_to_check + 1))
    print(paste0("Crate to be moved: ", latest_stacks[top_of_from_check, from_column]))

    latest_stacks[first_blank_in_to_check, to_column] <- latest_stacks[top_of_from_check, from_column]
    latest_stacks[top_of_from_check, from_column] <- ' '

  }

}

# get top characters from columns

top_stacks <- c()

for (column in colnames(latest_stacks)) {
  print(column)
  column_stack <- latest_stacks[, column]
  crates_in_stack <- column_stack[!column_stack %in% c(" ")]
  print(crates_in_stack)

  top_crate <- crates_in_stack[1]
  print(top_crate)

  top_stacks <- append(top_stacks, top_crate)

  print(top_stacks)
}

paste0(top_stacks, collapse = '')

# === Part 2 ===

# retain order - get number going down once the location is hit for from column and rows up in to column

latest_stacks <- stacks

for (nmove in 1:nrow(moves)) {

  print(paste0("Number Move: ", nmove))

  move <- moves[nmove,]

  # get columns that have to move
  from_column <- move$from_stack[1]
  to_column <- move$to_stack[1]
  number_of_moves <- move$number[1]

  print(paste0("From column: ", from_column))
  print(paste0("To column: ", to_column))

  # get moves value, then use this to decide on crates to move
  top_of_from_check <- ''
  first_blank_in_to_check <- ''

  for (row_check in 1:nrow(latest_stacks)) {

    if (top_of_from_check != '' & first_blank_in_to_check != '') {

      break

    }

    if (top_of_from_check == '') {
      if(latest_stacks[row_check, from_column] != ' ') {
        top_of_from_check <- row_check
      }
    }

    if (first_blank_in_to_check == '') {
      if(latest_stacks[row_check, to_column] != ' ') {
        first_blank_in_to_check <- row_check - 1
      }

    }

  }

  if (first_blank_in_to_check == '') {
    first_blank_in_to_check <- nrow(latest_stacks)
  }

  print(paste0("Top Position to move from in from column: ", nrow(latest_stacks) - top_of_from_check + 1))
  print(paste0("Top Position to move to in to column: ", nrow(latest_stacks) - first_blank_in_to_check + 1))
  print(paste0("Number of crates to move: ", number_of_moves))

  print(paste0("Positions to move from: ", paste0(top_of_from_check:(top_of_from_check + number_of_moves - 1), collapse = ', ')))
  print(paste0("Positions to move to: ", paste0((first_blank_in_to_check - number_of_moves + 1):first_blank_in_to_check, collapse = ', ')))

  print("Crates to move: ")
  print(latest_stacks[top_of_from_check:(top_of_from_check + number_of_moves - 1), from_column])

  latest_stacks[(first_blank_in_to_check - number_of_moves + 1):first_blank_in_to_check, to_column] <- latest_stacks[top_of_from_check:(top_of_from_check + number_of_moves - 1), from_column]
  latest_stacks[top_of_from_check:(top_of_from_check + number_of_moves - 1), from_column] <- ' '

}

# get top characters from columns

top_stacks <- c()

for (column in colnames(latest_stacks)) {
  print(column)
  column_stack <- latest_stacks[, column]
  crates_in_stack <- column_stack[!column_stack %in% c(" ")]
  print(crates_in_stack)

  top_crate <- crates_in_stack[1]
  print(top_crate)

  top_stacks <- append(top_stacks, top_crate)

  print(top_stacks)
}

paste0(top_stacks, collapse = '')

