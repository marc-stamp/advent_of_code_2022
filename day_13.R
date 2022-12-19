
### day 13

### read file in

file <- 'local_files/day_13.txt'

signal <- readLines(file)

library(jsonlite)
library(rrapply)
library(data.table)
library(tidyverse)

left_signals_indexes <- seq(from = 1, to = length(signal), by = 3)
right_signals_indexes <- seq(from = 2, to = length(signal), by = 3)

left_signals <- signal[left_signals_indexes]
right_signals <- signal[right_signals_indexes]

# ==== Part 1 ====

make_into_list <- function(line){
  out <- line %>%
    str_replace_all("\\[","list\\(") %>%
    str_replace_all("\\]","\\)") %>%
    parse(text = .) %>%
    eval()
}

check_signal <- function(left, right) {

  # if both have no items - then they're both removed and need to go back up to level above
  if(length(left) == 0 && length(right) == 0) {
    return(NA)
  }

  # if no left items left, then correct
  if (length(left) == 0) {
    return(T)
  }

  # if no right items left, then incorrect
  if (length(right) == 0) {
    return(F)
  }

  if (is.list(left[[1]]) && is.list(right[[1]])) {

    output <- check_signal(left[[1]], right[[1]])

    if (is.na(output)) {

      # if no output, then who list mathced, so now check next full list
      left[[1]] <- NULL
      right[[1]] <- NULL

      return(check_signal(left, right))

    } else {
      return(output)
    }
  }

  if (is.numeric(left[[1]]) & is.numeric(right[[1]])) {

    if (length(left[[1]]) == 0) {
      return(T)
    }
    if (length(right[[1]]) == 0) {
      return(F)
    }

    if (left[[1]] < right[[1]]) {
      return(T)
    }
    if (left[[1]] > right[[1]]) {
      return(F)
    }

    # null will remove values, so to check next index along
    left[[1]] <- NULL
    right[[1]] <- NULL
    return(check_signal(left,right))
  }

  # if one is numeric and other list - make as list then apply again
  if (is.numeric(left[[1]]) && is.list(right[[1]])){
    left[[1]] <- list(left[[1]])
    return(check_signal(left,right))
  }
  if (is.list(left[[1]]) && is.numeric(right[[1]])){
    right[[1]] <- list(right[[1]])
    return(check_signal(left,right))
  }
}

correct_signal_indexes <- c()

for (signal_index in 1:length(left_signals)) {

  print(signal_index)

  left_string <- left_signals[signal_index]

  right_string <- right_signals[signal_index]

  # make into list format
  left <- make_into_list(left_string)
  right <- make_into_list(right_string)

  if (check_signal(left,right)) {
    correct_signal_indexes <- append(correct_signal_indexes, signal_index)
  }

}

print(paste0("Total sum of indexes in right order: ", sum(correct_signal_indexes)))

# === Part 2 ===

# put all packets in right order - then determine where the next packets would fit in

divider_packet_1 <- list(2)
divider_packet_2 <- list(6)

divider_1_position <- 1
divider_2_position <- 2

for (signal_string in signal) {

  if (signal_string == '') {
    next
  }

  signal_list <- make_into_list(signal_string)

  if (check_signal(signal_list, divider_packet_1)) {
    divider_1_position <- divider_1_position + 1
  }
  if (check_signal(signal_list, divider_packet_2)) {
    divider_2_position <- divider_2_position + 1
  }

}

print(paste0("Combined index position: ", divider_1_position*divider_2_position))

