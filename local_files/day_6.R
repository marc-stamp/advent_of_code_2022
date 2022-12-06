
### Day 6

# get data

file <- 'local_files/day_6.txt'

signal <- readLines(file)

# === Part 1 ===

# Find 4 characters in a row which are unique

# split into vector of characters - then loop through
# take n to n - 3, make unique and see if same size
# if same size - break with value

signal_split <- strsplit(signal, '')[[1]]

total_unique_characters <- 4

for (position in total_unique_characters: length(signal_split)) {
  print(position)
  last_signals <- signal_split[(position - total_unique_characters + 1):position]
  print(last_signals)

  unique_last_signals <- unique(last_signals)

  if (length(unique_last_signals) == total_unique_characters) {

    print(paste0("First unique set of characters found at ", position, ": ", paste0(unique_last_signals, collapse = ', ')))
    break
  }

}

position
