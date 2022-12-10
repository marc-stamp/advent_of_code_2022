
### Day 2

# get data

file <- 'local_files/day_10.txt'

program <- readLines(file)

# === part 1 ===

# find signal strength at selected values

# initial conditions
X <- 1
cycle <- 1

strength_log <- data.frame(cycle, X)
strength_log$instruction <- ''

# put params of cycle length and addition
noop_length <- 1
noop_addition <- 0

addx_length <- 2
addx_addition <- 0

next_log <- function(strength_log, addition, instruction) {

  latest_log <- data.frame(cycle = strength_log$cycle[nrow(strength_log)] + 1,
                           X = strength_log$X[nrow(strength_log)] + addition,
                           instruction = instruction)

  return(rbind(strength_log, latest_log))
}

for (instruction in program) {

  print(paste0("Next instruction: ", instruction))

  # if noop, add cycle
  if (instruction == 'noop') {

    for (l in 1:noop_length) {

      strength_log <- next_log(strength_log, noop_addition, instruction)

    }

    next
  }

  instruction_split <- strsplit(instruction, ' ')[[1]]

  print(instruction_split)

  if (instruction_split[1] == 'addx') {

    for (l in 1:(addx_length-1)) {

      strength_log <- next_log(strength_log, addx_addition, instruction)

    }

    value <- as.numeric(instruction_split[2])

    print(paste0("Value to add: ", value))

    strength_log <- next_log(strength_log, value, instruction)
  }

  print("Value at end of instruction: ")
  print(strength_log[nrow(strength_log), ])

}

# calculate strength

strength_log$strength <- strength_log$cycle * strength_log$X

# sum selected cycles

selected_cycles <- c(20, 60, 100, 140, 180, 220)

selected_strengths <- strength_log[strength_log$cycle %in% selected_cycles, ]

print(paste0("Total strength of selected cycles: ", sum(selected_strengths$strength)))


# === Part 2 ===

# lite pixel if sprite position is one of positions being drawn

strength_log$lit <- abs(strength_log$X - strength_log$cycle) <= 1

#generate ctr screen, then iterate through to 'light up'
ctr_screen <- matrix('', nrow = 6, ncol = 40)

pixel <- function(X, col_position) {

  if (abs(X - col_position) <= 1) {
    return('#')
  }

  return('')

}

for (n in 2:nrow(strength_log)) {

  row_position <- ceiling((n-1) / ncol(ctr_screen))
  col_position <- (n-1) %% ncol(ctr_screen)
  X <- strength_log$X[n]

  print(paste0("Cycle: ", n , " - row position: ", row_position, " - column position: ", col_position, ' - sprite position: ', X))
  pixel_plotted <- pixel(X, col_position)
  print(paste0("Pixel Plotted: ", pixel_plotted))
  ctr_screen[row_position, col_position] <- pixel_plotted

}

print(ctr_screen)

