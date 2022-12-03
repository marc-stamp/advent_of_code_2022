
### Day 1 ###

# read data in

local_file = 'local_files/day_1.txt'

elf_calories = readLines(local_file)

# == Part 1 ===

largest_elf <- 0

current_elf <- 0

for (calorie in elf_calories) {

  print(paste0("Current Elf: ", current_elf, " - New Calorie: ", calorie))

  if (calorie == "") {

      if (current_elf >= largest_elf) {

        largest_elf <- current_elf

      }

    current_elf <- 0

    next
  }

  calorie <- as.numeric(calorie)

  current_elf <- current_elf + calorie

}

print(paste0("Elf with largest calories: ", largest_elf))

# == Part 2 ===

top_three_elves <- c(0, 0, 0)

current_elf <- 0

for (calorie in elf_calories) {

  print(paste0("Current Elf: ", current_elf, " - New Calorie: ", calorie))

  if (calorie == "") {

    if (current_elf >= top_three_elves[3]) {

      top_three_elves[3] <- current_elf

      top_three_elves <- sort(top_three_elves, decreasing = T)

      print(paste0("New Top 3 Elves : ", top_three_elves))

    }

    current_elf <- 0

    next
  }

  calorie <- as.numeric(calorie)

  current_elf <- current_elf + calorie

}

print(cat("top 3 elfes with largest calories: ", paste0(top_three_elves, sep = ' - ')))

print(paste0("Sum of top 3 elfes with largest calories: ", sum(top_three_elves)))
