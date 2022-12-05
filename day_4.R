
### day 4

# read data in

local_file = 'local_files/day_4.txt'

elf_tasks = readLines(local_file)

# === Part 1 ===

## iterate through each set of tasks, order numbers, check if min is lower and max is higher of other
## add to running total to check overlaps

complete_overlaps <- 0

for (pairs in elf_tasks) {

  sections <- strsplit(pairs, ',')[[1]]

  first_elf_sections <- sort(as.numeric(strsplit(sections[1], '-')[[1]]))
  second_elf_sections <- sort(as.numeric(strsplit(sections[2], '-')[[1]]))

  print(paste0("First Elf Sections: ", paste0(first_elf_sections, collapse = ',')))
  print(paste0("Second Elf Sections: ", paste0(second_elf_sections, collapse = ', ')))

  if ((first_elf_sections[1] <= second_elf_sections[1]) & (first_elf_sections[2] >= second_elf_sections[2])){
    print("Second sections are inside first section")
    complete_overlaps <- complete_overlaps + 1
    next
  }

  if ((first_elf_sections[1] >= second_elf_sections[1]) & (first_elf_sections[2] <= second_elf_sections[2])){
    print("First sections are inside second section")
    complete_overlaps <- complete_overlaps + 1
  }

}

print(paste0("Complete Overlaps: ", complete_overlaps))

# === Part 2 ===

partial_overlaps <- 0

for (pairs in elf_tasks) {

  sections <- strsplit(pairs, ',')[[1]]

  first_elf_sections <- sort(as.numeric(strsplit(sections[1], '-')[[1]]))
  second_elf_sections <- sort(as.numeric(strsplit(sections[2], '-')[[1]]))

  print(paste0("First Elf Sections: ", paste0(first_elf_sections, collapse = ',')))
  print(paste0("Second Elf Sections: ", paste0(second_elf_sections, collapse = ', ')))

  if ((first_elf_sections[2] >= second_elf_sections[1]) & (first_elf_sections[1] <= second_elf_sections[2])){
    print("Sections overlap")
    partial_overlaps <- partial_overlaps + 1
  }

}

print(paste0("Partial Overlaps: ", partial_overlaps))

