
### Day 3 ###

# Read Data In

local_file = 'local_files/day_3.txt'

rucksacks = read.table(local_file, header = F, col.names = 'rucksacks')$rucksacks

# === Part 1 ===

# create priorities
all_letters = c(letters, LETTERS)
priority = as.list(1:52)
names(priority) <- all_letters

# iterate through
# take length
# split by 2
# for for letter in part 1 in part 2
# if not found - raise error
# add to total sum

total_priority_sum <- 0

for (rucksack in rucksacks) {

  len_rucksack = nchar(rucksack)

  if (len_rucksack %% 2 != 0) {
    stop(paste0("length of rucksack ", rucksack, " is not even"))
  }

  compartment_1 <- substr(rucksack, 1, len_rucksack/2)
  compartment_2 <- substr(rucksack, 1+(len_rucksack/2), len_rucksack)

  for (item in strsplit(compartment_1, '')[[1]]) {

    if (grepl(item, compartment_2)) {

      print(paste0("item ", item, " in compartment 2: ", compartment_2))

      print(paste0("Priority of item: ", priority[item]))

      total_priority_sum <- total_priority_sum + priority[item][[1]]

      print(paste0("Latest priority sum: ", total_priority_sum))

      break
    }

  }

}


# === Part 2 ===

# take 3 sets at a time - do (n-1)*3 + 1 : n *3
# compare values in all three
# get value and add to priority

total_elfs_in_group <- 3

total_groups <- length(rucksacks) / total_elfs_in_group

for (group in 1:total_groups) {




}
