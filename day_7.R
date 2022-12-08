## day 7

### read file in

file <- 'local_files/day_7.txt'

commands <- readLines(file)

library(stringr)
# === part 1 ===

# ==== try to create files ====

previous_root_dir <- getwd()

# go into folder designed to have root location
local_locations_of_system <- "day_7_file_folder"
setwd(local_locations_of_system)

# checking size of files in each directory
size_in_directory <- c()

# delete any exisiting file
existing_files <- dir()

if (length(existing_files) > 0) {

  for (file in existing_files) {

    system(paste0("rm -rf ", file))

  }
}

current_ls_indicator <- 0

is_command <- function(command) {

  adjusted_command <- gsub("\\$ ", "", command)

  return(list(is_command = startsWith(command, "$ "), command = adjusted_command))

}

# say what to do in each command
cd_command <- function(command) {

  current_ls_indicator <<- 0

  cd_location <- strsplit(command, ' ')[[1]][2]

  print(paste0("cd at position ", n, ": ", cd_location))

  # if fails, then check 2,3,4,5 ...
  try_change <- try(setwd(cd_location))

  extension_number <- 1
  while (class(try_change) == "try-error") {

    print(paste0("Directory not there, trying: ", paste0(cd_location, extension_number)))
    try_change <- try(setwd(paste0(cd_location, extension_number)))

    extension_number <- extension_number + 1

  }

  print(paste0("New Directory: ", gsub(previous_root_dir, '', getwd())))

}

ls_command <- function(command) {

  print(paste0("ls has been found at position: ", n))
  current_ls_indicator <<- 1

}

# if not a command, see what is wanted to do in ls file
ls_output <- function(command) {

  command_sections <- strsplit(command, ' ')[[1]]

  print(command_sections)

  if (command_sections[1] == 'dir') {

    print(paste0("Directory ", command_sections[2], " found at position: ", n))

    # check if dir already exists, if so then make new command but with 2, 3, 4..
    if (command_sections[2] %in% names(size_in_directory)) {
      print(paste0("Directory already exists elsewhere: ", command_sections[2]))
      has_found_new_directory <- 0
      extension_number <- 1
      while(has_found_new_directory == 0) {

        new_directory <- paste0(command_sections[2], extension_number)

        if (new_directory %in% names(size_in_directory)) {

          extension_number <- extension_number + 1

        } else {

          command_sections[2] <- new_directory
          has_found_new_directory <- 1

        }

      }

      print(paste0("Changing to directory: ", command_sections[2]))

    }

    system(paste0("mkdir ", command_sections[2]))

    size_in_directory[[command_sections[2]]] <<- 0

  }
  else if (!(is.na(as.numeric(command_sections[1])))) {

    print(paste0("File ", command_sections[2], " created at position: ", n))

    current_directory <- tail(strsplit(getwd(), '/')[[1]], n=1)

    size_in_directory[[current_directory]] <<- size_in_directory[[current_directory]] + as.numeric(command_sections[1])
  }
  else {

    print(paste0("This file command not found: ", paste0(command_sections, sep = ',')))

  }

}

for (n in 1:length(commands)) {

  if (n == 1) {
    next
  }

  print(paste0("Position: ", n))

  command <- commands[n]

  # check if command
  command_checks <- is_command(command)

  if (command_checks$is_command) {

    if (startsWith(command_checks$command, "cd")) {

      cd_command(command_checks$command)

    }

    if(startsWith(command_checks$command, "ls")) {

      ls_command(command_checks$command)

    }

  }

  if (current_ls_indicator) {

    ls_output(command_checks$command)

  }

}

# == iterate through file locations - add to sum if lower than limit

maximum_directory_size <- 100000

# go back to root directory
setwd(paste0(previous_root_dir,'/' ,local_locations_of_system))

memory_list <- list()

all_dirs <- list.dirs()

# to get total size - look at all dirs with that code in the name - then aggregate based upon list

all_end_of_directories <- names(size_in_directory)

for (directory in all_end_of_directories) {

  print(directory)

  all_paths_from_dir <- all_dirs[str_detect(all_dirs, paste0("/", directory, "/"))]

  print("Paths from Directory: ")
  print(all_paths_from_dir)

  all_end_paths_of_dir <- sapply(all_paths_from_dir, function(x) { tail(strsplit(x, '/')[[1]], n=1)})

  print("All path ends from directory: ")
  print(all_end_paths_of_dir)

  memory_list[[directory]] <- size_in_directory[[directory]]

  for (end_directory_path in all_end_paths_of_dir) {

    memory_list[[directory]] <- memory_list[[directory]] + size_in_directory[[end_directory_path]]

  }

}

# === check file sizes less than specified amount

max_directory_size <- 100000

directory_size_data_frame <- data.frame(t(data.frame(memory_list)))
directory_size_data_frame  <- cbind(rownames(directory_size_data_frame), directory_size_data_frame)
colnames(directory_size_data_frame) <- c("directory", "size")

low_size_directories <- directory_size_data_frame[directory_size_data_frame$size <= max_directory_size,]
sum(low_size_directories$size)


# ==== Part 2 ====

# find lowest directory that would meet minimum removal space

required_free_space <- 30000000

total_file_system_size <- 70000000

# add up root dirs to get free space
root_dirs <- system('ls', intern = T)

total_used_space <- sum(directory_size_data_frame[directory_size_data_frame$directory %in% root_dirs,]$size)
total_used_space

free_space <- total_file_system_size - total_used_space

smallest_directory_delete_size <- required_free_space - free_space

largest_directories <- directory_size_data_frame[directory_size_data_frame$size >= smallest_directory_delete_size,]

min(largest_directories$size)
