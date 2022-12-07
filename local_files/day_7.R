
## day 7

### read file in

file <- 'local_files/day_7.txt'

commands <- readLines(file)

# === part 1 ===

# one of two ways
# 1 - create embedded lists in R
# 2 - actually run the commands to create folders then check
# embedd list too complex, just create files and see size of files

previous_root_dir <- getwd()

# go into folder designed to have root location
setwd("day_7_file_folder")

# delete any exisiting file
existing_files <- dir()

for (file in existing_files) {

  system(paste0("rm -rf ", file))

}

current_ls_indicator <- 0

is_command <- function(command) {

  adjusted_command <- gsub("\\$ ", "", command)

  return(list(is_command = startsWith(command, "$ "), command = adjusted_command))

}

# say what to do in each command
cd_command <- function(command) {

  current_ls_indicator <<- 0

  print(paste0("Command at position ", n, ": ", command))

  setwd(command)

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

    system(paste0("mkdir ", command_sections[2]))

  }
  else if (!(is.na(as.numeric(command_sections[1])))) {

    print(paste0("File ", command_sections[2], " created at position: ", n))

    system(paste0("fallocate -l ", command_sections[1], command_sections[2]))

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
  if (n == 10) {
    break
  }
}








