
### day 11

### read file in

file <- 'local_files/example_files/day_11_example.txt'

monkeys <- readLines(file)

# == split data into sections

monkey_string_to_list = function(monkey_string) {

  monkey_list = list()

  for (line in monkey_string) {

    print(line)

    line <- trimws(line, which = c("left"))

    print(line)

    if (startsWith(line, 'Monkey')) {

      monkey_number <- gsub("Monkey ", "", line) %>%
                        gsub(pattern = ':', replacement = '', x = .) %>%
                        as.numeric()

      print(monkey_number)

      monkey_list[[monkey_number+1]] <- list()

      current_monkey <- monkey_number + 1
    }

    if (startsWith(line, 'Starting items: ')) {

      items <- gsub("Starting items: ", "", line) %>%
                strsplit(., split = ',') %>%
                unlist() %>%
                as.numeric()

      print(items)

      monkey_list[[monkey_number+1]][['items']] <- items

    }

    # get operations - extract sign and value
    if (startsWith(line, 'Operation: ')) {

      operation <- gsub("Operation: new = old ", "", line) %>%
        strsplit(., split = ' ') %>%
        unlist()

      print(operation)

      monkey_list[[monkey_number+1]][['operation_sign']] <- operation[1]
      monkey_list[[monkey_number+1]][['operation_value']] <- operation[2]

    }

    # get test - get
    if (startsWith(line, 'Test: ')) {

      operation <- gsub("Operation: new = old ", "", line) %>%
        strsplit(., split = ' ') %>%
        unlist()

      print(operation)

      monkey_list[[monkey_number+1]][['operation_sign']] <- operation[1]
      monkey_list[[monkey_number+1]][['operation_value']] <- operation[2]

    }



  }

  return(monkey_list)
}

monkey_string_to_list(monkeys)
