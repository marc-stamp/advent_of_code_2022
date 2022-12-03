
### Day 2

# get data

file <- 'local_files/day_2.txt'

strategy_guide <- read.csv(file, sep = ' ', header = F, col.names = c("opponent", "you"))

# === part 1 ===

opponents_code = list('A' = 'rock', 'B' = 'paper', 'C' = 'scissors')
my_code = list('X' = 'rock', 'Y' = 'paper', 'Z' = 'scissors')

my_scores = list('rock' = 1, 'paper' = 2, 'scissors' = 3)

result_scores = list('loss' = 0, 'draw' = 3, 'win' = 6)

who_wins <- function(you, them) {
  if (you == them) {
      return('draw')
  }

  rock = 'rock'
  paper = 'paper'
  scissors = 'scissors'

  if (you == rock & them == scissors) {
      return('win')
  }
  if (you == paper & them == rock) {
      return('win')
  }

  if (you == scissors & them == paper) {
      return ('win')
  }
  return('loss')
}

total_score <- 0

for (n in 1:nrow(strategy_guide)) {

  opponent <- opponents_code[[strategy_guide$opponent[n]]]
  you <- my_code[[strategy_guide$you[n]]]

  result <- who_wins(you, opponent)

  my_score <- my_scores[[you]]
  result_score <- result_scores[[result]]

  print(strategy_guide[n, ])

  print(paste0("Me: ", you, " Them: ", opponent, " Result: ", result))
  print(paste0("My Points: ", my_score, " Result Points: ", result_score))

  total_score <- total_score + sum(c(my_score, result_score))

  print(paste0("Running Total Score: ", total_score))
}

print(paste0("Total Score: ", total_score))


# === Part 2 ===

# the code is the expected result, back calculate to determine what should be

result_code <- list("X" = 'loss', "Y" = 'draw', "Z" = 'win')

strategy_guide$expected_result <- strategy_guide$you

possible_options <- names(my_scores)

result_total_score <- 0

for (n in 1:nrow(strategy_guide)) {

  print(strategy_guide[n, ])

  opponent <- opponents_code[[strategy_guide$opponent[n]]]

  expected_result <- result_code[[strategy_guide$expected_result[n]]]

  for (option in possible_options) {

    option_result = who_wins(option, opponent)

    if (option_result == expected_result) {

      print(paste0(" Them: ", opponent, " Expected Results: ", expected_result))
      print(paste0("Get Result from : ", option))

      my_score <- my_scores[[option]]
      result_score <- result_scores[[expected_result]]

      print(paste0("My Points: ", my_score, " Result Points: ", result_score))

      result_total_score <- result_total_score + sum(c(my_score, result_score))

      print(paste0("Running Total Score: ", result_total_score))

      break
    }

  }

}
