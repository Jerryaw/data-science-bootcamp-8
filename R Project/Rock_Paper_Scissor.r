choice <- c(1, 2, 3)

win <- 0
loss <- 0
tie <- 0

game <- function() {
  print("Welcome to Rock, Paper, Scissors")
  print("Do you want to start the game? Type 'Yes' to start or 'No' to exit the game.")
  answer <- readLines("stdin", n=1)
  if (answer == "Yes") {
    while(TRUE) {
      print("Choose [1] for 'Rock', [2] for 'Paper', and [3] for 'Scissors',[Stop] to stop playing")
      answer2 <- readLines("stdin", n=1)
      computer_turn <- sample(choice, 1)
      if (answer2 == 1 & computer_turn == 2) {
        loss <- loss + 1
        cat("You lose, Win:", win, "Loss:", loss, "\n")
      } else if (answer2 == 1 & computer_turn == 3) {
        win <- win + 1
        cat("You win, Win:", win, "Loss:", loss, "\n")
      } else if (answer2 == 2 & computer_turn == 1) {
        win <- win + 1
        cat("You win, Win:", win, "Loss:", loss, "\n")
      } else if (answer2 == 2 & computer_turn == 3) {
        loss <- loss + 1
        cat("You lose, Win:", win, "Loss:", loss, "\n")
      } else if (answer2 == 3 & computer_turn == 1) {
        loss <- loss + 1
        cat("You lose, Win:", win, "Loss:", loss, "\n")
      } else if (answer2 == 3 & computer_turn == 2) {
        win <- win + 1
        cat("You win, Win:", win, "Loss:", loss, "\n")
      } else if (answer2 == computer_turn) {
        tie <- tie + 1
        cat("You got a tie, Win:", win, "Loss:", loss, "\n")
      } else if (answer2 == "Stop") {
        cat("Thank you for playing. Final Score: Win:", win, "Loss:", loss, "Ties:", tie, "\n")
        break  # Exit the game loop
      } else {
        print("Invalid choice. Please choose 1, 2, 3, or 'Stop'.")
      }
    }
  } else if (answer == "No") {
    print("Thank you for your interest.")
  } else {
    print("ERROR!")
  }
}

game()
