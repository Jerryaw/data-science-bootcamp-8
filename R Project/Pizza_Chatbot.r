# Define the menu and promotions as data frames
menu <- data.frame(
  Pizza_ID = c(1, 2, 3, 4, 5),
  Pizza_Menu = c("Hawaiian", "Seafood Deluxe", "Pepperoni", "Double Meat", "Seafood Cocktail"),
  Price = c(10.99, 16.99, 23.99, 19.99, 22.99)
)

promotions <- data.frame(
  Promotion_ID = c(1, 2, 3),
  Promotion_Menu = c("Medium Hawaiian + Chicken Wing", "Medium Seafood Deluxe + Coke", "Large Pepperoni for the price of Medium"),
  Price = c(23.99, 23.99, 16.99)
)

# Function to display a table
display_table <- function(table) {
  print(table)
}

# Function to calculate and display the total cost
calculate_total <- function(order, menu) {
  total <- sum(menu$Price[order])
  cat("Your total is", total, "USD. Thank you for your order!\n")
  return(total)
}

# Main chatbot function
chatbot <- function() {
  total_cost <- 0
  cat("Welcome to our pizza shop!\n")
  repeat {
    cat("Type [Promotion] to see promotions, [Menu] to see the menu, or [Exit] to leave.\n")
    
    user_input <- tolower(readLines("stdin", n = 1))
    
    if (user_input == "promotion") {
      display_table(promotions)
      cat("Please enter the Promotion_ID you'd like to order: ")
      promotion_id <- as.numeric(readLines("stdin", n = 1))
      
      if (promotion_id %in% promotions$Promotion_ID) {
        total_cost <- total_cost + promotions$Price[promotions$Promotion_ID == promotion_id]
        cat("Added to your order. ", promotions$Promotion_Menu[promotions$Promotion_ID == promotion_id], "\n")
      } else {
        cat("Invalid Promotion_ID. Please choose a valid ID.\n")
      }
      
    } else if (user_input == "menu") {
      display_table(menu)
      cat("Please enter the Pizza_ID you'd like to order: ")
      pizza_id <- as.numeric(readLines("stdin", n = 1))
      
      if (pizza_id %in% menu$Pizza_ID) {
        total_cost <- total_cost + menu$Price[menu$Pizza_ID == pizza_id]
        cat("Added to your order. ", menu$Pizza_Menu[menu$Pizza_ID == pizza_id], "\n")
      } else {
        cat("Invalid Pizza_ID. Please choose a valid ID.\n")
      }
      
    } else if (user_input == "exit") {
      cat("Thank you for visiting our pizza shop. Your total order cost is: ", total_cost, "USD. Have a great day!\n")
      break
    } else {
      cat("Invalid input. Please type 'Promotion', 'Menu', or 'Exit'.\n")
    }
  }
}

# Run the chatbot
chatbot()
