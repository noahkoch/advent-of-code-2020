map <- readLines('day3.txt')

calculate_trees <- function(move_right, move_down) {
  current_position <- 1

  number_of_trees <- 0
  index <- 0
  for(row in map) {
    index <- index + 1

    if((index - 1) %% move_down != 0) {
      next
    }

    if(nchar(row) < current_position) {
      # Go back to the beginning of the line
      current_position <- current_position - nchar(row)
    }

    item_at_coordinate <- unlist(strsplit(row, ''))[current_position]
    if(item_at_coordinate == '#') {
      number_of_trees <- number_of_trees + 1
    }

    current_position <- current_position + move_right 
  }

  number_of_trees
}

product_of_trees <- calculate_trees(1, 1) * calculate_trees(3, 1) * calculate_trees(5, 1) * calculate_trees(7, 1) * calculate_trees(1, 2)
print(product_of_trees)
