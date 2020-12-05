library('stringr')

is_invalid_line <- function(line) {
  match_groups <- str_match(line, "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)")

  min <- strtoi(match_groups[1,2])
  max <- strtoi(match_groups[1,3])
  letter <- match_groups[1,4] 
  string <- match_groups[1,5] 

  matching_letters <- length(str_match_all(string, letter)[[1]])

  return (matching_letters < min || matching_letters > max)
}

input_file <- readLines('day2.txt')

valid_passwords <- 0

for(line in input_file) {
  if(is_invalid_line(line)) {
    print(line)
  } else{
    valid_passwords <- valid_passwords + 1
  }
}

print(valid_passwords)
