library('stringr')

is_invalid_line <- function(line) {
  match_groups <- str_match(line, "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)")

  pos1 <- strtoi(match_groups[1,2])
  pos2 <- strtoi(match_groups[1,3])
  letter <- match_groups[1,4] 
  string <- strsplit(match_groups[1,5], '')[[1]]

  if(string[pos1] == letter && string[pos2] == letter) {
    TRUE 
  } else if(string[pos1] != letter && string[pos2] != letter) {
    TRUE 
  } else {
    FALSE
  }
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
