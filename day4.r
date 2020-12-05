library(data.table)

dt <- data.table('iyr' = character(), 'eyr' = character(), 'hgt' = character(), 'hcl' = character(), 'ecl' = character(), 'pid' = character(), 'cid' = character())

lines <- readLines('day4.txt')

found_fields <- 0 
valid_passports <- 0
invalid_passports <- 0

current_passport_attrs <- c()
current_passport_values <- c()

for(line in lines) {
  if(line == '') {
    names(current_passport_values) <- current_passport_attrs
    dt <- rbind(dt, as.data.frame(t(current_passport_values)), fill = TRUE)

    current_passport_values <- c()
    current_passport_attrs <- c()
    next
  }

  pairs <- unlist(strsplit(line, ' '))

  for(pair in pairs) {
    field <- unlist(strsplit(pair, ':'))
    current_passport_attrs <- append(current_passport_attrs, field[1])
    current_passport_values <- append(current_passport_values, field[2])
  }
}

names(current_passport_values) <- current_passport_attrs
dt <- rbind(dt, as.data.frame(t(current_passport_values)), fill = TRUE)

print(dt)
