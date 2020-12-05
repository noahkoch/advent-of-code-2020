library(data.table)
library(dplyr)
library(stringr)

dt <- data.table('byr' = integer(), 'iyr' = character(), 'eyr' = character(), 'hgt' = character(), 'hcl' = character(), 'ecl' = character(), 'pid' = character(), 'cid' = character())

lines <- readLines('~/Code/advent-of-code/day4.txt')

# Convert our data to a datatable
current_passport_attrs <- c()
current_passport_values <- c()

parseHeight <- function(height_string) {
  if(grepl('cm', height_string)) {
    c(str_sub(height_string, 1, -3), 'cm')
  } else if(grepl('in', height_string)) {
    c(str_sub(height_string, 1, -3), 'in')
  } else {
    c(height_string, NA)
  }
}

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

    if(field[1] == 'hgt') {
      current_passport_attrs <- append(current_passport_attrs, c('height', 'unit'))
      current_passport_values <- append(current_passport_values, parseHeight(field[2]))  
    }
    
    current_passport_attrs <- append(current_passport_attrs, field[1])
    current_passport_values <- append(current_passport_values, field[2])
  }
}

names(current_passport_values) <- current_passport_attrs
dt <- rbind(dt, as.data.frame(t(current_passport_values)), fill = TRUE)

# We now have the datatable, figure out how many valid
# row we have. 'cid' isn't important right now, so we won't worry if that's null
dt_without_cid <- dt[,-'cid']
complete_passports <- dt_without_cid[complete.cases(dt_without_cid),]

# convert our values to the correct data type
complete_passports$byr <- as.numeric(as.character(complete_passports$byr))
complete_passports$iyr <- as.numeric(as.character(complete_passports$iyr))
complete_passports$eyr <- as.numeric(as.character(complete_passports$eyr))
complete_passports$pid <- as.character(complete_passports$pid)
complete_passports$hcl <- as.character(complete_passports$hcl)
complete_passports$height <- as.numeric(as.character(complete_passports$height))


validated_passports <- filter(
  complete_passports, 
  byr >= 1920 & byr <= 2002 & 
    iyr >= 2010 & iyr <= 2020 & 
    eyr >= 2020 & eyr <= 2030 & 
    ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth') & 
    nchar(pid) == 9 &
    nchar(hcl) == 7 & substr(hcl, 1, 1) == '#',
    ((height >= 150 & height <= 193 & unit == 'cm') | (height >= 59 & height <= 76 & unit == 'in'))
  )
print(validated_passports)