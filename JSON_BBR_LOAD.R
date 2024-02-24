### LOAD JSON file (200 gb)

# Library load ----
library(jsonlite) 

#
con <- file("/Volumes/Filer_Backup/Speciale/BBR_2023_2024/BBR_2023_2024.json", "r")

# Læs den første linje
first_line <- readLines(con, n = 1)

# Konverter den første linje til en liste
data <- fromJSON(first_line)

# Luk filen
close(con)