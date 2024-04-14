#
# Load in XML 
#

# Package
library("XML")

# Load data
BBR <- xmlParse("/Volumes/Filer_Backup/Speciale/BBR_07_Gaeldende_20240305204506/BBR_07_Gaeldende_20240305204506.xml")

# Convert 
xml_data <- xmlToList(BBR)
