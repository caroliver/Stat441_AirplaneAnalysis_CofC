# Load in the dataset from csv file
# NOTE: Make sure to replace the path variable with your own path to the file
library(readr)
airline_safety <- read_csv("airline-safety.csv")
View(airline_safety)

# Get number of ASK based on trillions like in study - "supposedly"
airline_safety$ASKperTrillion = airline_safety[, "avail_seat_km_per_week"]*52*15/1000000000000
View(airline_safety)

#Get number of ASK based on billions to compare charts
airline_safety$ASKperBillion = airline_safety[, "avail_seat_km_per_week"]*52*15/1000000000
View(airline_safety)

# add columns to dataset for 85 to 99 (per trillion and per billion)
airline_safety$Fatalities_85_99_trillion = airline_safety$fatalities_85_99 / airline_safety$ASKperTrillion
airline_safety$Fatalities_85_99_billion = airline_safety$fatalities_85_99 / airline_safety$ASKperBillion

# add columns to dataset for 00 to 14 (per trillion and per billion)
airline_safety$Fatalities_00_14_trillion = airline_safety$fatalities_00_14 / airline_safety$ASKperTrillion
airline_safety$Fatalities_00_14_billion = airline_safety$fatalities_00_14 / airline_safety$ASKperBillion
View(airline_safety)

# Make the plot for 'per trillion'
# the data for the columns needed is originally stored as a list 
# must be converted to numeric vector to be used in plot function
tril_85_99 = as.numeric(as.character(unlist(airline_safety$Fatalities_85_99_trillion)))
tril_00_14 = as.numeric(as.character(unlist(airline_safety$Fatalities_00_14_trillion)))
plot(tril_85_99, tril_00_14, xlab="1985-99", ylab="2000-14", main = "Fatalities per Trillion")
abline(lm(tril_85_99 ~ tril_00_14))

# Make the plot for 'per billion'
# the data for the columns needed is originally stored as a list 
# must be converted to numeric vector to be used in plot function
bil_85_99 = as.numeric(as.character(unlist(airline_safety$Fatalities_85_99_billion)))
bil_00_14 = as.numeric(as.character(unlist(airline_safety$Fatalities_00_14_billion)))
plot(bil_85_99, bil_00_14, xlab="1985-99", ylab="2000-14", main = "Fatalities per Billion")
abline(lm(bil_85_99 ~ bil_00_14))

# NOTES SO FAR ON GRAPHS ABOVE VS STUDY
# RESOLVED:
# Per trillion does not yeild the same result axies as the study
# however, the graph points look the same pattern wise
# To attain the number of fatalities per trillion, I:
#     0) MULTIPLIED ASK PER WEEK BY 52 WEEKS AND 15 YEARS TO OBTAIN TOTAL ASK
#     1) Divided total ASK given in first column by a trillion
#     2) Divided the number of fatalites by the numbers calculated in 1

# The same steps were followed to attain per billion numbers

# Both scatterplots show no correlation between the two year groups
# This is consistent with the study performed

# As stated above, all graphs appear to have the points pattern
# The only difference appears to be the axes