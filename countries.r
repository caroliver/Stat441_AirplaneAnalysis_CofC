library(lattice)

airline_safety<-read.csv("/Users/koatl/Desktop/R/airline-safety.csv",header=T,sep=",")
data

View(data)
airline_safety$Incidents_85_99_trillion = airline_safety$incidents_85_99 / airline_safety$ASKperTrillion
airline_safety$Incidents_85_99_billion = airline_safety$incidents_85_99 / airline_safety$ASKperBillion


airline_safety$ASKperTrillion = airline_safety[, "avail_seat_km_per_week"]*52*15/1000000000000
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


# Add Countries column
countries<-c("Ireland", "Russia", "Argentina", "Mexico", "Canada", "France", "India", "New Zealand", "USA", "Italia", "Japan", "USA", "Austria", "Columbia", "Great Brittain", "Hong Kong", "China", "Germany", "Panama", "USA", "Egypt", "Israel", "Ethiopia", "Finland", "Indonasia", "Bahrain", "Hawaii", "Spain", "Japan", "Kenya", "Dutch", "Korean", "Chile", "Germany", "Malaysia", "Pakistan", "Philippine", "Australia", "Morocco", "Scandinavia", "Saudi Arabia", "Singapore", "South Africa", "USA", "Sri Lanka", "Switzerland", "Salvador", "Brazil", "Portugal", "Thaiwan", "Turkey", "USA", "USA", "Vietnam", "Great Britain", "China")
airline_safety$Countries <- countries

# Calculate cumulative fatalities per billion (85-14) and center the data
airline_safety$centered <-scale(airline_safety$Fatalities_00_14_billion+airline_safety$Fatalities_85_99_billion, scale = F)
plot(airline_safety$Countries, airline_safety$fatal_accidents_00_14)
counts <- table(airline_safety$centered, airline_safety$Countries)

# Make a bar plot. Colors represent the cumulative fatalities. The lighter the color - the bigger the number
barplot(counts, main = "Cumulative fatalities per billion per country", xlab = "Airline country", ylab = "Number of airlines per country")
counts
