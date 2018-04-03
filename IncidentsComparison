# NOTE: THIS CODE IS TO BE USED IN CONJUNCTION WITH THE CODE FROM FatalitiesComparisonRCode IN THE MASTER BRANCH

# add columns to airline_safety dataset for 85 to 99 - INCIDENTS (per trillion and per billion)
airline_safety$Incidents_85_99_trillion = airline_safety$incidents_85_99 / airline_safety$ASKperTrillion
airline_safety$Incidents_85_99_billion = airline_safety$incidents_85_99 / airline_safety$ASKperBillion

# add columns to airline_safety dataset for 00 to 14 - INCIDENTS (per trillion and per billion)
airline_safety$Incidents_00_14_trillion = airline_safety$incidents_00_14 / airline_safety$ASKperTrillion
airline_safety$Incidents_00_14_billion = airline_safety$incidents_00_14 / airline_safety$ASKperBillion
View(airline_safety)

# Make the plot for 'per trillion' - INCIDENTS
# the data for the columns needed is originally stored as a list 
# must be converted to numeric vector to be used in plot function
tril_85_99 = as.numeric(as.character(unlist(airline_safety$Incidents_85_99_trillion)))
tril_00_14 = as.numeric(as.character(unlist(airline_safety$Incidents_00_14_trillion)))
plot(tril_85_99, tril_00_14, xlab="1985-99", ylab="2000-14", main = "Incidents per Trillion")
abline(lm(tril_85_99 ~ tril_00_14))

# Make the plot for 'per billion' - INCIDENTS
# the data for the columns needed is originally stored as a list 
# must be converted to numeric vector to be used in plot function
bil_85_99 = as.numeric(as.character(unlist(airline_safety$Incidents_85_99_billion)))
bil_00_14 = as.numeric(as.character(unlist(airline_safety$Incidents_00_14_billion)))
plot(bil_85_99, bil_00_14, xlab="1985-99", ylab="2000-14", main = "Incidents per Billion")
abline(lm(bil_85_99 ~ bil_00_14))
