# Add Countries column
countries<-c("Ireland", "Russia", "Argentina", "Mexico", "Canada", "France", "India", "New Zealand", "United States", "Italy", "Japan", "United States", "Austria", "Colombia", "United Kingdom", "Hong Kong", "China", "Germany", "Panama", "United States", "Egypt", "Israel", "Ethiopia", "Finland", "Indonesia", "Bahrain", "Hawaii", "Spain", "Japan", "Kenya", "Netherlands", "South Korea", "Chile", "Germany", "Malaysia", "Pakistan", "Philippines", "Australia", "Morocco", "Scandinavia", "Saudi Arabia", "Singapore", "South Africa", "United States", "Sri Lanka", "Switzerland", "El Salvador", "Brazil", "Portugal", "Taiwan", "Turkey", "United States", "United States", "Vietnam", "United Kingdom", "China")
airline_safety$Country <- countries

# 2013 numbers from CIA World Factbook (and Wikipedia for Hawaii)
airports = read.csv("airports.csv", header = FALSE, stringsAsFactors = FALSE)
airports[1,1] = "United States"
colnames(airports) = c("Country", "Airports")

airline_safety_enhanced = merge(airline_safety, airports, all.x = TRUE)
airline_safety_enhanced[is.na(airline_safety_enhanced$Airports),"Airports"] = c(28,406,111)
airline_safety_enhanced$Airports = gsub(",", "", airline_safety_enhanced$Airports)
airline_safety_enhanced$Airports = as.numeric(airline_safety_enhanced$Airports)

# 2016 numbers from CIA World Factbook (and Wikipedia for Hawaii). area in square km
land_areas = read.csv("areas.csv", stringsAsFactors = FALSE)
colnames(land_areas)[1] = "Country"
land_areas = land_areas[2:nrow(land_areas),]
land_areas[4,1] = "United States"
airline_safety_enhanced = merge(airline_safety_enhanced, land_areas, all.x = TRUE)
airline_safety_enhanced$Area = gsub(",", "", airline_safety_enhanced$Area)
airline_safety_enhanced$Area = as.numeric(airline_safety_enhanced$Area)
airline_safety_enhanced[is.na(airline_safety_enhanced$Area),"Area"] = c(28311,757051,96920,241930,241930)

airline_safety_enhanced$AirportDensity = airline_safety_enhanced$Airports/airline_safety_enhanced$Area
write.csv(airline_safety_enhanced, "airline-safety.csv", row.names = FALSE)

gdp = read.csv("gdp.csv", stringsAsFactors = FALSE)
colnames(gdp)[1] = "Country"
gdp = gdp[,c("Country", "X1985", "X2014")]

#from The World Bank, the Federal Reserve Bank of St. Louis, and UN data
airline_safety_enhanced = merge(airline_safety_enhanced, gdp, all.x = TRUE)
airline_safety_enhanced[is.na(airline_safety_enhanced$X1985),"X1985"] = c(34689.56,4346734,35699.54,3305000,80196.62,100273.1,192058.92)
airline_safety_enhanced[is.na(airline_safety_enhanced$X2014),"X2014"] = c(305529.66,17393103,291459.36,2063662.67,475383.29,1411333.93,969652.31)
colnames(airline_safety_enhanced)[13:14] = c("GDP_85", "GDP_14")
write.csv(airline_safety_enhanced, "airline-safety.csv", row.names = FALSE)


crashes15 = read.csv("2015_crashes.csv", stringsAsFactors = FALSE)
crashes16 = read.csv("crashes_2016.csv", stringsAsFactors = FALSE)
crashes17 = read.csv("crashes_2017.csv", stringsAsFactors = FALSE)
crashes18 = read.csv("crashes_2018.csv", stringsAsFactors = FALSE)
colnames(crashes15)[1] = "Airline"
colnames(crashes16)[1] = "Airline"
colnames(crashes17)[1] = "Airline"
colnames(crashes18)[1] = "Airline"

airlines = unique(append(crashes15$Airline, c(crashes16$Airline, crashes17$Airline, crashes18$Airline)))

library(plyr)
airline_test = aggregate(freq ~ x, rbind(count(crashes15$Airline), count(crashes16$Airline), count(crashes17$Airline), count(crashes18$Airline)), "sum")
colnames(airline_test) = c("Airline", "incidents_15_18")

crashes15$FatalAccident = ifelse(crashes15$Fatalities > 0, 1, 0)
crashes16$FatalAccident = ifelse(crashes16$Fatalities > 0, 1, 0)
crashes17$FatalAccident = ifelse(crashes17$Fatalities > 0, 1, 0)
crashes18$FatalAccident = ifelse(crashes18$Fatalities > 0, 1, 0)

airline_test = merge(airline_test, aggregate(FatalAccident ~ Airline, rbind(crashes15, crashes16, crashes17, crashes18), "sum"), all.x = TRUE)

airline_test = merge(airline_test, aggregate(Fatalities ~ Airline, rbind(crashes15, crashes16, crashes17, crashes18), "sum"), all.x = TRUE)

colnames(airline_test) = c("airline", "incidents_15_18", "fatal_accidents_15_18", "fatalities_15_18")

airline_safety_enhanced = merge(airline_safety_enhanced, airline_test, all.x = TRUE)
airline_safety_enhanced[is.na(airline_safety_enhanced)] = 0

#changed some names, added airline association
airline_safety_enhanced = read.csv("airline-safety.csv", stringsAsFactors = FALSE)

write.csv(airline_test, "hff.csv", row.names = FALSE)
#changed names
airline_test = read.csv("hff.csv", stringsAsFactors = FALSE)
#airline_safety$airline[21] = "EgyptAir"
airline_safety_enhanced = airline_safety_enhanced[,1:14]

airline_test2 = aggregate(incidents_15_18 ~ airline, airline_test, "sum")
airline_test2 = merge(airline_test2, aggregate(fatal_accidents_15_18 ~ airline, airline_test, "sum"))
airline_test2 = merge(airline_test2, aggregate(fatalities_15_18 ~ airline, airline_test, "sum"))

airline_safety_enhanced = merge(airline_safety_enhanced, airline_test2, all.x = TRUE)
write.csv(airline_safety_enhanced, "airline-safety.csv", row.names = FALSE)
