library(readr)
airline_safety <- read_csv("Desktop/Stats2018/airline-safety.csv")
View(airline_safety)

# Step 1: subtract an airline’s crash rate from the average for all airlines since 1985
# 1985-2014 Average Crash Rate
# Incidents
sumIncidents = (sum(airline_safety$incidents_85_99) + sum(airline_safety$incidents_00_14)) / nrow(airline_safety)

# Fatal Incidents
sumFatalAccident = (sum(airline_safety$fatal_accidents_85_99) + sum(airline_safety$fatal_accidents_00_14)) / nrow(airline_safety)

# Total Fatalities
sumTotalFatalities = (sum(airline_safety$fatalities_85_99) + sum(airline_safety$fatalities_00_14)) / nrow(airline_safety)

# 85 to 99
airline_safety$Score_indident_Rate_85_99 = sumIncidents - airline_safety$incidents_85_99 
airline_safety$Score_fatalAccident_Rate_85_99 = sumFatalAccident - airline_safety$fatal_accidents_85_99
airline_safety$Score_fatalities_Rate_85_99 = sumTotalFatalities - airline_safety$fatalities_85_99

# 00 to 14
airline_safety$Score_indident_Rate_00_14 = sumIncidents - airline_safety$incidents_00_14
airline_safety$Score_fatalAccident_Rate_00_14 = sumFatalAccident - airline_safety$fatal_accidents_00_14
airline_safety$Score_fatalities_Rate_00_14 = sumTotalFatalities - airline_safety$fatalities_00_14

# Step 2: Multiply the result by the square root of the number of seat kilometers flown
# square root of ASK
airline_safety$ASKsqrt = sqrt((airline_safety$avail_seat_km_per_week * 52 * 15))

# 85 to 99
airline_safety$Score_indident_Rate_85_99 = airline_safety$Score_indident_Rate_85_99 * airline_safety$ASKsqrt
airline_safety$Score_fatalAccident_Rate_85_99 = airline_safety$Score_fatalAccident_Rate_85_99 * airline_safety$ASKsqrt
airline_safety$Score_fatalities_Rate_85_99 = airline_safety$Score_fatalities_Rate_85_99 * airline_safety$ASKsqrt

# 00 to 14
airline_safety$Score_indident_Rate_00_14 = airline_safety$Score_indident_Rate_00_14 * airline_safety$ASKsqrt
airline_safety$Score_fatalAccident_Rate_00_14 = airline_safety$Score_fatalAccident_Rate_00_14 * airline_safety$ASKsqrt
airline_safety$Score_fatalities_Rate_00_14 = airline_safety$Score_fatalities_Rate_00_14 * airline_safety$ASKsqrt

# Step 3: Standardize the score in each category to calculate how many standard deviations
# an airline is above or below the mean
# I used the scale() function in r to calculate z-score/standardize

# 85 to 99
airline_safety$Score_indident_Rate_85_99 = scale(airline_safety$Score_indident_Rate_85_99)
airline_safety$Score_fatalAccident_Rate_85_99 = scale(airline_safety$Score_fatalAccident_Rate_85_99)
airline_safety$Score_fatalities_Rate_85_99 = scale(airline_safety$Score_fatalities_Rate_85_99)

# 00 to 14
airline_safety$Score_indident_Rate_00_14 = scale(airline_safety$Score_indident_Rate_00_14)
airline_safety$Score_fatalAccident_Rate_00_14 = scale(airline_safety$Score_fatalAccident_Rate_00_14)
airline_safety$Score_fatalities_Rate_00_14 = scale(airline_safety$Score_fatalities_Rate_00_14)

# Step 3 Continued: average the scores from the three categories together to get the Safety Score
airline_safety$SafetyScore_85_99 = (airline_safety$Score_indident_Rate_85_99 
                                    + airline_safety$Score_fatalAccident_Rate_85_99 
                                    + airline_safety$Score_fatalities_Rate_85_99) / 3
airline_safety$SafetyScore_00_14 = (airline_safety$Score_indident_Rate_00_14
                                   + airline_safety$Score_fatalAccident_Rate_00_14
                                   + airline_safety$Score_fatalities_Rate_00_14) / 3

# Combined Safety Score
airline_safety$Combined_Safety_Score = (airline_safety$SafetyScore_85_99 + airline_safety$SafetyScore_00_14) / 2
