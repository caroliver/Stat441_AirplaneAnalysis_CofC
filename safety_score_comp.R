# This code uses the original airline_safety dataframe
# there is no reason to switch over and use the new dataframe

library(readr)
airline_safety <- read_csv("/Users/carolineoliver/airline-safety.csv")
# REPLACE LINE ABOVE WITH YOUR PATH: airline_safety <- read_csv("path_to_csv_file_here")

# Create dataframe of the study data
study_combined = list(0.9,0.88,0.88,0.88,0.73,0.71,0.67,0.61,0.6,0.59,0.57,0.51,0.45,0.44,0.37,0.35,0.33,0.33,0.31,0.24,0.23,0.22,0.22,0.2,0.19,0.19,0.16,0.14,0.14,0.11,0.08,0.05,0.01,0.01,-0.01,-0.01,-0.12,-0.13,-0.13,-0.15,-0.17,-0.23,-0.26,-0.35,-0.40,-0.42,-0.48,-0.63,-0.71,-0.74,-0.75,-0.81,-1.38,-1.49,-1.69,-2.26)
study_combined <- as.numeric(unlist(study_combined))
study_combined

airline = c("Southwest Airlines","Cathay Pacific*","Lufthansa*","British Airways*","Air Canada", "Qantas*","United / Continental*","KLM*","Virgin Atlantic","Singapore Airlines","All Nippon Airways","TAP - Air Portugal","Finnair","Hawaiian Airlines","LAN Airlines","Austrian Airlines","Aer Lingus","American*","Delta / Northwest*","Iberia","Air New Zealand*","Condor","COPA","Alaska Airlines*","Aerolineas Argentinas","El Al","Air France","Japan Airlines","Turkish Airlines","South African","Aeromexico*","SWISS*","Thai Airways","Sri Lankan / AirLanka","Alitalia","SAS*","Korean Air","TACA","US Airways / America West*","TAM","Xiamen Airlines","Gulf Air","Vietnam Airlines", "Air India*", "Saudi Arabian","Malaysia Airlines","Royal Air Maroc","Philippine Airlines","Avianca","Kenya Airways","Egyptair","Garuda Indonesia","China Airlines", "Pakistan International","Ethiopian Airlines","Aeroflot*")
airline

study_score_85_99 = list(.99,.91,.8,.9,.73,.77,.37,.46,.57,.6,.57,.51,.42,.47,.12,.35,.26,.4,-.16,.03,.39,.0,-.05,.39,.03,.04,.15,-.45,.11,-.24,-.06,-.31,-.54,-.08,-.34,.28,-.98,-.2,-.59,-.13,-.66,.32,-.98,-.5,-.87,.26,-1.11,-1.29,-1.86,.23,-1.49,-1.42,-2.45,-1.9,-2.74,-4.59)
study_score_85_99 <- as.numeric(unlist(study_score_85_99))
study_score_00_14 = list(.82,.86,.96,.85,.73,.65,.98,.76,.62,.58,.57,.51,.47,.41,.62,.35,.4,.26,.79,.46,.06,.44,.49,-0,.36,.34,.16,.74,.17,.47,.22,.42,.57,.1,.33,-.31,.74,-.05,.34,-.16,.32,-.77,.46,-.19,.07,-1.1,.15,.04,.43,-1.71,-.01,-.21,-.32,-1.07,-.64,.08)
study_score_00_14 <- as.numeric(unlist(study_score_00_14))

study_safety_scores = data.frame(airline, study_score_85_99, study_score_00_14, study_combined)
study_safety_scores = as.data.frame(study_safety_scores)
study_safety_scores = study_safety_scores[order(study_safety_scores$study_combined, decreasing = TRUE),]
study_safety_scores$study_combined_order = seq(1:56)

View(study_safety_scores)

# Create dataframe with the team calculated scores
calc_safety_score = airline_safety[ , 1]
calc_safety_score$Safety_scores_85_99 = airline_safety$SafetyScore_85_99

calc_safety_score$Safety_scores_00_14 = airline_safety$SafetyScore_00_14

calc_safety_score$Safety_scores_combined = airline_safety$Combined_Safety_Score
calc_safety_score = calc_safety_score[order(calc_safety_score$Safety_scores_combined, decreasing = TRUE),]


calc_safety_score$team_combined_order = seq(1:56)

View(calc_safety_score)

# Combine the two dataframes created above by airline name
All_safety_scores = merge(calc_safety_score, study_safety_scores, by = "airline")
View(All_safety_scores)

# COMBINED SCORE RANKING PLOT
plot(All_safety_scores$study_combined_order, All_safety_scores$team_combined_order, 
     xlab="Team Calculated Combined Score Ranking", ylab="Study Calculated Combined Score Ranking", 
     main = "Combined Safety Score Airline Ranking Comparison")
abline(lm(All_safety_scores$study_combined_order ~ All_safety_scores$team_combined_order))

# COMBINED SCORE PLOT
plot(All_safety_scores$Safety_scores_combined, All_safety_scores$study_combined, 
     xlab="Team Calculated Combined Score", ylab="Study Calculated Combined Score",
     xlim = c(-2, 1),
     main = "Combined Safety Score Comparison")

# 85 TO 99 SCORE COMPARISON
plot(All_safety_scores$Safety_scores_85_99, All_safety_scores$study_score_85_99, 
     xlab="Team Calculated 1985-99 Score", ylab="Study Calculated 1985-99 Score",
     xlim = c(-4.0,1.5), ylim = c(-4.0,1.5),
     main = "1985-99 Safety Score Comparison")

# 00 TO 14 SCORE COMPARISON
plot(All_safety_scores$Safety_scores_00_14, All_safety_scores$study_score_00_14, 
     xlab="Team Calculated 2000-14 Score", ylab="Study Calculated 2000-14 Score",
     xlim = c(-3.0,2.0), ylim = c(-3.0,2.0),
     main = "2000-14 Safety Score Comparison")

