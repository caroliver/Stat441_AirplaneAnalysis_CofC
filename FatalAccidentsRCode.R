#Load the dataset from CSV File
# NOTE: Make sure to replace the path variable with your own path to the file
airline_safety<-read.csv("C:/Users/Vlad/Desktop/Statistics Classes/Math 541/airline-safety.csv",header=T)

#Get the Number of ASK Based On Trillions
airline_safety$ASKperTrillion = airline_safety[, "avail_seat_km_per_week"]*52*15/1000000000000
View(airline_safety)
 
#Get the Number of ASK Based On Billions
airline_safety$ASKperBillion = airline_safety[, "avail_seat_km_per_week"]*52*15/1000000000
View(airline_safety)

# add columns to dataset for 85 to 99 (per trillion and per billion)
airline_safety$Fatal_Accidents_85_99_trillion = airline_safety$fatal_accidents_85_99 / airline_safety$ASKperTrillion
airline_safety$Fatal_Accidents_85_99_billion = airline_safety$fatal_accidents_85_99 / airline_safety$ASKperBillion

# add columns to dataset for 00 to 14 (per trillion and per billion)
airline_safety$Fatal_Accidents_00_14_trillion = airline_safety$fatal_accidents_00_14 / airline_safety$ASKperTrillion
airline_safety$Fatal_Accidents_00_14_billion = airline_safety$fatal_accidents_00_14 / airline_safety$ASKperBillion
View(airline_safety)

# Make the plot for 'per trillion'
# the data for the columns needed is originally stored as a list 
# must be converted to numeric vector to be used in plot function
fatal_acc_tril_85_99 = as.numeric(as.character(unlist(airline_safety$Fatal_Accidents_85_99_trillion)))
fatal_acc_tril_00_14 = as.numeric(as.character(unlist(airline_safety$Fatal_Accidents_00_14_trillion)))
plot(fatal_acc_tril_85_99, fatal_acc_tril_00_14, xlab="1985-99", ylab="2000-14", main = "Fatal Accidents per Trillion")
abline(lm(fatal_acc_tril_00_14 ~ fatal_acc_tril_85_99))

#Find the correlation coefficient for the correlation between the two year groups for 'per trillion'
cor(fatal_acc_tril_00_14, fatal_acc_tril_85_99)

# Make the plot for 'per billion'
# the data for the columns needed is originally stored as a list 
# must be converted to numeric vector to be used in plot function
fatal_acc_bil_85_99 = as.numeric(as.character(unlist(airline_safety$Fatal_Accidents_85_99_billion)))
fatal_acc_bil_00_14 = as.numeric(as.character(unlist(airline_safety$Fatal_Accidents_00_14_billion)))
plot(fatal_acc_bil_85_99, fatal_acc_bil_00_14, xlab="1985-99", ylab="2000-14", main = "Fatal Accidents per Billion")
abline(lm(fatal_acc_bil_00_14 ~ fatal_acc_bil_85_99))

#Find the correlation coefficient for the correlation between the two year groups for 'per billion'
cor(fatal_acc_bil_00_14, fatal_acc_bil_85_99)

# To attain the number of fatal accidents per trillion, I:
#     0) MULTIPLIED ASK PER WEEK BY 52 WEEKS AND 15 YEARS TO OBTAIN TOTAL ASK
#     1) Divided total ASK given in first column by a trillion
#     2) Divided the number of fatal accidents by the numbers calculated in 1

# The same steps were followed to attain per billion numbers

Both scatterplots and correlation coefficients show no correlation between the two year groups.
