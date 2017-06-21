#Import Libraries
library(ggplot2)

#Load Banking data
loan <- read.csv("data/loan.csv")


#remove the column with na values and more than 70% with 0
loan <- loan[,!apply(loan , 2 , function(x)
  all(is.na(x)))]
loan <- loan[, -which(colMeans(loan == 0 | is.na(loan)) > 0.7)]

#data cleaning

#rounding off the funded_amt_inv, annual_inc

loan$funded_amnt_inv <- round(loan$funded_amnt_inv)
loan$annual_inc <- round(loan$annual_inc)
loan$out_prncp <- round(loan$out_prncp)
loan$out_prncp_inv <- round(loan$out_prncp_inv)
loan$total_pymnt <- round(loan$total_pymnt)
loan$total_pymnt_inv <- round(loan$total_pymnt_inv)
loan$total_rec_prncp <- round(loan$total_rec_prncp)
loan$total_rec_int <- round(loan$total_rec_int)
loan$total_rec_late_fee <- round(loan$total_rec_late_fee)
loan$recoveries <- round(loan$recoveries)
loan$collection_recovery_fee <- round(loan$collection_recovery_fee)
loan$last_pymnt_amnt <- round(loan$last_pymnt_amnt)

#removing the months from the term
loan$term <- substr(loan$term, 2, 3)

#removing percent symbol from int_rate and revol_util
loan$int_rate <- as.character(loan$int_rate)
loan$int_rate <-
  as.numeric(substr(loan$int_rate, 0, nchar(loan$int_rate) - 1))

loan$revol_util <- as.character(loan$revol_util)
loan$revol_util <-
  as.numeric(substr(loan$revol_util, 0, nchar(loan$revol_util) - 1))

#remove na values
for (i in 1:ncol(loan)) {
  loan[is.na(loan[, i]), i] <- 0
}


#Now we are going to consider only the charged off conditon in loan_status

ggplot(loan, aes(x = loan_status)) + geom_histogram(stat = "count") + geom_text(stat =
                                                                                  'count', aes(label = ..count..), vjust = -1)

charged_off <- subset(loan, loan$loan_status == "Charged Off")
View(charged_off)

#So out of 39717 loans granted 5627 were charged off


#segmented univariate analysis

rented_house <- subset(charged_off, charged_off$home_ownership == "RENT")
mortgage_house <-
  subset(charged_off, charged_off$home_ownership == "MORTGAGE")
own_house <- subset(charged_off, charged_off$home_ownership == "OWN")

ggplot(charged_off, aes(x = home_ownership)) + geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1)

#So the in charged of people 2839 people are rented and 2327 peoples houses are under mortgage

#Verification status

ggplot(charged_off, aes(x = grade)) + geom_bar(stat = "count") + geom_text(stat =
                                                                             'count', aes(label = ..count..), vjust = -1)
