#install package to link to GA if not already installed
install.packages('googleAnalyticsR')

#install anaysis packages
install.packages('arules')



#load packages
library(googleAnalyticsR)
library(arules)
library(tidyr)



#sign into velux@ecapacity.dk Google Account
ga_auth()

#function to find start of last month
som <- function(x){
  as.Date(format(x,"%Y-%m-01"))
}
som(som(Sys.Date())-1)

#function to find end of last month
eom<- function(x){
  som(som(x)+35)-1
}
som(Sys.Date())-1


#pull data from GA (ID for German eshop)
ga_data <- google_analytics(viewId="37002615",
                              date_range=c(som(som(Sys.Date())-1),som(Sys.Date())-1),
                              metrics=c("ga:itemQuantity","ga:itemRevenue"),
                              dimensions=c("ga:productName","transactionId"),anti_sample=TRUE)


new_data <- ga_data[,c("itemQuantity","productName","transactionId")]

test <- spread(new_data,"productName","itemQuantity")
