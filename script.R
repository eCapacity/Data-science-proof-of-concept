#install package to link to GA if not already installed
install.packages('googleAnalyticsR')

#install anaysis packages
install.packages('arules')



#load packages
library(googleAnalyticsR)
library(arules)
library(tidyr)
library(dplyr)



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


#looks just at transactionId and product, not quantities and lists products per ttransaction
temp <- new_data %>% group_by(transactionId) %>% summarise(products =list(productName))

#seperates list of products and makes into comma seperated string
temp$products <- lapply(temp$products,function(x){paste(unlist(x),collapse = ',')})

#splits comma seperated strings into column per product
#No one in this data has more than 4 different products
temp2 <- temp %>% separate("products",c('1','2','3','4'),sep=',')

#remove excess columns
temp2 <- data.frame(temp2[,c('1','2','3','4')])



transaction_data <- lapply(temp2,function(x){as.factor(x)})


transaction_data <- as(transaction_data,"transactions")

#generate number of unique values in each column (eg. 11 unique quantities, 3167 unique product names and 6325 unique transaction ids)
size(head(transaction_data))


LIST(head(transaction_data,2))












