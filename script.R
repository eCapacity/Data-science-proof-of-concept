#install package to link to GA if not already installed
install.packages('googleAnalyticsR')

#install anaysis packages
install.packages('arules')
install.packages('arulesViz')


#load packages
library(googleAnalyticsR)
library(arules)
library(arulesViz)
library(tidyr)
library(dplyr)



#sign into velux@ecapacity.dk Google Account
ga_auth()

#function to find start of last month
som <- function(x){
  as.Date(format(x,"%Y-%m-01"))
}
som(som(Sys.Date())-40)

#function to find end of last month
eom<- function(x){
  som(som(x)+35)-1
}
som(Sys.Date())-1


#pull data from GA (ID for German eshop)
ga_data <- google_analytics(viewId="37002615",
                              date_range=c(som(som(Sys.Date())-40),som(Sys.Date())-1),
                              metrics=c("ga:itemQuantity","ga:itemRevenue"),
                              dimensions=c("ga:productName","transactionId"),anti_sample=TRUE)


#subset data
new_data <- ga_data[,c("productName","transactionId")]

#modify skew to remove window size because that is specific to a user
for(i in 1: length(new_data$productName)){
  temp <- strsplit(as.character(ga_data$productName[i]),split=" ")
  if(length(temp[[1]])>2){
    new_data$productName[i] <- paste(temp[[1]][1],temp[[1]][3])
  }
}


# 
# #looks just at transactionId and product, not quantities and lists products per ttransaction
# temp <- new_data %>% group_by(transactionId) %>% summarise(products =list(productName))
# 
# #seperates list of products and makes into comma seperated string
# temp$products <- lapply(temp$products,function(x){paste(unlist(x),collapse = ',')})
# 
# #splits comma seperated strings into column per product
# #No one in this data has more than 4 different products
# temp2 <- temp %>% separate("products",c('1','2','3','4'),sep=',')
# 
# #remove excess columns
# temp2 <- data.frame(temp2[,c('1','2','3','4')])
# 
# 
# 
# transaction_data <- sapply(temp2,function(x){as.factor(x)})
# 
# 
# transaction_data <- as(transaction_data,"transactions")
# 
# #generate number of unique values in each column (eg. 11 unique quantities, 3167 unique product names and 6325 unique transaction ids)
# size(head(transaction_data))
# 
# 
# LIST(head(transaction_data,2))

#changing the data to transaction data
trans <- as(split(new_data[,"productName"],new_data[,"transactionId"]),"transactions")

#see which items are purchased the most frequently (just number of times it appears in transaction (not quantity))
frequentItems <- eclat(trans,parameter = list(supp=0.001,maxlen=15))
inspect(head(frequentItems))
itemFrequencyPlot(trans, topN=10,type="absolute",main="Item Frequency")

#generate rules
rules <- apriori(trans, parameter = list(support = 0.001, confidence = 0.05,minlen=2))

#plot rules
plot(rules)

#view first 6 rules
inspect(head(rules))

#sort rules by lift amd view top 6 rules
rules_lift <- sort (rules, by="lift", decreasing=TRUE)
inspect(head(rules_lift)) 

#find rules when ZZZ 220 is purchased
rules <- apriori (data=trans, parameter=list (supp=0.001,conf = 0.05,minlen=2), appearance = list (default="rhs",lhs="MHL 5060D"), control = list (verbose=F))

#sort and view top rules by confidence
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

