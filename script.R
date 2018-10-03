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
library(ggplot2)



#sign into velux@ecapacity.dk Google Account
ga_auth(new_user = T)

#function to find start of last month
som <- function(x){
  as.Date(format(x,"%Y-%m-01"))
}
som(som(Sys.Date())-70)

#function to find end of last month
eom<- function(x){
  som(som(x))-1
}
eom(Sys.Date()-10)


#pull data from GA 
ga_data <- google_analytics(viewId="37002615",
                              date_range=c(som(som(Sys.Date())-70),eom(Sys.Date()-10)),
                              metrics=c("ga:itemQuantity","ga:itemRevenue"),
                              dimensions=c("ga:productName","transactionId"),anti_sample=TRUE)



#Fortnum GA ID 93647780
#VELUX DE Shop GA ID 37002615


#subset data
new_data <- data.frame(ga_data[,c("productName","transactionId")])

#modify sku to keep blind type and color
for(i in 1: length(new_data$productName)){
  temp <- strsplit(as.character(ga_data$productName[i]),split=" ")
  if(length(temp[[1]])>2){
    new_data$productName[i] <- paste(temp[[1]][1],temp[[1]][3])
  }
}

#modify sku to keep just blind type
for(i in 1: length(new_data$productName)){
  temp <- strsplit(as.character(ga_data$productName[i]),split=" ")
  if(length(temp[[1]])>1){
    new_data$productName[i] <- temp[[1]][1]
  }
}


####Analysis####

#changing the data to transaction data
trans <- as(split(new_data[,"productName"],new_data[,"transactionId"]),"transactions")

#see which items are purchased the most frequently (just number of times it appears in transaction (not quantity))
frequentItems <- eclat(trans,parameter = list(supp=0.001,maxlen=15))
inspect(head(frequentItems))
itemFrequencyPlot(trans, topN=5,type="absolute",main="Item Frequency")

#generate rules
#multiple items in basket
rules <- apriori(trans, parameter = list(support = 0.001, confidence = 0.05,minlen=2))

#any number of items in basket
rules <- apriori(trans, parameter = list(support = 0.001, confidence = 0.05))

#only 1 item in cart
rules <- apriori(trans, parameter = list(support = 0.0001, confidence = 0.005, maxlen=1))

#sort and view top rules by confidence
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))


#Scatterplot
plot(rules, method = "scatterplot",engine='htmlwidget')


#circle chart
plot(rules,method="graph",control=list(layout=igraph::in_circle()))
plot(rules,method="graph")



#find rules when MHL is purchased
rules <- apriori (data=trans, parameter=list (supp=0.001,conf = 0.005,minlen=2), appearance = list (default="rhs",lhs="MHL"))


#sort and view top rules by confidence
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

#circle chart
plot(rules,method="graph",control=list(layout=igraph::in_circle()))


#Scatterplot
plot(rules, method = "scatterplot",engine='htmlwidget')


#confidence vs number of rules
j<-1
rule_details<-data.frame(matrix(NA,length(seq(0.05,1,by=.05)),2))
colnames(rule_details)<-c('confidence','number_of_rules')
for(i in seq(0.05,1,by=.05)){
  
  rules <- apriori (data=trans, parameter=list (supp=0.001,conf = i))
  
  rule_details$confidence[j]<-i
  rule_details$number_of_rules[j]<-length(size(rules))
  
  j<-j+1
  }


ggplot(rule_details, aes(x = confidence, y = number_of_rules)) + 
  geom_point() + geom_line()



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
#
#
# 
# #plot rules
# plot(rules)
# 
# #view first 6 rules
# inspect(head(rules))
# 
# 
# subrules <- subset(rules, count>15)
# rules_conf <- sort (subrules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
# inspect(head(rules_conf))
# 
# 
# plot(rules,method="graph",control=list(layout=igraph::in_circle()))
# 
# 
# plot(rules, method="matrix3D", measure="lift")
# 
# 
# plot(rules, method="grouped")
# 
# 
# plot(rules, method = "paracoord")
# 
# 
# onerule<-sample(rules,1)
# plot(onerule, method = "doubledecker",data = trans)
# 
# 
# 
# 
# ## for itemsets
# itemsets <- eclat(trans, parameter = list(support = 0.001, minlen=2))
# plot(itemsets)
# plot(itemsets, method="graph")
# plot(itemsets, method="paracoord", control=list(alpha=.05, reorder=TRUE))
# 
# ## add more quality measures to use for the scatterplot
# quality(itemsets) <- interestMeasure(itemsets, trans=trans)
# head(quality(itemsets))
# plot(itemsets, measure=c("support", "allConfidence"), shading="lift",engine='htmlwidget')

