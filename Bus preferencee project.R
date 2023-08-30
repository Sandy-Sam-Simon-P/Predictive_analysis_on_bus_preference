library(readxl)
data = read_excel(file.choose())

#structure of data
str(data)




#subset(mode of transport is bus)
i.prefer.bus = subset(data,data$`MODE OF TRANSPORT?`== "Bus")
View(i.prefer.bus)
bus = i.prefer.bus

#removing UNWANTED variables
bus$Timestamp = NULL
bus$NAME = NULL
bus$AGE = NULL
bus$GENDER = NULL
bus$`DESIGNATION?` = NULL
bus$`HOW OFTEN DO YOU TRAVEL IN A MONTH?` = NULL
bus$`MODE OF TRANSPORT?` = NULL
bus$`WHAT WOULD YOU LIKE TO DO WHILE TRAVELING?` = NULL
bus$REASON = NULL
View(bus)
final.data = bus

#converting characters  to numeric private = 1 and govt = 2
change1 = replace(final.data,final.data == "Private buses",1)
changed.data = replace(change1,change1 == "Government buses",2)
View(changed.data)
for(i in 1:ncol(changed.data)){
  changed.data[ , i] = as.numeric(changed.data[ ,i],na.rm = TRUE)
}
#checking missing values
any(is.na(changed.data))
str(changed.data)


# //building model//
#training and testing sets
trainingset = changed.data[1:60,1:6]
testingset = changed.data[61:96,1:6]
View(trainingset)
View(testingset)

#training and testing outcomes
trainingoutcome = changed.data[1:60,7]
testingoutcome = changed.data[61:96,7]
print(trainingoutcome)

#applying Knn algorithm
library(class)
predictions = knn(train = trainingset, cl =trainingoutcome, k = 8,test = testingset)
predictions

vis = table(testingoutcome,predictions)
print(vis)
#visualization
#barplot
colours = c("red","blue")
regions = c("private","public")
barplot(vis,col = colours,besides = TRUE)
legend("topleft",regions,cex=0.4,fill = colours)
#pie chart
pie(vis,labels = c("private","wrongly predicted","null","govt"),col = c("red","green","yellow","blue"))

#accuracy
actual_preds = data.frame(cbind(actuals = testingoutcome,predicted = predictions))
correlation_accuracy = cor(actual_preds)
print(actual_preds)
print(correlation_accuracy)



