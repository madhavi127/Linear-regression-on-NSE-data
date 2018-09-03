# ** OBJECTIVE ** 

scrip.data <- read.csv("D:/Rclass/idea/idea_90 (1).csv")


#Replace whitespaces in the column names with underscore


#2.numeric columns have COMMA, hence are stored as factors(vectors + levles) instead of numeric vector
# replace the COMMA with BLANK, convert each item in vector to numeric value - > done for 
# all columns in a loop using lapply() function, but as out of lappy fn is LIST, convert it back to data.frame
scrip.data.new <- data.frame(lapply(scrip.data[4:15], function(x) { as.numeric(gsub(",", "", x)) }))

#2. Strip only columns required

scrip.data.new <- scrip.data.new[c(2:4,7)]

plot(scrip.data.new$Open.Price, scrip.data.new$High.Price)



#plots the scatter graph for all columns with each other


#Identify the split for train(75%) and test data(25%), using sample() funtion.
split <- sample(seq_len(nrow(scrip.data.new)), size = floor(0.75 * nrow(scrip.data.new)))



spli#divide the data set in tran and test data sets based on split we identified in above step
trainData <- scrip.data.new[split, ]
testData <- scrip.data.new[-split, ]

# train the model (multi linear), to predic CLOSE wrt all oter columns in traindata(open,high,low)
fm1 <- lm(High.Price ~ Open.Price, data = trainData)
#fm2<- lm(High.Price ~ Open.Price, data = scrip.data.new)


with(trainData,plot(Open.Price, High.Price))
abline(fm1, col='red')


# view summary of the model predicted (Intercepts, slope, t/p values, )
summary(fm1)

# use predict() function to prform prediction on the model using testdata
prediction <- predict(fm1,newdata = testData, interval='confidence')
#predictionn <- predict(fm1,newdata = testData)

#view predicted values and actual values sid by side in data frame 
p <- data.frame(prediction, testData$High.Price)
p


# use predict() function to prform prediction on the model using testdata
prediction1 <- predict(fm1,newdata = scrip.data.new, interval='confidence')

#view predicted values and actual values sid by side in data frame 
p1 <- data.frame(prediction1, scrip.data.new$High.Price)
cbind(Date=scrip.data[,3], p1)

