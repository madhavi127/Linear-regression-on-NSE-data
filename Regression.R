# ** OBJECTIVE ** 

scrip.data <- read.csv("D:/Rclass/idea/idea_90 (1).csv")


#Replace whitespaces in the column names with underscore
# names(scrip.data) <- gsub(" ", "_", names(scrip.data))

#2.numeric columns have COMMA, hence are stored as factors(vectors + levles) instead of numeric vector
# replace the COMMA with BLANK, convert each item in vector to numeric value - > done for 
# all columns in a loop using lapply() function, but as out of lappy fn is LIST, convert it back to data.frame
scrip.data.new <- data.frame(lapply(scrip.data[4:15], function(x) { as.numeric(gsub(",", "", x)) }))

#2. Strip only columns required
# Note, relation Close ~ LTP is called multicollinearity
scrip.data.new <- scrip.data.new[c(2:4,7)]

plot(scrip.data.new$Open.Price, scrip.data.new$High.Price)

#Perfom log(x) on outcome variable if residuals are not normally distributed
# predictions need to be converted to exp(x)
#scrip.data.new$High.Price <- log(scrip.data.new$High.Price)


#plots the scatter graph for all columns with each other
#pairs(scrip.data.new, main = "infy")

#Identify the split for train(75%) and test data(25%), using sample() funtion.
split <- sample(seq_len(nrow(scrip.data.new)), size = floor(0.85 * nrow(scrip.data.new)))



spli#divide the data set in tran and test data sets based on split we identified in above step
trainData <- scrip.data.new[split, ]
testData <- scrip.data.new[-split, ]

# train the model (multi linear), to predic CLOSE wrt all oter columns in traindata(open,high,low)
fm1 <- lm(High.Price ~ Open.Price, data = trainData)
#fm2<- lm(High.Price ~ Open.Price, data = scrip.data.new)


with(trainData,plot(Open.Price, High.Price))
abline(fm1, col='red')

#legend("topleft", bty="n", legend=paste('y =', coef(fm1)[[2]], '* x', '+', coef(fm1)[[1]], '-- ', "R2 is", format(summary(fm1)$adj.r.squared, digits=4)))


# view summary of the model predicted (Intercepts, slope, t/p values, )
summary(fm1)

# use predict() function to prform prediction on the model using testdata
prediction <- predict(fm1,newdata = testData, interval='confidence')
#predictionn <- predict(fm1,newdata = testData)

#view predicted values and actual values sid by side in data frame 
p <- data.frame(prediction, testData$High.Price)
p

#cbind(Date=scrip.data[c(3,4,12,13,19,24,25,33,46,53),3], p)

# if we have Perfomed log(x) on outcome variable if residuals are not normally distributed
# predictions need to be converted to exp(x)
#p <- data.frame(lapply(p, function(x) {exp(x)}))
#p

# use predict() function to prform prediction on the model using testdata
prediction1 <- predict(fm1,newdata = scrip.data.new, interval='confidence')

#view predicted values and actual values sid by side in data frame 
p1 <- data.frame(prediction1, scrip.data.new$High.Price)
cbind(Date=scrip.data[,3], p1)

"
# use predict() function to prform user defined data
user_data <- data.frame(Open.Price=50.2, High.Price=51.65, Low.Price=49.2, Average.Price=50.34)
prediction2 <- predict(fm1, newdata = user_data, interval='confidence')
prediction2
#view predicted values and actual values sid by side in data frame 
p2 <- data.frame(prediction2, log(user_data$High.Price))
p2
# if we have Perfomed log(x) on outcome variable if residuals are not normally distributed
# predictions need to be converted to exp(x)
p2 <- data.frame(lapply(p2, function(x) {exp(x)}))
p2
"


