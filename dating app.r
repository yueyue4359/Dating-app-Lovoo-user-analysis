#explanatory analysis
data_full<- read.csv('D:\\Downloads\\lovoo_v3_users_api-results.csv')

data <- data_full[, c(2,3,6,7,25,11,12,13)]
head(data)
summary(data$flirtInterests_date)

High = ifelse(data$counts_profileVisits >3705, 'Yes','No')
data=data.frame(data, High)
data$High <- as.factor(data$High)
data= data[-c(4)]

data<- data[data$genderLooking != 'none', ] 
data$genderLooking = ifelse(data$genderLooking=='M', 1, 0)

data$flirtInterests_chat = toupper(data$flirtInterests_chat)
data$flirtInterests_friends = toupper(data$flirtInterests_friends)
data$flirtInterests_date = toupper(data$flirtInterests_date)
data$genderLooking = as.factor(data$genderLooking)
data$flirtInterests_chat = as.factor(data$flirtInterests_chat)
data$flirtInterests_friends = as.factor(data$flirtInterests_friends)
data$flirtInterests_date = as.factor(data$flirtInterests_date)

class(data$flirtInterests_chat)
summary(data$counts_pictures)
      
summary(data$lang_count)

head(data)
dim(data)

set.seed(1)
training.obs <- sample(1:3626,2900)
train <- data[training.obs,]
dim(train)
test<- data[-training.obs,]
dim(test)


library(rpart)
library(rpart.plot)

fit <- rpart(High~., data = train, method='class',parms=list(split='gini'),
             control = rpart.control(xval = 10)) 

print(fit)
rpart.plot(fit, fallen.leaves=FALSE)
fit$variable.importance
plotcp(fit)
printcp(fit)
fit2=prune(fit,cp=0.01)
rpart.plot(fit2, fallen.leaves=FALSE)

pred<- predict(fit2, test,type="class")
length(pred)

table(pred[0:20])
table(pred[0:20],test$High[0:20])
conf.matrix <- table(test$High, pred)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

write.csv(data,"D:\\Downloads\\datingapp_data.csv", row.names = FALSE)

data_y=head(test$High, n=20)
data_x=tail(test$genderLooking,test$age, test$counts_pictures, test$lang_count,
            test$flirtInterests_chat, test$flirtInterests_friends, test$flirtInterests_date,n=20)
pred<- predict(fit,data_x)
pred
tail(test$High, n=20)
pred[0:20]

install.packages("writexl")
library("writexl") 
write_xlsx(data, 'C:\\Users\\dapao\\Desktop\\New Microsoft Excel Worksheet (2).xlsx')
boxplot(data$age, data$counts_pictures, data$lang_count)
data.frame(unclass(summary(data)))

