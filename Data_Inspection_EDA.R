setwd("/Users/hyung/documents/class/Spring 2017/Stat 154 - Machine Learning")
#
review_train<-read.csv('yelp/yelp_academic_dataset_review_train.csv')
review_test<-read.csv('yelp/yelp_academic_dataset_review_test.csv')
user<-read.csv('yelp/yelp_academic_dataset_user.csv')
checkin<-read.csv('yelp/yelp_academic_dataset_checkin.csv')
business_train<-read.csv('yelp/yelp_academic_dataset_business_train.csv')
business_test<-read.csv('yelp/yelp_academic_dataset_business_test.csv')
tip<-read.csv('yelp/yelp_academic_dataset_tip.csv')

train <- business_train

names(train)
ggplot(train, aes(x = avg_dist_3rest, y = stars)) + geom_jitter()
ggplot(train, aes(x = log(avg_dist_3rest))) + geom_histogram()
ggplot(train, aes(x = avg_dist_5rest, y = stars)) + geom_jitter()
ggplot(train, aes(x = avg_dist_10rest, y = stars)) + geom_jitter()
ggplot(train, aes(x = num_rest_0.1km, y = stars)) + geom_jitter()

ggplot(train, aes(x = num_rest_0.5km, y = stars)) + geom_jitter()
ggplot(train, aes(x = num_rest_1km, y = stars)) + geom_jitter()

a <- ggplot(train, aes(x = avg_dist_3rest)) + geom_histogram() + ggtitle("Avg Dist") + xlab("Avg Dist(Km) of 3 Rest")
a
b <- ggplot(train, aes(x = log(avg_dist_3rest))) + geom_histogram() + ggtitle("Avg Distance (Log)") + xlab("Avg Dist(Km) of 3 Rest")
b
multiplot(a,b, cols = 2)
a <- ggplot(train, aes(x = num_rest_0.3km, y = stars)) + geom_jitter() + xlab("number of restaurants") + ggtitle("Number of restaurants within 300m in each Category")
a
ggplot(train, aes(x = avg_dist_3rest)) + geom_histogram(aes(fill = stars))
ggplot(train, aes(x = avg_dist_1rest)) + geom_histogram(aes(fill = stars))
varImp(model.gbm)

names(train)

ggplot(train, aes(x = rank, y = stars)) + geom_point() 
ggplot(train, aes(x = Alcohol, y = stars)) + geom_jitter()

c <- ggplot(train, aes(x = Alcohol, y = stars)) + geom_jitter(width = 0.1) + ggtitle("Rests Selling Alcohol")
d <- ggplot(train, aes(x = NoiseLevel, y = stars)) + geom_jitter(width = 0.1) + ggtitle("Rests with Noiselevel") 
multiplot(c,d, cols = 2)
names(train)


model.gbm <- readRDS("rds/")
model.for <- readRDS("rds/model.forest.rds")
model.gbm
pred.gbm <- predict(model.gbm, test)
pred.for <- predict(model.for, test)

gbm.rm <- submit(id.te, pred.gbm)
#svm.rm <- submit(id.te, model.svm)
for.rm <- submit(id.te, pred.for)
for.rm$rmse
gbm.rm$rmse

plot <- data.frame(model = c("GBM", "SVM", "RandomForest"), rmse = c(gbm.rm, svm.rm, for.rm))
ggplot(plot, aes(x = model, y = rmse)) + geom_bar()

