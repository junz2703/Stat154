library(dplyr);library(caret);library(lubridate);library(ggplot2);library(data.table);library(rjson);library(cluster);library(Rtsne);library(tidyr);library(readr);library(stringr);library(jsonlite);library(tm)

setwd("~/Dropbox/yelp_challenge/")
source("~/Desktop/finda/analysis/function.R")
source("util.R")

# Load data
user <- fread("yelp_academic_dataset_user.csv.crdownload", stringsAsFactors = FALSE)
bus.tr <- fread("yelp_academic_dataset_business_train.csv", stringsAsFactors = FALSE, header = T)
bus.te <- fread("yelp_academic_dataset_business_test.csv", stringsAsFactors = FALSE, header = T)
tip <- fread("actual_yelp/yelp_dataset_challenge_round9/tip.csv", stringsAsFactors = FALSE)
tip <- tip %>% dplyr::filter(business_id %in% id.tot)
dist <- readRDS("rds/distance.all.final1.rds")
checkin <- fread("yelp_academic_dataset_checkin.csv", stringsAsFactors = FALSE)

# Business total
bus.te$V1 <- NULL;bus.tr$V1 <- NULL;bus.te$`Unnamed: 0` <- NULL
bus.te$stars <- NA
business <- dplyr::bind_rows(bus.tr, bus.te)

# id.tr <- unique(bus.tr$business_id)
# id.te <- unique(bus.te$business_id)
# id.tot <- c(id.tr, id.te)

# length(unique(tip$business_id))
# length(unique(checkin$business_id))

#
tip$date <- as.Date(tip$date)
tip$month <- month(tip$date)
tip$year <- year(tip$date)
tip <- tip %>% dplyr::group_by(business_id) %>% dplyr::summarise(tip_num = n(), tip_monthDist = n_distinct(month), tip_yearDist = n_distinct(year))


# checkin data
ch <- read.csv("busi_tst_cln.csv")
ch <- ch %>% dplyr::select(-X, -time)

# attribute data
tot <- dplyr::bind_rows(bus.tr, bus.te)
#att <- get_attribute(tot)
#att <- att %>% dplyr::select(business_id, Alcohol:Open24Hours)
att <- readRDS("rds/attribute.rds")

# Distance
h <- get_rest_dist_cat(0.1)
a <- get_rest_dist_cat(0.3)
b <- get_rest_dist_cat(0.5)
c <- get_rest_dist_cat(1)
i <- get_closest_rest_cat(1)
d <- get_closest_rest_cat(3)
e <- get_closest_rest_cat(5)
f <- get_closest_rest_cat(10)
g <- get_closest_rest_cat(20)

t <- a %>% dplyr::left_join(b, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(c, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(d, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(e, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(f, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(g, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(h, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(i, by = c("business_id" = "business_id"))
rm(a,b,c,d,e,f,g,h,i)

h <- get_rest_dist(0.1)
a <- get_rest_dist(0.3)
b <- get_rest_dist(0.5)
c <- get_rest_dist(1)
i <- get_closest_rest(1)
d <- get_closest_rest(3)
e <- get_closest_rest(5)
f <- get_closest_rest(10)
g <- get_closest_rest(20)

tt <- a %>% dplyr::left_join(b, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(c, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(d, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(e, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(f, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(g, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(h, by = c("business_id" = "business_id")) %>% 
  dplyr::left_join(i, by = c("business_id" = "business_id"))
rm(a,b,c,d,e,f,g,h,i)

# 100M, 300m, 500m, 1000m how many restaurants in the boundary
# closest 3, 5, 10 average distance


# aggregate based on business
business <- business %>% dplyr::select(business_id, stars, is_open, categories, city, review_count, state, stars)
business$categories <- tolower(business$categories)
business$cat <- ifelse(grepl("chinese", business$categories), "chiense",
                     ifelse(grepl("mexican", business$categories), "mexican",
                            ifelse(grepl("pizza", business$categories), "pizza", "coffee,tea,food")))
business$categories <- NULL

business$rank <- 1:nrow(business)
business$index <- c(rep("train", 2510), rep("test", 440))
#business$business_id <- id
#write.csv(business, "submission/business.hyung.csv", row.names = FALSE)

# left _ join 
business <- business %>% left_join(ch, by = c("business_id" = "business_id"))
business <- business %>% left_join(att, by = c("business_id" = "business_id"))
business <- business %>% left_join(t, by = c("business_id" = "business_id"))
business <- business %>% left_join(tip, by = c("business_id" = "business_id"))
business <- business %>% left_join(tt, by = c("business_id" = "business_id"))
business[is.na(business)] <- 0

rm(att, bus.te, bus.tr, ch, checkin, dist, t, tip, tot, user,tt)

#saveRDS(business, "rds/business.5.6.rds")
###################################################################################################################
# model validation 
library(data.table);library(dplyr);library(doParallel);library(caret)
detectCores()
registerDoParallel(cores = 8)

# split test and train
business <- readRDS("rds/business.5.6.rds")
id.te <- filter(business, index == "test")$business_id
id.tr <- filter(business, index == "train")$business_id
business$business_id <- NULL;business$rank <- NULL

train <- business %>% filter(index == "train")
test <- business %>% filter(index == "test")

# ind <- caret::createDataPartition(train$stars, p = 0.8, list = FALSE)
# test <- train[-ind,]
# train <- train[ind,]
tmp <- readRDS("~/Dropbox/")
tmp <- tmp %>% dplyr::select(business_id, scores)
train <- train %>% dplyr::left_join(tmp, by = c("business_id" = "business_id"))

train$index <- NULL;test$index <- NULL;train$stars <- as.numeric(train$stars);train$business_id <- NULL


# model 
tmp <- readRDS("~/Dropbox/val_grade.RDS")
model.gbm <- train(as.numeric(stars) ~., data = train, 
                   method = 'gbm') 
saveRDS(model.gbm, "rds/model.gbm.5.6.rds")
saveRDS(model.forest, "~/Dropbox/yelp_challenge/rds/model.forest.rds")
model.for <- train(as.numeric(stars) ~., data = train, 
                      method = 'cforest') 
saveRDS(model.svm, "~/Dropbox/yelp_challenge/rds/model.svm.rds")


pred <- predict(model.gbm, test, type = "raw")
pred <- predict(model.for, test, type = "raw")
sqrt(mean((test$stars - pred)^2))
sqrt(mean((test$stars - pred)^2))
varImp(model.gbm)

submit(id.te, pred)

model.gbm$bestTune

saveRDS(model.gbm, "~/Dropbox/yelp_challenge/rds/model.gbm.4.27.plain.rds")
model.gbm <- readRDS("~/Dropbox/yelp_challenge/rds/model.gbm.4.27.plain.rds")
pred <- predict(model.gbm, test, type = "raw")


sub <- submit(id.te, pred)
sub$df

write.csv(sub$df, "submission/5_5.csv", row.names = FALSE)
write.csv(business, "submission/business.csv", row.names = FALSE)
varImp(model.gbm)


id <- c(bus.tr$business_id, bus.te$business_id)

tmp <- tot %>% select(business_id, longitude, latitude, stars)
temp <- business
temp <- temp %>% dplyr::left_join(tmp, by = c("business_id" = "business_id"))
write.csv(temp, "submission/bus.csv", row.names = FALSE)



