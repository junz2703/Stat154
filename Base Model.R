library(dplyr)
library(tm)
library(slam)
review <- review_train #your review_trian data
get_tdm <- function(review) {
  library(dplyr)
  library(tm)
  star_5_reviews <-
    review %>% filter(review$stars == 5)
  star_4_reviews <-
    review %>% filter(review$stars == 4)
  star_3_reviews <-
    review %>% filter(review$stars == 3)
  star_2_reviews <-
    review %>% filter(review$stars == 2)
  star_1_reviews <-
    review %>% filter(review$stars == 1)
  star_5_corp <- tm::Corpus(VectorSource(star_5_reviews$text))
  star_4_corp <- tm::Corpus(VectorSource(star_4_reviews$text))
  star_3_corp <- tm::Corpus(VectorSource(star_3_reviews$text))
  star_2_corp <- tm::Corpus(VectorSource(star_2_reviews$text))
  star_1_corp <- tm::Corpus(VectorSource(star_1_reviews$text))
  star_5_corp <- tm_map(star_5_corp, removeWords , stopwords("english")) 
  star_4_corp <- tm_map(star_4_corp, removeWords , stopwords("english")) 
  star_3_corp <- tm_map(star_3_corp, removeWords , stopwords("english")) 
  star_2_corp <- tm_map(star_2_corp, removeWords , stopwords("english")) 
  star_1_corp <- tm_map(star_1_corp, removeWords , stopwords("english"))
  #tdm
  star_5_tdm <- tm::TermDocumentMatrix(star_5_corp)
  star_4_tdm <- tm::TermDocumentMatrix(star_4_corp)
  star_3_tdm <- tm::TermDocumentMatrix(star_3_corp)
  star_2_tdm <- tm::TermDocumentMatrix(star_2_corp)
  star_1_tdm <- tm::TermDocumentMatrix(star_1_corp)
  # sparse = 0.999, only terms occurring in 0.1% of the documents were retained.
  #	nrow(review_train) == 116474; 116474 * 0.001 = 116.474; terms occuring more than 116 documents are retained
  star_1_tdm<-	removeSparseTerms(star_1_tdm, 0.999) 
  star_2_tdm<-	removeSparseTerms(star_2_tdm, 0.999)
  star_3_tdm<-	removeSparseTerms(star_3_tdm, 0.999)
  star_4_tdm<-	removeSparseTerms(star_4_tdm, 0.999)
  star_5_tdm<-	removeSparseTerms(star_5_tdm, 0.999)
  gimme_dic <- function (tdm, df, score) {
    gimme_wordlist <- function(tdm) {
      tdm <- rowSums(as.matrix(tdm))
      tdm <- tdm[order(tdm, decreasing = T)]
      return(tdm)
    }
    df<-data.frame()
    gimme_freq <- function(tdm, df, score) {
      df <- as.data.frame(tdm)
      df$word <- unlist(attributes(tdm))
      df$score <- score
      colnames(df)[1] <- 'count'
      return(df)
    }
    return(gimme_freq(gimme_wordlist(tdm), df, score))
  }
  star_1_dic<-gimme_dic(star_1_tdm, df, 1)
  star_2_dic<-gimme_dic(star_2_tdm, df, 2)
  star_3_dic<-gimme_dic(star_3_tdm, df, 3)
  star_4_dic<-gimme_dic(star_4_tdm, df, 4)
  star_5_dic<-gimme_dic(star_5_tdm, df, 5)
  star_dic <- rbind(star_1_dic, star_2_dic, star_3_dic, star_4_dic, star_5_dic)
  star_dic$weight <- star_dic$count * star_dic$score
  dic<-star_dic %>%
    select(word, count, score, weight) %>%
    group_by(word) %>%
    summarise(mean = sum(weight) / sum(count),
              count = sum(count)) %>%
    arrange(desc(count)) %>%
    filter(count > max(count)*0.001) 
  #max(count) * 0.001 = 113.007; words appreared more than 113 times
  #61464 words --> max(count) * 0.001 --> 4009
  return(dic)
}
word_dic <- get_tdm(review_train)
saveRDS(word_dic, 'word_dic.RDS')

lets_grade <- function(review_data) {
  star_corp <- Corpus(VectorSource(review_data$text))
  star_corp <- tm_map(star_corp, removeWords , stopwords("english"))
  star_dtm <- tm::DocumentTermMatrix(star_corp)
  star_dtm <-  as.data.frame((as.matrix(star_dtm)))
  score_wgt<- apply(star_dtm, 1, sum)
  change_colname<-function(data) {for(i in 1:ncol(data)) {
    if (colnames(data)[i] %in% word_dic$word) {
      colnames(data)[i]<-word_dic$mean[word_dic$word==colnames(data)[i]]
    }
    else  colnames(data)[i]<-0
  }
    return(data)
  }
  star_dtm <- change_colname(star_dtm)
  for (i in 1:ncol(star_dtm)) {
    star_dtm[i] <- as.numeric(colnames(star_dtm)[i])*star_dtm[i] 	
  }
  star_dtm$score <- apply(star_dtm, 1, sum)
  for (i in 1:nrow(star_dtm)) {
    star_dtm$score[i] <- star_dtm$score[i]/score_wgt[i]
  }
  return(star_dtm$score)
}

#sorting commonly used words close to 'positivity'
diff <- function (x,num) {
  diff <- (x$mean - num)^2
  return(diff)
}
close5<-word_dic %>%
  arrange((diff(word_dic, 5))) %>%
  head(5)
close4<-word_dic %>%
  arrange((diff(word_dic, 4))) %>%
  head(5)
close3<-word_dic %>%
  arrange((diff(word_dic, 3))) %>%
  head(5)
close2<-word_dic %>%
  arrange((diff(word_dic, 2))) %>%
  head(5)
close1<-word_dic %>%
  arrange((diff(word_dic, 1))) %>%
  head(5)

ptme <- proc.time()
test_grade <- lets_grade(review_test) 
val_grade <- lets_grade(val_test)
proc.time() - ptme

val_test$score <-val_grade
review_test$score <- test_grade

val<-val_test %>%
  group_by(business_id) %>%
  summarise(score = mean(score))
test <- review_test %>%
  group_by(business_id) %>%
  summarise(score = mean(score))

get_rmse <- function(x,y) {
  sqrt(mean((x-y)^2))
}
val_result <- merge(business_train, val, by= 'business_id')
test_result <- merge(business_test, test, by= 'business_id')
get_rmse(val_result$stars, val_result$score) #0.7222058

saveRDS(val_result$score, 'val_grade.RDS')
saveRDS(test_result$score, 'test_grade.RDS')