get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = distance_list[[1]]
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function. 
  }
  distance
}

attr = function(lst, table, num) {
  need = function(char) {
    if (char %in% c(letters, LETTERS)) {
      return(TRUE)
    } else {
      return(!(is.na(as.numeric(char)))) 
    }
  }
  
  find = function(txt, ind, dir) {
    ind1 = ind + dir
    while(!need(substr(text, ind1, ind1))) {
      if(substr(text, ind1, ind1) == "{") {
        return("")
      } else {
        ind1 = ind1 + dir
      }
    }
    
    ind2 = ind1 + dir
    while(need(substr(text, ind2, ind2))) {
      ind2 = ind2 + dir
    }
    ind2 = ind2 - dir
    
    return(substr(txt, min(ind1, ind2), max(ind1, ind2)))
  }
  
  proper = function(str) {
    return(paste0(toupper(substr(str, 1, 1)), substring(str, 2)))
  }
  
  text = factor(lst)
  posi = 1
  while(substr(text, posi, posi) != "") {
    if (substr(text, posi, posi) == ":") {
      col = proper(find(text, posi, -1))
      val = tolower(find(text, posi, 1))
      if (val != "") {
        if (val %in% c("true", "yes", "formal", "full", "free", "very", "4")) {
          val = 1
        } else if (val %in% c("loud", "3")) {
          val = 0.66
        } else if (val %in% c("dressy", "beer", "paid")) {
          val = 0.5
        } else if (val %in% c("average", "2")) {
          val = 0.33
        } else if (val %in% c("false", "no", "casual", "none", "no", "quiet", "1"))  {
          val = 0
        }
        if (!(col %in% colnames(table))) {
          table[,col] = as.numeric(NA)
        }
        table[num,col] = val
      }
    }
    posi = posi + 1
  }
  return(table)
}

#read.csv("yelp_academic_dataset_business_train.csv")
get_attribute <- function(busi_att) {
  for(i in 1:nrow(busi_att)) {
    busi_att = attr(busi_att$attributes[i], busi_att, i)
  }
  return(busi_att)
}


get_rest_dist <- function(distance) {
  
  # Prepare Data
  dist <- readRDS("~/Dropbox/yelp_challenge/rds/distance.all.final.rds")
  tr <- fread("~/Dropbox/yelp_challenge/yelp_academic_dataset_business_train.csv", stringsAsFactors = FALSE)
  te <- fread("~/Dropbox/yelp_challenge/yelp_academic_dataset_business_test.csv", stringsAsFactors = FALSE)
  tot <- dplyr::bind_rows(tr,te)
  tot <- tot %>% select(business_id)
  tot$rank <- as.character(1:nrow(tot))
  
  # Wrangling
  tmp <- dist %>% filter(dist <= distance)
  a <- tmp %>% dplyr::group_by(p1) %>% dplyr::summarise(num_rest = n(), ave_dist = mean(dist, na.rm = TRUE))
  a$p1 <- as.character(a$p1)
  tot <- tot %>% dplyr::left_join(a, by = c("rank" = "p1"))
  tot[is.na(tot)] <- 0
  tot$rank <- NULL
  
  names(tot)  <- c("business_id", paste0("num_rest_",distance, "km"), paste0("avg_dist", distance, "km"))
  
  return(tot)
  
}

get_closest_rest <- function(number, cat = "all") {
  # Prepare Data
  dist <- readRDS("~/Dropbox/yelp_challenge/rds/distance.all.final.rds")
  tr <- fread("~/Dropbox/yelp_challenge/yelp_academic_dataset_business_train.csv", stringsAsFactors = FALSE)
  te <- fread("~/Dropbox/yelp_challenge/yelp_academic_dataset_business_test.csv", stringsAsFactors = FALSE)
  tot <- dplyr::bind_rows(tr,te)
  tot <- tot %>% select(business_id)
  tot$rank <- as.character(1:nrow(tot))
  
  # Wrangle
  dist <- dist %>% dplyr::arrange(p1, dist)
  temp <- dist[0,]
  for (i in 1:number) {
    ind <- duplicated(dist$p1)
    a <- dist[!ind,]
    dist <- dist[ind,]
    temp = dplyr::bind_rows(temp, a)
  }
  
  temp <- temp %>% group_by(p1) %>% dplyr::summarise(avg_dist = mean(dist, na.rm = TRUE))
  temp$p1 <- as.character(temp$p1) 
  tot <- tot %>% dplyr::left_join(temp, by = c("rank" = "p1"))
  tot[is.na(tot)] <- 0
  tot$rank <- NULL
  
  names(tot)  <- c("business_id", paste0("avg_dist_",number, "rest"))
  
  return(tot)
  
}

get_rest_dist_cat <- function(distance) {
  
  # Prepare Data
  dist <- readRDS("rds/dist.renew.rds")
  tr <- fread("~/Dropbox/yelp_challenge/yelp_academic_dataset_business_train.csv", stringsAsFactors = FALSE)
  te <- fread("~/Dropbox/yelp_challenge/yelp_academic_dataset_business_test.csv", stringsAsFactors = FALSE)
  tot <- dplyr::bind_rows(tr,te)
  tot <- tot %>% select(business_id)
  tot$rank <- as.character(1:nrow(tot))
  dist <- dist %>% filter(cat1 == cat2)
  
  # Wrangling
  tmp <- dist %>% filter(dist <= distance)
  a <- tmp %>% dplyr::group_by(p1) %>% dplyr::summarise(num_rest = n(), ave_dist = mean(dist, na.rm = TRUE))
  a$p1 <- as.character(a$p1)
  tot <- tot %>% dplyr::left_join(a, by = c("rank" = "p1"))
  tot[is.na(tot)] <- 0
  tot$rank <- NULL
  
  names(tot)  <- c("business_id", paste0("num_rest_",distance, "km"), paste0("avg_dist", distance, "km"))
  
  return(tot)
  
}

get_closest_rest_cat <- function(number, cat = "all") {
  # Prepare Data
  dist <- readRDS("rds/dist.renew.rds")
  tr <- fread("~/Dropbox/yelp_challenge/yelp_academic_dataset_business_train.csv", stringsAsFactors = FALSE)
  te <- fread("~/Dropbox/yelp_challenge/yelp_academic_dataset_business_test.csv", stringsAsFactors = FALSE)
  tot <- dplyr::bind_rows(tr,te)
  tot <- tot %>% select(business_id)
  tot$rank <- as.character(1:nrow(tot))
  dist <- dist %>% filter(cat1 == cat2)
  
  # Wrangle
  dist <- dist %>% dplyr::arrange(p1, dist)
  temp <- dist[0,]
  for (i in 1:number) {
    ind <- duplicated(dist$p1)
    a <- dist[!ind,]
    dist <- dist[ind,]
    temp = dplyr::bind_rows(temp, a)
  }
  
  temp <- temp %>% group_by(p1) %>% dplyr::summarise(avg_dist = mean(dist, na.rm = TRUE))
  temp$p1 <- as.character(temp$p1) 
  tot <- tot %>% dplyr::left_join(temp, by = c("rank" = "p1"))
  tot[is.na(tot)] <- 0
  tot$rank <- NULL
  
  names(tot)  <- c("business_id", paste0("avg_dist_",number, "rest"))
  
  return(tot)
  
}

