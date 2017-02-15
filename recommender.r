#https://muffynomster.wordpress.com/2015/06/07/building-a-movie-recommendation-engine-with-r/
#https://github.com/iamishalkin/airbnbrecommender
library(recommenderlab)

#1 try with spb data
#create item matrix at first without preprocessing
library(readr)
spbaprt <- read_csv("Data-city/Saint-Petersburg--Russia.csv")
#df<- read_csv("Saint-Petersburg--Russia.csv")
selectcolumns=function(df){
  library(dplyr)
  df=df[,1:77]
  df=dplyr::filter(df, native_currency=='RUB')
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                         as.factor)
  df <- df[!duplicated(df$apart_id),]
  #df <- df[order(df$apart_id),]
  df=as.data.frame(df)
  rownames(df)=df$apart_id
  df=subset(df, select=-c(apart_name,apart_id, city,description, special_offer,Paid_parking_off_premises, in_building, host_id,lat,lng, native_currency))
}
spbapart=selectcolumns(spbaprt)

#now users
#load reviews

review <- read_csv("Data-review/Review_Saint-Petersburg--Russia.csv")
library(reshape2)
user_apart_df=dcast(review, apart_id ~ author_id, length)

#Now we should delete Aparts, that have never been rated
apart_for_user_profile_matrix=spbapart[rownames(user_apart_df),]


#HELLO!
