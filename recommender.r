#https://muffynomster.wordpress.com/2015/06/07/building-a-movie-recommendation-engine-with-r/
#https://github.com/iamishalkin/airbnbrecommender
library(recommenderlab)

#1 try with spb data
#create item matrix at first without preprocessing
library(readr)

spbapart <- read_csv("Data-city/Saint-Petersburg--Russia.csv")
mskapart=read_csv("Data-city/Moscow--Russia.csv")
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
  df=subset(df, select=-c(apart_name, city,description, special_offer,Paid_parking_off_premises, in_building, host_id,lat,lng, native_currency))
}
spbapart=selectcolumns(spbapart)
mskapart=selectcolumns(mskapart)
one_city_apart_fix_price=function(apart){
 apart$fixed_price=apart$price_native- mean(apart$price_native)
 return(apart)
}
mskapart=one_city_apart_fix_price(mskapart)
spbapart=one_city_apart_fix_price(spbapart)
aparts=rbind(mskapart, spbapart)
#now users
#load reviews

spbreview <- read_csv("Data-review/Review_Saint-Petersburg--Russia.csv")


mskreview=read_csv("Data-review/Review_Moscow--Russia.csv")
review=rbind(spbreview,mskreview)

user_reviews=review[,1:2]

user_reviews=inner_join(user_reviews, aparts, by='apart_id')

library(reshape2)
user_apart_df=dcast(review, apart_id ~ author_id, length)

#Now we should delete Aparts, that have never been rated
apart_for_user_profile_matrix=spbapart[rownames(user_apart_df),]



#Now get user profile matrix by dot product





#Hello world

#HELLO:)
