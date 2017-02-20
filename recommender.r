#https://muffynomster.wordpress.com/2015/06/07/building-a-movie-recommendation-engine-with-r/
#https://github.com/iamishalkin/airbnbrecommender
library(recommenderlab)

#1 try with spb data
#create item matrix at first without preprocessing
library(readr)
library(dplyr)
spbapart <- read_csv("Data-city/Saint-Petersburg--Russia.csv")
mskapart=read_csv("Data-city/Moscow--Russia.csv")
klgapart=read_csv("Data-city/Kaliningrad--Kaliningrad-Oblast--Russia.csv")
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
klgapart=selectcolumns(klgapart)
one_city_apart_fix_price=function(apart){
 apart$fixed_price=apart$price_native- mean(apart$price_native)
 return(apart)
}
mskapart=one_city_apart_fix_price(mskapart)
spbapart=one_city_apart_fix_price(spbapart)
klgapart=one_city_apart_fix_price(klgapart)
aparts=rbind(mskapart, spbapart, klgapart)
#now users
#load reviews

spbreview <- read_csv("Data-review/Review_Saint-Petersburg--Russia.csv")


mskreview=read_csv("Data-review/Review_Moscow--Russia.csv")
review=rbind(spbreview,mskreview)
review <- review[!duplicated(review$review),]#delete dublicates
user_reviews=review[,1:2]

#find the most active user of airbnb
users_activity=user_reviews %>% group_by(author_id) %>% summarise(count=n())
#the moast active user is 16262021 (11 reviews) but only one commmodity
#the next 4155392 and he evaluated different aparts. Nice!

#get all users' apartment features
user_reviews=inner_join(user_reviews, aparts, by='apart_id')

#create user profile
profile=filter(user_reviews, author_id==4155392)
#to create a profile we should count mean for all integers and moda for all factor variables
#http://stackoverflow.com/questions/17907944/how-to-select-all-factor-variables-in-r

create_profile=function(profile){
  column_order=colnames(profile)
  library(dplyr)
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  #all factor features get mode(мода)
  is.fact <- sapply(profile, is.factor)
  factors.profile <- profile[, is.fact]
  factor_modes=as.data.frame(t(apply(factors.profile,2,Mode)))
  #all continouos features get mean
  is.num <- sapply(profile, is.numeric)
  numeric.profile=profile[,is.num]
  numeric_means=as.data.frame(t(colMeans(numeric.profile, na.rm =TRUE)))#Среди прочего, мы считаем среднее по апарт айди, но, может, это нам и не помешает
  final_profile=cbind(factor_modes,numeric_means)
  final_profile=subset(final_profile, select=column_order)
  profile=final_profile
  }









profile=create_profile(profile)
#we would like to recommend him apart in kaliningrad



library(reshape2)
user_apart_df=dcast(review, apart_id ~ author_id, length)

#Now we should delete Aparts, that have never been rated
apart_for_user_profile_matrix=spbapart[rownames(user_apart_df),]



#Now get user profile matrix by dot product





#Hello world

#HELLO:)
