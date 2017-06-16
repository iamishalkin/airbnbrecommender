#1. Надо попровать профиль брать по логину паролю
#Идея: вероятность, что понравится определенному полу как новая фича в квартирах
#2. Холодный старт: квартиры с лучшей оценкой по параметрам указанным на старте
#3. Кластеризуем людей по KNN и присваиваем квартирам кластеры(соответсвенно людям) затем относим залогинившихся пользователей к определенному кластеру и выводим квартиры, удовлетворяющие только кластеру.
#4. По спальным местам создаем репрезентативного пользователя(по 1 гостю среднее по цене, по 2 гостям среднее по цене-это два столбика)
#5. По репрезентативному делаем KNN и соотносим квартиры, которые он посещал с его кластером.
#6. Заполнять квартиры без кластера можно по похожести, например, по косинусам
#7. Ранжируем в конце по сентименту

library(recommenderlab)
library(zoo)
library(readr)
library(dplyr)
library(tidyr)
library(readr)



selectcolumns=function(df){
  df=df[,2:91]
  df=dplyr::filter(df, native_currency=='RUB')
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                         as.factor)
  df <- df[!duplicated(df$apart_id),]
  #df <- df[order(df$apart_id),]
  df=as.data.frame(df)
  rownames(df)=df$apart_id
  df=subset(df, select=-c(apart_name, description,Paid_parking_off_premises, in_building, host_id,lat,lng, native_currency, country, extra_user_info, has_agreed_to_legal_terms, house_rules, interaction, max_nights, min_nights, 	
                          neighborhood, neighborhood_overview, notes, photo_url, room_type_category, space, transit, url))
  #delete inimportatant columns with NA
  df=subset(df, select=-c(extra_price_native, cleaning_fee_native,square_feet))
  #df$cleaning_fee_native[-is.na]=mean
}
one_city_apart_fix_price=function(apart){
  apart$fixed_price=apart$price_native- mean(apart$price_native)
  return(apart)
}
selectcolumns2=function(df){
  library(dplyr)
  df=df[,1:77]
  df=dplyr::filter(df, native_currency=='RUB')
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], 
                                         as.factor)
  df <- df[!duplicated(df$apart_id),]
  #df <- df[order(df$apart_id),]
  df=as.data.frame(df)
  rownames(df)=df$apart_id
  df=subset(df, select=-c(apart_name, city,description, special_offer,Paid_parking_off_premises, in_building, host_id,lat,lng, native_currency, at_page))
  #delete inimportatant columns with NA
  df=subset(df, select=-c(extra_price_native, cleaning_fee_native,square_feet))
  #df$cleaning_fee_native[-is.na]=mean
  df = mutate(df, city = "Spb")
  }
#Функция, которая делает профиль
make_profile=function(profile){
  profile = as.data.frame(profile)
  profile=unique(profile)
  profile = select(profile, select = -c(cancellation_policy, city))
  #turn logical into numerical
  indx <- which(t(sapply(profile, is.factor)))
  for (i in indx){
    levels(profile[,i])=c(1,0)
    profile[,i] = as.numeric(as.character(profile[,i]))
  }
  #разберёмся со спальными местами - ДОПУЩЕНИЕ: гости платят поровну
  #1
  profile$price_for1 = profile$price_native
  #2
  profile$price_for2 = NA
  profile$price_for2[profile$person_capacity >1] = (profile$price_native[profile$person_capacity >1]+profile$price_for_extra_person_native[profile$person_capacity >1])/2
  #3
  profile$price_for3 = NA
  profile$price_for3[profile$person_capacity >2] = (profile$price_native[profile$person_capacity >2]+2*profile$price_for_extra_person_native[profile$person_capacity >2])/3
  #4
  profile$price_for4 = NA
  profile$price_for4[profile$person_capacity >3] = (profile$price_native[profile$person_capacity >3]+3*profile$price_for_extra_person_native[profile$person_capacity >3])/4
  #5 or more
  profile$price_for5plus = NA
  profile$price_for5plus[profile$person_capacity >4] = (profile$price_native[profile$person_capacity >4]+4*profile$price_for_extra_person_native[profile$person_capacity >4])/5
  #delete unuseful variables
  profile$price_native = NULL
  profile$price_for_extra_person_native = NULL
  profile$person_capacity = NULL
  #функция профиля
  profile = subset(profile, select = c(-apart_id, -beds))
  is.num <- sapply(profile, is.numeric)
  numeric.profile=profile[,is.num]
  profile=as.data.frame(t(colMeans(profile, na.rm =TRUE)))
}
changing = function(user_reviews){
  user_reviews1 = user_reviews
  user_reviews1=unique(user_reviews1)
  levels(user_reviews1$bed_type) = c("Sofa","Sofa_bed","Sofa_futon","Air_mattress","Bed")
  levels(user_reviews1$property_type) = c("Aparts","Bungalow","Boutique_hotel","Villa","Guesthouse","Gostevoy_dom","House","Tree_house","Cottage_in_nature","Other","Castle","Dugout","Flat","Condominium","Boat","Loft","Dorminotary","Historical_building","Tent","Boarding_house","Cave","Ryokan","Time-share","Townhouse","Hut","Hostel","Chalet","Yurt")
  levels(user_reviews1$room_type) = c("Whole_house_flat","Common_room","Separate_room")
    #user_reviews1$cancellation_policy = as.numeric(user_reviews1$cancellation_policy)
  user_reviews1$num = 1
  user_reviews1 = spread(user_reviews1, key = bed_type, value = num, fill = 0, sep = "_")
  user_reviews1$num = 1
  user_reviews1 = spread(user_reviews1, key = property_type, value = num, fill = 0, sep = "_")
  user_reviews1$num = 1
  user_reviews1 = spread(user_reviews1, key = room_type, value = num, fill = 0, sep = "_")
    #GENDER
  kazan_users1 = subset(kazan_users, select = c(first_name, id))
  russian_names = rbind(russian_names, foreign_names, names_translit)
  kazan_users1 = left_join(kazan_users1, russian_names, by = "first_name")
  kazan_users1$Sex = as.factor(kazan_users1$Sex)
  kazan_users1$first_name = NULL
  names(kazan_users1) = c("author_id","Sex" )
  kazan_users1 = na.omit(kazan_users1)
  user_reviews1 = left_join(user_reviews1, kazan_users1, by = "author_id")
  user_reviews1=unique(user_reviews1)
  #summary(user_reviews1$Sex)
  # у 2567+2130=4697 человек появился пол!
}







#Загрузим все квартиры
apart <- read_csv("full_data/city.csv")
spbapart <- read_csv("Data-city/Saint-Petersburg--Russia.csv")

apart1 = apart

apart = selectcolumns(apart)
spbapart = selectcolumns2(spbapart)
apart=one_city_apart_fix_price(apart)
spbapart=one_city_apart_fix_price(spbapart)
apart = rbind(apart, spbapart)
#Kazan (1005), Moscow (848), NizhnyNovgorod (603), Novosibirsk (695), Samara (1970), Sochi (728), Volgograd (229), Spb (300)
#все квартиры (6378) в apart

#Загрузим все ревью
sochireview <- read_csv("full_data/reviews/Review_sochi.csv")
spbreview <- read_csv("full_data/reviews/Review_spb.csv")
kazanreview <- read_csv("full_data/reviews/Review_kazan.csv")
samarareview <- read_csv("full_data/reviews/Review_samara.csv")
mskreview=read_csv("full_data/reviews/Review_msk.csv")
nizhnyreview=read_csv("full_data/reviews/Review_Nizhny-Novgorod.csv")
volgreview=read_csv("full_data/reviews/Review_Volgograd.csv")
novosibreview=read_csv("full_data/reviews/Review_Novosibirsk.csv")
#let's unite them all!
review=rbind(spbreview,mskreview,sochireview,kazanreview,samarareview,nizhnyreview,volgreview,novosibreview)
review <- review[!duplicated(review$review),]#delete dublicates
user_reviews=review[,1:2]
#get all users' apartment features
user_reviews=inner_join(user_reviews, apart, by='apart_id')

users_activity=user_reviews %>% group_by(author_id) %>% summarise(count=n())


#ПОЛ
#Загрузка данных
kazan_users=read_csv("Data-profiles/kazan_users.csv")
#Загрузка словарей
russian_names <- read_csv("~/AIRBNB2/airbnbrecommender/Data-profiles/russian_names.csv")
foreign_names <- read_csv("~/AIRBNB2/airbnbrecommender/Data-profiles/foreign_names.csv")
names_translit <- read_csv("~/AIRBNB2/airbnbrecommender/Data-profiles/names_translit.csv")


#доработка данных
user_reviews1 = changing(user_reviews)

#rename types and преобразовываем в dummy
#bed_type, cancellation_policy, property_type, room_type




#CREATION OF REPRESENTATIVE PROFILE (Sex = 1 - female)
#create user profile
profile=filter(user_reviews1, author_id==102099192)
profile = make_profile(profile)

prof_num = unique(user_reviews1$author_id)
profiles = as.data.frame(matrix(nrow =length(prof_num), ncol = 96))
names(profiles) = names(profile)
for (i in c(1:length(unique(user_reviews1$author_id)))){
  profiles[i,] = make_profile(filter(user_reviews1, author_id==prof_num[i]))
}
#write.csv(profiles, "profiles.csv")



#еще нужно:
#1 - разобраться с числом кроватей и гостей, с режимом бронирования (нельзя усреднять)
#2 - создать первый профиль усреднением всего (функция profile)
#3 - создать всем профили (для этого нужны все данные)
#4 - knn грамотно провести - присвоить всем людям номер кластера
#5 - присвоить кластер квартирам

