#1. Надо попровать профиль брать по логину паролю
#Идея: вероятность, что понравится определенному полу как новая фича в квартирах
#2. Холодный старт: квартиры с лучшей оценкой по параметрам указанным на старте
#3. Кластеризуем людей по KNN и присваиваем квартирам кластеры(соответсвенно людям) затем относим залогинившихся пользователей к определенному кластеру и выводим квартиры, удовлетворяющие только кластеру.
#4. По спальным местам создаем репрезентативного пользователя(по 1 гостю среднее по цене, по 2 гостям среднее по цене-это два столбика)
#5. По репрезентативному делаем KNN и соотносим квартиры, которые он посещал с его кластером.
#6. Заполнять квартиры без кластера можно по похожести, например, по косинусам
#7. Ранжируем в конце по сентименту

library(tidyr)
#Загрузка данных
kazan_users=read_csv("Data-profiles/kazan_users.csv")

#Загрузка словарей
russian_names <- read_csv("~/AIRBNB2/airbnbrecommender/Data-profiles/russian_names.csv")
foreign_names <- read_csv("~/AIRBNB2/airbnbrecommender/Data-profiles/foreign_names.csv")
names_translit <- read_csv("~/AIRBNB2/airbnbrecommender/Data-profiles/names_translit.csv")


#доработка данных
user_reviews1 = user_reviews
user_reviews1=unique(user_reviews1)

#rename types and преобразовываем в dummy
#bed_type, cancellation_policy, property_type, room_type
levels(user_reviews1$bed_type) = c("Sofa","Sofa_bed","Sofa_futon","Bed","Air_mattress")
levels(user_reviews1$property_type) = c("Aparts","Guesthouse","Gostevoy_dom","Flat","Loft","Dorminotary","Hostel","Boutique_hotel","House","Other","Condominium","Townhouse")
levels(user_reviews1$room_type) = c("Whole_house_flat","Common_room","Separate_room")

user_reviews1$cancellation_policy = as.numeric(user_reviews1$cancellation_policy)
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
user_reviews1 = left_join(user_reviews1, kazan_users1, by = "author_id")
# у 158 человек появился пол!


#CREATION OF REPRESENTATIVE PROFILE (Sex = 1 - female)
#create user profile
profile=filter(user_reviews1, author_id==95480393)
#profile=filter(user_reviews1, author_id==10340565)
#profile=filter(user_reviews1, author_id==9809129)

profile = as.data.frame(profile)
profile=unique(profile)

#turn logical into numerical
indx <- which(t(sapply(profile, is.factor)))
for (i in indx){
  levels(profile[,i])=c(1,0)
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



#еще нужно:
#1 - разобраться с числом кроватей и гостей, с режимом бронирования (нельзя усреднять)
#2 - создать первый профиль усреднением всего (функция profile)
#3 - создать всем профили (для этого нужны все данные)
#4 - knn грамотно провести - присвоить всем людям номер кластера
#5 - присвоить кластер квартирам





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
  rownames(final_profile)=final_profile$author_id
  profile=subset(final_profile, select=-c(apart_id, author_id))
  
}

profile=create_profile(profile)
