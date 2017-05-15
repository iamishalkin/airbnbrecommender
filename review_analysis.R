library(recommenderlab)
library(zoo)

library(readr)
library(dplyr)

#now users
#load reviews

#review analysis starts here
#let's load tons of cities and reviews
sochireview <- read_csv("~/airbnbrecommender2/full_data/reviews/Review_sochi.csv")
spbreview <- read_csv("~/airbnbrecommender2/full_data/reviews/Review_spb.csv")
kazanreview <- read_csv("~/airbnbrecommender2/full_data/reviews/Review_Kazan.csv")
samarareview <- read_csv("~/airbnbrecommender2/full_data/reviews/Review_Samara.csv")
mskreview=read_csv("~/airbnbrecommender2/full_data/reviews/Review_msk.csv")
ekat <- read_csv("~/airbnbrecommender2/full_data/reviews/Ekaterinburg.csv")
kazan <- read_csv("~/airbnbrecommender2/full_data/reviews/Kazan.csv")
nn <- read_csv("~/airbnbrecommender2/full_data/reviews/Nizhny-Novgorod.csv")
novosib <- read_csv("~/airbnbrecommender2/full_data/reviews/Novosibirsk.csv")
rekat <- read_csv("~/airbnbrecommender2/full_data/reviews/Review_Ekaterinburg.csv")
rnn <- read_csv("~/airbnbrecommender2/full_data/reviews/Review_Nizhny-Novgorod.csv")
rsib <- read_csv("~/airbnbrecommender2/full_data/reviews/Review_Novosibirsk.csv")
volgo <- read_csv("~/airbnbrecommender2/full_data/reviews/Review_Volgograd.csv")
samara <- read_csv("~/airbnbrecommender2/full_data/reviews/Samara.csv")
volgograd <- read_csv("~/airbnbrecommender2/full_data/reviews/Volgograd.csv")
msk <- read_csv("~/airbnbrecommender2/full_data/reviews/moscow.csv")
sochi <- read_csv("~/airbnbrecommender2/full_data/listings/sochi.csv")
spb <- read_csv("~/airbnbrecommender2/full_data/listings/spb.csv")
review=rbind(spbreview,mskreview,sochireview,kazanreview,samarareview,rekat,rnn,rsib,volgo)
aparts=rbind(ekat, kazan, nn, novosib, samara, volgograd, msk, sochi, spb)
review <- review[!duplicated(review$review),]#delete dublicates
aparts <- aparts[!duplicated(aparts$apart_id),]#delete dublicates
user_reviews=review[,1:2]
library(stringr)
eng = str_detect(review$review, "[abcdefghijklmnopqrstuvwxyz]")
eng = data.frame(eng)
reviewlang = data.frame(review, eng)

rus = str_detect(review$review, "[абвгдеёжзийклмнопрстуфхцчшщъыьэюя]")
rus = data.frame(rus)
reviewlang = data.frame(reviewlang, rus)

rus = filter(reviewlang, rus == "TRUE")
users_activity=rus %>% group_by(author_id) %>% summarise(count=n())
#the moast active user is 16262021 (11 reviews) but only one commmodity
apart_activity=rus %>% group_by(apart_id) %>% summarise(count=n())
users_activity = filter(users_activity, count > 2)

aparts_activity = filter(apart_activity, count > 4)

rus_active = left_join(rus, users_activity, by = "author_id")
rus_active = left_join(rus_active, aparts_activity, by = "apart_id")

rus_active = na.omit(rus_active)
library(quanteda)
amazonCorpus <- corpus(rus_active$review)
amazonCorpus <- tokenize(amazonCorpus)
amazondfmStop <- dfm(amazonCorpus, ignoredFeatures = stopwords("russian"))

amazondfmStop <- dfm(amazonCorpus, ignoredFeatures = c("жилье", stopwords("russian")))

amazondfmStop <- dfm(amazonCorpus, stem = TRUE, language = "russian", ignoredFeatures = c("квартира", "квартире", "квартиры", stopwords("russian")))

pos <- read.csv2("~/airbnbrecommender/pozitiv.csv")
pos$word = as.character(pos$word)
pos <- corpus(pos$word)
pos <- dfm(pos, stem = TRUE, language = "russian")

neg <- read.csv2("~/airbnbrecommender/negative.csv")
neg$word = as.character(neg$word)
neg <- corpus(neg$word)
neg <- dfm(neg, stem = TRUE, language = "russian")

myDict <- dictionary(list(negative=colnames(neg),
                          positive=colnames(pos)))
g = applyDictionary(amazondfmStop, myDict, valuetype = "glob",case_insensitive = TRUE)
g = as.data.frame(g)
g = mutate(g, sum=negative+positive)
g = mutate(g, rating=positive/sum)
grus = data.frame(g, rus_active)
gruss = select(grus, author_id, apart_id, rating)
gruss$apart_id <- as.character(gruss$apart_id)
gruss$author_id <- as.character(gruss$author_id)

gruss <- na.omit(gruss)

duplicated(gruss)
gruss[duplicated(gruss), ]
grussd <- gruss[!duplicated(gruss), ]
grussds <- unite(grussd, author_in_apart, c(author_id, apart_id), remove = FALSE)
grussds = select(grussds, author_in_apart, rating)
grussds = summarise(grussds, mean(rating))
grussdss = separate(grussds, author_in_apart, c("author_id", "apart_id"), sep = "_")

library(dplyr)

#let's build a recommendation model
library(recommenderlab)
library(tidyr)
crosspredict = spread(grussdss, key = apart_id, value = "mean(rating)")
crosspredict$author_id <- as.numeric(crosspredict$author_id)
crossmatrix <- as.matrix(crosspredict)
crossmatrix <- sapply(data.frame(crossmatrix),as.numeric)
r <- as(crossmatrix, "realRatingMatrix")

("Recommender based on item-based collaborative filtering (real
  data).")

recommender_models <- recommenderRegistry$get_entries(dataType =
                                                        "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters
recc_model <- Recommender(data = r, method = "IBCF",
                          parameter = list(k = 20))

#or here
recc_predicted <- predict(object = recc_model, newdata = r, n = 200)
str(recc_predicted)
recc_user_1 <- recc_predicted@items[[5]]
recc_user_1
movies_user_1 <- recc_predicted@itemLabels[5]
movies_user_1

#here i create the data frame that contents all the apartments in the given order
apartcn <- colnames(crossmatrix)
recc_user_1 <- c(1:1578)
recc_user_1 <- data.frame(recc_user_1)
apartch <- data.frame(recc_user_1, apartcn)

#here i look for a specific person in our data frame
recc_user_1 <- data.frame(recc_user_1)
apart_names = left_join(recc_user_1, apartch, by = "recc_user_1")
apart_names$apartcn
author <- data.frame(crosspredict$author_id, c(1:968))



#i guess this thing is irrelevant
X <- read_csv2("~/airbnbrecommender2/Data-city/X.csv")
apartmute <- data.frame(X$X, aparts$apart_id)

apartmute <- unite(apartmute, apartcn, c(X.X, aparts.apart_id), sep = "")
apartmute = data.frame(apartmute, aparts)
apart_names_mute = left_join(apart_names, apartmute, by="apartcn") 
apartchm = left_join(apartch, apartmute, by="apartcn")
apartchms = select(apartchm, apart_name, price_native, city, transit, neighborhood, description, min_nights, cancellation_policy, person_capacity, beds, bedrooms, bathrooms, interaction, notes, recc_user_1)

my_id = filter(gruss, author_id=="43516650")
r <- as(gruss, "realRatingMatrix")
my_r <- as(me_in_apart, "realRatingMatrix")
my_predicted <- predict(object = recc_model, newdata = my_r, n = 6)
me_in_apart <- full_join(aparts_activity, my_id, by="apart_id")
my_id$apart_id <- as.character(my_id$apart_id)
aparts_activity$apart_id <- as.character(aparts_activity$apart_id)
me_in_apart <- select(me_in_apart, author_id, apart_id, rating)
me_in_apartomit <- na.omit(me_in_apart)
