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
review=rbind(spbreview,mskreview,sochireview,kazanreview,samarareview,rekat,rnn,rsib,volgo)
review <- review[!duplicated(review$review),]#delete dublicates
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
crosspredict = spread(grussdss, key = author_id, value = "mean(rating)")
crosspredict[is.na(crosspredict)] <- 0
crosspredict = na.omit(crosspredict)
crossmatrix <- as.matrix(crosspredict)
crossmatrix <- sapply(data.frame(crossmatrix),as.numeric)
crossmatrix <- as.numeric(crossmatrix)
r <- as(crossmatrix, "realRatingMatrix")

set.seed(100) #You shoud put here your own number
r.test.ind = sample(seq_len(nrow(r)), size = nrow(r)*0.02)
r.test = r[r.test.ind,]
r.main = r[-r.test.ind,]

("Recommender based on item-based collaborative filtering (real
  data).")

recommender_models <- recommenderRegistry$get_entries(dataType =
                                                        "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters
recc_model <- Recommender(data = r.main, method = "IBCF",
                          parameter = list(k = 1))
recc_predicted <- predict(object = recc_model, newdata = r.test, n = 6)
str(recc_predicted)
recc_predicted <- predict(object = recc_model, newdata = r.test, n = 6)
recc_user_1 <- recc_predicted@items[["29455279"]]
recc_user_1
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]
movies_user_1

recc_predicted <- predict(object = recc_model, newdata = r.test, n = 6)
recc_user_1 <- recc_predicted@items[[3]]
recc_user_1
movies_user_1 <- recc_predicted@itemLabels[3]
movies_user_1

apartcn <- colnames(crossmatrix)
recc_user_1 <- c(1:969)
recc_user_1 <- data.frame(recc_user_1)
apartch <- data.frame(recc_user_1, apartcn)

recc_user_1 <- data.frame(recc_user_1)

apart_names = left_join(recc_user_1, apartch, by = "recc_user_1")

my_id = filter(gruss, author_id=="43516650")
r <- as(gruss, "realRatingMatrix")
my_r <- as(me_in_apart, "realRatingMatrix")
my_predicted <- predict(object = recc_model, newdata = my_r, n = 6)
me_in_apart <- full_join(aparts_activity, my_id, by="apart_id")
my_id$apart_id <- as.character(my_id$apart_id)
aparts_activity$apart_id <- as.character(aparts_activity$apart_id)
me_in_apart <- select(me_in_apart, author_id, apart_id, rating)
me_in_apartomit <- na.omit(me_in_apart)
