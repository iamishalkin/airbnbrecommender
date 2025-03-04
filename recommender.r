#https://muffynomster.wordpress.com/2015/06/07/building-a-movie-recommendation-engine-with-r/
#https://github.com/iamishalkin/airbnbrecommender
#ggg
library(recommenderlab)
library(zoo)
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
  df=subset(df, select=-c(apart_name, city,description, special_offer,Paid_parking_off_premises, in_building, host_id,lat,lng, native_currency, at_page))
  #delete inimportatant columns with NA
  df=subset(df, select=-c(extra_price_native, cleaning_fee_native,square_feet))
  #df$cleaning_fee_native[-is.na]=mean
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

#review analysis starts here
#let's load tons of cities and reviews
sochireview <- read_csv("full_data/reviews/Review_sochi.csv")
spbreview <- read_csv("full_data/reviews/Review_spb.csv")
kazanreview <- read_csv("full_data/reviews/Review_kazan.csv")
samarareview <- read_csv("full_data/reviews/Review_samara.csv")
mskreview=read_csv("full_data/reviews/Review_msk.csv")
#let's unite them all!
review=rbind(spbreview,mskreview,sochireview,kazanreview,samarareview)
review <- review[!duplicated(review$review),]#delete dublicates
user_reviews=review[,1:2]

#now let's devide the dataset into russian and english (also other languages) groups
library(stringr)
str_detect(review$review, "[abcdefghijklmnopqrstuvwxyz]")
str_detect(review$review, "[абвгдеёжзийклмнопрстуфхцчшщъыьэюя]")

eng = str_detect(review$review, "[abcdefghijklmnopqrstuvwxyz]")
eng = data.frame(eng)
reviewlang = data.frame(review, eng)

rus = str_detect(review$review, "[абвгдеёжзийклмнопрстуфхцчшщъыьэюя]")
rus = data.frame(rus)
reviewlang = data.frame(reviewlang, rus)

rus = filter(reviewlang, rus == "TRUE")

#code wrote by me concerning the review analysis continues at 158

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
  rownames(final_profile)=final_profile$author_id
  profile=subset(final_profile, select=-c(apart_id, author_id))
  
}









profile=create_profile(profile)

#we would like to recommend him apart in kaliningrad
#let's calculate cosine distance between profile vector and all the flats in klg

library(lsa)
klgapart=subset(klgapart, select=-apart_id)

indx <- sapply(profile, is.factor)
klgapart[indx] <- lapply(klgapart[indx], function(x) as.numeric(x))
profile[indx] <- lapply(profile[indx], function(x) as.numeric(x))


klgapart=na.aggregate(klgapart)
profile=na.aggregate(profile)
klg_matrix=as.matrix(klgapart)
profile_matrix=as.matrix(profile)


res <- as.data.frame(apply(klg_matrix, 1, lsa::cosine, y=profile_matrix[1,]))
res$apart_id=rownames(res)
res=arrange(res, desc(res[,1]))
six_best_recoms=as.data.frame(head(res))
six_best_recoms$url=paste('https://www.airbnb.com/rooms/',as.character(six_best_recoms$apart_id), sep='')

###PCA
class(klgapart)
data.pca <- prcomp(klgapart,
                   center = TRUE,
                   scale. = TRUE) 
data.pca1 <- as.data.frame(data.pca) 
plot(data.pca)
summary(data.pca)

colnames(data.pca) <- NULL

library(caret)
preprocessParams <- preProcess(klgapart, method=c("center", "scale", "pca"))
# summarize transform parameters
preprocessParams

preprocessParams$rotation

#let's upload the libraries!
library(quanteda)
library(jsonlite)
library(stringr)

amazonCorpus <- corpus(rus$review)

summary(amazonCorpus, n = 10)

texts(amazonCorpus)[1]
texts(amazonCorpus)[2]

docvars(amazonCorpus, "Author") <- rus$author_id
summary(amazonCorpus, n = 10)
head(kwic(amazonCorpus, "понравилось"))
example <- texts(amazonCorpus)[1]
tokenize(example)
tokenize(example, removePunct = T)
tokenize(example, removePunct = TRUE, removeNumbers = TRUE)
tokenize(amazonCorpus)
tokenize(example, what = "character")
tokenize(example, what = "sentence")
amazondfm <- dfm(amazonCorpus)

tokenize(rus$review)

amazondfm[1:10,1:6]
amazondfmStop <- dfm(amazonCorpus, ignoredFeatures = stopwords("russian"))
amazondfmStop[1:10,1:6]
amazondfmStop <- dfm(amazonCorpus, ignoredFeatures = c("жилье", stopwords("russian")))
amazondfmStop[1:10,1:6]
head(stopwords("russian"), 10)
topfeatures(amazondfmStop, 30)
amazondfmStop <- dfm(amazonCorpus, stem = TRUE, language = "russian", 
                     ignoredFeatures = c("квартира", "квартире", "квартиры",
                                         stopwords("russian")))
plot(amazondfmStop[1:20,])
neg = readLines("~/materials/minor/datatech/text/negative-words.txt")
pos = readLines("~/materials/minor/datatech/text/positive-words.txt")
pos=pos[36:length(pos)]
neg=neg[36:length(neg)]
myDict <- dictionary(list(negative=neg,
                          positive=pos))
g = applyDictionary(amazondfmStop, myDict, valuetype = "glob",case_insensitive = TRUE) 
head(g)

pos <- read.csv2("~/airbnbrecommender/pozitiv.csv")
pos$word = as.character(pos$word)
pos <- corpus(pos$word)
pos <- dfm(pos, stem = TRUE, language = "russian")
head(pos)

neg <- read.csv2("~/airbnbrecommender/negative.csv")
neg$word = as.character(neg$word)
neg <- corpus(neg$word)
neg <- dfm(neg, stem = TRUE, language = "russian")
head(neg)

myDict <- dictionary(list(negative=colnames(neg),
                          positive=colnames(pos)))
g = applyDictionary(amazondfmStop, myDict, valuetype = "glob",case_insensitive = TRUE) 
head(g)
View(g)
head(g@x)
View(amazondfmStop)

g = as.data.frame(g)

g = mutate(g, sum=negative+positive)
g = mutate(g, rating=positive/sum)
grus = data.frame(g, rus)
gruss = select(grus, apart_id, author_id, rating)
gruss$apart_id <- as.character(gruss$apart_id)
gruss$author_id <- as.character(gruss$author_id)

grussapart <- gruss$apart_id
grussapart = data.frame(grussapart)

grussauthor <- gruss$author_id
grussauthor = data.frame(grussauthor)

gruss <- na.omit(gruss)
gruss$apart_id <- as.factor(gruss$apart_id)

grouprus <- group_by(gruss, author_id)

library(dplyr)

library(recommenderlab)
?sparse
r <- as(gruss, "realRatingMatrix")


rd = as.data.frame(r)

set.seed(100) #You shoud put here your own number
r.test.ind = sample(seq_len(nrow(r)), size = nrow(r)*0.2)
r.test = r[r.test.ind,]
r.main = r[-r.test.ind,]
library(randomForest)
rfModel <-randomForest(factor(win) ~ ., data=victory.main)
summary(rfModel) 

("Recommender based on item-based collaborative filtering (real
data).")

recommender_models <- recommenderRegistry$get_entries(dataType =
                                                        "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters
recc_model <- Recommender(data = r.main, method = "IBCF",
                          parameter = list(k = 30))
recc_model

similarity_users10 <- similarity(r[1:10, ], method = "cosine", which = "users")
as.matrix(similarity_users10)