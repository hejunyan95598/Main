install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("choroplethrZip")
install.packages("extracat")
install.packages("gridExtra")
install.packages("leaflet")
install.packages("plfm")
install.packages("factoextra")
install.packages("extracat")
install.packages("devtools")
install.packages("date")
install.packages("data.table", dependencies=TRUE)
noinstall_github('arilamstein/choroplethrZip@v1.4.0')
install.packages("text2vec")
install.packages("tokenizers")
install.packages("wordcloud")
library(extracat)
library(data.table)
library(gridExtra)
library(tidyverse)
library(devtools)
library(readr)
library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(tm)
library(GGally)
library(lubridate)
library(zoo)
library(scales)
library(ggmap)
library(scales)
library(stringr)
library(zipcode)
library(text2vec)
library(leaflet)
library(tidyverse)
library(gridExtra)
library(dslabs)
library(ggplot2)
library(glmnet)
library(plfm)
library(factoextra)
library(date)
library(tokenizers)
library(wordcloud)
source("DataAnalyticsFunctions.R")

DATA<-read.csv(file="london_v2.csv")

# take a look at data set
names(DATA)
str(DATA)
head(DATA)
summary(DATA)


# drop some insignificant variables
data1 <- DATA[,-c(1,3,5,19,30,40,36,42)]

# price is very wide-spread, so we want to do drop some unresonable price
data<-data1%>%
  filter(price>0 & price <500)

names(data)
str(data)
head(data)
summary(data)


# data visulization

# how popular is airbnb
reviews<-read.csv(file="reviews.csv")
reviewsNum <- reviews %>% mutate(date123=as.Date(reviews$date-26000))%>%group_by(date123) %>% summarise(number = n())
ggplot(reviewsNum, aes(date123, number)) +
  geom_point(na.rm=TRUE, color ="tomato" , alpha=0.5) +geom_smooth(color = "#007A87")+
  labs(x = "Year", y = "Number of Reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey68")) +
  theme(plot.caption = element_text(color = "grey35"))

# price overview
ggplot(data=data, aes(data$price)) + 
  geom_histogram(col = "black", fill = "cornflowerblue")+xlim(0,500)+xlab("Price")+
  ylab("Frequency")+ labs(fill="")+theme(
    axis.title.x = element_text(color="black", size=18),
    axis.title.y = element_text(color="black", size=18),
    axis.text = element_text(color="black", size=14),
    legend.text=element_text(color="black", size=14),
 plot.title = element_text(hjust = 0.5))


# experience offered
ggplot(data) + 
  geom_bar(aes(as.factor(experiences_offered), price, fill = as.factor(experiences_offered),width = 0.8), 
           position = "dodge", stat = "summary", fun.y = "mean")+
  xlab("Experiences Offered")+
  ylab("Average Price")+ labs(fill="")+theme(
    axis.title.x = element_text(color="black", size=18),
    axis.title.y = element_text(color="black", size=18),
    axis.text = element_text(color="black", size=14),
    legend.text=element_text(color="black", size=14))+
  scale_fill_brewer(palette="Set1")

### Clustering
haha <- subset(DATA, DATA$review_scores_rating != "NA")
summary(haha)
str(haha)
names(haha)
simple <- haha[,c(37,40)]
simple_kmeans <- kmeans(simple,4,nstart=10)
colorcluster <- 1+simple_kmeans$cluster
plot(simple, col = colorcluster,xlab="price", ylab="Percentage of Bachelors")

points(simple_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)
apply(simple,2,sd) # compute sd for each column
apply(simple,2,mean) # compute mean for each column

Ssimple <- scale(simple)
apply(Ssimple,2,sd) 
# we should get sd=1
apply(Ssimple,2,mean) 
# we should get mean = 0
### Recomputing kmeans
Ssimple_kmeans <- kmeans(Ssimple,4,nstart=10)
colorcluster <- 1+Ssimple_kmeans$cluster
### The command
Ssimple_kmeans$centers
## displays the k centers (good to interpret)
Ssimple_kmeans$size
## displays the size of each cluster
####
sum(Ssimple_kmeans$size)
# In-Sample R^2 
1 - sum(Ssimple_kmeans$tot.withinss)/Ssimple_kmeans$totss

  
# test base model
lm1<-lm(price~.,data)
summary(lm1)

# Lasso
Mx<- model.matrix(price~., data)[,-26] # removes intercept
str(Mx)
My<- data[,26]
head(data)
head(My)
str(My)


lassoCV <- cv.glmnet(Mx,My)
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))


# Lasso min
lassoCV$lambda.min
lambda.min <- glmnet(Mx,My,lambda = lassoCV$lambda.min)
summary(lambda.min)
support(lambda.min$beta)
colnames(Mx)[support(lambda.min$beta)]
length(support(lambda.min$beta))
lambda.min$lambda
log(lassoCV$lambda.min)

# Lasso  with lambda=1
lassoTheory2 <- glmnet(Mx,My,lambda = 1)
summary(lassoTheory2)
support(lassoTheory2$beta)
colnames(Mx)[support(lassoTheory2$beta)]
length(support(lassoTheory2$beta))
log(1)


# Lasso 1SE
lassoCV$lambda.1se
lambda.1se <- glmnet(Mx,My,lambda = lassoCV$lambda.1se)
summary(lambda.1se)
support(lambda.1se$beta)
colnames(Mx)[support(lambda.1se$beta)]
length(support(lambda.1se$beta))
lambda.1se$lambda
log(lassoCV$lambda.1se)


#Do k-fold
nfold <- 10
n <- nrow(data)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
OOS <- data.frame(lasso_min=rep(NA,nfold),lasso_1se=rep(NA,nfold),lambda_1=rep(NA,nfold),all_variable=rep(NA,nfold),null=rep(NA,nfold)) 
### Loop
for(k in 1:nfold){ 
  train <- which(foldid!=k)
  lasso_1se <- lm(price~host_time+host_is_superhost+host_listings_count+host_identity_verified+neighbourhood_cleansed+is_location_exact+room_type+accommodates+bathrooms+bedrooms+kitchen_Mic+parking+tv+refrigerator+garden+coffee_maker+lock+dryer+essentials+security_deposit+cleaning_fee+minimum_nights+availability_365+number_of_reviews+number_of_reviews_ltm+instant_bookable+reviews_per_month,data=data,subset=train)               
  all_variable <- lm(price~ ., data=data, subset=train)
  null <- lm(price~1, data=data, subset=train)
  lasso_min<-lasso_min <- lm(price~ experiences_offered+host_time+host_response_time+host_is_superhost+host_listings_count+host_has_profile_pic+host_identity_verified+neighbourhood_cleansed+is_location_exact+room_type+accommodates+bathrooms+bedrooms+beds+bed_type+kitchen_Mic+parking+tv+refrigerator+balcony+garden+coffee_maker+lock+dryer+essentials+security_deposit+cleaning_fee+guests_included+minimum_nights+availability_365+number_of_reviews+number_of_reviews_ltm+instant_bookable+reviews_per_month,data=data,subset=train)                     
  lambda_1<- lm(price~ host_time+host_is_superhost+host_listings_count+host_identity_verified+neighbourhood_cleansed+room_type+accommodates+bathrooms+bedrooms+kitchen_Mic+parking+tv+refrigerator+garden+coffee_maker+lock+dryer+essentials+security_deposit+cleaning_fee+minimum_nights+availability_365+number_of_reviews+number_of_reviews_ltm+instant_bookable+reviews_per_month,data=data,subset=train)
  
  pred.lasso_1se <- predict(lasso_1se, newdata=data[-train,], type="response")
  pred.all_variable<-predict(all_variable, newdata=data[-train,], type="response")
  pred.null <- predict(null, newdata=data[-train,], type="response")
  pred.lasso_min <- predict(lasso_min, newdata=data[-train,], type="response")
  pred.lambda_1<-predict(lambda_1, newdata=data[-train,], type="response")
  
  OOS$lasso_1se[k] <- R2(y=data$price[-train], pred=pred.lasso_1se)
  OOS$all_variable[k] <- R2(y=data$price[-train], pred=pred.all_variable)
  OOS$null[k] <- R2(y=data$price[-train], pred=pred.null)
  OOS$lasso_min[k] <- R2(y=data$price[-train], pred=pred.lasso_min)
  OOS$lambda_1 <- R2(y=data$price[-train], pred=pred.lambda_1)
  
  
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

# k-fold visualization
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), col=c("blue","red","green","grey","black"),beside=TRUE, legend=TRUE, args.legend=c(xjust=-0.1, yjust=0.3),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))

if (nfold >= 10){
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
}

#Run final linear regression
lambda_1<- lm(price~ host_time+host_is_superhost+host_listings_count+host_identity_verified+neighbourhood_cleansed+room_type+accommodates+bathrooms+bedrooms+kitchen_Mic+parking+tv+refrigerator+garden+coffee_maker+lock+dryer+essentials+security_deposit+cleaning_fee+minimum_nights+availability_365+number_of_reviews+number_of_reviews_ltm+instant_bookable+reviews_per_month,data=data)
summary(lambda_1)

#word cloud
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
tokens <- space_tokenizer(as.character(reviews$comments))
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
word_vectors = glove$fit_transform(tcm, n_iter = 20)
word_vectors1 <- glove$components
head(word_vectors1)
head(word_vectors)
# comfortable
p2 = word_vectors["comfortable", , drop = FALSE] 
p2sim = sim2(x = word_vectors, y = p2, method = "cosine", norm = "l2")
p2 = sort(p2sim[,1], decreasing = TRUE)

df2 = data.frame(item = as.character(names(p2)),freq = as.numeric(p2))
df2$item = gsub(",","",df2$item)
df2 = df2[!duplicated(df2$item), ]

set.seed(2020)

suppressWarnings(wordcloud(words = df2$item, freq = df2$freq, scale = c(2,0.2),
                           max.words=80, random.order=FALSE, rot.per=0.2,
                           colors = c("grey80", "darkgoldenrod1", "tomato","#357b8a")))


#uncomfortable
p1 = word_vectors["uncomfortable", , drop = FALSE] 
p1sim = sim2(x = word_vectors, y = p1, method = "cosine", norm = "l2")
p1 = sort(p1sim[,1], decreasing = TRUE)

df = data.frame(item = as.character(names(p1)),freq = as.numeric(p1))
df$item = gsub(",","",df$item)
df = df[!duplicated(df$item), ]
set.seed(2019)
suppressWarnings(wordcloud(words = df$item, freq = df$freq, scale = c(2,0.2),
                           max.words=80, random.order=FALSE, rot.per=0.2,
                           colors = c("#7db5b8", "darkgoldenrod1", "tomato","#357b8a")))



# seasonality 
xixi <- reviews %>% mutate(date321=as.Date(reviews$date-25000))%>%group_by(date321) %>% summarise(number = n())

ggplot(xixi[year(xixi$date321) == 2018,], aes(date321, number)) +
  geom_point(na.rm=TRUE, color =  "#357b8a", alpha=0.5) +geom_smooth(color = "#FF5A5F")+
  labs(x = "Month", y = "Number of reviews") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color ="grey68")) +
  theme(plot.caption = element_text(color =  "grey35"))


