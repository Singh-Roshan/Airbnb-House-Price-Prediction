data <- read.csv("C:/Users/rosha/OneDrive/Desktop/listings.csv", header = TRUE,stringsAsFactors = F)
View(data)
names(data)

#***************************************Data Preprocessing****************************

a <- strsplit(data$amenities,",")

# Removing variables that contain only a single value
data <- data
a <- apply(data,2,unique)
is.vector(a)
length(a)
b <- NULL
for(i in 1:length(a))
{
  if(length(a[[i]])==1){
    b <- rbind(b,names(a[[i]]))
  }
}
b <- as.vector(b)
head(b)
data[,b] <- NULL

#This leads to a removal of 9 such variables.

#All the url related variables can be removed as they contain weblinks which aren't useful for any kind of analysis
data[,names(data)[grep("url",names(data))]] <- NULL
grep("url",names(data))

#This leads to removal of 8 such variables.
#c. Next we removed all those variables which have more than 80% missing values.
a <- apply(data,2,is.na)
head(a)
b <- apply(a,2,sum)
is.vector(b)  #TRUE
is.list(b)
head(b)
b/5207>=0.80   #b/5207>=0.80 - missing
b[b/5207>=0.80] 
names(b[b/5207>=0.80])   
data[,names(b[b/5207>=0.80])] <- NULL
data$square_feet

#This leads to a removal of 1 variable


#d. Based on intuition and further inspection we observed that below variables does not add any value to our analysis and should be removed
data[,c("security_deposit","weekly_price","monthly_price","first_review","last_review","jurisdiction_names","zipcode","street","market","cleaning_fee","name","interaction","access","space","notes","description","host_name","host_has_profile_pic","host_verifications","host_neighborhood","require_guest_profile_picture","require_guest_phone_verification","calculated_host_listings_count", "host_location","transit","neighborhood_overview","house_rules","host_about","license", "requires_license","host_neighbourhood")] <- NULL

#e. Removing variables with duplicated values
data[,names(data[(duplicated(t(data)))])] <- NULL

#f)We observed that different listings have different amenities and hence we created flag/dummy variables for the most important amenities we thought a user looks for while booking at Airbnb.
#These amenities are TV, Internet, Air Conditioning, Breakfast, Kitchen, and Pets.
# adding amenities categories
a <- data$amenities
data$TV <- ifelse(grepl("TV",a, ignore.case = T)==T,1,0)
data$Internet <- ifelse(grepl("Internet",a, ignore.case= T)==T,1,0)
data$AirCondition <- ifelse(grepl("conditioning",a, ignore.case =T)==T,1,0)
data$Pets <- ifelse(grepl("Pet",a, ignore.case = T)==T,1,0)
data$Pets <- ifelse(grepl("Dog",a, ignore.case = T)==T,1,data$Pets)
data$Pets <- ifelse(grepl("Cat",a, ignore.case = T)==T,1,data$Pets)
data$Kitchen <- ifelse(grepl("Kitchen",a, ignore.case = T)==T,1,0)
data$Breakfast <- ifelse(grepl("breakfast",a, ignore.case = T)==T,1,0)
data[,c("amenities")] <- NULL

#The data types of variables which we thought might be significant were converted to the appropriate data types.
# Converting price variable to integer
data$price <- sub("\\$","",data$price)
data$price <- sub(",","",data$price)
data$price <- as.integer(data$price)
# Changing the default character data type of categorical variable to factor
data$host_response_time <- as.factor(data$host_response_time)
data$host_is_superhost <- as.factor(data$host_is_superhost)
data$host_identity_verified <- as.factor(data$host_identity_verified)
data$neighbourhood_cleansed <- as.factor(data$neighbourhood_cleansed)
data$is_location_exact <- as.factor(data$is_location_exact)
data$property_type <- as.factor(data$property_type)
data$room_type <- as.factor(data$room_type)
data$bed_type <- as.factor(data$bed_type)
data$calendar_updated <- as.factor(data$calendar_updated)
data$instant_bookable <- as.factor(data$instant_bookable)
data$cancellation_policy <- as.factor(data$cancellation_policy)

# Treating host_response_rate and extra_people from character to a numeric variable
data$host_response_rate<- as.numeric(sub("%", "", data$host_response_rate))
data$host_response_rate <- data$host_response_rate/100
data$extra_people <- as.numeric(sub("\\$","",data$extra_people))


# Removing listings which have price = 0
data <- data[-c(717,1334,3093),]

#We also created a variable about the number of days since the listing was on Airbnb.
data$host_since <- as.Date(data$host_since)
data$host_since <- as.Date("2017-05-10")-data$host_since


#In order to reduce the number of levels in factor variables with large number of levels, we clubbed the factors with low number of listings. In the dataset, we observed that neighbourhood_cleansed, which gives us an idea of the nighbourhood of the listing, has 72 levels. 
library(dplyr)
a <- data %>% group_by(neighbourhood_cleansed) %>% summarise(len = length(neighbourhood_cleansed))

data <- merge(data,a,by = "neighbourhood_cleansed")
data$neighbourhood_cleansed <- as.character(data$neighbourhood_cleansed)
data$neighbourhood_cleansed <- ifelse(data$len<150,"Others",data$neighbourhood_cleansed)
data$len <- NULL

###Missing Value Treatment
# checking missing values in different columns
missing = data.frame(col=colnames(data), type = sapply(data, class), 
missing = sapply(data, function(x) sum(is.na(x)| x=="" | x=="N/A"))/nrow(data) * 100)

# Replace missing values in host_response_time with the most common occurring category (its mode)
data$host_response_time<- sub("N/A","within an hour", data$host_response_time)
# replace missing values in host_response rate with mean values observed for host_response_time when it is within an hour
data$host_response_rate <- ifelse(is.na(data$host_response_rate)==T,0.99,data$host_response_rate)
# Replace missing values in bathrooms, bedrooms and beds with the median value
data$bathrooms <- ifelse(is.na(data$bathrooms)==T, 1, data$bathrooms)
data$bedrooms <- ifelse(is.na(data$bedrooms)==T,1,data$bedrooms)
data$beds <- ifelse(is.na(data$beds)==T,1,data$beds)
# Replace the missing values in review scores with the median values as well
data$review_scores_rating <- ifelse(is.na(data$review_scores_rating)==T,97,data$review_scores_rating)
data$review_scores_accuracy <- ifelse(is.na(data$review_scores_accuracy)==T,10,data$review_scores_accuracy)
data$review_scores_cleanliness <- ifelse(is.na(data$review_scores_cleanliness)==T,10,data$review_scores_cleanliness)
data$review_scores_checkin <- ifelse(is.na(data$review_scores_checkin)==T,10,data$review_scores_checkin)
data$review_scores_communication <- ifelse(is.na(data$review_scores_communication)==T,10,data$review_scores_communication)
data$review_scores_location <- ifelse(is.na(data$review_scores_location)==T,10,data$review_scores_location)
data$review_scores_value <- ifelse(is.na(data$review_scores_value)==T,10,data$review_scores_value)
data$reviews_per_month <- ifelse(is.na(data$reviews_per_month)==T,1.55,data$reviews_per_month)


###Outliers
# Histogram of price
library(ggplot2)
ggplot(data=data, aes(price)) + 
geom_histogram(fill="red") + 
labs(title="Histogram of Price") +
labs(x="Price", y="Count")
# Percentile of price
quantile(data$price, c(.9, .95, .97, 0.975, 0.98, 0.99, 0.995, 0.999, 0.9999))

#As we can see from the histogram as well as the percentile distribution of Price, there are extreme values in Price,So we performed Winsorization at 99% level and captured the maximum value of price at 650 USD.
# Capture the extreme values of Price at 99 percentile level
data$price <- ifelse(data$price>650,650,data$price)
ggplot(data=data, aes(price)) + 
  geom_histogram(fill="red") + 
  labs(title="Histogram of Price") +
  labs(x="Price", y="Count")

# Visualizing the distribution of no of reviews for different cancellation policy using a boxplot
library(ggpubr)
ggboxplot(data, x = "cancellation_policy", y = "reviews_per_month", 
          color = "cancellation_policy",
          ylab = "Bookings per month", xlab = "Cancellation Policy")

#Initial inspection of the data using the boxplot suggests that there are differences in the booking rates for different cancellation policies: the accommodations with Strict and Moderate cancellation policies have higher average booking rates (staying rate). To investigate for if there are significant differences among groups quantitatively, we then fited an one-way ANOVA model as below.

# Compute the analysis of variance
anova1 <- aov(reviews_per_month ~ cancellation_policy, data = data)
# Summary of the analysis
summary(anova1)

#Since, p-value is less than the significance level 0.05 in the moel summary,we reject the null hypothesis and we can say that the average booking rates are different with different cancellation policies.

# Visualizing the distribution of review_scores_rating for different bed type using a boxplot
ggboxplot(data, x = "bed_type", y = "review_scores_rating", 
          color = "bed_type",
          ylab = "Review Scores Rating", xlab = "Bed Type")
#Initial inspection of the data using the boxplot suggests that there are no differences in the review rating for different bed types. To investigate for if there are differences among different groups quantitatively, we fitted an one-way ANOVA model.

# Compute the analysis of variance
anova2 <- aov(review_scores_rating ~ bed_type, data = data)
# Summary of the analysis
summary(anova2)

#Since P-value is larger than 0.05, we accpet the null hypothesis that the average review scores do not change with the bed types.


##lm

data_size <- floor(0.8 * nrow(data))
train_data <- sample(seq_len(nrow(data)), size = data_size)
train <- data[train_data, ]
test <- data[-train_data, ]
price_model <- lm(price ~ as.factor(neighbourhood_cleansed) + host_since + Breakfast + Kitchen + Pets + AirCondition + Internet + TV + reviews_per_month + review_scores_rating + number_of_reviews + review_scores_accuracy + review_scores_value + review_scores_cleanliness + review_scores_checkin + review_scores_communication + review_scores_location + as.factor(cancellation_policy) + guests_included + beds + as.factor(bed_type) + bathrooms + as.factor(property_type) + as.factor(room_type), 
data = train)
summary(price_model)
predictions=predict(price_model,newdata = test)
predictions
baseline_model <- mean(data$price)
(RMSE <- sqrt(mean(baseline_model- test$price)^2))
coefficients(price_model)



#Following are the results that we obtained from the above linear regression model along with the hypothesis/explanations of behaviors of the variables:

#Based on the p-values for the above regression model, we can say that amenities like TV and Air Conditioning significantly impact the prices. Since the Beta-values are positive, we can say that TV and AC have positive effects on prices (considering other variables are fixed). This can be because AC and TV are comparatively expensive amenties.**<br>

#Also, the booking rate is inversely proportional to the prices considering the other factors remain unchanged.**<br>
#Number of guests, number of beds, and number of bathrooms directly affect the prices. As the number of guests, beds or bathrooms increases, prices increase (considering amenities/comfort and location is same). This is reasonable and hotels share the same theory.**<br>

#The categorical variables: neighbourhood, property type, and room type have significant impacts on prices. This is generally true as properties in luxurious localities will be more expensive than other housing areas where provide similar services. Also, a shared room will be cheaper than a whole house/apartment in the same locality.**<br>


#XGBoost

library(xgboost)
target_train=train$price
target_test=test$price
str(target_train)
str(train)
train$price=NULL
feature_names <- names(train)
test$price=NULL
dtrain <- xgb.DMatrix(as.matrix(sapply(train, as.numeric)),label=target_train, missing=NA)
dtest <- xgb.DMatrix(as.matrix(sapply(test, as.numeric)),label=target_test, missing=NA)

# Set up cross-validation scheme (3-fold)
library(caret)
foldsCV <- createFolds(target_train, k=7, list=TRUE, returnTrain=FALSE)
param <- list(booster = "gblinear"
              , objective = "reg:linear"
              , subsample = 0.7
              , max_depth = 5
              , colsample_bytree = 0.7
              , eta = 0.037
              , eval_metric = 'mae'
              , base_score = 0.012 #average
              , min_child_weight = 100)

xgb_cv <- xgb.cv(data=dtrain,
                 params=param,
                 nrounds=100,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 early_stopping_rounds = 30,
                 print_every_n = 5)

# Check best results and get best nrounds
print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
nrounds <- xgb_cv$best_iteration

xgb <- xgb.train(params = param
                 , data = dtrain
                 # , watchlist = list(train = dtrain)
                 , nrounds = nrounds
                 , verbose = 1
                 , print_every_n = 5
                 #, feval = amm_mae
)


# Feature Importance
importance_matrix <- xgb.importance(feature_names,model=xgb)
xgb.plot.importance(importance_matrix[1:15,])
head(test)
# Predict
preds <- predict(xgb,dtest)
preds


#*********************************Wordcloud***********************
library(tm)
library(wordcloud) 
library(RColorBrewer)
Description= data$summary

data1<-Corpus(VectorSource(Description))
data1<-tm_map(data1,stripWhitespace)
data1<-tm_map(data1,tolower)
data1<-tm_map(data1,removeNumbers)
data1<-tm_map(data1,removePunctuation)



data1<-tm_map(data1,removeWords, stopwords('english'))
data1<-tm_map(data1,removeWords, c('and','the','our','that','for','are','also','more','has','must','have','should','this','with'))


wordcloud(data1, scale=c(3,0.5), max.words=1000, 
          random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,'Dark2'))




