set.seed(1015)

# importing libraries
library(survival)
library(dplyr)
library(ggplot2)
library(car)
library(caret)
library(kernlab)
library(MASS)
library(klaR)

########################## reading data from csv file
data_contacts <- read.csv("~/Desktop/Data_for_contacts_final.csv")
# initial exploration
View(data_contacts)
str(data_contacts)
ggplot(data_contacts,aes(x=contacts))+geom_histogram(aes(y=..density..))
 
######################### exploring contacts(dependent variable)
######################### deciding on threshold separating contact(1) and no contact(0)
data_accId_contacts<- select(data_contacts, c(1,7))
head(data_accId_contacts)
str(data_accId_contacts)  # 320995 obs. of 2 variables 
# removing dulticate values from data_accId_contacts
data_accId_contacts<- data_accId_contacts[!duplicated(data_accId_contacts),]
head(data_accId_contacts)
str(data_accId_contacts)  # 115864 obs. of  2 variables
# Assumption: NA in contacts column implies that no contact was made between customer and company.
# therefore replacing NA by 0 in contacts feature 
data_accId_contacts$contacts[is.na(data_accId_contacts$contacts)]<-0
head(data_accId_contacts)
table(data_accId_contacts$contacts)    # checking distribution of contacts
boxplot(data_accId_contacts$contacts)
str(data_accId_contacts)
# Assumption : contacts>140 are outliers
data_accId_contacts<- data_accId_contacts%>%filter(data_accId_contacts$contacts<=140)    
# removing outliers
str(data_accId_contacts)
table(data_accId_contacts$contacts)
boxplot(data_accId_contacts$contacts)
median(data_accId_contacts$contacts) # 0
mean(data_accId_contacts$contacts) # 2.78

#Threshold separating contacts is 2

hist(data_accId_contacts$contacts)
data_accId_contacts%>%summarise(count=n()) # 115817
data_accId_contacts%>%filter(contacts>2)%>%summarise(count=n()) # 26231 
# whwn considering 1 contact per account ID: 
# ~77.35 made no contact, ~22.64 made contact 

######################### adding new class variable:contacts_flag
# making copy of origial dataset
data_contacts2 <- data_contacts
#1)if contacts=NA, equate it to zero
data_contacts2$contacts[is.na(data_contacts2$contacts)]<-0
str(data_contacts2)
#2)add contact_flag variable
data_contacts2 <- data_contacts2%>%mutate(contact_flag=ifelse(contacts>2,1,0))
data_contacts2$contact_flag<- as.factor(data_contacts2$contact_flag) 
str(data_contacts2$contact_flag)
table(data_contacts2$contact_flag)
# 57.24% -0(no contact), 42.7% -1(contact)


########################  UNIVARIATE ND BI-VARIATE(with Class) ANALYSIS
########################  Relation of each independent variable with dependent variable

cc2 <- data_contacts2  # Making copy of dataset for analysis
names(cc2)
#########"Account_id" 
# not needed for model training, needed only for identification

#########"age_in_months"
cc_age<-cc2[,c(1,2,31)]
head(cc_age)
cc_age<- cc_age[!duplicated(cc_age),]  # age unique per acount Id 
head(cc_age)
summary(cc_age$age_in_months)
# right skewed
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.00    4.00   13.00   16.07   26.00   82.00   30315 
boxplot(cc_age$age_in_months)
cc_age2<-cc_age%>%filter(age_in_months<60)
boxplot(cc_age2$age_in_month~cc_age2$contact_flag)
# removed outliers to see boxplot clearly, created cc_age2
# we observe slight variation

####################### "billing_amount" 
cc_bill<-cc2[,c(9,3,31)]
head(cc_age)
cc_bill<- cc_bill[!duplicated(cc_bill),]  # billing amunt unique per order Id 
head(cc_bill)
str(cc_bill)
summary(cc_bill$billing_amount)
boxplot(cc_bill$billing_amount)
cc_bill2<-cc_bill%>%filter(billing_amount<1500)
boxplot(cc_bill2$billing_amount)
boxplot(cc_bill2$billing_amount~cc_bill2$contact_flag)
# removed outliers to see boxplot clearly, created filtered cc_bill2
# we observe slight variation

#########################"brand"
cc_brand <- cc2[,c("brand","contact_flag")]
a<-table(cc_brand$brand)
barplot(a)
b <- table(cc_brand$brand,cc_brand$contact_flag)
b
prop.table(b, 1)

all_dodge <- function(c,i){
  ggplot(c,aes(x=i,fill=as.factor(c$contact_flag)))+geom_bar(position = "dodge",  color="black")+
    scale_fill_brewer(palette = "Pastel1")+
    theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10)) 
}
all_dodge(cc_brand,cc_brand$brand) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

##############################"category" 
cc_category <- cc2[,c("category","contact_flag")]
a<-table(cc_category$category)
barplot(a)
b <- table(cc_category$category,cc_sc$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_category,cc_category$category) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

############################"City" 
cc_city <- cc2[,c("City","contact_flag")]
a<-table(cc_city$City)
barplot(a)
b <- table(cc_city$City,cc_city$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_city,cc_city$City) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

########################### "contacts"
# basis of contact_flag

########################### "cust_flag"
cc_cf <- cc2[,c("cust_flag","contact_flag")]
a<-table(cc_cf$cust_flag)
barplot(a)
b <- table(cc_cf$cust_flag,cc_cf$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_cf,cc_cf$cust_flag) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

###########################"order_id" 
# not needed for model training, needed only for identification

###########################"FF" 
# ignore

###########################"Flag..Contact."
#ignore

###########################"incidents"
# ignore

###########################"IT"
#ignore

###########################"LargeAppliance"                             
#ignore

###################"no_category" 
cc_cat<-cc2[,c("Account_id","no_category","contact_flag")]
head(cc_cat)
cc_cat[400:500,]  # random checking
#unique per account id
cc_cat<- cc_cat[!duplicated(cc_cat),]  # no_category unique per acount Id 
head(cc_cat)
str(cc_cat)
boxplot(cc_cat$no_category)
boxplot(cc_cat$no_category~cc_cat$contact_flag)
# variation is there

###################"no_orders"  
cc_ord<-cc2[,c("Account_id","no_orders","contact_flag")]
# no_order unique per acount Id 
head(cc_ord)
cc_ord<- cc_ord[!duplicated(cc_ord),] 
head(cc_ord)
str(cc_ord)
boxplot(cc_ord$no_orders)
boxplot(cc_ord$no_orders~cc_ord$contact_flag)
cc_ord2<-cc_ord%>%filter(no_orders<100) # removing outliers
boxplot(cc_ord2$no_orders~cc_ord2$contact_flag)
#variation is there 

###################"no_super_category" 
cc_scat<-cc2[,c("Account_id","no_super_category","contact_flag")]
head(cc_scat)
cc_scat[400:500,]  # random checking
#unique per account id
cc_scat<- cc_scat[!duplicated(cc_scat),]  # no_super_category unique per acount Id 
head(cc_scat)
str(cc_scat)
boxplot(cc_scat$no_super_category)
boxplot(cc_scat$no_super_category~cc_scat$contact_flag)
# variation is there

##################"offers"
cc_offer <- cc2[,c("offers","contact_flag")]
a<-table(cc_offer$offers)
barplot(a)
b <- table(cc_offer$offers,cc_offer$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_offer,cc_offer$offers) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# transforming offers into 0-offers 1-no offer
cc_offer1<- cc_offer%>%mutate(offers_flag=ifelse(offers=='NotFound',0,1))
head(cc_offer1)
b<- table(cc_offer1$offers_flag,cc_offer1$contact_flag)
b
mosaicplot(b,color = TRUE)

names(a[which.max(a)])
cc_offer_fil<- cc_offer[cc_offer$offers!="NotFound",]
str(cc_offer_fil)
#ignore , max data is Not Found

##################"paid_amount"
# ignore

###################"payment_method"
cc_pm <- cc2[,c("payment_method","contact_flag")]
a<-table(cc_pm$payment_method)
barplot(a)
b <- table(cc_pm$payment_method,cc_pm$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_pm,cc_pm$payment_method) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

########################"product_id"
# ignore

#######################"sales_channel"
cc_s_chan <- cc2[,c("sales_channel","contact_flag")]
a<-table(cc_s_chan$sales_channel)
barplot(a)
b <- table(cc_s_chan$sales_channel,cc_s_chan$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_s_chan,cc_s_chan$sales_channel) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

####################### "seller_id"         
# ignore

##################    "State"
cc_state <- cc2[,c("State","contact_flag")]
a<-table(cc_state$State)
barplot(a)
b <- table(cc_state$State,cc_state$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_state,cc_state$State) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

##################    "sub_category"
cc_subcat <- cc2[,c("sub_category","contact_flag")]
a<-table(cc_subcat$sub_category)
barplot(a)
b <- table(cc_subcat$sub_category,cc_sc$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_subcat,cc_subcat$sub_category) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

##################          "super_category"
cc_sc <- cc2[,c("super_category","contact_flag")]
a<-table(cc_sc$super_category)
barplot(a)
b <- table(cc_sc$super_category,cc_sc$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_sc,cc_sc$super_category)
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is present

###################"title"  
#ignore

###################"total_billing_amount"
#ignore

###################"unit_sla..expected.delivery.time."
cc_time<-cc2[,c("order_id","unit_sla..expected.delivery.time.","contact_flag")]
head(cc_time)
View(cc_time)
str(cc_time) #320995
#unique per order id
cc_time<- cc_time[!duplicated(cc_time),]  # time unique per acount Id 
head(cc_time)
str(cc_time) #143341
cc_time1 <- cc_time%>%group_by(order_id)%>%filter(n()==1)
View(cc_time1)
cc_time1<-cc_time1%>%filter(unit_sla..expected.delivery.time.<20) # removing outliers
View(cc_time1)
boxplot(cc_time1$unit_sla..expected.delivery.time.)
boxplot(cc_time1$unit_sla..expected.delivery.time.~cc_time1$contact_flag)
# no variation is there

####################"vertical.between.category.and.sub.category."
cc_ver <- cc2[,c("vertical.between.category.and.sub.category.","contact_flag")]
a<-table(cc_ver$vertical.between.category.and.sub.category.)
barplot(a)
b <- table(cc_ver$vertical.between.category.and.sub.category.,cc_sc$contact_flag)
b
prop.table(b, 1)
all_dodge(cc_ver,cc_ver$vertical.between.category.and.sub.category.) + labs(fill="contacts")
mosaicplot(b,color = TRUE)
chisq.test(b)
# variation is there

####################"contact_flag"
# class variable



############## BINNING AND SELECTING VARIABLES FOR MODELLING
# relevent variables after analysis:
#'Account_id','order_id','age_in_months', 'billing_amount', 'no_category', 
#'no_orders', 'no_super_category',
#'unit_sla..expected.delivery.time','Brand,category','City','cust_flag',
#'payment_method','sales_channel','State','sub_category','super_category',
#"vertical.between.category.and.sub.category.",'offers','contact_flag')

data_contacts_relevent <- data_contacts2[,c(1,2,3,4,5,6,8,9,15,16,17,20,22,24,25,26,29,30,31)]
View(data_contacts_relevent)
dim(data_contacts_relevent)
names(data_contacts_relevent)
head(data_contacts_relevent)

## BINNING
#converting categorical variables with large no. of levels into categorical variables with small no of levels(<50)
#removing brand,city,sub_category,vertical.between.category.and.sub.category(For the removed variables, variables with small no of levels already exist)
# for city----> state, for others ----> super-category,category
# also removing account id,order id, as they are not required for modelling

data_contacts_relevant2<- data_contacts_relevent[,c(2,3,5,7,9,10,11,12,13,14,16,17,19)]
str(data_contacts_relevant2) #320995 obs. of  13 variables

############################# MISSING VALUE TREATMENT

sapply(data_contacts_relevant2, function(x) sum(is.na(x)))
cc_temp <- data_contacts_relevant2[,c(1,5,6,7)]
str(cc_temp)
cc_temp2 <- cc_temp%>%filter(!is.na(age_in_months))
sapply(cc_temp2, function(x) sum(is.na(x)))
## all na is these four columns occur together
## thus deleting all rows which these four features as NA
data_contacts_relevant3<- data_contacts_relevant2[complete.cases(data_contacts_relevant2),]
str(data_contacts_relevant3)   #247268 obs. of  13 variables
# ratio of complete cases: 247268/320995 ~77%

# checking for other missing/irregular values
summary(data_contacts_relevant3$age_in_months)     
summary(data_contacts_relevant3$billing_amount)
summary(data_contacts_relevant3$category)        # NotFound-11
summary(data_contacts_relevant3$cust_flag)   
summary(data_contacts_relevant3$no_category)
summary(data_contacts_relevant3$no_orders)
summary(data_contacts_relevant3$no_super_category)
summary(data_contacts_relevant3$payment_method)
summary(data_contacts_relevant3$sales_channel)
summary(data_contacts_relevant3$State)          # " "- 22234
summary(data_contacts_relevant3$super_category)  # NotFound -11
summary(data_contacts_relevant3$unit_sla..expected.delivery.time.)

# category,subcategory
cc_temp3 <- data_contacts_relevant3[,c(3,11)]
head(cc_test3)
cc_temp3%>%filter(category=='NotFound')
# treating NotFound in category/subgategory as a separate level as they occur together

# State
cc_temp4 <- data_contacts_relevent[complete.cases(data_contacts_relevent),]
str(cc_temp4) #247268 obs. of  19 variables
cc_temp5 <-cc_temp4[,c("City","State")]
cc_temp5%>%filter(State=="")%>%group_by(City)%>%summarise(count=n())
#City count
# <fctr>     <int>
#1              42     
#2 NEW DELHI 22192     
# 99.8% percent of missing values in State belong to NEW Delhi
# Thus replacing missing values in state by New_Delhi
levels(data_contacts_relevant3$State) <- c(levels(data_contacts_relevant3$State), 'New_Delhi')
data_contacts_relevant3$State[data_contacts_relevant3$State == ''] <- 'New_Delhi'
summary(data_contacts_relevant3$State)


#########################   SKEWNESS AND TRANSFORMATIONS
cc12 <- data_contacts_relevant3
str(cc12)

plot(density(cc12$age_in_months)) # right-skewed, taking log
summary(cc12$age_in_months)
age_log <- log(cc12$age_in_months) 
plot(density(age_log))
# may be advantageous

plot(density(cc12$billing_amount))
summary(cc12$billing_amount) # taking sqrt, not log because contains values equal to zero
billing_sqrt <- sqrt(cc12$billing_amount)
plot(density(billing_sqrt))
#may be advantageous

plot(density(cc12$no_category))
summary(cc12$no_category)
plot(density(log(cc12$no_category)))
# no need

plot(density(cc12$no_orders))
summary(cc12$no_orders) # right-skewed, taking log
no_orders_log <- log(cc12$no_orders)
plot(density(no_orders_log))
# advantageous

plot(density(cc12$no_super_category))
summary(cc12$no_super_category)
# no need

plot(density(cc12$unit_sla..expected.delivery.time.))
summary(cc12$unit_sla..expected.delivery.time.)
# taking sqrt, not log because contains values equal to zero
time_sqrt<- sqrt(cc12$unit_sla..expected.delivery.time.)
plot(density(time_sqrt))
# a bit advantageous, but still skewed

#### adding new variables to dataset, age_log, bill_sqrt,no_orders_log,time_sqrt
data_contacts_relevant_transformed <- data_contacts_relevant3%>%mutate(age_log=log(age_in_months),bill_sqrt=sqrt(billing_amount),no_orders_log=log(no_orders),time_sqrt=sqrt(unit_sla..expected.delivery.time.))
str(data_contacts_relevant_transformed)
# droping original columns
data_contacts_final <- data_contacts_relevant_transformed[,(c(-1,-2,-6,-12))]
str(data_contacts_final)


############################   MODELING

################ GLM/Logistic
model_logistic <-glm(contact_flag~.,data =data_contacts_final,family = binomial(link="logit"))
summary(model_logistic)
# in presence of super-category , no use for category, reflected in summary as NA
# individually category in fine, but with super-category, it can be ignored
model_logistic_test <-glm(contact_flag~super_category,data =data_contacts_final,family = binomial(link="logit"))
summary(model_logistic)
model_logistic_test2 <-glm(contact_flag~category+super_category,data =data_contacts_final,family = binomial(link="logit"))
summary(model_logistic)

data_contacts_final1 <- data_contacts_final[,-1]
str(data_contacts_final1)
model_logistic1 <-glm(contact_flag~.,data =data_contacts_final1,family = binomial(link="logit"))
summary(model_logistic1)
#vif
vif(model_logistic1)  # no muti-collearity
# cross-validation
train_control <- trainControl(method="cv",number=5)
model_logistic_cv <- train(contact_flag~.,data =data_contacts_final1,trControl = train_control, method = "glm")
print(model_logistic_cv)  
# Accuracy   Kappa    
# 0.6996821  0.3853365


### GBM
train_control <- trainControl(method="cv",number=3)
modelGbm <- train(contact_flag~., data=data_contacts_final1,trControl=train_control, method="gbm",verbose=FALSE)
print(modelGbm) 



