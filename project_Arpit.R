# reading data
set.seed(1015)
Data_for_contacts_final <- read.csv("/home/arpit/Documents/BDAP03/Project/Data_for_contacts_final.csv")
cc <- Data_for_contacts_final
View(cc)
str(cc)

# exploration
library(ggplot2) 
ggplot(cc,aes(x=contacts))+geom_histogram(aes(y=..density..))+geom_density()

##### ignoring incidents for now
#str(cc$incidents)
#dim(cc$incidents)
#summary(cc$incidents)
#cc1 <- cc$incidents[is.na(cc$incidents)==FALSE]
#cc2 <- select(cc,c())
#str(cc1)
#cc_contacts <- cc$contacts[is.na(cc$incidents)==FALSE]
#str(cc_contacts)

#cc3 <- filter(cc1,account_id=unique(cc$Account_id))

########
# cc1 is subset of cc with only Account_id and contacts column
library(dplyr)
cc1<- select(cc, c(1,7))
head(cc1)
str(cc1)  # 320995 obs. of 2 variables 

# removing dulticate values from dataset
cc1<- cc1[!duplicated(cc1),]
head(cc1)
str(cc1)  # 115864 obs. of  2 variables
# Assumption NA in contacts column = 0 
#cc1[is.na(cc1)]<- 0
cc1$contacts[is.na(cc1$contacts)]<-0
head(cc1)
table(cc1$contacts)    # checking distribution of contacts
boxplot(cc$contacts)
# Assumption : contacts>140 are outliers
cc1<- cc1%>%filter(cc1$contacts<=140)    # removing outliers
table(cc1$contacts)
median(cc1$contacts) # 0
mean(cc10$contacts) # 2.78

# Assumption : Threshold separating contacts is 2

hist(cc1$contacts)
cc1%>%summarise(count=n()) # 115817
cc1%>%filter(contacts>2)%>%summarise(count=n()) # 26231 
# ~77.35 made no contact, ~22.64 made contact

#making new dataset cc2 with class variable
#1)if contacts=NA, equate to zero
#2)add contact_flag variable
cc2 <- cc
cc2$contacts[is.na(cc2$contacts)]<-0
str(cc2)
cc2 <- cc2%>%mutate(contact_flag=ifelse(contacts>2,1,0))
str(cc2)
table(cc2$contact_flag)
# 57.24% -0(no contact), 42.7% -1(contact)

factcols <- c(4:6,8:9,13:14,18,20:27,30,31)
numcols <- setdiff(1:31,factcols)

library(data.table)
cc2<- as.data.table(cc2)
cc2[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
cc2[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

#subset categorical and numerical variables
cat_cc <- cc2[,factcols, with=FALSE]
View(cat_cc)
num_cc <- cc2[,numcols,with=FALSE]
#adding contact_flag to num_cc
num_cc[,contact_flag := cc2$contact_flag]

tr <- function(c, a){
  ggplot(data = c, aes(x= a,))+geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) 
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# UNIVARIATE ANALYSIS FOR NUMERIC VARIABLES(AGE IN MONTHS)
p<- num_cc[,c(1:2,14), with= FALSE]
# removed duplicate values
num_cc1<- p[!duplicated(p),]
age1 <-ggplot(data=num_cc1,aes(x=contact_flag, y= age_in_months)) + geom_boxplot(aes(colour=contact_flag))
age2 <-tr(num_cc1,num_cc1$age_in_months)+ labs(x="age_in_months")
multiplot(age1,age2,cols = 2)

# billing amount on order id
q<- cc2[,c(3,9,31),with=FALSE]
num_cc2<- q[!duplicated(q),]
numcc2<- num_cc2[num_cc2$order_id!="od_21151"]
bill1 <-ggplot(data=num_cc2,aes(x=contact_flag, y= billing_amount)) + geom_boxplot(aes(colour=contact_flag))
bill2 <-tr(num_cc2,num_cc2$billing_amount)+labs(x="billing_amount")
tr(num_cc2,log(num_cc2$billing_amount))
multiplot(bill1,bill2,cols = 2)
# billing amount outliers not taken...

#fF ignore 
#incidents ignore

#no_catregory( important feaature)
r<- cc2[,c(1,15,31),with=FALSE]
num_cc3<- r[!duplicated(r),]
category1 <-ggplot(data=num_cc3,aes(x=contact_flag, y= no_category)) + geom_boxplot(aes(colour=contact_flag))
category2<-tr(num_cc3,num_cc3$no_category)+labs(x="no_category")

multiplot(category1,category2,cols = 3)

#no_orders
# considering outliers are removed
s<- cc2[,c(1,16,31),with=FALSE]
num_cc4<- s[!duplicated(s),]
num_cc4<- num_cc4[no_orders<250,]
orders1 <-ggplot(data=num_cc4,aes(x=contact_flag, y= no_orders)) + geom_boxplot(aes(colour=contact_flag))
orders2<-tr(num_cc3,num_cc3$no_category)+labs(x="no_orders")

multiplot(orders1,orders2,cols = 3)

#no_supercategory
t<- cc2[,c(1,17,31),with=FALSE]
num_cc5<- t[!duplicated(t),]
num_cc4<- num_cc4[no_orders<250,]
super1 <-ggplot(data=num_cc5,aes(x=contact_flag, y= no_super_category)) + geom_boxplot(aes(colour=contact_flag))
super2<-tr(num_cc5,num_cc5$no_super_category)+ labs(x="no_supercategory")

multiplot(super1,super2,cols = 2)

#expected delivery time
#variable with rohit 
u<- cc2[,c(1,29,31),with=FALSE]
u<- as.data.frame(u)
num_cc6<- u[duplicated(u$unit_sla..expected.delivery.time.),]

all_dodge <- function(c,i){
  ggplot(c,aes(x=i,fill=c$contact_flag))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

#super category
all_dodge(cat_cc,cat_cc$super_category)+ labs(fill="Contact_Flag")+labs(x="super_category")

library(MASS)

tbl = table(cat_cc$super_category,cat_cc$contact_flag) 
tbl  
chisq.test(tbl) 

mosaicplot(tbl,color = TRUE)
#category
all_dodge(cat_cc,cat_cc$category)+ labs(fill="Contact_Flag")

tbl = table(cat_cc$category,cat_cc$contact_flag) 
tbl  
chisq.test(tbl) 

mosaicplot(tbl,color = TRUE)

#association test bewteen super_category and category
tbl1 = table(cat_cc$super_category,cat_cc$category)
chisq.test(tbl1)

#sub-category
all_dodge(cat_cc,cat_cc$sub_category)+ labs(fill="Contact_Flag")

tbl2 = table(cat_cc$sub_category,cat_cc$contact_flag) 
tbl  
chisq.test(tbl2) 

mosaicplot(tbl,color = TRUE)

#vertical between category and sub category
all_dodge(cat_cc,cat_cc$vertical.between.category.and.sub.category.)+ labs(fill="Contact_Flag")

tbl3 = table(cat_cc$vertical.between.category.and.sub.category.,cat_cc$contact_flag) 
tbl3  
chisq.test(tbl3) 

mosaicplot(tbl3,color = TRUE)

##brand
all_dodge(cat_cc,cat_cc$brand)+ labs(fill="Contact_Flag")

tbl4 = table(cat_cc$brand,cat_cc$contact_flag) 
tbl4  
chisq.test(tbl4) 

mosaicplot(tbl4,color = TRUE)

#city
all_dodge(cat_cc,cat_cc$City)+ labs(fill="Contact_Flag")

tbl4 = table(cat_cc$City,cat_cc$contact_flag) 
tbl4  
chisq.test(tbl4) 

mosaicplot(tbl4,color = TRUE)

#state
all_dodge(cat_cc,cat_cc$State)+ labs(fill="Contact_Flag")

tbl5 = table(cat_cc$State,cat_cc$contact_flag) 
tbl5  
chisq.test(tbl5) 

mosaicplot(tbl5,color = TRUE)


#cust_flag
all_dodge(cat_cc,cat_cc$cust_flag)+ labs(fill="Contact_Flag")

tbl6 = table(cat_cc$cust_flag,cat_cc$contact_flag) 
tbl6  
chisq.test(tbl6) 

mosaicplot(tbl6,color = TRUE)
#space....data is not there

#payment method
all_dodge(cat_cc,cat_cc$payment_method)+ labs(fill="Contact_Flag")

tbl7 = table(cat_cc$payment_method,cat_cc$contact_flag) 
tbl7  
chisq.test(tbl7) 

mosaicplot(tbl7,color = TRUE)

#sales channel

all_dodge(cat_cc,cat_cc$sales_channel)+ labs(fill="Contact_Flag")

tbl8 = table(cat_cc$sales_channel,cat_cc$contact_flag) 
tbl8  
chisq.test(tbl8) 

mosaicplot(tbl7,color = TRUE)

cc10 <- cc2[,c(1,2,3,4,5,6,8,9,15,16,17,20,22,24,25,26,29,30,31), with = FALSE]
View(cc10)
dim(cc10)

###
names(cc10)
head(cc10)

#[1] "Account_id"                                  "age_in_months"                              
#[3] "billing_amount"                              "brand"                                      
#[5] "category"                                    "City"                                       
#[7] "cust_flag"                                   "order_id"                                   
#[9] "no_category"                                 "no_orders"                                  
#[11] "no_super_category"                           "payment_method"                             
#[13] "sales_channel"                               "State"                                      
#[15] "sub_category"                                "super_category"                             
#[17] "unit_sla..expected.delivery.time."           "vertical.between.category.and.sub.category."
#[19] "contact_flag"                 

str(cc10)
## BINNING
## assumption: converting categorical variables with large no. of levels 
#              into categorical variables with small no of levels(<50)
#  removing brand,city,sub_category,vertical.between.category.and.sub.category.
# for the removed variables, variables with small no of levels already exist
# for city----> state, for others ----> super-category,category
# also removing account id,order id

cc11<- cc10[,c(2,3,5,7,9,10,11,12,13,14,16,17,19), with= FALSE]
########################################################Missing value treatment
str(cc11)
#320995 obs. of  13 variables
sapply(cc11, function(x) sum(is.na(x)))
cc_test <- cc11[,c(1,5,6,7), with= FALSE]
str(cc_test)
cc_test2 <- cc_test%>%filter(!is.na(age_in_months))
sapply(cc_test2, function(x) sum(is.na(x)))
## all na is these four columns occur together
## thus deleting all rows which these four features as NA


cc12<- cc11[complete.cases(cc11),]
str(cc_12)
#247268 obs. of  13 variables
# ratio of complete cases: 247268/320995~77%

summary(cc12$age_in_months)   
summary(cc12$billing_amount)
summary(cc12$category)        # NotFound-11
summary(cc12$cust_flag)
summary(cc12$no_category)
summary(cc12$no_orders)
summary(cc12$no_super_category)
summary(cc12$payment_method)
summary(cc12$sales_channel)
summary(cc12$State)          # " "- 22234
summary(cc12$super_category)  # NotFound -11
summary(cc12$unit_sla..expected.delivery.time.)

cc_test3 <- cc12[,c(3,11), with = FALSE]
head(cc_test3)
cc_test3%>%filter(category=='NotFound')
# treating NotFound in category/subgategory as a separate level as they occur together


#####   outliers
str(cc12)
names(cc12)
#[1] "age_in_months"                     "billing_amount" 
#[3] "category"                          "cust_flag"                        
#[5] "no_category"                       "no_orders"                        
#[7] "no_super_category"                 "payment_method"                   
#[9] "sales_channel"                     "State"                            
#[11] "super_category"                    "unit_sla..expected.delivery.time."
#[13] "contact_flag"     

cc13 <- cc12[,c(1,2,5,6,7), with= FALSE]
library(car)
a<- cor(cc13)
library(corrplot)
corrplot.mixed(a)
plot(density(cc12$age_in_months))
# right-skewed, taking log
age_log <- log(cc12$age_in_months)
plot(density(age_log))
# may be advantageous

plot(density(cc12$billing_amount))
summary(cc12$billing_amount)
cc12%>%select(billing_amount)%>%filter(billing_amount==0)%>%summarise(count=n())
# right-skewed, cant take log, because there are values that are zero
# possible when, discount/scheme is applied
# taking square root
billing_sqrt<- sqrt(cc12$billing_amount)
plot(density(billing_sqrt))
# a bit advantageous, but still skewed

plot(density(cc12$no_category))
summary(cc12$no_category)
plot(density(log(cc12$no_category)))
# no need

plot(density(cc12$no_orders))
# right-skewed, taking log
no_orders_log <- log(cc12$no_orders)
plot(density(no_orders_log))
# advantageous

plot(density(cc12$no_super_category))
summary(cc12$no_super_category)
# no need

plot(density(cc12$unit_sla..expected.delivery.time.))
summary(cc12$unit_sla..expected.delivery.time.)
cc12%>%select(unit_sla..expected.delivery.time.)%>%filter(unit_sla..expected.delivery.time.==0)%>%summarise(count=n())
# right-skewed, cant take log, because there are values that are zero
# taking square root
plot(density(sqrt(cc12$unit_sla..expected.delivery.time.)))
# a bit advantageous, but still skewed


# adding new variables to dataset, age_log, bill_sqrt,no_orders_log,time_sqrt
cc13 <- cc12%>%mutate(age_log=log(age_in_months),bill_sqrt=sqrt(billing_amount),no_orders_log=log(no_orders),time_sqrt=sqrt(unit_sla..expected.delivery.time.))
str(cc13)
library(dplyr)
# droping original columns
cc14 <- cc13[,-c(1,2,6,12)]
str(cc14)

#####modelling
#1] "category"          "cust_flag"         "no_category"       "no_super_category"
#[5] "payment_method"    "sales_channel"     "State"             "super_category"   
#[9] "contact_flag"      "age_log"           "bill_sqrt"          "no_orders_log"    
#[13] "time_sqrt"
 # logistic regression model
model_logistic <-glm(contact_flag~.,data =cc14,family = binomial(link="logit"))
summary(model_logistic)

library(caret)
train_control <- trainControl(method="cv",number=5)
model_logistic_cv <- train(contact_flag~.,data =cc14,trControl = train_control, method = "glm")
print(model_logistic_cv) 

# decision tree model
model_decision_tree_cv <- train(contact_flag~.,data =cc14,trControl = train_control, method = "rpart")

# print decision tree model
print(model_decision_tree_cv)

# svm model 
model_svm_cv <- train(contact_flag~.,data =cc14,trControl = train_control, method = "svmLinear2")


