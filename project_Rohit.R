# reading data
Data_for_contacts_final <- read.csv("~/Desktop/Data_for_contacts_final.csv")
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
cc1[is.na(cc1)]<- 0
head(cc1)
table(cc4$contacts)    # checking distribution of contacts
boxplot(cc$contacts)
# Assumption : contacts>140 are outliers
cc1<- cc1%>%filter(cc1$contacts<=140)    # removing outliers
table(cc10$contacts)
median(cc10$contacts) # 0
mean(cc10$contacts) # 2.78

# Assumption : Threshold separating contacts is 2

hist(cc1$contacts)
cc1%>%summarise(count=n()) # 115817
cc1%>%filter(contacts>2)%>%summarise(count=n()) # 26231 
# ~77.35 made no contact, ~22.64 made contact




