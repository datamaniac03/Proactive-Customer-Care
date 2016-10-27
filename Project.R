# set working directory
path <- "/home/arpit/Documents/BDAP03/Project"
setwd(path)

seed <-100
set.seed(seed)

#loading libraries 
library(data.table)
library(ggvis)
library(car)
library(dplyr)

#loading data

data <- fread("/home/arpit/Documents/BDAP03/Project/Data_for_contacts_final.csv")

#dimension of data
dim(data)

#attach the data
attach(data)

# converting NA in contacts to 0
temp <- is.na(contacts)
contacts[temp==TRUE]<-0

#plotting histogram for contacts
data %>%
  ggvis(~contacts, fill:= "red") %>%
  layer_histograms(width= input_slider(0,30), center= 0)
  
#counting the frequency of contacts and incidents
data_contacts <- data[, .(count_contacts = .N),.(contacts)]
is.na(incidents)<- 0

data_incidents <- data[, .(count_incidents = .N),.(incidents)]

#setting the contacts and incidents in increasing order
setorder(data_incidents,incidents)

data_contacts[,":="(contacts=as.factor(contacts),count_contacts=as.numeric(count_contacts))]
setorder(data_contacts,contacts)

data_contacts[,cumm_contact := cumsum(count_contacts)]
data_contacts$contacts[is.na(data_contacts$contacts)]<-0
head(data_contacts)

# checking the no. of incidents and no. of contacts  for each account id
data2<- select(data, c(1,7,12))
data3<- filter(data2, is.na(data2$incidents)==FALSE)
data4<- data3[!duplicated(data3[,1:3]),]


data4 %>%
  ggvis(~data4$contacts, fill:= "red") %>%
  layer_histograms(width = input_slider(0,30))

data4 %>%
  ggvis(~data4$incidents, fill:= "red") %>%
  layer_histograms(width = input_slider(0,30))

###predict outcome is whether a customer will contact to report incident ###

mean_contacts <- mean(data4$contacts)

mean_incidents <- mean(data4$incidents)

#converting contacts where contacts>5 to 1 and less than equal to 5 to zero 
data<- data[,prediction:= ifelse(contacts >5, 1,0)]

temp1 <- is.na(data$prediction)
data$prediction[temp1==TRUE]<-0

