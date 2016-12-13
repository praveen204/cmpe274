R-Code for Spotify Data Analaysis
##### Storing the data from raw csv files into different data frames ####################

data1←read.csv("C:\\Users\\bhanu \\Documents\\Mining_Project\\Raw_Data\\Spotify\\avg_Premium_usrs_Account_period.csv")
 data2←read.csv("C:\\Users\\bhanu \\Documents\\Mining_Project\\Raw_Data\\Spotify\\femaleAgeRange.csv")
 data3←read.csv("C:\\Users\\bhanu \\Documents\\Mining_Project\\Raw_Data\\Spotify\\maleAgeRange.csv")
 data4←read.csv("C:\\Users\\bhanu \\Documents\\Mining_Project\\Raw_Data\\Spotify\\AvgStream_MinutesperSession.csv")
 data5←read.csv("C:\\Users\\bhanu \\Documents\\Mining_Project\\Raw_Data\\Spotify\\TotalDataset.csv")


###### Merging the raw data files of different genders  ################################

my_data<-rbind(data2,data3)

###### Checking the data #######################################################

head(my_data) tail(my_data) str(my_data)
####### Storing the required attribute names into a data frame ########################

select_attribute<-c("UniqueCarrier","Month","DayofMonth","DayOfWeek","DepTime","DepDelay", "Origin")

######## Creating a subset from the main data set with necessary attribute ##############

data_subset<-subset(my_data,select = select_attribute)

######## Preprocessing the data: Selecting entries with gender of male and female ########

new_data1<-subset(data_subset,gender=’male’| gender=’Female’)



####### Removing the rows with missing values ###################################

cleansed_dataset<-new_data2[complete.cases(new_data2),]

####### Removing the N/A values #################################################

Cleansed_dataset<-na.omit(new_data2)
####### Converting nominal value to numeric (UniqueCarrier) #######################

cleansed_dataset$UniqueCarrier=ifelse(cleansed_dataset$UniqueCarrier=="WN",1, ifelse(cleansed_dataset$UniqueCarrier=="OO",2, ifelse(cleansed_dataset$UniqueCarrier=="UA",3, ifelse(cleansed_dataset$UniqueCarrier=="DL",4, ifelse(cleansed_dataset$UniqueCarrier=="AA",5, ifelse(cleansed_dataset$UniqueCarrier=="NW",6, "NA"))))))

######## Adding Data Label for spotify data analyzed data ##################################

cleansed_dataset$Label_Departure_Delay_Time<-ifelse(cleansed_dataset$DepDelay>10, "Yes","No")



####### Filtering out DepDelay for KNN analysis ##################

cleansed_dataset<-subset(cleansed_dataset,select=-c(DepDelay))

####### Converting char to numeric for attribute UniqueCarrier #######################

cleansed_dataset$UniqueCarrier=as.numeric(cleansed_dataset$UniqueCarrier)

####### Creating function to normalize the values ##################################

function(x) {
return ((x - min(x)) / (max(x) - min(x))) }

####### Applying min-max normalization to cleansed_dataset ########################

spotify_normalize<-as.data.frame(lapply(cleansed_dataset[1:6], normalize))

####### Storing the Cleansed_Dataset Labels in a data frame ########################

spotify_label<-cleansed_dataset$Label_Departure_Delay_Time

######## Installing package “class” for KNN classifier functions ######################

install.packages("class") library(class)

####### Predicting the test labels using trained dataset #############################

test_prediction<-knn(train=spotify_normalize,test=spotify_normalize,cl=spotify_label,k=400)

######### Installing package “gmodels” to apply cross-table validations #############
install.packages("gmodels")




Efficiency = 122453/164588 =74.399%
K=5


Accuracy= 115926/164588 = 70.43%
Accuracy= 118885/164588 = 72.23% K=100


































Accuracy= 121618/164588 = 73.89%

K=200


K=300


Accuracy = 122182/164588 = 74.23


K=485


Accuracy = 12588/164588 = 74.48%
