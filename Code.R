library(rgdal)
library(sp)
library(dplyr)
library(tidyverse)
library(stringr)
library(caret)
library(AppliedPredictiveModeling)
library(pROC)

############ Address point dataset

address_points <- read.csv('D:/study/Gmu/sem2/or568/fire/data/Address_Points.csv', sep = ',')

address_points<-address_points[c(1,2,6,8)]
# Removing unnecessary columns 

colnames(address_points)[1]<-"X"

address_points <- as.data.frame( apply(address_points,2, function(x) gsub("\\s+", "", x)))

# This dataset can be used to join other important datasets as it contains lat, long, address key and Parid


address_points$PARCEL_PIN<-gsub('\\s+', '', address_points$PARCEL_PIN)





############## Fire incidents dataset


finc <- read.csv('D:/study/Gmu/sem2/or568/fire/data/fire_incidents.csv', sep = ',')


finc_clean <- finc[ -c(1:5,7) ] 
# Removing unnecessary columns 

finc_clean$IncidentCategory <- as.character(finc_clean$IncidentCategory)

dim(finc)
finc_clean$IncidentCategory <- trimws(finc_clean$IncidentCategory, which = c("both", "left", "right"))
finc_clean$IncidentCategory <- replace(finc_clean$IncidentCategory, finc_clean$IncidentCategory == "FALSE", "F")
finc_clean <- finc_clean %>% filter(IncidentCategory == "F")
# To keep rows only related to fire
str(finc_clean)

finc_clean <- finc_clean[ -c(2:6,15,20:21,25,27:35) ] 

table(finc_clean$NFIRSTypeCode)

# Filtering so we can keep only fire related codes 
finc_clean <- filter(finc_clean, !(NFIRSTypeCode %in% 700:729))
finc_clean <- filter(finc_clean, !(NFIRSTypeCode %in% 747:815))
finc_clean <- filter(finc_clean, !(NFIRSTypeCode %in% 500:530))
finc_clean <- filter(finc_clean, !(NFIRSTypeCode %in% 532:555))
finc_clean <- filter(finc_clean, !(NFIRSTypeCode %in% 300:350))
finc_clean <- filter(finc_clean, !(NFIRSTypeCode %in% 352:381))
finc_clean <- filter(finc_clean, !(NFIRSTypeCode %in% 611:622))
table(finc_clean$NFIRSTypeCode)
# Filtering rows only according to fire violations and violations which can be a reason for fire 

finc_clean <- finc_clean[ -c(19:32,34:57) ] 
# Removing unnecessary columns 

finc_clean$Incident_Happ <- 1



############ Fire inspection dataset


finsp <- read.csv('D:/study/Gmu/sem2/or568/fire/data/fire_inspections.csv', sep = ',')


Dataclean<-finsp[ -c(1,2,6,7,9,10,12,19,21) ]
# Removing unnecessary columns  

colnames(finsp)

table(finsp$StatusName)
finsp_clean <- finsp

table(finsp_clean$Office)
Dataclean<-finsp_clean[ -c(1,2,6,7,9,10,12,19,21) ]
Dataclean<-Dataclean[ -c(4,7)]


# Filtering data because we are predicting fire in House etc.
finsp_clean <- Dataclean %>% filter(ApplicationDescription != "TRANSPORT VEHICLE FOR BLASTING")
finsp_clean<- finsp_clean[-2]

sum(is.na(finsp_clean)) # No null values



##################### Tax_buildings dataset

tax<- read.csv('D:/study/Gmu/sem2/or568/fire/data/Tax_Administration_s_Real_Estate_-_Dwelling_Data.csv', sep = ',')

tax$Total_No_Years<- 2022-tax$YRBLT 

# Remodifying the data and created one more column to see if the buildings were modified or not
tax$YRREMOD[is.na(tax$YRREMOD)] <- "NO"

tax$YRREMOD[!is.na(tax$YRREMOD)]<- "YES"


table(tax$USER9_DESC)


# Dropping columns which are not needed
tax<-tax[-c(1,4,5,13,14,15,22,23,24,25,26,27)]


colnames(tax)[3]<- "Remodified"

tax$PARID<-gsub('\\s+', '', tax$PARID)

colSums(is.na(tax))

################################


################# Tax data


tax_adm_real_estate <- read.csv('D:/study/Gmu/sem2/or568/fire/data/Tax_Administration_s_Real_Estate_-_Land_Data.csv', sep = ',')

#Object id is unique and taxyr has one value 2023 - removing them
tax_adm_real_estate <- tax_adm_real_estate[ -c(1,3) ] 
head(tax_adm_real_estate)
colSums(is.na(tax_adm_real_estate))
#SF,ACRES,UNITS have na values - can drop these columns too if these values are not required then 
#tax_adm_real_estate <- finc[ -c(3:5) ] 
tax_adm_real_estate$PARID<-gsub('\\s+', '',tax_adm_real_estate$PARID)



colSums(is.na(tax_adm_real_estate))

########################################################################################################


### Joining datasets

#Joining tax land and tax dwelling datasets using Parcel ID
first_merge<-merge(x=tax, y= tax_adm_real_estate, by.x="PARID", by.y="PARID")

#Joining Address points and merged tax data using parcel ID
second_merge<-merge(x=first_merge, y= address_points, by.x="PARID", by.y="PARCEL_PIN")


#Converting the location coordinates of fire incidents from virginia state plane coordinate system
#to lattitude and longitude format (WGS84 Coordinate System)
data1= data.frame(x=finc_clean$xCoordinate,y=finc_clean$yCoordinate)
coordinates(data1) <- ~ x + y
proj4string(data1) <- CRS("+init=epsg:2283")
finc_clean[,14:15] = data.frame(spTransform(data1, CRS("+init=epsg:4326")))
head(finc_clean)

#Reducing the location coordinates to 4 decimal places to get matches for joined data
finc_clean[,14] <- sprintf(as.numeric(finc_clean[,14]), fmt = '%#.4f')
finc_clean[,15] <- sprintf(as.numeric(finc_clean[,15]), fmt = '%#.4f')
finsp_clean[,5] <- sprintf(as.numeric(finsp_clean[,5]), fmt = '%#.4f')
finsp_clean[,6] <- sprintf(as.numeric(finsp_clean[,6]), fmt = '%#.4f')

#Joining fire inspections and fire incidents data using location coordinates
finsp_finc <- merge(x=finsp_clean,y=finc_clean, by.x=c("X","Y"), by.y=c("xCoordinate","yCoordinate"),all.x=TRUE)

#Creating the binary response variable by making null values 0 after joining data
finsp_finc$Incident_Happ[is.na(finsp_finc$Incident_Happ)] <- 0
table(finsp_finc$Incident_Happ)

#Dropping unneccessary columns
finsp_finc<- finsp_finc[-c(13:29)]

#Removing the duplicate records from joined data
finsp_finc<-finsp_finc %>% distinct(X, Y,ApplicationType,ApplicationStatus,StatusName, .keep_all = TRUE)
table(finsp_finc$Incident_Happ)

#Reducing the location coordinates to 4 decimal places to get matches for joined data
finsp_finc[,1] <- sprintf(as.numeric(finsp_finc[,1]), fmt = '%#.4f')
finsp_finc[,2] <- sprintf(as.numeric(finsp_finc[,2]), fmt = '%#.4f')
second_merge[,22] <- sprintf(as.numeric(second_merge[,22]), fmt = '%#.4f')
second_merge[,23] <- sprintf(as.numeric(second_merge[,23]), fmt = '%#.4f')

#Joining the fire data with the other merged datasets using location coordinates
fin <- merge(x=finsp_finc,y=second_merge, by=c("X","Y"))
table(fin$Incident_Happ)

#Removing the duplicate records from joined data
dist_fin<-fin %>% distinct(X, Y,ApplicationType, .keep_all = TRUE)

table(dist_fin$Incident_Happ)



a<- dist_fin %>% filter(Incident_Happ == 1)

colSums(is.na(dist_fin))



colnames(dist_fin)

#Computed nearZeroVar and removed 4 features
zero_cols = nearZeroVar(dist_fin) 
zero_cols

clean_data<- dist_fin[-c(11,16,25,30)]

#Removing unnecessary columns
clean_data<- clean_data[-c(1,2,6)]
clean_data<- clean_data[-c(10,28)]

colSums(is.na(clean_data))


# columns with more than 25% NA values to be removed 

clean_data<- clean_data[-c(14,15,16)]

colSums(is.na(clean_data))

table(clean_data$FIXHALF)

clean_data$UNITS[is.na(clean_data$UNITS)] <- 1


#Imputing median values for null values
clean_data$RMBED[is.na(clean_data$RMBED)] <- median(clean_data$RMBED, na.rm=TRUE)
clean_data$FIXBATH[is.na(clean_data$FIXBATH)] <- median(clean_data$FIXBATH, na.rm=TRUE)
clean_data$FIXHALF[is.na(clean_data$FIXHALF)] <- median(clean_data$FIXHALF, na.rm=TRUE)
#clean_data$ACRES[is.na(clean_data$ACRES)] <- mean(clean_data$ACRES, na.rm=TRUE)

table(clean_data$ApplicationType)

#Filtering the data which cannot be used due to very less number of records
clean_data <- clean_data %>% filter(ApplicationType != "FPCP PLANS")
clean_data <- clean_data %>% filter(ApplicationStatus != "V")
clean_data <- clean_data %>% filter(StatusName != "None")
clean_data <- clean_data %>% filter(STYLE_DESC != "Split Foyer")
clean_data <- clean_data %>% filter(STYLE_DESC != "Split Level 2-Levels")
clean_data <- clean_data %>% filter(USER13_DESC != "SYNTHETIC SLATE")


table(clean_data$EXTWALL_DESC)

#Getting the data if the building walls are built with wood or not
clean_data$EXTWALL_DESC_With_Wood_<-grepl("WOOD|wood|Wood", clean_data$EXTWALL_DESC)
clean_data<-clean_data[-16]

#Making the response variable Incident Happen as a factor
clean_data$Incident_Happ<- as.factor(clean_data$Incident_Happ)
clean_data<-clean_data[-c(19)]
colSums(is.na(clean_data))

#Imputing the median values into Acres feature
clean_data$ACRES[is.na(clean_data$ACRES)]<-median(clean_data$ACRES,na.rm=TRUE)
clean_data

#Creating a binary variable which tells if the building is made of wood or not
clean_data$EXTWALL_DESC_With_Wood_ <- replace(clean_data$EXTWALL_DESC_With_Wood_,clean_data$EXTWALL_DESC_With_Wood_ == "FALSE",0)
clean_data$Total_No_Years

#Removing the data anomalies in years feature
clean_data%>% filter(Total_No_Years < 0) %>% view()

#Splitting the data into 75% train and 25% test data
set.seed(300)
trainIndex <- createDataPartition(clean_data$Incident_Happ, p = .75, 
                                  list = FALSE, 
                                  times = 1)

clean_data %>% write_csv("clean_data.csv")

clean_data$Incident_Happ<-as.factor(clean_data$Incident_Happ)

Train <- clean_data[ trainIndex,]
Test  <- clean_data[-trainIndex,]

prop.table(table(Train$Incident_Happ))
prop.table(table(Test$Incident_Happ))

Train_<-  Train[-c(9)]

Train_ <- upSample(x = Train_,y =Train$Incident_Happ, yname = "Incident_Happ")

table(Train_$Incident_Happ)

X_train<-Train_[-c(22)]
Y_train<- Train_$Incident_Happ

X_Test<-Test[-c(9)]
Y_Test<- Test$Incident_Happ

logisticReg <- train(Incident_Happ ~ ., data = Train_, method = "glm", trControl = trainControl(method = "repeatedcv", repeats = 10), preProc=c("center","scale","BoxCox"))
summary(logisticReg)

y_pred_test = predict( logisticReg,Test[-c(9)])
y_pred_test


cm_test= confusionMatrix( data=y_pred_test, reference=Test$Incident_Happ)
cm_test

lr_test=list( classifier=logisticReg, confusionMatrix=cm_test )
lr_test$confusionMatrix



probabilities <- logisticReg %>% predict(Test, type = "prob")

options(scipen = 100)
probabilities



probabilities <- probabilities %>% rename("No" = "0",
                                          "Yes" = "1")
final_test_predict<-merge(Test,probabilities)
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)




confusionMatrix(Test$Incident_Happ, y_pred_test)

y_pred_test<-as.ordered(y_pred_test)

lr_Roc <- roc(response = Test$Incident_Happ ,
             predictor = y_pred_test)

# Logistic regression ROC 
lr_Roc




################################################################################################################################


library(gbm)
library(ipred)
library(party)
library(partykit)
library(randomForest)
library(ROCR)
library(pROC)
library(lattice)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Decision Tree~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)
library(e1071)
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))

train(Incident_Happ ~.
      ,data = Train_, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)



library(rpart)
TreeCV <- rpart(Incident_Happ ~.
                ,data = Train_,method = "class", cp = 0.01)

predictionCV <- predict(TreeCV, newdata = Test[-c(9)], type = "class")
cm=table(Test$Incident_Happ, predictionCV)
cm
library(rpart)
library(rpart.plot)
prp(TreeCV)

names <- factor(c("No", "Yes")) 
levels(Train_$Incident_Happ) <- make.names(levels(names))
prob_dt = predict(TreeCV, Train_ %>% select(-Incident_Happ), type="prob")
prob_dt



accuracy2 <- (sum(diag(cm))/sum(cm))
accuracy2

#~~~~~~~~~~~~~~~~~~~ Random Forest~~~~~~~~~~~~~~~~~~~~~~~
dim(X_train)
sqrt(21)# determinig mtry value
library(randomForest)
library(gbm)
library(ipred)
library(party)
library(partykit)
library(randomForest)
library(ROCR)
library(pROC)
library(lattice)



numFolds <- trainControl(method = "cv", number = 10,search="random")

rf_classifier = randomForest(Incident_Happ ~ ., data=Train_, ntree=100, mtry=4,method="rf",
                             trcontrol=numFolds,importance=TRUE)
rf_classifier
impt=varImp(rf_classifier,Scale=T)
impt # Finding the importance
plot1=plot(varImp(rf_classifier,Scale=T))
plot1
prob2 = c()
prob2 <-  predict(rf_classifier, Test, type = "response")
cm2 <- table(actual = Test$Incident_Happ, fitted = prob2)
cm2
accuracy2 <- (sum(diag(cm2))/sum(cm2))
accuracy2
#precision
precision2 = cm2[1,1]/sum(cm2[1,1:2])

# Recall: tp/(tp + fn):
recall2 = cm2[1,1]/sum(cm2[1:2,1])

# F-Score: 2 * precision * recall /(precision + recall):
f2 = 2 * precision2 * recall2 / (precision2 + recall2)


precision2
recall2
f2
plot(rf_classifier)

p = vector()

for (i in 1:length(prob2)){
  p = c(p,prob2[[i]])
}
table(prob2)

p[p==1] = 0
p[p==2] = 1
p
table(p)
table(Test$Incident_Happ)



rocobj <- roc(Test$Incident_Happ, p)


#create ROC plot
ggroc(rocobj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve using Random forest Classifier'))

prob2[1][1]






################################################################################################################




