# Load packages
library(randomForest)
library(corrplot)
library(plotrix)
library(dplyr)

data <- read.csv("C:/Users/Allu/Downloads/tita/train.csv")
#test_data <- read.csv("C:/Users/Allu/Downloads/tita/test.csv")
Raw_Data <- data.frame(data$Survived,data$Pclass,data$Name,data$Sex,data$Age,data$SibSp,data$Parch)
#Replace All The DataFrame's NA values with 0
Raw_Data[is.na.data.frame(Raw_Data)]<-0
# Replace All the Ages with Value zero to mean of all Ages in Mr. and Mrs.
Raw_Data$data.Age[(grepl("Mr.|Mrs.|Dr.",Raw_Data$data.Name)==1) & (Raw_Data$data.Age==0)] <- mean(Raw_Data$data.Age[(grepl("Mr.|Mrs.|Dr.",Raw_Data$data.Name)==1)])
#Replace All the Ages with Value zero to mean of all Ages in Master. and Miss.
Raw_Data$data.Age[(grepl("Master.|Miss.",Raw_Data$data.Name)==1) & (Raw_Data$data.Age==0)] <- mean(Raw_Data$data.Age[(grepl("Master.|Miss.",Raw_Data$data.Name)==1)])
Raw_Data$sum <- Raw_Data$data.SibSp + Raw_Data$data.Parch
Raw_Data$data.Sex <- ifelse(Raw_Data$data.Sex=="male",1,0)



# Plotting the correlational matrix graph
M <- data.frame(Raw_Data$data.Survived,Raw_Data$data.Pclass,Raw_Data$data.Sex,Raw_Data$data.Age,Raw_Data$data.SibSp,Raw_Data$data.Parch,Raw_Data$sum)
corrplot(cor(M), method="circle")
# Histograms
# Survivals Based on age
hist(Raw_Data$data.Age[Raw_Data$data.Survived==1],main="Survivals Based on Age",xlab="Age",ylab="People Survived",col = "#ADD8E6",breaks=10)
# Pie Survivals Based on Class
Label <- c(paste("1st Class", round(length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1 & Raw_Data$data.Pclass==1])*100/length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1]), digits=2) ,"%", sep= " "),paste("2nd Class", round(length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1 & Raw_Data$data.Pclass==2])*100/length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1]),digits=2) ,"%" , sep= " "),paste("3rd Class", round(length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1 & Raw_Data$data.Pclass==3])*100/length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1]),digits=2) ,"%" , sep= " "))
class1 <- length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1 & Raw_Data$data.Pclass==1])
class2 <- length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1 & Raw_Data$data.Pclass==2])
class3 <- length(Raw_Data$data.Pclass[Raw_Data$data.Survived==1 & Raw_Data$data.Pclass==3])
Pclass <- c(class1,class2,class3)
pie3D(Pclass,labels=Label,col = rainbow(length(Pclass)),explode = 0.1, main = "Pie Chart of Plcass Survivals ")

# Survivals Based on  Sex
sexM <- length(Raw_Data$data.Sex[(Raw_Data$data.Survived==1) & (Raw_Data$data.Sex==1)])

sexF <- length(Raw_Data$data.Sex[(Raw_Data$data.Survived==1) & (Raw_Data$data.Sex==0)])

sumsex <- sexM+sexF

genderplot <- c(sexM,sexF)

gender_label <- c(paste("Male",round(sexM*100/sumsex,digits=2)),paste("Female",round(sexF*100/sumsex,digits=2)))

pie3D(genderplot,labels = gender_label,col=topo.colors(length(genderplot)), main = "Pie Chart of Survivals based on gender")

# survival Based on Sum column

#hist(Raw_Data$sum[Raw_Data$data.Survived==1],col=heat.colors(6),breaks=6)

summ <- c()
summ_label <- c()
for (i in 0:max(Raw_Data$sum))
{
summ <- append(summ,length(Raw_Data$sum[(Raw_Data$sum==i) & (Raw_Data$data.Survived==1)])/length(Raw_Data$sum[Raw_Data$sum==i]))
summ_label <- append(summ_label,i)
}
barplot(summ,names.arg=(summ_label),xlab = "number of siblings/parent/child/spouse",ylab="probability Of Survival",col=topo.colors(length(summ)))


# Survivals based on Sibling Spouse


sibsp <- c()
sibsp_label <- c()
for (i in 0:max(Raw_Data$data.SibSp))
{
sibsp <- append(sibsp,length(Raw_Data$data.SibSp[(Raw_Data$data.SibSp==i) & (Raw_Data$data.Survived==1)])/length(Raw_Data$data.SibSp[Raw_Data$data.SibSp==i]))
sibsp_label <- append(sibsp_label,i)
}
barplot(sibsp,names.arg=(sibsp_label),xlab = "number of siblings/spouses",ylab="probability Of Survival",col=topo.colors(length(sibsp)))

# Survivals Based upon number of Parent Children


Parch <- c()
Parch_label <- c()
for (i in 0:max(Raw_Data$data.Parch))
{
Parch <- append(Parch,length(Raw_Data$data.Parch[(Raw_Data$data.Parch==i) & (Raw_Data$data.Survived==1)])/length(Raw_Data$data.Parch[Raw_Data$data.Parch==i]))
Parch_label <- append(Parch_label,i)
}
barplot(Parch,names.arg=(Parch_label),xlab = "number of Parent/Children",ylab="probability Of Survival",col=topo.colors(length(Parch)))




####################### Prediction ############################################










# Prediction

train <- read.csv('C:/Users/Allu/Downloads/tita/train.csv', stringsAsFactors = F)
test  <- read.csv('C:/Users/Allu/Downloads/tita/test.csv', stringsAsFactors = F)
tester  <- read.csv('C:/Users/Allu/Downloads/tita/test.csv')
all_data  <- bind_rows(train, test) # bind training & test data

#cleaning the Embarked Data
all_data$Embarked[(all_data$Embarked=="")] <- "S"
all_data$Embarked[(all_data$Embarked=="C")] <- 1
all_data$Embarked[(all_data$Embarked=="Q")] <- 2
all_data$Embarked[(all_data$Embarked=="S")] <- 3

#cleaning the Sex
all_data$Sex <- ifelse(all_data$Sex=="male",1,0)

#cleaning the age
# Replace all NA's with 0
all_data$Age[is.na(all_data$Age)] <- 0
# Replace All the Ages with Value zero to mean of all Ages in Mr. and Mrs.
all_data$Age[(grepl("Mr.|Mrs.|Dr.",all_data$Name)==1) & (all_data$Age==0)] <- mean(all_data$Age[(grepl("Mr.|Mrs.|Dr.",all_data$Name)==1)])
#Replace All the Ages with Value zero to mean of all Ages in Master. and Miss.
all_data$Age[(grepl("Master.|Miss.|Ms.",all_data$Name)==1) & (all_data$Age==0)] <- mean(all_data$Age[(grepl("Master.|Miss.|Ms.",all_data$Name)==1)])

# Name Regression
all_data$surnames <- gsub(",.*", "", all_data$Name)
all_data$title <- sub(" .*","",sub(".*, ", "", all_data$Name))


# sub("\\(.*","",sub(".* Mrs. ", "", meow2))
#


# Surname Index
surnames <- unique(all_data$surnames)
count=0
for (i in surnames)
{
  all_data$surname_index[all_data$surnames==i] <- count
  count=count+1
}




# Seperating spouse , siblings , parent , child  , father , mother

#spouses
#
#2 - All children
#0 - All female spouses
#1 - All Male Spouses
#3 - All with no spouse
#4 - All others

# spouses

all_data$second_name[all_data$Sex==1] <- sub(".*Mr. |Master. |Don. |Rev. |Dr. |Major. |Sir. |Col. |Capt. |Jonkheer. ","",all_data$Name[all_data$Sex==1])
all_data$second_name[all_data$Sex==0] <- sub(" \\(.*","",sub(".* Mrs. | Miss. | Mme. | Ms. | Lady. | Mlle. | the. | Dr. | Dona. ", "", all_data$Name[all_data$Sex==0]))




#all_data$spouse[(grepl("\\(|\\)",all_data$Name)==1) & (all_data$SibSp>0) & (all_data$Sex==0) & (all_data$Age > 17)] <- 1
#all_data$spouse[(all_data$SibSp>0) & (all_data$Sex==1) & (all_data$)] <- 2
for (i in 1:length(all_data$Name))
{
	  if(length(all_data$Name[all_data$second_name[i]==all_data$second_name]) >1 )
	  {
      all_data$spouse[(all_data$second_name[i]==all_data$second_name) & (all_data$SibSp > 0) & (all_data$Sex==0)] <- 0
	  }
  }


for (i in 1:length(all_data$Name))
{
		if(length(all_data$Name[all_data$second_name[i]==all_data$second_name]) >1 )
	  {
      all_data$spouse[(all_data$second_name[i]==all_data$second_name) & (all_data$SibSp > 0) & (all_data$Sex==1)] <- 1
	  }
  }
  




all_data$spouse[is.na(all_data$spouse)] <- 4
all_data$spouse[(grepl("Master.|Miss.|Ms.",all_data$title)==1)] <- 2
all_data$spouse[all_data$SibSp==0] <- 3









#all_data$spouse[(grepl("Mr.|Mrs.|Dr.",all_data$Name)==1)] <- 1


# Siblings
# 0 for no siblings

all_data$siblings[(all_data$spouse==0|1) & (all_data$SibSp==1)] <- 0
all_data$siblings[all_data$SibSp==0] <- 0
all_data$siblings[(grepl("Miss.|Ms.",all_data$title)==1) & (all_data$SibSp>0)] <- 1
all_data$siblings[(grepl("Master.|",all_data$title)==1) & (all_data$SibSp>0)] <- 2
all_data$siblings[(all_data$spouse==1) & (all_data$SibSp > 1)] <- 2
all_data$siblings[(all_data$spouse==0) & (all_data$SibSp > 1)] <- 1
all_data$siblings[is.na(all_data$siblings)] <- 3


##################### Perfect Till Here ###############################


# Parents

all_data$parents[(all_data$Parch==0)] <- 0
#all_data$parents[(all_data$Parch>0) & (all_data$spouse!=3|4)] <- 1
all_data$parents[(all_data$Parch>0) & (grepl("Master.|Miss.|Ms.",all_data$title)==1)] <- 1
for (i in 1:length(all_data$Name))
{
		if(length(all_data$Name[(all_data$Age-all_data$Age[i] > 10) & (all_data$surnames==all_data$surnames[i])]) > 0)
		{
			all_data$parents[i] <- 1
		}
		
}

all_data$parents[is.na(all_data$parents)] <- 2

# Children

all_data$child[(all_data$Parch==0)] <- 0
all_data$child[(grepl("Master.|Miss.|Ms.",all_data$title)==1)] <- 0
all_data$child[all_data$parents==2] <- 1
all_data$parents[all_data$parents==2] <- 0

for (i in 1:length(all_data$Name))
{
		if(length(all_data$Parch[i]-all_data$parents[i]) > 0)
		{
			all_data$parents[i] <- 1
		}
		
}

for (i in 1:length(all_data$Name))
{
		if(length(all_data$Parch[i]-all_data$parents[i]) == 0)
		{
			all_data$parents[i] <- 0
		}
		
}


all_data$child[(all_data$Parch==0)] <- 0
all_data$child[(grepl("Master.|Miss.|Ms.",all_data$title)==1)] <- 0
all_data$child[all_data$parents==2] <- 1
all_data$child[is.na(all_data$child)] <- 0


# title index

title_index <- unique(all_data$title)
count=0
for (i in title_index)
{
  all_data$title_index[all_data$title==i] <- count
  count=count+1
}

# Fare

all_data$Fare[is.na(all_data$Fare)] <- 0
all_data$Fare[(all_data$Fare==0) & (all_data$Pclass==1)] <- mean(all_data$Fare[all_data$Pclass==1])
all_data$Fare[(all_data$Fare==0) & (all_data$Pclass==2)] <- mean(all_data$Fare[all_data$Pclass==2])
all_data$Fare[(all_data$Fare==0) & (all_data$Pclass==3)] <- mean(all_data$Fare[all_data$Pclass==3])

#all_data <- data.frame(all_data$Survived,all_data$Pclass,all_data$Name,all_data$Sex,all_data$Age,all_data$SibSp,all_data$Parch)


# Family Size
#all_data$family_size <- all_data$SibSp + all_data$Parch


########### To be changed #######
colnames(all_data) <- c("Passenger_ID", "Survived","Pclass","Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked", "Surname", "Title", "Surname_Index", "second_name", "Spouse", "Siblings", "parents", "child","title_index")








# Split the data back into a train set and a test set
train <- all_data[1:891,]
test <- all_data[892:1309,]


# Set seed for reproducibility
set.seed(123)


# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Surname_Index +  Spouse + Siblings + parents + child + title_index  ,data = train, importance = TRUE)


my_forest

# Show model error
plot(my_forest, ylim=c(0,0.36))
legend('topright', colnames(my_forest$err.rate), col=1:3, fill=1:3)

# The black line shows the overall error rate which falls below 20%. The red and green lines show the error rate for 'died' and 'survived' respectively. We can see that right now we're much more successful predicting death than we are survival. 
## Variable importance

#Let's look at relative variable importance by plotting the mean decrease in Gini calculated across all trees.


# Get importance
importance    <- importance(my_forest)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

rankImportance



# Make your prediction using the test set
prediction <- predict(my_forest, test)


# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
solution <- data.frame(PassengerId = tester$PassengerId, Survived = prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(solution, file = "C:/Users/Allu/Desktop/my_solution.csv", row.names = FALSE)
