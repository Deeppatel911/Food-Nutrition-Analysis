library(NutrienTrackeR)
library(ggplot2)

#importing the dataset and creating the dataframe
dataset<-read.csv("USDA.csv")

#selecting data with 1000 entries and 9 columns as the actual data is too large 
fd<-dataset[1:1000,1:10]

#determining the row/entry which has the max calories
cal<-which.max(fd$Calories)
fd$Description[cal]  #determining the name of the item with the given row

summary(fd$Calories)  #summary of calories i.e what is the max, min value of Calories found, avg,mode,etc
hist(fd$Calories) #shows the frequency distribution of items with their calorie values
subset(fd$Description,fd$Calories>=200 & fd$Calories<500) #sources which gives good amount of calories



prtn<-which.max(fd$Protein)
fd$Description[prtn]

summary(fd$Protein)
subset(fd$Description,fd$Protein>=12 & fd$Protein<=25) #good source of proteins
 


tfmx<-which.max(fd$TotalFat)
fd$Description[tfmx] #food item with max fat
tfmi<-which.min(fd$TotalFat)
fd$Description[tfmi] #food item with min fat

summary(fd$TotalFat)
plot(x=fd$Protein,y=fd$TotalFat,xlab="Protein",ylab="Fat",main="Fat vs Protein")
#scatter plot to express the relationship between protein and fat graphically


carb<-which.max(fd$Carbohydrate)
fd$Description[carb]

summary(fd$Carbohydrate)
subset(fd$Description,fd$Carbohydrate>=15 & fd$Carbohydrate<=65) #good source of carbohydrates



sg<-which.max(fd$Sugar)
fd$Description[sg]

summary(fd$Sugar)
boxplot(fd$Sugar,ylab="Sugar(mg)") #distribution of food items based on their sugar value 



chol<-which.max(fd$Cholesterol)
fd$Description[chol]

summary(fd$Cholesterol)
subset(fd$Description,fd$Cholesterol>=175 & fd$Cholesterol<=485) #high cholesterol food to avoid 



#food items based on the recommended daily intake of nutrients
subset(fd$Description,fd$Protein<=50 & fd$Sugar<=90  & fd$Carbohydrate<=130 & fd$Sodium<=2.3)


#The function dietBalance() calculates the daily nutrient intake of an individual and compares it with the NIH recommendations (recommended dietary allowances (RDA)
daily_intake <- dietBalance(my_daily_food = sample_diet_USDA, food_database = "USDA",  age = 27, gender = "female")


#The function nutrientPiePlot() generates a pie-chart showing the relative contribution of each food to the total intake of a given nutrient.
nutrientPiePlot(daily_intake, nutrient_name = "Iron, Fe (mg)")


