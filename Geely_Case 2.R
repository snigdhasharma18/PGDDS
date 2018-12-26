library(stats)
library(tidyr)
library(tidyverse)
library(car)
library(MASS)
library(stringr)
library(gridExtra)
library(dplyr)
setwd("~/Downloads")

geely <- read.csv("CarPrice_Assignment.csv", header = T, stringsAsFactors = FALSE)
## Review the structure and dimension of the data frame : geely
str(geely)
dim(geely)
colnames(geely)
head(geely)
summary(geely)
sum(duplicated(geely$car_ID))
sum(is.na(geely))
#Name all the CarName to lower case:
geely$CarName <- tolower(geely$CarName)

## Seperating the Car Comapny name and car model:
geely <- separate(geely, CarName, into = c("Car_Company", "Model"), sep = " ", remove = FALSE)

#Removing the Model Column:
geely <- geely[-5]
#Removing the CarName column
geely <- geely[-3]
##It is seen that in the Car_Company, few company names are written in diff ways.
## For analysis purpose lets name them:

#mazda and maxda is written. So lets make it as mazda:
geely$Car_Company[which(geely$Car_Company == "maxda")] = "mazda"
#porcshce to porsche:
geely$Car_Company[which(geely$Car_Company == "porcshce")] = "porsche"
# toyouta to toyota:
geely$Car_Company[which(geely$Car_Company == "toyouta")] = "toyota"
# From vokswagen and vw to volkswagen
geely$Car_Company[which(geely$Car_Company == "vokswagen")] = "volkswagen"
geely$Car_Company[which(geely$Car_Company == "vw")] = "volkswagen"

#****###Understanding of the data and basic Analysis #****###

##1. From the data it can be seen that car_ID is a serial no. so its ok to remove this from the geely data frame
geely <- geely[-1]

##2. Symboling column is related to car insurance in the range of -3 to 3. Let us see the distribution:
table(geely$symboling)
# Comment: from the above table it can be seen that very few cars are unde the safe rating that is +3. Majority of the cars falls under the range of 0 and +1.
#If we put it in graphical presentation let's see what we come up with:
price_symboling <- ggplot(geely, aes(as.factor(geely$symboling), geely$price)) + geom_boxplot() + labs(title = "Price v/s Symboling", x= "Symboling-Insurance Risk", y= "Car Price")
price_symboling
cor(geely$symboling,geely$price)
# From the above it can be seen that where the symboling value is either 1 or 0 they have outliers and the correlation between the car price 
#and symboling is negative 0.08(approx)
#And most of the cars are in the low price range and the simboling value is approx 1 although there are some outliers.

##3. Car_Company Analysis: LEts Analyse the diffrent car Company with the Price
table(geely$Car_Company)
#Comment: It can be seen from the distribution table that the Mercury has the least no. of cars followed by renault, alfa-romero, chevrolet,jaguar
#and Toyota has the most number of cars i.e. 32.
#Lets see the distribution in the box_plot
carco_price<- ggplot(geely, aes(geely$Car_Company, geely$price))+ geom_boxplot()+ labs(title = "Car Company and Price", x= "Car_Company", y= "Price" )
carco_price
#Comment: From the above boxplot it is clear that the most expensive cars belong to buick, BMW, Jaguar & Porche. The least expensive one is chevrolet. 

##4. Fuel Type and Price Analysis: From the levels it can be seen that it has two category Diesel or Petrol.
#Let's see the distribution of cars between petrol and diesel:
table(geely$fueltype)
#It can be seen that cars with petrol(gas) is more that the diesel. It is approx 90% of the cars run on petrol.
#Lets see the comparision with the price:
fueltype_price <-  ggplot(geely, aes(geely$fueltype, geely$price))+ geom_boxplot()+ labs(title = "Fueltype and Price", x= "Fuel Type", y= "Price")
fueltype_price
#From the above it can be seen that the diesel cars are less costly(few outliers) than the gas(petrol) cars and another interesting observation is
#the few costlier cars are running in diesel. From the below histogram it can be seen that there are few outliers.
fuetype_price<- ggplot(geely, aes(geely$price, fill=as.factor(geely$fueltype)))+ geom_histogram(binwidth = 2500)+ labs(title = "Fuel type and Price", x= "price", y= "fule type")  +scale_fill_discrete(name = "Fuel Type")
fuetype_price

##5. Analysis of aspiration with relation to price: Let's see the distribution of aspiration:
table(geely$aspiration)
aspiration_price <- ggplot(geely, aes(geely$aspiration, geely$price)) + geom_boxplot() + labs(title = "aspiration v/s price", x= "aspiration", y = "price")
aspiration_price
#Comment: the median of STD.(Naturally aspired engine) is lest than the (turbo)Turbocharger engines. and the Std. vehicles are more than the turbo ones.

##6. doornumber Analysis: 
table(geely$doornumber)
doorno_price <- ggplot(geely, aes(geely$doornumber, geely$price)) + geom_boxplot() + labs(title = "doornumber v/s price", x= "doornumber", y = "price")
doorno_price
#Commnet: There is not much difference between the price of two door vehicles and 4 door vehicles. But there are some outliers that is few high price cars have two doors.

##7: carbody analysis:
table(geely$carbody)
carbody_price <- ggplot(geely, aes(geely$carbody, geely$price)) + geom_boxplot() + labs(title = "carbody v/s price", x= "carbody", y = "price")
carbody_price
#Comment: Majority of the vehicles belongs to the sedan and hatchback and their prices are low but there are some outlier in sedan and few in hastchback.
#Hatchback has lowest median value.
#Let's do further analysis:
cbody_price<- ggplot(geely, aes(geely$price, fill=as.factor(geely$carbody)))+ geom_histogram(binwidth = 2500, position = "dodge")+labs(title = "Carbody and Price", x= "Price", y= "carbody")+scale_fill_discrete(name = "Carbody Type")
cbody_price
#Majority of the vehicles in the dataset are priced below US$25,000.and majority of low price cars belongs to hatchback, sedan. 

##8. drivewheel Analysis: From the data it can be seen that majority of the drivewheel belongs to rwd(i.e. most of them belongs to Front wheel drive.and rear wheel drive)
table(geely$drivewheel)
#Graphical analysis using box plot:
drwheel_price<- ggplot(geely, aes(geely$drivewheel, geely$price)) + geom_boxplot() + labs(title = "drivewheel Vs Price", x= "Drivewheel", y="price")
drwheel_price
## Comment: the rwd cars are more expensive than the other two. and most of the low cost vehicles have fwd drivewheels.

##9. enginelocation Analysis: It can be seen that majority of the cars have front engine
table(geely$enginelocation)
#Graphical Analysis using boxplot:
engloc_price <- ggplot(geely, aes(geely$enginelocation, geely$price)) + geom_boxplot() + labs(title = "engine location and Price of the car", x= "engine location", y= "Price")
engloc_price
# Commnet: From the above it can be seen that majority of the cars belongs to font engine. But the rear engine is majorly found in expensive vehicles.
#as most of the vehicles belongs to front engine then the rear engine vehicles can be ignored.

##10. Wheelbase Analysis:It is a numeric data. We will check whether there is any outliers and treat accordingly:
summary(geely$wheelbase)
boxplot(geely$wheelbase) 
title(main = "boxplot of wheelbase", ylab = "wheelbase")
quantile(geely$wheelbase, seq(0,1,0.01))
# Commnet: It can be seen that majority of the the data falls between 93 to 115. At 100% we ee a sudden jump from 99%. We will cap it at 99% 
#which is 115.54.
#*# Treating the Outliers and capping it at 99% i.e. 115.54
geely$wheelbase[which(geely$wheelbase >115.54)] <- 115.54
## Creating a scatter plot to make it better understanding:
wheelbase_price<- ggplot(geely, aes(geely$wheelbase, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of the Car wrt wheelbase", x= "wheelbase", y= "price")
wheelbase_price
# From the above it can be seen that at around 95 wheel base the price decrease but after it the price increases with the increase in wheelbase.
## 11. Car Length Analysis: As it is a numeric data lets do the summary check to see if there is any outliers and treat accordingly:
summary(geely$carlength)
boxplot(geely$carlength)
title (main = "boxplot of carlength" , ylab = "carlength")
# Commnet: Majoriy of the data is in the ranje of 165 to 185. there is one outlier at the lower side. so we will not consider he outlier.
quantile(geely$carlength, seq(0,1,0.01))
## Scatterplot presentation:
carlength_price<- ggplot(geely, aes(geely$carlength, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Car Price wrt Carlength", x= "Carlength", y= "Price")
carlength_price
# Commnet: It can be seen that with the increase in the length of the car the price of car is increasing. After the point 180 of carlength there in steep increase in car price with car length.

##12. carwidth Analysis: IT a numeric data lets do summary check and boxplot to see if there is any outliers and treat accordingly:
summary(geely$carwidth)
boxplot(geely$carwidth)
title(main = "Boxplot of carwidth", y = "carwidth")
# Most of the carwidths are in between 64 to 67. Let's see he quantile value of the same:
quantile(geely$carwidth, seq(0,1,0.01))
# There is no sudden spike so we can say that no need to treat any outliers.
#Lets see relation b/n car price with carwidth using scatterplot:
carwidth_price<- ggplot(geely, aes(geely$carwidth, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Car Price wrt carwidth", x="carwidth", y= "Price")
carwidth_price
#Comment: There is the increase in price with the increase in carwidth

##13:carheight Analysis:It's a numeric data. we will first see the summary and use the boxplot to see it graphically whether there is any outliers
summary(geely$carheight)
boxplot(geely$carheight)
title(main ="Boxplot of carheight", y="carheight")
quantile(geely$carheight, seq(0,1,0.01))
# Comment: All the careheight data is evely distributed. There are no outliers.
#Let's see the relation between price and carheight using scatterplot:
carheight_price<- ggplot(geely, aes(geely$carheight, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of the Car wrt Carheight", x= "carheight", y="price")
carheight_price
#From the above it can be seen that the price of car increases between the range of 54 (approx) to 56(approx) but after that it again declines.

##14: curbweight Analysis: It's a numeric value. Let's see if there is any outliers:
summary(geely$curbweight)
boxplot(geely$curbweight) 
title(main = "Boxplot of curbweight", ylab = "curbweight")
quantile(geely$curbweight, seq(0,1,0.01))
#Commnet: Majority of curbweight is in between the range of 2200 to 2800 approx. when observing the spread it can be seen that there is sudden spike between 0 to 1. But it will not be considered as outliers.
# Lets see the curbweight with respect to price:
curbwt_price<- ggplot(geely, aes(geely$curbweight, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of Car wrt Curbweight", x= "Curbweight", y= "Price")
curbwt_price
# It can be seen that with the increase in the curbweight the price of the car increases.

##15: enginetype Analysis to see if there is any outliers:
table(geely$enginetype)
enginetype_price <- ggplot(geely, aes(geely$enginetype, geely$price)) + geom_boxplot() + labs(title = "enginetype vs Price", x= "enginetype", y= "price")
enginetype_price
#Comment: Majority of the cars belongs to ohc category and the price of these cars are also in the lower range there are some outliers as well.

##16: cylendernumber Analysis to check whether there is any outliers:
table(geely$cylindernumber)
cylenderno_price <- ggplot(geely, aes(geely$cylindernumber, geely$price)) + geom_boxplot() + labs(title = "cylendernumber vs Price", x= "cylendernumber", y= "price")
cylenderno_price
#Commnet: It can be seen that majority of the cars have four cylender followed by 6. But the price of the cars with 8 cylender is high.

## 17: enginesize Analysis(numeric value) to see any outliers:
summary(geely$enginesize)
boxplot(geely$enginesize)
title( main = "Boxplot of enginesize", ylab = "enginesize")
quantile(geely$enginesize, seq(0,1,0.01))
#Comment: It can be seen that most of the engine size is in between 100 to 150.
#There is sudden spike from the range of 2% to 3% and 98% to 99% it could be because of the increase in number of cylender increase as both of them are highly correlated.
# Lets find out the enginesize wrt price:
enginesize_price<- ggplot(geely, aes(geely$enginesize, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of car wrt enginesize", x= "enginesize", y= "price")
enginesize_price
# From the scatterplot it can be seen that with the increase in the engine size there is steep increase in the price of the cars.

## 18: fuelsystem Analysis(categorical value) to see outliers:
table(geely$fuelsystem)
fuelsys_price <- ggplot(geely, aes(geely$fuelsystem, geely$price)) + geom_boxplot() + labs(title = "fuelsystem vs Price", x= "fuelsystem", y= "price")
fuelsys_price
#Commnet: It can be seen from the boxplot that majority of the cars have mpfi and 2bbl fuelsystem. there is only 1 car withmfi fulesystem.
# Most of the high price cars have mpfi fulesystem. the median of the mpfi is highest.

## 19: Boreratio Analyis: Its a numeric data. LEts find out if there is any outliers:
summary(geely$boreratio)
boxplot(geely$boreratio)
title(main = "Boxplot of boreratio", ylab = "boreratio")
quantile(geely$boreratio, seq(0,1,0.01))
#Comment: It can be seen that engine boreratio is evenly spread out. Most of the boreration are in the range of 3.1 to 3.6.
#Scatterplot Analysis of boreratio wrt price:
boreratio_price<- ggplot(geely, aes(geely$boreratio, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of car wrt boreratio", x= "boreratio", y= "price")
boreratio_price
# From 2.5 to 3 value of boreratio there is decrease in the price of car but there is upward trend or increase in price from the boreratio 3.

## 20: Stroke or volume inside the engine Analysis:Its a numeric data. LEts find out if there is any outliers:
summary(geely$stroke)
boxplot(geely$stroke)
title(main = "Boxplot of stroke", ylab = "stroke")
quantile(geely$stroke, seq(0,1,0.01))
#Comment: From the above it shows that there is a sudden jump from 1%(value= 2.1968) to 2%(value = 2.64. So it is an outlier. it can be seen that majority of the car's Stroke or volume inside the engine are in the range of 3.03 to 3.4. 
#Lets fix the outlier:
geely$stroke[which(geely$stroke < 2.64)] <- 2.64
#Lets create a scatterplot:
stroke_price <- ggplot(geely, aes(geely$stroke, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of car wrt Stroke", x= "stroke", y= "price")
stroke_price
# It can be seen that there is not major change in price with the increase in the range of stroke length till the point 3.5. But there issteady in price after that.

## 21: compressionratio of the car Analysis: LEtscheck if there is any outliers(numeric):
summary(geely$compressionratio)
boxplot(geely$compressionratio)
title(main = "Boxplot of Compressionratio", ylab = "stroke")
quantile(geely$compressionratio, seq(0,1,0.01))
#Majority of of the data are in the range of 8 to 9.5. But there is sudden spike after 90%(10.94). At 91% the calue jumped to 21.
#Let's fix the max outlier within 10.94
geely$compressionratio[which(geely$compressionratio < 10.94)]<- 10.94
#Let's make a scatter plot of price wrt compressionratio:
compratio_price<- ggplot(geely, aes(as.numeric(geely$compressionratio), geely$price))+ geom_point()+ labs(title = "Price of car wrt Engine compressionratio", x= "engine compressionratio", y= "price")
compratio_price
#The least price cars mostly belongs to the cars with engine compressionration between 8 to 9.4

##22: Horsepower Analysis: Let's check if there is any outliers:
summary(geely$horsepower)
boxplot(geely$horsepower)
title(main = "Boxplot of Horsepower", ylab = "horsepower")
quantile(geely$horsepower, seq(0,1,0.01))
#Comment: Majority of the cars are in the range of 90 to 120. There is sudden spike from the 98%. So lets fix the outliersat 97% i.e.184.0:
geely$horsepower[which(geely$horsepower > 184.0)]<- 184.0
#Lets see it through scatterplot:
horpow_price <- ggplot(geely, aes(geely$horsepower, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of car wrt horsepower", x= "horsepower", y= "price")
horpow_price
#It can be seen that with the increase in horsepower the price of the car increases.

## 23. Peak RPM Analysis: Lets see the outliers(numeric value):
summary(geely$peakrpm)
boxplot(geely$peakrpm)
title(main = "Boxplot of peak RPM", ylab = "Peak RPM")
quantile(geely$peakrpm, seq(0,1,0.01))
#Comment: Majority of the car's peakrpm is in the range of 4800 to 5500. However there is sudden spike at 99%.
#Lets treat the Outlier and the limit cap is 6000:
geely$peakrpm[which(geely$peakrpm >6000)] <- 6000
#Scatterplot to see the price v/s peakrpm
peakrpm_price <- ggplot(geely, aes(geely$peakrpm, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of car wrt peakrpm", x= "peakrpm", y= "price")
peakrpm_price
# There is very low fluctuation in price with the change in peakrpm. So there is not a significant relationship b/n peakrpm and car price.

## citympg Analysis: Check for Outliers and its treatment:
summary(geely$citympg)
boxplot(geely$citympg)
title(main = "Boxplot of citympg", ylab = "citympg")
quantile(geely$citympg, seq(0,1,0.01))
#Comment: From the above analysis it can be seen that the citypmg of cars are in the range 19% to 31%. there is suddenspike from 99%. 
# Let's treat the outlier and fix it to 38.0
geely$citympg[which(geely$citympg >38.0)] <- 38.0
# Scatterplot Analysis:
citympg_price <- ggplot(geely, aes(geely$citympg, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of car wrt citympg", x= "citympg", y= "price")
citympg_price
#There is decrease in price with the increase in citympg(mileage in city)

## 25: highwaympg: Finding of outliers:
summary(geely$highwaympg)
boxplot(geely$highwaympg)
title(main = "Boxplot of highwaympg", ylab = "highwaympg")
quantile(geely$highwaympg, seq(0,1,0.01))
#Comment: Majority of cars are within the range of 25 to 34. There is sudden spike at 99%.
#Tretment of Outliers. capping the max highwaympg to 46.92
geely$highwaympg[which(geely$highwaympg > 46.92)] <- 46.92
#Scatterplot of price wrt mileage in the highway
highmpg_price <- ggplot(geely, aes(geely$highwaympg, geely$price))+ geom_point()+geom_smooth(method = "loess")+ labs(title = "Price of car wrt highwaympg", x= "highwaympg", y= "price")
highmpg_price
## Same as in citympg the price of cars declines with increase in highwaympg.

##****### Creating dummy variables for categorical values to convert them into numerical value

#Let's make a work_geely for the  calculation purpose:
work_geely <- geely

## D_1: car_manu facturer - using model.matrix
dummy_car_manufacturer <- model.matrix(~Car_Company,data=work_geely)
work_geely <- cbind(work_geely %>% dplyr::select(-Car_Company),dummy_car_manufacturer[,-1])

##D_2 : Converting the fueltype(gas=0,diesel=1) into numerical 1,0
work_geely$fueltype <- ifelse(work_geely$fueltype == "gas",0,1)

##D_3: aspiration - std = 0 and turbo = 1
work_geely$aspiration <- ifelse(work_geely$aspiration == "std", 0,1)

##D_4: doornumber - two door = 0 and 4 door = 1
work_geely$doornumber <- ifelse(work_geely$doornumber == "std", 0,1)

##D_5: car body - using model.matrix 
dummy_car_body <- model.matrix(~carbody,data=work_geely)
work_geely <- cbind(work_geely %>% dplyr::select(-carbody),dummy_car_body[,-1])

##D_6: drivewheel - using model.matrix 
dummy_drivewheel <- model.matrix(~drivewheel,data=work_geely)
work_geely<- cbind(work_geely %>% dplyr::select(-drivewheel),dummy_drivewheel[,-1])

##D_7:  enginelocation - rear = 0 and front = 1 
work_geely$enginelocation <- ifelse(work_geely$enginelocation == "rear",0,1)

##D_8: enginetype - using model.matrix
dummy_enginetype <- model.matrix(~enginetype,data=work_geely)
work_geely <- cbind(work_geely %>% dplyr::select(-enginetype),dummy_enginetype[,-1])

##D_9: cylindernumber - using model.matrix
dummy_cylindernumber <- model.matrix(~cylindernumber,data=work_geely)
work_geely <- cbind(work_geely %>% dplyr::select(-cylindernumber),dummy_cylindernumber[,-1])

##D_10: fuelsystem - using model.matrix
dummy_fuelsystem <- model.matrix(~fuelsystem,data=work_geely)
work_geely<- cbind(work_geely %>% dplyr::select(-fuelsystem),dummy_fuelsystem[,-1])

##***Derived Matrix***##

##1. In the automobile sector the power(hp)/curbweight ratio is an important parameter. Usually vehicles with a high power to weight ratio are performance vehicles and will therefore be priced higher than regular vehicles.
work_geely$powerwtrat<- round(work_geely$horsepower/work_geely$curbweight, 3)

## Setting seed to achieve reproducibility
set.seed(1000)
options(scipen = 1000)


## Seperating the training and test data set: 
trainindices = sample(1:nrow(work_geely), 0.7*nrow(work_geely))
train = work_geely[trainindices,]
test = work_geely[-trainindices,]

## Build m_1 containing all variables:
m_1 <- lm(price~., data = train)
summary(m_1)
step <- stepAIC(m_1, direction="both")
step

## Saving the last model which he stepAIC has given under the m_2 variable:
m_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
       wheelbase + carlength + carwidth + carheight + enginesize + 
       boreratio + stroke + horsepower + peakrpm + citympg + Car_Companyaudi + 
       Car_Companybmw + Car_Companybuick + Car_Companydodge + Car_Companyhonda + 
       Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
       Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
       Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
       Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
       carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
       enginetyperotor + cylindernumberfive + cylindernumberfour + 
       cylindernumbersix + fuelsystemmpfi + powerwtrat, data = train)

summary(m_2)

## Let's check for multicollinearity. If VIF > 2 we would drop the variable if it is statistically insignificant:
vif(m_2)
#From the Variable inflation factors of m_2 it is clear that there are several variables with VIF>10.
#We will now check the significance [p>0.05] of these variables through the summary and decide which variable to remove.
# the pvalue of fuelsystemmpfi is very high i.e. greater than 0.05 and its vif value isalso high. Let's first removie it:
m_3 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
            wheelbase + carlength + carwidth + carheight + enginesize + 
            boreratio + stroke + horsepower + peakrpm + citympg + Car_Companyaudi + 
            Car_Companybmw + Car_Companybuick + Car_Companydodge + Car_Companyhonda + 
            Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
            Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
            Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
            Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
            carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
            enginetyperotor + cylindernumberfive + cylindernumberfour + 
            cylindernumbersix + powerwtrat, data = train) 
summary(m_3)
#Let's check for multicollinearity. If VIF > 2 we would drop the variable if it is statistically insignificant:
vif(m_3)
#From the vif(m_3)(15.86)  and after checking the pvalue(0.228859 ) of enginetypeopeohc we can safly remove it.

m_4 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                   wheelbase + carlength + carwidth + carheight + enginesize + 
                   boreratio + stroke + horsepower + peakrpm + citympg + Car_Companyaudi + 
                   Car_Companybmw + Car_Companybuick + Car_Companydodge + Car_Companyhonda + 
                   Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
                   Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
                   Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
                   Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                   carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + powerwtrat, data = train) 
summary(m_4)
## ## Let's check for multicollinearity. If VIF > 2 we would drop the variable if it is statistically insignificant:
vif(m_4)

## From the pvalue and vif we can safely remove the car length:
m_5 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
            wheelbase + carwidth + carheight + enginesize + 
            boreratio + stroke + horsepower + peakrpm + citympg + Car_Companyaudi + 
            Car_Companybmw + Car_Companybuick + Car_Companydodge + Car_Companyhonda + 
            Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
            Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
            Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
            Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
            carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
            cylindernumbersix + powerwtrat, data = train) 
summary(m_5)
#Let's check for multicollinearity. If VIF > 2 we would drop the variable if it is statistically insignificant:
vif(m_5)
#Let's remove citympg: pvalue(0.27) and vif value: 10.232102  
m_5 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
            wheelbase + carwidth + carheight + enginesize + 
            boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
            Car_Companybmw + Car_Companybuick + Car_Companydodge + Car_Companyhonda + 
            Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
            Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
            Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
            Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
            carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
            cylindernumbersix + powerwtrat, data = train) 
summary(m_5)
#Let's check VIF
vif(m_5)
# Let's remove car_Companyhonda which has pvalue of 0.11 and vif value: 2.166 .
m_6<- lm(formula = price ~ fueltype + aspiration + enginelocation + 
     wheelbase + carwidth + carheight + enginesize + 
     boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
     Car_Companybmw + Car_Companybuick + Car_Companydodge + 
     Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
     Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
     Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
     Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
     carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
     cylindernumbersix + powerwtrat, data = train)
summary(m_6)
vif(m_6)
# Let's remove fueltype:
m_7 <- lm(formula = price ~ + aspiration + enginelocation + 
            wheelbase + carwidth + carheight + enginesize + 
            boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
            Car_Companybmw + Car_Companybuick + Car_Companydodge + 
            Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
            Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
            Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
            Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
            carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
            cylindernumbersix + powerwtrat, data = train)
summary(m_7)
vif(m_7)
#Lets remove cylendernumbersix:
m_8 <- lm(formula = price ~ + aspiration + enginelocation + 
            wheelbase + carwidth + carheight + enginesize + 
            boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
            Car_Companybmw + Car_Companybuick + Car_Companydodge + 
            Car_Companyjaguar + Car_Companymazda + Car_Companymitsubishi + 
            Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
            Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
            Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
            carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
            powerwtrat, data = train)
summary(m_8)
vif(m_8)
# Let's remove car_Companyjaguar
m_9 <- lm(formula = price ~ + aspiration + enginelocation + 
            wheelbase + carwidth + carheight + enginesize + 
            boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
            Car_Companybmw + Car_Companybuick + Car_Companydodge + 
            Car_Companymazda + Car_Companymitsubishi + 
            Car_Companynissan + Car_Companypeugeot + Car_Companyplymouth + 
            Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
            Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
            carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
            powerwtrat, data = train)
summary(m_9)
vif(m_9) <- 
#Let's remove Car_Companymouth
m_10 <- lm(formula = price ~ + aspiration + enginelocation + 
             wheelbase + carwidth + carheight + enginesize + 
             boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
             Car_Companybmw + Car_Companybuick + Car_Companydodge + 
             Car_Companymazda + Car_Companymitsubishi + 
             Car_Companynissan + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
             powerwtrat, data = train)
summary(m_10)
vif(m_10)
#LEt's remove Car_Companydodge
m_11 <- lm(formula = price ~ + aspiration + enginelocation + 
              wheelbase + carwidth + carheight + enginesize + 
              boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
              Car_Companybmw + Car_Companybuick + 
              Car_Companymazda + Car_Companymitsubishi + 
              Car_Companynissan + Car_Companypeugeot + 
              Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
              Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
              powerwtrat, data = train)
summary(m_11)
vif(m_11)
#Let's remove car_Companymitsubishi:
m_12 <- lm(formula = price ~ + aspiration + enginelocation + 
             wheelbase + carwidth + carheight + enginesize + 
             boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
             Car_Companybmw + Car_Companybuick + 
             Car_Companymazda + Car_Companynissan + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
             powerwtrat, data = train)
summary(m_12)
vif(m_12)
#Let's removee company height:
m_13 <- lm(formula = price ~ + aspiration + enginelocation + 
             wheelbase + carwidth + enginesize + 
             boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
             Car_Companybmw + Car_Companybuick + 
             Car_Companymazda + Car_Companynissan + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             Car_Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
             carbodywagon + drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive + cylindernumberfour + 
             powerwtrat, data = train)
summary(m_13)
vif(m_13)
#Let's remove car_Companyvolvo:
m_14 <- lm(formula = price ~ + aspiration + enginelocation + 
             wheelbase + carwidth + enginesize + 
             boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
             Car_Companybmw + Car_Companybuick + 
             Car_Companymazda + Car_Companynissan + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
             drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_14)
vif(m_14)
#Let's remove car_Companymazda:
m_15 <- lm(formula = price ~ + aspiration + enginelocation + 
     wheelbase + carwidth + enginesize + 
     boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
     Car_Companybmw + Car_Companybuick + 
     Car_Companynissan + Car_Companypeugeot + 
     Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
     carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
     drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive +
     cylindernumberfour + powerwtrat, data = train)
summary(m_15)
vif(m_15)
#LEt's remove Car_Companybuick:
m_16 <- lm(formula = price ~ + aspiration + enginelocation + 
         wheelbase + carwidth + enginesize + 
         boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
         Car_Companybmw+ Car_Companynissan + Car_Companypeugeot + 
         Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
         carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
         drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive +
         cylindernumberfour + powerwtrat, data = train)
summary(m_16)
vif(m_16)
#Remove Company_Caraudi:
m_17 <- lm(formula = price ~ + aspiration + enginelocation + 
             wheelbase + carwidth + enginesize + 
             boreratio + stroke + horsepower + peakrpm + Car_Companyaudi + 
             Car_Companybmw+ Car_Companynissan + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
             drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_17)
vif(m_17)
#Lets remove car_Companyaudi:
m_18 <- lm(formula = price ~ + aspiration + enginelocation + 
             wheelbase + carwidth + enginesize + 
             boreratio + stroke + horsepower + peakrpm + 
             Car_Companybmw+ Car_Companynissan + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
             drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_18)
vif(m_18)
#REmove Car_Companynissan
m_19 <- lm(formula = price ~ + aspiration + enginelocation + 
             wheelbase + carwidth + enginesize + 
             boreratio + stroke + horsepower + peakrpm + 
             Car_Companybmw + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
             drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_19)
vif(m_19)
#Remove Wheelbase:
m_20 <- lm(formula = price ~ + aspiration + enginelocation + 
            carwidth + enginesize + 
             boreratio + stroke + horsepower + peakrpm + 
             Car_Companybmw + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
             drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_20)
vif(m_20)
#Remove Companysedan:
m_21 <- lm(formula = price ~ + aspiration + enginelocation + 
            carwidth + enginesize + 
            boreratio + stroke + horsepower + peakrpm + 
            Car_Companybmw + Car_Companypeugeot + 
            Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
            carbodyhardtop + carbodyhatchback +carbodywagon + 
            drivewheelrwd + enginetypel + enginetyperotor + cylindernumberfive +
            cylindernumberfour + powerwtrat, data = train)
summary(m_21)
vif(m_21)
#Remove Car_Companywagon:
m_22 <- lm(formula = price ~ + aspiration + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + peakrpm + 
             Car_Companybmw + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab + Car_Companysubaru + 
             carbodyhardtop + carbodyhatchback + drivewheelrwd + enginetypel +
             enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_22)
vif(m_22)

## After this point all the variables has star. Now let's look at the variables which has 1 star:
#Remove Companysubaru:
m_23 <- lm(formula = price ~ + aspiration + enginelocation + 
     carwidth + enginesize + 
     boreratio + stroke + horsepower + peakrpm + 
     Car_Companybmw + Car_Companypeugeot + 
     Car_Companyporsche + Car_Companysaab+ 
     carbodyhardtop + carbodyhatchback + drivewheelrwd + enginetypel +
     enginetyperotor + cylindernumberfive +
     cylindernumberfour + powerwtrat, data = train)
summary(m_23)
vif(m_23)
#Remove peakrpm 
m_24 <- lm(formula = price ~ + aspiration + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + Car_Companypeugeot + 
             Car_Companyporsche + Car_Companysaab+ 
             carbodyhardtop + carbodyhatchback + drivewheelrwd + enginetypel +
             enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_24)
vif(m_24)
#Remove Car_Companysaab:
m_25 <- lm(formula = price ~ + aspiration + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + Car_Companypeugeot + 
             Car_Companyporsche + 
             carbodyhardtop + carbodyhatchback + drivewheelrwd + enginetypel +
             enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_25)
vif(m_25)
#REmove Car_Companyporche:
m_26 <- lm(formula = price ~ + aspiration + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + Car_Companypeugeot + 
             carbodyhardtop + carbodyhatchback + drivewheelrwd + enginetypel +
             enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_26)
vif(m_26)
#Remove aspiration: 
m_27<- lm(formula = price ~  + enginelocation + 
            carwidth + enginesize + 
            boreratio + stroke + horsepower + 
            Car_Companybmw + Car_Companypeugeot + 
            carbodyhardtop + carbodyhatchback + drivewheelrwd + enginetypel +
            enginetyperotor + cylindernumberfive +
            cylindernumberfour + powerwtrat, data = train)
summary(m_27)
vif(m_27)
#REmove carhardrop:
m_28 <- lm(formula = price ~  + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + 
             carbodyhardtop + carbodyhatchback + drivewheelrwd + enginetypel +
             enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_28)
vif(m_28)
#Remove carbodyhardtop:
m_29 <- lm(formula = price ~  + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + carbodyhatchback + drivewheelrwd + enginetypel +
             enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_29)
vif(m_29)
#Remove enginetypel:
m_30<- lm(formula = price ~  + enginelocation + 
            carwidth + enginesize + 
            boreratio + stroke + horsepower + 
            Car_Companybmw + carbodyhatchback + drivewheelrwd +
            enginetyperotor + cylindernumberfive +
            cylindernumberfour + powerwtrat, data = train)
summary(m_30)
vif(m_30)
#Remove carbodyhatchback
m_31 <- lm(formula = price ~  + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + drivewheelrwd +
             enginetyperotor + cylindernumberfive +
             cylindernumberfour + powerwtrat, data = train)
summary(m_31)
vif(m_31)
# Remove drivewheelrwd 
m_32 <-lm(formula = price ~  + enginelocation + 
            carwidth + enginesize + 
            boreratio + stroke + horsepower + 
            Car_Companybmw + enginetyperotor + cylindernumberfive +
            cylindernumberfour + powerwtrat, data = train)
summary(m_32)
vif(m_32)
#Remove cylindernumberfour
m_33 <- lm(formula = price ~  + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + enginetyperotor + cylindernumberfive +
              powerwtrat, data = train)
summary(m_33)
vif(m_33)
#Remove cylindernumberfive 
m_34 <- lm(formula = price ~  + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + enginetyperotor +
             powerwtrat, data = train)
summary(m_34)
vif(m_34) 
#LEt's remove powerwtrat 
m_35 <- lm(formula = price ~  + enginelocation + 
             carwidth + enginesize + 
             boreratio + stroke + horsepower + 
             Car_Companybmw + enginetyperotor, data = train)
summary(m_35)
vif(m_35)
#Let's remove horsepower
m_36 <- lm(formula = price ~  + enginelocation + 
                     carwidth + enginesize + 
                     boreratio + stroke + 
                     Car_Companybmw + enginetyperotor, data = train)
summary(m_36)
vif(m_36)
 
#Remove stroke  
m_37 <- lm(formula = price ~  + enginelocation + 
             carwidth + enginesize + 
             boreratio + 
             Car_Companybmw + enginetyperotor, data = train)
summary(m_37)
vif(m_37)
#Remove boreratio
m_38 <- lm(formula = price ~  + enginelocation + 
             carwidth + enginesize + 
             Car_Companybmw + enginetyperotor, data = train)
summary(m_38)
vif(m_38)
###Model Evaluation
###Finally we have arrived at the final model [m_38] with R-squared=0.8969 and Adjusted R-Squared=0.8931
#Since both R-Squared and Adjusted R-Squared values are immensly close together this model can be tested for the final model

#1. Using the mod_35 to predict the price of cars in the testing dataset.
test$predictedprice<- predict(m_38, test[,-18])
#We are storing the predicted prices of the test dataset in a new column called predicted_price
#Now we will calculate the correlation between the predicted_price and the original price of the vehicle. We shall store the results in evaluation_correlation.
evaluation_correlation<- cor(test$price, test$predictedprice)
evaluation_correlation
#The evaluation_correlation is equal to 0.9467. Which shows that our predicted prices are very close to the actual price.
evaluation_rsquare<- round((evaluation_correlation)^2, 3)
evaluation_rsquare
#Since the evaluation_rsquare is 0.896 stating that our model can account for 89.9% of the variability in prices of the vehicles. We can say that the model is a very good model to predict price of the automobile.
test$error_pred<- test$price - test$predictedprice
#Create a carID variable to assign a random unique ID to each row in the dataset  
test$carID<- sample(1:nrow(test), nrow(test), replace = FALSE)

#Plotting the Error in Prediction versus the Car_ID. This is to show the randomness in the prediction error generated.
error_noise<- ggplot(test, aes(x=test$carID, y=test$error_pred))+geom_point()+ xlab("Car_ID")+ ylab("Predicted Price Error") + ggtitle("Plt27. Plot Showing Random distribution of Predicted Error")
error_noise
