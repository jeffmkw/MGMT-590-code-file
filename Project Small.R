#Setting the Working Directory
setwd("F:/Purdue 16-17/CourseWork/Fall Mod 2/Web Data Analytics\\Project")

#Importing the Datasets
vg <- read.csv("vgsales.csv", sep=",", header=T)
ign <- read.csv("ign.csv",sep=",",header=T)

#Cleaning the data to create uniformity

colnames(ign)[3] <- paste('Name')
colnames(ign)[5]<- paste('Platform')
ign <-within(ign, levels(Platform)[levels(Platform) == "PlayStation 3"] <- "PS3")
#ign <-within(ign, levels(Platform)[levels(Platform) == "PlayStation 2"] <- "PS2")
ign <-within(ign, levels(Platform)[levels(Platform) == "PlayStation 4"] <- "PS4")
ign <-within(ign, levels(Platform)[levels(Platform) == "Xbox 360"] <- "X360")
#ign <-within(ign, levels(Platform)[levels(Platform) == "Xbox"] <- "XB")
ign <-within(ign, levels(Platform)[levels(Platform) == "Xbox One"] <- "XOne")
ign <-within(ign, levels(Platform)[levels(Platform) == "NES"] <- "NES ")
#ign <-within(ign, levels(Platform)[levels(Platform) == "Super NES"] <- "SNES")
#ign <-within(ign, levels(Platform)[levels(Platform) == "WonderSwan"] <- "WS")
#ign <-within(ign, levels(Platform)[levels(Platform) == "Game Boy"] <- "GB")
#ign <-within(ign, levels(Platform)[levels(Platform) == "Game Boy Advance"] <- "GBA")
#ign <-within(ign, levels(Platform)[levels(Platform) == "Game Boy Color"] <- "GC")
ign <-within(ign, levels(Platform)[levels(Platform) == "Nintendo DS"] <- "DS")
ign <-within(ign, levels(Platform)[levels(Platform) == "Nintendo 3DS"] <- "3DS")
#ign <-within(ign, levels(Platform)[levels(Platform) == "Nintendo 64"] <- "N64")
ign <-within(ign, levels(Platform)[levels(Platform) == "PlayStation Vita"] <- "PSV")
#ign <-within(ign, levels(Platform)[levels(Platform) == "PlayStation"] <- "PS")
ign <-within(ign, levels(Platform)[levels(Platform) == "Wii U"] <- "WiiU")

#Merging the Datasets 
df <- merge(vg,ign,by=c('Name',"Platform"))

#Deleting unimportant Columns
cols.dont.want <- c("score_phrase","EU_Sales","JP_Sales","Other_Sales","NA_Sales","X","genre","release_year") # if you want to remove multiple columns
data <- df[, ! names(df) %in% cols.dont.want, drop = F]

#Importing CPI Index to adjust for Inflation
library(dplyr)
library(lubridate)
monthly_cpi <-read.csv("http://research.stlouisfed.org/fred2/data/CPIAUCSL.csv", header = TRUE)
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %.% group_by(cpi_year) %.% summarize(cpi = mean(VALUE))
yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2016]
colnames(yearly_cpi)[1] <- paste('Year')

#Merging the CPI to the orginial dataset
data<- merge(data,yearly_cpi,by=c("Year"))

#Adjusting Globa_Sales to take care of inflation
data$Global_Sales = data$Global_Sales / data$adj_factor

#Exporting merged to Python to add "Sequels"
write.csv(data, "mergedsmall.csv")

#Importing new dataset modified in Python
data1<- read.csv("3pmsmall.csv",sep=",",header=T)

#Creating a train and test dataset
library(caTools)
set.seed(123)
split = sample.split(data1$Global_Sales,SplitRatio = 0.80)
trainListings = subset(data1, split==TRUE)
testListings = subset(data1, split==FALSE)


#Finding the best fit model
library(MASS)
fit <- lm(Global_Sales ~Platform+ Platform:log(Rank)+Platform:score+ log(Rank)+ Genre + Genre:log(Rank)+ Genre:score+ Publisher:score + Publisher+ score+ editors_choice+ editors_choice:log(Rank) + editors_choice:score+ release_month+ release_month:Genre + release_month:Platform + seq +seq:Genre+ seq:Platform, data=trainListings)
step <- stepAIC(fit, direction="both")
step$anova # display results

best_model <- lm(Global_Sales ~ Platform + log(Rank) + Genre + score + editors_choice + 
                   seq + Platform:log(Rank) + Platform:score + log(Rank):Genre + 
                   Genre:score + log(Rank):editors_choice + score:editors_choice,data=trainListings)
summary(best_model)

## Running the model on the test dataset
pred <- predict(best_model, testListings)
head(pred)

testListings$pred <- pred 
head(testListings)

SS_total <- sum((testListings$Global_Sales - mean(testListings$Global_Sales))^2)
SS_total
SS_residual   <- sum((testListings$Global_Sales - testListings$pred)^2)
SS_residual
SS_regression <- sum((testListings$pred - mean(testListings$Global_Sales))^2)
SS_regression
SS_total - (SS_regression+SS_residual)

test_rsq = SS_regression/SS_total
test_rsq
summary(best_model)$r.squared
