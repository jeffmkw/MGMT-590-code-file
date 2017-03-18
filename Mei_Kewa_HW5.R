hw5 = read.csv("/Users/Jeffery/Google Drive/Homework/web data 590/HW/HW5/keywords.csv",sep=',',header = T)

#-------------------------------------------
#Studied with JiaNing Hu
#overall comments in the end to save time.
#------------------------------------------
names(hw5)

summary(hw5)

hist(hw5$num_impressions)
hist(hw5$num_word,xlab = "num of words",ylab = 'keywords', main = 'distribution of num of words in keywords', density=10)

sd(hw5$log_trans)

library(moments)
skewness(hw5$num_impressions)
skewness(hw5$log_trans)
kurtosis(hw5$num_impressions)
kurtosis(hw5$log_trans)

#comments from the summary and plots.
#base on the summary statistics and plots here is what I found:
#most of the keywords contain 1 or 2 words.
#more than 50% of the keywords can help customers decide making a purchase, we should try to put our money on these keywords.

#both plots show that the number of words in keywords and number of impressions are heavily right skewed.
#the high kurtosis means that the numbers are quite clustered together around the median. 
#from the summary statistics we can see that most of the keywords have limited chances to be showed to people.
#More specifically, they are shown less than 1000 times. In the meantime some, very few, keywords can be showed a lot. 
#If the exposure to people will increase the advertising result, I would recommend spending more money on these keywords.

cor(hw5$log_trans,hw5$brand)
# an brand indicator is unlikely to increase the amount of impression

hw5$ctr = hw5$num_clicks/hw5$num_ads
#make a click through rate
hist(hw5$ctr)
cor(hw5$avg_ad_quality,hw5$ctr)
#an increase in ad quality is pretty like to increase the click through rate
#so we should make the ad look better, and with better quality.

brandedkeywords = hw5[hw5$brand==1,]
generickeywords = hw5[!hw5$brand==1,]

boxplot(ctr~brand,data = hw5)
#ctr is actually lower on keywords with brand indicator

boxplot(ctr ~ num_word, data = hw5)
#short keywords is more likely to have better click through rate.

t.test(brandedkeywords$ctr , generickeywords$ctr, alternative = 'greater')

t.test(brandedkeywords$log_imp, generickeywords$log_imp, alternative = 'greater')


#comments about the ttest
#We made an analysis by comparing the click through rate of the keywords with and without brand name indicator
#The result shows that keywords with brand name indicator has a lower click through rate.
#Also, the keywords with brand names can give more impression to the viewers, which may backfire ourselves.
#Therefore, we should first consider the keywords not including our brand names.

linear.model = lm(ctr~num_word, data = hw5)
summary(linear.model)
#when trying to compare the relationship between click through rate and number of words in keywords, I found there is no relationship between them.
#the model I made between these two variables cant not explain the relationship.
#therfore I believe the number of words does not matter to the performance.

#overall,
#when we are looking for keywords to buy, we should cosider those without brand indicator,
#we should focus on making/buying higher quality ads,
#more researches need to be conducted to see how it works out.