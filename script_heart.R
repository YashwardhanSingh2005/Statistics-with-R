rm(list = ls()) #Command for erasing environment variables

data_hrt <- read.csv("C:/Datasets/heart_statlog_cleveland_hungary_final.csv")

head(data_hrt)

library(Hmisc)

describe(data_hrt) # Hmisc command to get info about the data
# Finding Ratio of number of people having a heart disease
rat_hrt <- sum(data_hrt$target)/nrow(data_hrt)
rat_hrt # An astonishing 52% of the people have a heart disease

#Gender
# Claim : People who have heart diseases are generally male
hrt_dis_hai = subset(data_hrt , data_hrt$sex==1)
hrt_dis_nhi_hai = subset(data_hrt , data_hrt$sex==0)
mean(hrt_dis_hai$target)#61% wtf thats too many males eating wrong food
mean(hrt_dis_nhi_hai$target)# only 25%!
#is this statistically significant?
t.test(hrt_dis_hai$target , hrt_dis_nhi_hai$target , alternative = "two.sided" , conf.level = 0.99)
# since p value is less than 0.05 we can reject the null hypothesis and say that this difference is in fact statistically significant and is a huge disparity.
library(ggplot2)

ggplot(data_hrt, aes(x = sex, fill = target)) +
  geom_bar(stat = "count", color = "black") +  # Create bar chart with black borders
  labs(title = "Heart Disease Distribution by Sex",  # Add informative title
       x = "Sex", y = "Count", fill = "Heart Disease Status") +  # Add labels
  theme_minimal()  # Use a clean theme for better presentation

ggplot(data_hrt, aes(x = age, y = factor(target), color = target)) +
  geom_point(size = 3) +  # Plot points with size 3 for better visibility
  labs(title = "Heart Disease by Age",  # Add informative title
       x = "Age", y = "Heart Disease Status", color = "Heart Disease Status") +
  theme_minimal()  # Use a clean theme for better presentation
 


ggplot(data_hrt, aes(x = age, y = cholesterol)) +
  geom_point(size = 3) +  # Plot points for data points
  geom_smooth(method = "lm", color = "blue") +  # Add a linear trend line in blue
  labs(title = "Cholesterol Level by Age Trendline",  # Add informative title
       x = "Age", y = "Cholesterol Level") +
  theme_minimal()  # Use a clean theme for better presentation




