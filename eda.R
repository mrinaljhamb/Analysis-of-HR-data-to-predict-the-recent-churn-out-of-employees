HR_Data=read.csv("E:\\R_Lectures\\dr. mohit\\PROJECT\\HR_Data.csv")
View(HR_Data)
library(ggplot2)

#Impact of work accident on people leaving their jobs.
ggplot(HR_Data,aes(Work_accident))+geom_histogram(fill="steelblue",binwidth = 1)+facet_grid(left~.)+scale_x_continuous(breaks = c(0,1))+scale_y_continuous(breaks = c(1000,2000,3000,4000,5000,6000,7000,8000))
    #From the second histogram we can quite clearly see that majority of people leaving didn't have a work accident. To take a decisive call of its impact on leaving we will find out the proportion of people who had work accident in subcategories made on the basis of whether they have left or not.
#Proportion of people who had work accident in subcategories made on the basis of whether they have left or not.
table(HR_Data$Work_accident,HR_Data$left,dnn=c("Work_accident","left"))
    #From this table we can quite clearly see that proportion of people having work accident is significantly low in people who have left as compared to those who haven't. So it is quite clear that it is not as considerable a factor.

#Impact of not getting promotion on people leaving their jobs. Now this has to be for those who have worked above significant time and according to the data provided this is maintained for whether someone has been promoted in last five yaers or not.
ggplot(HR_Data[which(HR_Data$time_spend_company>=5),],aes(promotion_last_5years))+geom_histogram(fill="steelblue",binwidth = 1)+facet_grid(left~.)+scale_x_continuous(breaks = c(0,1))+ylab("count(time spent>4)")
    #From the second hisogram it is quite clear that almost all of the senior employes who have left are those who weren't promoted in last 5 years.So now we will try to look at the proportion of people who were promoted in the subcategories made on the basis of whether they have left or not.
#proportion of people who were promoted in the subcategories made on the basis of whether they have left or not
data=HR_Data[which(HR_Data$time_spend_company>4),]
table(data$promotion_last_5years,data$left,dnn=c("promotion","left"))
    #We can quite clearly see that proportion of people who have been promoted is significantly low in people who have left as compared to who haven't. This shows that there is quite a possibility of it being a significant factor.

#Combined impact of getting promotion and time spent in the company for those who have worked for more than 4 years.
ggplot(HR_Data[which(HR_Data$left==1 & HR_Data$time_spend_company>=5), ],aes(time_spend_company))+geom_bar(fill="steelblue")+facet_grid(Work_accident~.)+scale_x_continuous(breaks = c(5,6,7,8,9,10))
    #We can see that the count of people who were not promoted and have left is less for those who have spent 7 years in company as compared to those who have spent 6 years and there after that is for those who have spent more than 7 years this count is zero. But to be more certain about this we will find out the proportion of people left without promotion for the categorise made on the basis of their time spent in the company.
#proportion of people left without promotion for the categories made on the basis of their time spent in the company.
data=HR_Data[which(HR_Data$left==1 & HR_Data$time_spend_company>=5), ]
table(data$promotion_last_5years, data$time_spend_compan,dnn=c("promotion","time_spent"))
    #we can pretty clearly see that almost all of the people who have have left after working 5 years or more are those which have not been promoted except one but still ther are people who have not been promoted and still working without promotion. So there combined effect can be significant while modelling.

#Seprate density plots of people who have left and those who haven't left on the basis of their respective satisfaction level.
ggplot(HR_Data[which(HR_Data$left==1),],aes(satisfaction_level))+geom_density(color="steelblue")
 

#Seprate density plots of people who have left and those who haven't left on the basis of how they have been evaluated last time.
ggplot(HR_Data[which(HR_Data$left==1),],aes(last_evaluation))+geom_density(color="steelblue")

#Count of people leaving of different departments.
ggplot(HR_Data[which(HR_Data$left==1),],aes(department,fill=department))+geom_bar()
    #By this we can quite clearly see that amount of people leaving from sales department is max but we also have to look at the proportion of people leaving from each department.
#proportion of people leaving.
table(HR_Data$department,HR_Data$left,dnn=c('department','left'))
    #This pretty well tells that department has the role to play as we can see that ratio of people left from each deparment to the people who didn't is varying.    

#count of people leaving with high, medium and low salary.
ggplot(HR_Data[which(HR_Data$left==1),],aes(salary,fill=salary))+geom_bar()
    #Clearly the salary category has the role to play since count of people leaving is highest in low salary category and is lowest in high salary category.
#proportion of people leaving from each salary category.
table(HR_Data$salary,HR_Data$left,dnn=c('salary','left'))
    #Table pretty clearly tells that salary is significant as the proportion of people who have left with low salary is the hisghest and those left with the high salary is the lowest.

#Impact of time spent in company and salary on the decision of people leaving the job.
ggplot(HR_Data[which(HR_Data$left==1),],aes(time_spend_company))+geom_bar(fill="steelblue")+facet_grid(salary~.)+scale_x_continuous(breaks = c(2,3,4,5,6,7,8,9,10))
#proportion of people leaving those who have spent the same amount of time in the company.
table(HR_Data$salary,HR_Data$left,HR_Data$time_spend_company, dnn=c('salary','left','time spent'))
    #These two factors have the combined effect as we can quite clearly see that the ratio of people leaving to that of who have stayed for almost all categories of salary is increasing with inccrease in the number of years spend in the company and there after the count of people leaving is 0 as no one has left the company after working for 6 years.

#Density plot of people leaving on the basis of their average monthly working hours.
ggplot(HR_Data[which(HR_Data$left==1),],aes(average_montly_hours))+geom_density(color="steelblue")
#Seprate density plots of people of different salary category leaving on the basis of their average monthly working hours.
ggplot(HR_Data[which(HR_Data$left==1),],aes(average_montly_hours,color=salary))+geom_density()
    #The count of people leaving in each salary category follows the similar trend so we will check the proportion of people leaving in each of the subcategories made on the basis of average monthly hours:
    #average_monthly_hours<175,175<average_monthly_hours<225,225<average_monthly_hours
#proportion of people leaving in each of the subcategories.
data1=HR_Data[which(HR_Data$average_montly_hours<=175),]
table(data1$left)
data2=HR_Data[which(HR_Data$average_montly_hours>175 & HR_Data$average_montly_hours>225),]
table(data2$left)
data3=HR_Data[which(HR_Data$average_montly_hours>=225),]
table(data3$left)
    #We can roughly say that the proportion of people leaving is continuously decreasing with increse in average monthly working hours. So this can be significant factor in modelling.
#combined effect of salary and average monthly working hours.
data1=HR_Data[which(HR_Data$average_montly_hours<=175),]
table(data1$left,data1$salary)
data2=HR_Data[which(HR_Data$average_montly_hours>175 & HR_Data$average_montly_hours>225),]
table(data2$left,data2$salary)
data3=HR_Data[which(HR_Data$average_montly_hours>=225),]
table(data3$left,data3$salary)
    #We can quite clearly see that proportion of people leaving in each category of average monthly working hours is varying in different salary categories. So these can have the combined effect in modelling.

#Seprate histogrmas for people with different number of projects on the basis of their average monthly working hours.
ggplot(HR_Data,aes(HR_Data$average_montly_hours))+geom_histogram(fill="steelblue")+facet_grid(number_project~.)
    #Histograms quite clearly show that there is no corelation between average monthly working hours and number of projects.

#Plotting seprate histograms on the basis of satisfaction level for each salary category, time spent in the company & whtehter they have left or not.
ggplot(HR_Data[which(HR_Data$left==1),],aes(satisfaction_level))+geom_histogram(fill="steelblue")+facet_grid(salary~time_spend_company)
ggplot(HR_Data[which(HR_Data$left==0),],aes(satisfaction_level))+geom_histogram(fill="steelblue")+facet_grid(salary~time_spend_company)
    #We can quite clearly see that initially for first 2 years satisfacation level is not that significant in each of the salary category. But for those who have spend 3 & 4 years we can quite clearly see that count of poeple leving with satisfaction level less than 0.5 is more than those who have left this difference in count is comparitively less in high salary category. And for those who have spent 5 & 6 years count of people leaving with high satisfaction level is more in medium and low salary category as compared to those who have stayed. No one has left the company after working more than 6 years and in high salary category no one has left after working more than 5 years. This shows that there can be interaction between these three terms which can be significant while modelling. 

#Scatter plot between last evaluation and satisfaction level.
ggplot(HR_Data,aes(HR_Data$satisfaction_level,HR_Data$last_evaluation))+geom_point(color="steelblue")+geom_smooth(color="red")
    #Plot shows no clear cut trend.Hencce, no corelation between the used two attributes.