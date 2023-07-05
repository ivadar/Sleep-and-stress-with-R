genderTable <- table(health$Gender)
genderTable
genderPerc <- round(genderTable*100/sum(genderTable),1)
myColors <- c("pink","slateblue1")
pie(genderTable, main = "Gender", labels = genderPerc,col= myColors)
legend(x="topright", legend = c("Female","Male"),fill=myColors)

#3.	How is sleep duration related to the high stress levels compared to the mean sleep duration of all respondents?
mean(health$`Sleep Duration`)       
sleepDTable <-table(health$`Sleep Duration`)
sleepDTable
stressLevelTable <- table(health$`Stress Level`)
x<-health$`Sleep Duration`
y<-health$`Stress Level`
plot(x,y, main= "Stress level to sleep duration", xlab = "Sleep duration", ylab = "Stress level",pch = 10,  frame  = FALSE)
sd(health$`Sleep Duration`)
sleep68 <- filter(health, health$`Sleep Duration`>=6)
sleep68
sleep68Table <-table(sleep68)
sleep68Table
a<-5
if (a> 0){
  print("negative")
}
if(health$`Sleep Duration`>=6){
  b <- mean(health$`Stress Level`)
  print(b)
}
genderTable
library(dplyr)
sleep68 = filter(health, health$`Sleep Duration`>=6)
sleep68
sleepOver7 <-filter(health, health$`Sleep Duration`>=7)
mean(sleepOver7$`Stress Level`)
overweight <- table(health$`BMI Category`)
overweight
overweightPercentage <- round(overweight*100/sum(overweight),1)
overweightPercentage
otherColors<-c("black", "lightblue", "coral", "brown")
barplot(overweight, main = "BMI Category", col =otherColors )
legend("topright", legend = c("Normal", "Normal weight", "Obese", "Overweight"), fill=otherColors, cex=0.7)
overweightObese <- filter(health, health$`BMI Category`%in% c("Overweight","Obese"))
overweightObese
overweightObeseTable <- table(overweightObese$`Sleep Duration`)
overweightObeseTable
normalWeight <-filter(health, health$`BMI Category`%in% c("Normal", "Normal Weight"))
normalweightTable <-table(normalWeight$`Sleep Duration`)
normalweightTable
overweight
a<-health[health$`BMI Category`%in% c("Overweight", "Obese")]
overweight <-sum(health_results$BMICategory=="Overweight" | health_results$BMICategory=="Obese")
overweight
sum(health_results$BMICategory=="Overweight" | health_results$BMICategory=="Obese")
round(sum(!is.na(health_results$BMICategory))*100/sum(health_results$BMICategory=="Overweight" | health_results$BMICategory=="Obese"),1)
overweight
round(table(health_results$BMICategory)*100/table(overweight),1)
t <- table(health_results$BMICategory)
round(100*overweight/t,2)
round(t*100/sum(t),2)
health_results %>% filter(health_results$BMICategory=="Overweight" | health_results$BMICategory=="Obese")
ggplot(data=health_results)
ggplot(aes(health_results$BMICategory, fill  = health_results$StressLevel))
view(starwars)
starwars%>% filter(hair_color=="black" | hair_color=="brown") %>%
  drop_na(sex) %>%
  ggplot(aes(hair_color,fill=sex))
View(health_results)
health_results%>% filter(health_results$BMICategory %in% c("Overweight", "Obese"))%>%
  ggplot(aes(PhysicalActivityLevel,SleepDuration))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~PhysicalActivityLevel)+
  labs(title = "Physical activity and sleep duration by overweight people", x = "Physiscal activity level", y = "Sleep duration")

view(health_results)
health_results%>% filter(health_results$BMICategory %in% c("Normal", "Normal Weight")) %>%
  ggplot(aes(PhysicalActivityLevel, SleepDuration))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~PhysicalActivityLevel)+
  labs(title = "Physical activity and sleep duration by people with normal weight", x = "Physiscal activity level", y = "Sleep duration")
qualitySleep <-table(health_results$QualityofSleep)
summary(health_results$DailySteps)
normalDist <- rnorm(100, mean = mean(health_results$DailySteps),sd = sd(health_results$DailySteps))
qqplot(health_results$DailySteps, normalDist)
abline(a=0, b=1)
view(health_results)
walkers <- filter(health_results, health_results$DailySteps>=6817)
nonwalkers <-filter(health_results, health_results$DailySteps<6817)
mean(walkers$QualityofSleep)
mean(nonwalkers$QualityofSleep)
mean(walkers$StressLevel)
sd(walkers$StressLevel)
mean(nonwalkers$StressLevel)
sd(nonwalkers$StressLevel)
