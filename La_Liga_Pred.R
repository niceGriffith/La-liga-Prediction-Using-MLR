setwd("Manik Project")
library(xlsx)
library(dplyr)
library(tidyverse)
mk<-read.xlsx("Project_updated.xlsx",sheetIndex = 1)
#Data Cleaning
mk<-mk[-1,]
colnames(mk)[4:6]<-c("Income","Expenditure","Gross")
mk<-mk[,-12:-10]
mk <- mk %>%
  # Create a group identifier based on the row number
  mutate(group = (row_number() - 1) %/% 5) %>%
  # Group by the new group identifier
  group_by(group) %>%
  # Fill the missing values in the team column with the first non-missing value in each group
  fill(Team, .direction = "down") %>%
  # Ungroup and remove the temporary group column
  ungroup() %>%
  select(-group)

mk <- mk %>% mutate(across(-2:-1,as.numeric))
mk<-mk[-153:-136,]
#Removing NA's
fin_mk<-na.omit(mk_fil)
#Scatterplots
par(mfrow=c(2,2))
plot(fin_mk$Average.Age.of.Players,fin_mk$Points.Scored,xlab = "Average Age of players",ylab="Points Scored")
plot(fin_mk$Expenditure,fin_mk$Points.Scored,xlab = "Expenditure",ylab="Points Scored")
plot(fin_mk$Manager.s.Years.of.experience,fin_mk$Points.Scored,xlab="Managers Year of Experience", ylab="Points Scored")
plot(fin_mk$Income,fin_mk$Points.Scored,xlab="Income", ylab="Points Scored")

plot(fin_mk$Average.Age.of.Players,fin_mk$Gross,xlab="Average Age of players",ylab="Financial Situation")
plot(fin_mk$Average.Age.of.Players,fin_mk$Manager.s.Years.of.experience,xlab="Average Age of players",ylab="Managers year of Experience")
plot(fin_mk$Average.Age.of.Players,fin_mk$Previous.Standing,xlab="Average Age of players",ylab="Previous Standings")
plot(fin_mk$Previous.Standing,fin_mk$Manager.s.Years.of.experience,xlab="Previous Standing",ylab="Managers year of Experience")
plot(fin_mk$Gross,fin_mk$Manager.s.Years.of.experience,xlab="Financial Situation",ylab="Managers year of Experience")
plot(fin_mk$Previous.Standing,fin_mk$Gross,xlab="Previous Standing",ylab="Financial Situation")
#Histo Of Plot
mk$Points.Scored%>%hist(main="Histogram of Points Scored")
View(fin_mk)

#Modeling 
model<-lm(Points.Scored~Average.Age.of.Players+Manager.s.Years.of.experience+Expenditure+Previous.Standing,fin_mk)
summary(model)
kk<-round(predict(model),digits = 0)
df1<-data.frame(fin_mk$Team,kk)
colnames(df1)<-c("Team","Points")
team_means <- df1 %>%
  group_by(Team) %>%
  summarize(mean_value = round(mean(Points, na.rm = TRUE),digits = 0))
standings<-team_means%>%arrange(desc(mean_value))
library(writexl)
write.xlsx(standings,"Predicted Standings.xlsx")
#Correlation Matrix
mk_num<-fin_mk[,-2:-1]
cor_mat<-cor(mk_num)
corrplot::corrplot(cor_mat)

