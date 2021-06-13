library(psych)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)
library(gridExtra)


#Setting the directory
setwd("~/Desktop")

#Importing the data 
data <- read.csv('Expediadata.csv')
head(data)
dim(data)
str(data)

#Generate descriptive statistics for the key variables in the data set.
describe(data)
data %>% split(.$click_for_details) %>% map(describe)

#Visualization

#Influence of refinement tools
p1<- ggplot(data, aes(x=click_for_details, y = search_minstar, fill = click_for_details))+ geom_bar(stat = 'summary', fun.y = 'mean')+xlab("click_for_details")
p2<-ggplot(data, aes(x=click_for_details, y = sort_by_price, fill = click_for_details))+ geom_bar(stat = 'summary', fun.y = 'mean')+xlab("click_for_details")
p3<-ggplot(data, aes(x=click_for_details, y = sort_by_star, fill = click_for_details))+ geom_bar(stat = 'summary', fun.y = 'mean')+xlab("click_for_details")
p4<-ggplot(data, aes(x=click_for_details, y = sort_by_distance, fill = click_for_details))+ geom_bar(stat = 'summary', fun.y = 'mean')+xlab("click_for_details")
p5<-ggplot(data, aes(x=click_for_details, y = webpage_position, fill = click_for_details))+ geom_bar(stat = 'summary', fun.y = 'mean')+xlab("click_for_details")
grid.arrange(p1, p2,p3,p4,p5, nrow = 3)

#Influence of hotel price, star and discount
p6<- ggplot(data, aes(x=click_for_details, y = hotel_price, fill = click_for_details))+ geom_bar(stat = 'summary', fun.y = 'mean')+xlab("Clicked")
p7<- ggplot(data, aes(x=click_for_details, y = hotel_star, fill = click_for_details))+ geom_bar(stat = 'summary', fun.y = 'mean')+xlab("Clicked")
p8<-ggplot(data, aes(x=click_for_details, y = hotel_discouted, fill = click_for_details))+ geom_bar(stat = 'summary', fun.y = 'mean')+xlab("click_for_details")
grid.arrange(p6, p7,p8, nrow = 3)

#Logistic Regression including refinment tools and hotel features variables

reg2 <- glm(click_for_details ~  search_minstar + hotel_star + hotel_discouted + hotel_price + sort_by_distance + sort_by_price + sort_by_star + webpage_position, family=binomial, data=data )
summary(reg2)


