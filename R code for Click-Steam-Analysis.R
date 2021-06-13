library(readxl)
ExpediaData <- read_excel("Downloads/ExpediaData.xlsx")
View(ExpediaData)  
#removed the click_datetime column since most of them are blank
ExpediaData$click_datetime <- NULL

#for city1, city2, city3, and city 4 we made all the blank values = 0, meaning they were not included in the search
ExpediaData$city1[is.na(ExpediaData$city1)] <- 0
ExpediaData$city2[is.na(ExpediaData$city2)] <- 0
ExpediaData$city3[is.na(ExpediaData$city3)] <- 0
ExpediaData$city4[is.na(ExpediaData$city4)] <- 0

#make the hotel_discounted column binary
ExpediaData$hotel_discounted <- ifelse(ExpediaData$hotel_discounted == "Y", 1, 0)

#separated each city and created a new dataset 
ExpediaDataCity1 <- subset(ExpediaData, select=c(1:12, 16:23))
ExpediaDataCity2 <- subset(ExpediaData, select=c(1:11, 13, 16:23))
ExpediaDataCity3 <- subset(ExpediaData, select=c(1:11, 14, 16:23))
ExpediaDataCity4 <- subset(ExpediaData, select=c(1:11, 15, 16:23))

#delete rows where each city1, city2, city3, or city4 equals 0
ExpediaDataCity1<-ExpediaDataCity1[!(ExpediaDataCity1$city1==0),]
ExpediaDataCity2<-ExpediaDataCity2[!(ExpediaDataCity2$city2==0),]
ExpediaDataCity3<-ExpediaDataCity3[!(ExpediaDataCity3$city3==0),]
ExpediaDataCity4<-ExpediaDataCity4[!(ExpediaDataCity4$city4==0),]

#summarized each new dataset
summary(ExpediaDataCity1)
summary(ExpediaDataCity2)
summary(ExpediaDataCity3)
summary(ExpediaDataCity4)

#number of different hotels clicked in each city
city1Hotel_uniq <- unique(ExpediaDataCity1$hotel_id)
length(city1Hotel_uniq)
city2Hotel_uniq <- unique(ExpediaDataCity2$hotel_id)
length(city2Hotel_uniq)
city3Hotel_uniq <- unique(ExpediaDataCity3$hotel_id)
length(city3Hotel_uniq)
city4Hotel_uniq <- unique(ExpediaDataCity4$hotel_id)
length(city4Hotel_uniq)

#remove NA values in city price column from each dataset and rename
city1Hotel_priceAvgData <- ExpediaDataCity1[!(is.na(ExpediaDataCity1$hotel_price) | ExpediaDataCity1$hotel_price==""), ]
city2Hotel_priceAvgData <- ExpediaDataCity2[!(is.na(ExpediaDataCity2$hotel_price) | ExpediaDataCity2$hotel_price==""), ]
city3Hotel_priceAvgData <- ExpediaDataCity3[!(is.na(ExpediaDataCity3$hotel_price) | ExpediaDataCity3$hotel_price==""), ]
city4Hotel_priceAvgData <- ExpediaDataCity4[!(is.na(ExpediaDataCity4$hotel_price) | ExpediaDataCity4$hotel_price==""), ]

#find average hotel price for each city
avgCity1Hotel_price <- mean(city1Hotel_priceAvgData$hotel_price)
avgCity2Hotel_price <- mean(city2Hotel_priceAvgData$hotel_price)
avgCity3Hotel_price <- mean(city3Hotel_priceAvgData$hotel_price)
avgCity4Hotel_price <- mean(city4Hotel_priceAvgData$hotel_price)

#create vector with averages
avgHotel_Price <- c(avgCity1Hotel_price, avgCity2Hotel_price, avgCity3Hotel_price, avgCity4Hotel_price)

#create bar graph for average hotel price for each city
avgHotel_PriceGraph <- barplot(avgHotel_Price, main="Average Hotel Price", horiz=FALSE, names.arg=c("City 1", "City 2", "City 3", "City 4"))

#remove NA values in hotel_discounted column from each dataset and rename
city1Hotel_DiscountedData <- ExpediaDataCity1[!(is.na(ExpediaDataCity1$hotel_discounted) | ExpediaDataCity1$hotel_discounted==""), ]
city2Hotel_DiscountedData <- ExpediaDataCity2[!(is.na(ExpediaDataCity2$hotel_discounted) | ExpediaDataCity2$hotel_discounted==""), ]
city3Hotel_DiscountedData <- ExpediaDataCity3[!(is.na(ExpediaDataCity3$hotel_discounted) | ExpediaDataCity3$hotel_discounted==""), ]
city4Hotel_DiscountedData <- ExpediaDataCity4[!(is.na(ExpediaDataCity4$hotel_discounted) | ExpediaDataCity4$hotel_discounted==""), ]

#table showing frequency of Y and N for hotel_discounted
as.data.frame(table(city1Hotel_DiscountedData$hotel_discounted))
as.data.frame(table(city2Hotel_DiscountedData$hotel_discounted))
as.data.frame(table(city3Hotel_DiscountedData$hotel_discounted))
as.data.frame(table(city4Hotel_DiscountedData$hotel_discounted))

#percent of discounted hotels percent
percentDiscountedCity1 <- 9913/30160
percentDiscountedCity2 <- 20327/28563
percentDiscountedCity3 <- 12438/39993
percentDiscountedCity4 <- 20681/36095

#create vector with percent hotels were discounted in each city
percentDiscounted <- c(percentDiscountedCity1, percentDiscountedCity2, percentDiscountedCity3, percentDiscountedCity4)

#create bar graph for average hotel price for each city
percentDiscountedbargraph <- barplot(percentDiscounted, main="Percentage of Hotels Discounted", horiz=FALSE, names.arg=c("City 1", "City 2", "City 3", "City 4"))

#create new column for difference in trip_end and trip_begin
ExpediaDataCity1$tripDuration <- difftime(ExpediaDataCity1$trip_end, ExpediaDataCity1$trip_begin, units = "days")
ExpediaDataCity2$tripDuration <- difftime(ExpediaDataCity2$trip_end, ExpediaDataCity2$trip_begin, units = "days")
ExpediaDataCity3$tripDuration <- difftime(ExpediaDataCity3$trip_end, ExpediaDataCity3$trip_begin, units = "days")
ExpediaDataCity4$tripDuration <- difftime(ExpediaDataCity4$trip_end, ExpediaDataCity4$trip_begin, units = "days")

#create new column for difference in trip_begin and trip_begin
ExpediaDataCity1$daysPriorToTrip <- difftime(ExpediaDataCity1$trip_begin, ExpediaDataCity1$search_datetime, units = "days")
ExpediaDataCity2$daysPriorToTrip <- difftime(ExpediaDataCity2$trip_begin, ExpediaDataCity2$search_datetime, units = "days")
ExpediaDataCity3$daysPriorToTrip <- difftime(ExpediaDataCity3$trip_begin, ExpediaDataCity3$search_datetime, units = "days")
ExpediaDataCity4$daysPriorToTrip <- difftime(ExpediaDataCity4$trip_begin, ExpediaDataCity4$search_datetime, units = "days")

#remove NA values in tripDuration column from each dataset and rename
city1tripDuration_AvgData <- ExpediaDataCity1[!(is.na(ExpediaDataCity1$tripDuration) | ExpediaDataCity1$tripDuration==""), ]
city2tripDuration_AvgData <- ExpediaDataCity2[!(is.na(ExpediaDataCity2$tripDuration) | ExpediaDataCity2$tripDuration==""), ]
city3tripDuration_AvgData <- ExpediaDataCity3[!(is.na(ExpediaDataCity3$tripDuration) | ExpediaDataCity3$tripDuration==""), ]
city4tripDuration_AvgData <- ExpediaDataCity4[!(is.na(ExpediaDataCity4$tripDuration) | ExpediaDataCity4$tripDuration==""), ]

#find average trip_duration for each city 
avgCity1trip_duration <- mean(city1tripDuration_AvgData$tripDuration)
avgCity2trip_duration <- mean(city2tripDuration_AvgData$tripDuration)
avgCity3trip_duration <- mean(city3tripDuration_AvgData$tripDuration)
avgCity4trip_duration <- mean(city4tripDuration_AvgData$tripDuration)

#create vector with average trip duration in each city
avgTrip_duration <- c(3.62, 5.52, 3.61, 3.75)

#create bar graph for average trip duration for each city
Trip_durationbargraph <- barplot(avgTrip_duration, main="Average Trip Duration", horiz=FALSE, names.arg=c("City 1", "City 2", "City 3", "City 4"))

#remove NA values in daysPriorToTrip column from each dataset and rename
city1daysPriorToTrip_AvgData <- ExpediaDataCity1[!(is.na(ExpediaDataCity1$daysPriorToTrip) | ExpediaDataCity1$daysPriorToTrip==""), ]
city2daysPriorToTrip_AvgData <- ExpediaDataCity2[!(is.na(ExpediaDataCity2$daysPriorToTrip) | ExpediaDataCity2$daysPriorToTrip==""), ]
city3daysPriorToTrip_AvgData <- ExpediaDataCity3[!(is.na(ExpediaDataCity3$daysPriorToTrip) | ExpediaDataCity3$daysPriorToTrip==""), ]
city4daysPriorToTrip_AvgData <- ExpediaDataCity4[!(is.na(ExpediaDataCity4$daysPriorToTrip) | ExpediaDataCity4$daysPriorToTrip==""), ]

#find average daysPriorToTrip for each city
avgCity1_daysPriorToTrip <- mean(city1daysPriorToTrip_AvgData$daysPriorToTrip)
avgCity2_daysPriorToTrip <- mean(city2daysPriorToTrip_AvgData$daysPriorToTrip)
avgCity3_daysPriorToTrip <- mean(city3daysPriorToTrip_AvgData$daysPriorToTrip)
avgCity4_daysPriorToTrip <- mean(city4daysPriorToTrip_AvgData$daysPriorToTrip)

#create vector with average daysPriorToTrip in each city
avgdaysPriorToTrip <- c(52.05, 80.49, 42.44, 51.38)

#create bar graph for average days prior to trip begin for each city
Trip_durationbargraph <- barplot(avgdaysPriorToTrip, main="Average Number of Days Hotel is Clicked Before Trip", horiz=FALSE, names.arg=c("City 1", "City 2", "City 3", "City 4"))

#create vector with average click_for_purchase in each city
avgclick_for_purchase<- c(mean(ExpediaDataCity1$click_for_purchase), mean(ExpediaDataCity2$click_for_purchase), mean(ExpediaDataCity3$click_for_purchase), mean(ExpediaDataCity4$click_for_purchase))

#create bar graph for average click for purchase for each city
click_for_purchaseBargraph <- barplot(avgclick_for_purchase, main="Average Clicks for Purchase", horiz=FALSE, names.arg=c("City 1", "City 2", "City 3", "City 4"))

#create vector with average click_for_details in each city
avgclick_for_details<- c(mean(ExpediaDataCity1$click_for_details), mean(ExpediaDataCity2$click_for_details), mean(ExpediaDataCity3$click_for_details), mean(ExpediaDataCity4$click_for_details))

#create bar graph for average click for details for each city
click_for_detailsBargraph <- barplot(avgclick_for_details, main="Average Clicks for Details", horiz=FALSE, names.arg=c("City 1", "City 2", "City 3", "City 4"))

#created new column to indicate city
ExpediaData$cityType <- ifelse(ExpediaData$city1==1, 1, ifelse(ExpediaData$city2==1, 2, ifelse(ExpediaData$city3==1, 3, ifelse(ExpediaData$city4==1, 4, NA  ))))

#Find Correlations between variables
importantData <- ExpediaData[c(3:6, 8:11, 19:24)]
cor(importantData, use="complete.obs", method="pearson")
rcorr(as.matrix(importantData))
CorrelationComplete <- cor(importantData, use="complete.obs")
CorrelationPairwise <- cor(importantData, use="pairwise.complete.obs")
corrplot::corrplot(cor(Correlation))
