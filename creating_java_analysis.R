library(dplyr)
library(foreign)

#creating unique homes dataset to be modified in GIS
build <- read.dbf("TBL_BUIL.DBF")
build <- filter(build, USE=="1s")
land <- read.dbf("TBL_LAND.DBF")
homes <- inner_join(build, land, by="PID")
homes$PID <- as.factor(as.numeric(as.character(homes$PID)))
homes$total_sqft <- homes$GRND_SQFT+homes$FLR2_SQFT+homes$BSMT_FNSH
homes_GIS_export <-data.frame(cbind(homes$PID,homes$total_sqft,homes$X,homes$Y))
names(homes_GIS_export)=c("PID","SQFT","X","Y")
write.csv(homes_GIS_export, "home_info_for_GIS.csv")

#read in with new coordinates
options(scipen=999)
homes_updatedXY <- read.csv("home_lat_long.csv")[,c(2:6)]
homes_updatedXY$PID<- as.factor(homes_updatedXY$PID)

#filter out small and large homes
homes_updatedXY <- filter(homes_updatedXY, total_SQFT>200)%>%filter(total_SQFT<10000)

write.csv(homes_updatedXY, "homes_qgis.csv", row.names=F)
#qgis analysis to get buffer
homes_in_buffer_half <- data.frame("OBJECTID" = unique(read.csv("buffered_homes_halfMile.csv")$OBJECTID))
homes_updated_buffer_half <- inner_join(homes_updatedXY, homes_in_buffer_half, by="OBJECTID")

homes_in_quarter_buffer <- data.frame("OBJECTID"=unique(read.csv("buffered_homes_quarterMile.csv")$OBJECTID))
homes_updated_quarter_buffer <- inner_join(homes_updatedXY,homes_in_quarter_buffer,by="OBJECTID")

#figure out homes sold
sales <- read.csv("Property_Sales_20102013.csv", stringsAsFactors = F)
sale_date <- data.frame(PIN = as.factor(sales$PIN), Sale_Date=sales$Sale_Date)
test <- data.frame(PIN = as.factor(sale_date$PIN), duplicate = duplicated(sale_date))
sales$repeatedEntry <- duplicated(sale_date)
sales <- filter(sales, repeatedEntry==FALSE)
sales$Gross_Sale_Price <- as.numeric(sales$Gross_Sale_Price)

sales_cleaned <- filter(sales, Gross_Sale_Price>10000)
sales_cleaned <- filter(sales_cleaned,Gross_Sale_Price<5000000)
sales_cleaned <- data.frame(cbind(sales_cleaned$PIN,sales_cleaned$Sale_Date,sales_cleaned$Gross_Sale_Price),stringsAsFactors = F)
names(sales_cleaned) <- c("PID","Date","Price")
sales_cleaned$PID <- as.factor(sales_cleaned$PID)
sales_cleaned$Price<-as.numeric(sales_cleaned$Price)

test2 <- inner_join(test, sales_cleaned, by = c("PIN"="PID"))

#2 different needs: homes that have been sold for java 
#records or each sale for regression

unique_homes_sold <- data.frame(PID = unique(sales_cleaned$PID))
#homes_with_sales <- inner_join(homes_updatedXY, sales_cleaned)
unique_homes_with_sales_half <- inner_join(homes_updated_buffer_half, unique_homes_sold)

unique_homes_with_sales_quarter <- inner_join(homes_updated_quarter_buffer,unique_homes_sold)
unique_homes_with_sales_all <-inner_join(homes_updatedXY, unique_homes_sold)
parcels <- read.dbf("parcels_henn.dbf")

parcels$PID <- as.factor(as.numeric(as.character(parcels$PIN2)))
sold_homes_half <- data.frame(PID=(inner_join(unique_homes_with_sales_half,parcels)$PID))
sold_homes_half$soldHalfMileBuffer <- TRUE

sold_homes_quarter <- data.frame(PID=(inner_join(unique_homes_with_sales_quarter,parcels)$PID))
sold_homes_quarter$soldQuarterMileBuffer <- TRUE

sold_homes_all <- data.frame(PID=inner_join(unique_homes_with_sales_all,parcels)$PID)
sold_homes_all$sold <- TRUE

homes_final_all<- left_join(homes_updatedXY,sold_homes_all)
homes_final_all$sold[is.na(homes_final_all$sold)]<-FALSE

#this would need to be moidfied to have homes_updated xy fixed
homes_final_half<- left_join(homes_updatedXY,sold_homes_half)
homes_final_quarter <- left_join(homes_updatedXY,sold_homes_quarter)
homes_final_half$soldHalfMileBuffer[is.na(homes_final_half$soldHalfMileBuffer)]<-FALSE
homes_final_quarter$soldQuarterMileBuffer[is.na(homes_final_quarter$soldQuarterMileBuffer)]<-FALSE

sold_homes_half$inHalfBuffer <- 1
sold_homes_quarter$inQuarterBuffer <- 1


write.csv(homes_final_all[,-c(2)],"javahomes.csv",row.names=F)
temp_homes <- read.csv("javahomes.csv")
sold_homes_export <- filter(temp_homes, sold==TRUE)
sold_homes_export$ID <- 1:14664
write.csv(sold_homes_export, "sold_homes_weights.csv", row.names = F)
write.csv(homes_final_half[,-c(2)],"half_mile_buffer_javahomes.csv",row.names=F)
write.csv(homes_final_quarter[,-c(2)],"quarter_mile_buffer_javahomes.csv",row.names=F)
write.csv(homes_final_quarter[,1:2],"homes_OBJID_PID.csv",row.names=F)
write.csv(homes_final_all[,1:2],"homes_OBJID_PID.csv",row.names=F)


sold_homes_all <- left_join(sold_homes_all,sold_homes_quarter[,c(1,3)])
sold_homes_all$inQuarterBuffer[is.na(sold_homes_all$inQuarterBuffer)]<- 0

sold_homes_all <- left_join(sold_homes_all,sold_homes_half[,c(1,3)])
sold_homes_all$inHalfBuffer[is.na(sold_homes_all$inHalfBuffer)]<- 0

#reduce charecteristics needed
all_sales <- inner_join(sales_cleaned, sold_homes_all)
final_data <- inner_join(all_sales, homes)
#final_data <- inner_join(sold_homes_all,homes)
final_data <- inner_join(final_data, parcels, by="PID")
names(homes_updatedXY)[4:5] <- c("long", "lat")
latLongInfo <- data.frame(PID = homes_updatedXY$PID,long=homes_updatedXY$long, lat = homes_updatedXY$lat)
final_data <- inner_join(final_data, latLongInfo, by = "PID")

#sales_cleaned_no_repeats <- sales_cleaned[!duplicated(sales_cleaned),]

#duplicate_sales <- data.frame(PID = final_data$PID, date=final_data$SALE_DATE)
#sum(duplicated(duplicate_sales))
#sum(duplicated(duplicate_sales$PID))

final_data$Year <- as.numeric(substr(final_data$Date,1,4))
final_data$Month <- as.factor(substr(final_data$Date,6,7))

#final_data2 <- inner_join(sales_cleaned_no_repeats,final_data)

school_info <- read.csv("homes_with_school_info.csv")
school_info <-data.frame(PID=as.character(school_info$PID), elemSchool = school_info$ELEMENTARY, 
                         midSchool = school_info$MIDDLE,highSchool=school_info$HIGH)
highSchoolsInfo <- data.frame(highSchool=levels(school_info$highSchool), highSchool_ranking=c(80,73,69,54,20,38))

final_data <- inner_join(final_data,school_info)
final_data <- inner_join(final_data,highSchoolsInfo)
write.csv(final_data,"final_data.csv", row.names=F)


k8_averages <- read.csv("deviations_k_8.csv",header=FALSE)
names(k8_averages)<- c("OBJECTID","k8_ave")
k8_averages <- inner_join(k8_averages,homes_final_quarter)
k8_averages <- data.frame(PID=k8_averages$PID,k8_ave=k8_averages$k8_ave)

final_data <- inner_join(final_data,k8_averages)
final_data$percentDeviation <- (final_data$k8_ave-final_data$total_sqft)/final_data$k8_ave
relevant_data <- data.frame(percentDeviation=((final_data$k8_ave-final_data$total_sqft)/final_data$k8_ave),
                            price=final_data$Price, total_sqft=final_data$total_sqft,year_built=final_data$YEAR_BUILT.y,
                            school_district = final_data$WSHD_DIST, year = final_data$Year, month=final_data$Month,
                            stories = final_data$STORIES, roof = final_data$ROOF_TYPE, acres = final_data$ACRES_POLY,
                            condition = final_data$CONDITION, heat = final_data$PRMY_HEAT, 
                            neighborhood = final_data$NEIGHBORHO, fireplace = final_data$FIREPLACES, 
                            ac = ifelse(final_data$AC_PERCENT>0,1,0), baths = (as.numeric(final_data$BATH_DELUX)+
                                                                                 as.numeric(final_data$BATH_3QTR)+
                                                                                 as.numeric(final_data$BATH_FULL)+
                                                                                 as.numeric(final_data$BATH_HALF)),
                            fullbaths = final_data$BATH_FULL, halfbaths = final_data$BATH_HALF)

relevant_data <- relevant_data[(complete.cases(relevant_data)),]
relevant_data$neighborhood <- as.factor(relevant_data$neighborhood)
relevant_data$condition <- as.numeric(relevant_data$condition)
relevant_data$year<- as.factor(relevant_data$year)
relevant_data$ac <- as.factor(relevant_data$ac)
test <- lm(log(price)~log(total_sqft)+percentDeviation+year_built+school_district+
             year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
             ac+baths, data=relevant_data)
summary(test)
plot(test)

plot(test$residuals~relevant_data$percentDeviation)
plot(log(final_data$Price)~(final_data$BATH_HALF))
plot(log(relevant_data$price)~(relevant_data$baths))

pairs(~log(price)+(total_sqft)+percentDeviation+year_built, data=relevant_data)
