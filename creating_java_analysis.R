library(dplyr)
library(foreign)

#creating unique homes dataset to be modified in GIS
build <- read.dbf("TBL_BUIL.DBF")
build <- filter(build, USE=="1s")
land <- read.dbf("TBL_LAND.DBF")
homes <- inner_join(build, land, by="PID")
homes$total_sqft <- homes$GRND_SQFT+homes$FLR2_SQFT+homes$BSMT_FNSH
homes_GIS_export <-data.frame(cbind(homes$PID,homes$total_sqft,homes$X,homes$Y))
names(homes_GIS_export)=c("PID","SQFT","X","Y")
write.csv(homes_GIS_export, "home_info_for_GIS.csv")

#read in with new coordinates
options(scipen=999)
homes_updatedXY <- read.csv("home_lat_long.csv")[,c(2:6)]

#filter out small and large homes
homes_updatedXY <- filter(homes_updatedXY, total_SQFT>200)%>%filter(total_SQFT<10000)

write.csv(homes_updatedXY, "homes_qgis.csv", row.names=F)
#qgis analysis to get buffer
homes_in_buffer <- data.frame("OBJECTID" = unique(read.csv("buffered_homes_halfMile.csv")$OBJECTID))
homes_updated_buffer <- inner_join(homes_updatedXY, homes_in_buffer, by="OBJECTID")


homes_in_quarter_buffer <- data.frame("OBJECTID"=unique(read.csv("buffered_homes_quarterMile.csv")$OBJECTID))
homes_updated_quarter_buffer <- inner_join(homes_updatedXY,homes_in_quarter_buffer,by="OBJECTID")

#figure out homes sold
sales <- read.csv("Property_Sales_20102013.csv")
homes_with_sales <- inner_join(homes_updated_buffer, sales, by=c("PID"="PIN"))
homes_with_sales_quarter <- inner_join(homes_updated_quarter_buffer,sales, by=c("PID"="PIN"))

parcels <- read.dbf("parcels_henn.dbf")
parcels$PIN2 <- as.numeric(as.character(parcels$PIN2))
sold_homes <- data.frame(PID=unique(inner_join(homes_with_sales,parcels,by=c("PID"="PIN2"))$PID))
sold_homes$soldHalfMileBuffer <- TRUE

sold_homes_quarter <- data.frame(PID=unique(inner_join(homes_with_sales_quarter,parcels,by=c("PID"="PIN2"))$PID))
sold_homes_quarter$soldQuarterMileBuffer <- TRUE

homes_final<- left_join(homes_updatedXY,sold_homes)
homes_final_quarter <- left_join(homes_updatedXY,sold_homes_quarter)
homes_final$soldHalfMileBuffer[is.na(homes_final$soldHalfMileBuffer)]<-FALSE
homes_final_quarter$soldQuarterMileBuffer[is.na(homes_final_quarter$soldQuarterMileBuffer)]<-FALSE
write.csv(homes_final[,-c(2)],"half_mile_buffer_javahomes.csv",row.names=F)
write.csv(homes_final_quarter[,-c(2)],"quarter_mile_buffer_javahomes.csv",row.names=F)
write.csv(homes_final_quarter[,1:2],"homes_OBJID_PID.csv",row.names=F)

#reduce charecteristics needed

