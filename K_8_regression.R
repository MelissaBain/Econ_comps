library(car)
final_data <- read.csv("final_data.csv")
k8_averages <- read.csv("deviations_k_8.csv",header=FALSE)
names(k8_averages)<- c("OBJECTID","k8_ave")
k8_averages$OBJECTID <- as.numeric(as.character(k8_averages$OBJECTID))
homes_final_quarter <- read.csv("homes_OBJID_PID.csv")
k8_averages <- inner_join(k8_averages,homes_final_quarter)
k8_averages <- data.frame(PID=k8_averages$PID,k8_ave=k8_averages$k8_ave)
final_data <- inner_join(final_data,k8_averages)

# quarter_ave <-read.csv("deviations_quarter_mile.csv",header=F)
# names(quarter_ave)<- c("OBJECTID","quarter_ave")
# quarter_ave <- inner_join(quarter_ave,homes_final_quarter)
# quarter_ave <- data.frame(PID=quarter_ave$PID,quarter_ave=quarter_ave$quarter_ave)
# final_data <- inner_join(final_data, quarter_ave)

relevant_data <- data.frame(PID = final_data$PID,k8_ave = final_data$k8_ave,
                            deviation_k8=(final_data$total_sqft-final_data$k8_ave),
                            percentDeviation_k8=((final_data$total_sqft-final_data$k8_ave)/final_data$k8_ave),
                            price=final_data$Price, total_sqft=final_data$total_sqft,year_built=final_data$YEAR_BUILT.y,
                            water_shed = final_data$WSHD_DIST, year = final_data$Year, month=final_data$Month,
                            stories = final_data$STORIES, roof = final_data$ROOF_TYPE, acres = final_data$ACRES_POLY,
                            condition = final_data$CONDITION, heat = final_data$PRMY_HEAT, 
                            neighborhood = final_data$NEIGHBORHO, fireplace = final_data$FIREPLACES, 
                            ac = ifelse(final_data$AC_PERCENT>0,1,0), baths = (as.numeric(final_data$BATH_DELUX)+
                                                                                 as.numeric(final_data$BATH_3QTR)+
                                                                                 as.numeric(final_data$BATH_FULL)+
                                                                                 as.numeric(final_data$BATH_HALF)),
                            fullbaths = final_data$BATH_FULL, halfbaths = final_data$BATH_HALF, homestead = final_data$HOMESTEAD,
                            elemSchool = final_data$elemSchool, midSchool=final_data$midSchool, highSchool =final_data$highSchool,
                            highSchoolRank = final_data$highSchool_ranking, lat = final_data$lat, long=final_data$long)

write.csv(relevant_data, "k8_relevant_data.csv")
relevant_data <- relevant_data[(complete.cases(relevant_data)),]
relevant_data$neighborhood <- as.factor(relevant_data$neighborhood)
relevant_data$condition <- as.numeric(relevant_data$condition)
relevant_data$year<- as.factor(relevant_data$year)
relevant_data$ac <- as.factor(relevant_data$ac)
relevant_data$month <- as.factor(relevant_data$month)


test1 <- lm(log(price)~log(total_sqft)+log(k8_ave)+deviation_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchoolRank, data=relevant_data)

test2 <- lm(log(price)~log(total_sqft)+log(k8_ave)*deviation_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchoolRank, data=relevant_data)
test3 <- lm(log(price)~log(total_sqft)+percentDeviation_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchool, data=relevant_data)

test4 <- lm(log(price)~log(total_sqft)+log(k8_ave)*percentDeviation_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchool, data=relevant_data)

anova(test3, test4)


summary(test2)
scatterplotMatrix(~log(relevant_data$price)+relevant_data$percentDeviation_k8)
pos <- filter(relevant_data, deviation_k8>0)
scatterplotMatrix(~log(pos$price)+pos$deviation_k8)
neg <- filter(relevant_data, deviation_k8<=0)
scatterplotMatrix(~log(neg$price)+neg$deviation_k8)
posP <- filter(relevant_data, percentDeviation_k8>0)
scatterplotMatrix(~log(posP$price)+posP$percentDeviation_k8)
negP <- filter(relevant_data, percentDeviation_k8<=0)
scatterplotMatrix(~log(negP$price)+negP$percentDeviation_k8)





relevant_data$posDev_k8 <- ifelse(relevant_data$deviation_k8>0,relevant_data$deviation_k8,0)
relevant_data$negDev_k8 <- ifelse(relevant_data$deviation_k8<0,relevant_data$deviation_k8,0)
relevant_data$posPerDev_k8 <- ifelse(relevant_data$percentDeviation_k8>0,relevant_data$percentDeviation_k8,0)
relevant_data$negPerDev_k8 <- ifelse(relevant_data$percentDeviation_k8<0,relevant_data$percentDeviation_k8,0)
scatterplotMatrix(~log(relevant_data$price)+relevant_data$posDev_k8+relevant_data$negDev_k8)
scatterplotMatrix(~log(relevant_data$price)+relevant_data$posPerDev_k8+relevant_data$negPerDev_k8)

test <- lm(log(price)~log(total_sqft)+negPerDev_k8+log(k8_ave)*posPerDev_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchool, data=relevant_data)
test6 <- lm(log(price)~log(total_sqft)+negPerDev_k8+log(k8_ave)+posPerDev_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchool, data=relevant_data)

test7 <- lm(log(price)~log(total_sqft)+negDev_k8+log(k8_ave)*posDev_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchool, data=relevant_data)

AIC(test7,test6)
summary(test5)
anova(test6, test5)
summary(test_interaction)
anova(test1, test2)
AIC(test2, test3)
plot(test)
leverage_problems <- data.frame(PID=relevant_data[((hatvalues(test)/mean(hatvalues(test)))>3),1])
resid_problems <- data.frame(PID =relevant_data[(abs(test$residuals)>2),1])
cooks_problems <- data.frame(PID=relevant_data[((cooks.distance(test)/mean(cooks.distance(test),na.rm=T))>10),1])
outliers <- full_join(leverage_problems,resid_problems)
outliers<-full_join(outliers,cooks_problems)

relevant_data_no_out <- anti_join(relevant_data,outliers)
test_noOut<- lm(log(price)~log(total_sqft)+negPerDev_k8+log(k8_ave)*posPerDev_k8+year_built+water_shed+
                  year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
                  ac+baths+highSchool, data=relevant_data_no_out)

test_percent <- lm(log(price)~log(total_sqft)+percentDeviation_k8+year_built+water_shed+
                     year*month+roof+stories+sqrt(acres)+condition+heat+neighborhood +sqrt(fireplace)+
                     ac+baths+highSchool, data=relevant_data_no_out)
plot(test_noOut$fitted.values~test_percent$fitted.values)

summary(test_noOut)
plot(test_noOut)

summary(test_percent)

AIC(test_noOut,test_percent)

quarter_regression<- lm(log(price)~log(total_sqft)+log(quarter_ave)+year_built+water_shed+
                  year*month+roof+stories+sqrt(acres)+condition+heat+neighborhood +sqrt(fireplace)+
                  ac+baths+highSchool, data=relevant_data)

summary(quarter_regression)
leverage_problems <- data.frame(PID=relevant_data[((hatvalues(quarter_regression)/mean(hatvalues(quarter_regression)))>3),1])

resid_problems <- data.frame(PID =relevant_data[(abs(quarter_regression$residuals)>2),1])
cooks_problems <- data.frame(PID=relevant_data[((cooks.distance(quarter_regression)/mean(cooks.distance(quarter_regression),na.rm=T))>10),1])
outliers2 <- full_join(leverage_problems,resid_problems)
outliers2<-full_join(outliers2,cooks_problems)
OutBoth <- semi_join(outliers,outliers2)

scatterplotMatrix(~log(relevant_data$price)+log(relevant_data$quarter_ave))

plot(test_noOut$fitted.values~quarter_regression$fitted.values)
plot(test_interaction$residuals~relevant_data$percentDeviation)
plot(log(final_data$Price)~(final_data$BATH_HALF))
plot(log(relevant_data$price)~(relevant_data$baths))

scatterplotMatrix(~log(price)+(total_sqft)+percentDeviation+year_built, data=relevant_data)
scatterplotMatrix(~log(price)+log(total_sqft), data=relevant_data)
interaction <- (log(relevant_data$total_sqft)*log(relevant_data$k8_ave))
scatterplotMatrix(~log(relevant_data$price)+log(relevant_data$total_sqft)+log(relevant_data$k8_ave)+interaction)
scatterplotMatrix(~log(relevant_data_no_out$price)+year_built+acres+condition, data=relevant_data_no_out)

scatterplotMatrix(~log(relevant_data_no_out$price)+sqrt(relevant_data_no_out$acres)+relevant_data_no_out$acres)[1,]
