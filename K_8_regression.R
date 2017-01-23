library(car)
final_data <- read.csv("final_data.csv")
k8_averages <- read.csv("deviations_k_8.csv",header=FALSE)
names(k8_averages)<- c("OBJECTID","k8_ave")
homes_final_quarter <- read.csv("homes_OBJID_PID.csv")
k8_averages <- inner_join(k8_averages,homes_final_quarter)
k8_averages <- data.frame(PID=k8_averages$PID,k8_ave=k8_averages$k8_ave)
final_data <- inner_join(final_data,k8_averages)

quarter_ave <-read.csv("deviations_quarter_mile.csv",header=F)
names(quarter_ave)<- c("OBJECTID","quarter_ave")
quarter_ave <- inner_join(quarter_ave,homes_final_quarter)
quarter_ave <- data.frame(PID=quarter_ave$PID,quarter_ave=quarter_ave$quarter_ave)
final_data <- inner_join(final_data, quarter_ave)

final_data$percentDeviation <- (final_data$k8_ave-final_data$total_sqft)/final_data$k8_ave
relevant_data <- data.frame(PID = final_data$PID,k8_ave = final_data$k8_ave,
                            percentDeviation_k8=((final_data$k8_ave-final_data$total_sqft)/final_data$k8_ave),
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
                            elemSchool = final_data$elemSchool, midSchool=final_data$midSchool, highSchool =final_data$highSchool)

relevant_data <- relevant_data[(complete.cases(relevant_data)),]
relevant_data$neighborhood <- as.factor(relevant_data$neighborhood)
relevant_data$condition <- as.numeric(relevant_data$condition)
relevant_data$year<- as.factor(relevant_data$year)
relevant_data$ac <- as.factor(relevant_data$ac)
relevant_data$month <- as.factor(relevant_data$month)
test_interaction <- lm(log(price)~log(total_sqft)*log(k8_ave)+year_built+water_shed+
             year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
             ac+baths+elemSchool, data=relevant_data)
test<- lm(log(price)~log(total_sqft)+log(k8_ave)+year_built+water_shed+
                     year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
                     ac+baths+highSchool, data=relevant_data)
summary(test)
anova(test, test_interaction)
plot(test)
leverage_problems <- data.frame(PID=relevant_data[((hatvalues(test)/mean(hatvalues(test)))>3),1])

resid_problems <- data.frame(PID =relevant_data[(abs(test$residuals)>2),1])
cooks_problems <- data.frame(PID=relevant_data[((cooks.distance(test)/mean(cooks.distance(test),
                                          na.rm=T))>10),1])
outliers <- full_join(leverage_problems,resid_problems)
outliers<-full_join(outliers,cooks_problems)

relevant_data_no_out <- anti_join(relevant_data,outliers)
test_noOut<- lm(log(price)~log(total_sqft)+log(k8_ave)+year_built+water_shed+
                           year*month+roof+stories+sqrt(acres)+condition+heat+neighborhood +sqrt(fireplace)+
                           ac+baths+highSchool, data=relevant_data_no_out)

summary(test_noOut)

plot(test_noOut)
plot(test_interaction$residuals~relevant_data$percentDeviation)
plot(log(final_data$Price)~(final_data$BATH_HALF))
plot(log(relevant_data$price)~(relevant_data$baths))

scatterplotMatrix(~log(price)+(total_sqft)+percentDeviation+year_built, data=relevant_data)
scatterplotMatrix(~log(price)+log(total_sqft), data=relevant_data)
interaction <- (log(relevant_data$total_sqft)*log(relevant_data$k8_ave))
scatterplotMatrix(~log(relevant_data$price)+log(relevant_data$total_sqft)+log(relevant_data$k8_ave)+interaction)
scatterplotMatrix(~log(relevant_data_no_out$price)+year_built+acres+condition, data=relevant_data_no_out)

scatterplotMatrix(~log(relevant_data_no_out$price)+sqrt(relevant_data_no_out$acres)+relevant_data_no_out$acres)[1,]
