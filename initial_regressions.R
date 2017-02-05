library(car)
library(lattice)
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

relevant_data <- data.frame(PID = final_data$PID,k8_ave = final_data$k8_ave, quarter_ave = final_data$quarter_ave,
                            deviation_k8=(final_data$total_sqft-final_data$k8_ave),
                            deviation_quarter=(final_data$total_sqft-final_data$quarter_ave),
                            percentDeviation_k8=((final_data$total_sqft-final_data$k8_ave)/final_data$k8_ave),
                            percentDeviation_quarter=((final_data$total_sqft-final_data$quarter_ave)/final_data$quarter_ave),
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
                            highSchoolRank = final_data$highSchool_ranking)

relevant_data <- relevant_data[(complete.cases(relevant_data)),]
relevant_data$neighborhood <- as.factor(relevant_data$neighborhood)
relevant_data$condition <- as.numeric(relevant_data$condition)
relevant_data$year<- as.factor(relevant_data$year)
relevant_data$ac <- as.factor(relevant_data$ac)
relevant_data$month <- as.factor(relevant_data$month)
relevant_data$heat <- as.factor(relevant_data$heat)
relevant_data$roof <- as.factor(relevant_data$roof)

relevant_data$posDummy_k8 <- ifelse(relevant_data$deviation_k8>=0,1,0)


xyplot(log(price) ~ percentDeviation_k8 | equal.count(log(relevant_data$total_sqft),
                                                      number = 9, overlap = 0), 
       data=relevant_data,panel = function(x,y) { panel.xyplot(x,y); panel.lmline(x,y)} )

quantile(relevant_data$total_sqft, probs = c(.1,.9))

relevant_data$sizeDecile <- ifelse(relevant_data$total_sqft<886, "first", ifelse(relevant_data$total_sqft>2174, "last", "middle"))
relevant_data$sizeDecile <- ifelse(relevant_data$total_sqft<886, "first", "normal")

relevant_data$sizeDecile <- as.factor(relevant_data$sizeDecile)
relevant_data$sizeDecile <- relevel(relevant_data$sizeDecile, ref="normal")


test <- lm(log(price)~log(total_sqft)+ percentDeviation_k8+
             sizeDecile:percentDeviation_k8+year_built+water_shed+
              year*month+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths, data=relevant_data)
summary(test)

test0 <- lm(log(price)~log(total_sqft)+ percentDeviation_k8+posDummy_k8+percentDeviation_k8:posDummy_k8+
              log(total_sqft):percentDeviation_k8+log(total_sqft):posDummy_k8+
              log(total_sqft):percentDeviation_k8:posDummy_k8+year_built+water_shed+
              year*month+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+roof+highSchoolRank, data=relevant_data)

summary(test0)
test1 <- lm(log(price)~log(total_sqft)+ percentDeviation_k8+percentDeviation_k8:posDummy_k8+
              log(total_sqft):percentDeviation_k8+
              log(total_sqft):percentDeviation_k8:posDummy_k8+year_built+water_shed+
              year*month+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths, data=relevant_data)
anova(test0,test1)
summary(test2)
AIC(test1,test)
summary(test1)


resid_problems <- data.frame(PID =relevant_data[(abs(test1$residuals)>2),1])
cooks_problems <- data.frame(PID=relevant_data[((cooks.distance(test1)/mean(cooks.distance(test1),na.rm=T))>5),1])
outliers <- inner_join(cooks_problems,resid_problems)
data_k8 <- anti_join(relevant_data,outliers)

test2 <- lm(log(price)~log(total_sqft)+ percentDeviation_k8+percentDeviation_k8:posDummy_k8+
              log(total_sqft):percentDeviation_k8+
              log(total_sqft):percentDeviation_k8:posDummy_k8+year_built+water_shed+
              year*month+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths, data=data_k8)

summary(test2)
final_k8 <-tidy(test2)
final_k8_rel <- data.frame(final_k8[c(3,112,113,147),])


~~~~~~~~~~~~~~~~~~~~~~~~~~
relevant_data_pos_k8 <- filter(relevant_data, percentDeviation_k8>=0)
scatterplotMatrix(~log(relevant_data_pos_k8$price)+(relevant_data_pos_k8$percentDeviation_k8))
relevant_data_neg_k8 <- filter(relevant_data, percentDeviation_k8<=0)
scatterplotMatrix(~log(relevant_data_neg_k8$price)+(relevant_data_neg_k8$percentDeviation_k8))


scatterplotMatrix(~log(price)+log(total_sqft)+log(k8_ave)+percentDeviation_k8, data=relevant_data_pos_k8)

test1 <- lm(log(price)~log(total_sqft)*percentDeviation_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchoolRank, data=relevant_data_pos_k8)
test2 <- lm(log(price)~log(total_sqft)*percentDeviation_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchoolRank, data=relevant_data_neg_k8)
summary(test1)
summary(test2)
plot(test1)

test3 <- lm(log(price)~log(total_sqft)+percentDeviation_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths, data=relevant_data_neg_k8)
anova(test2,test3)

test4 <- lm(log(price)~log(total_sqft)*deviation_k8+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths, data=relevant_data_neg_k8)
summary(test4)

AIC(test3,test4)
BIC(test3,test4)

relevant_data_pos_quarter <- filter(relevant_data, percentDeviation_quarter>=0)
relevant_data_neg_quarter <- filter(relevant_data, percentDeviation_quarter<=0)
test1 <- lm(log(price)~log(total_sqft)*percentDeviation_quarter+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths+highSchoolRank, data=relevant_data_pos_quarter)

test3 <- lm(log(price)~log(total_sqft)*percentDeviation_quarter+year_built+water_shed+
              year*month+stories+acres+condition+neighborhood+heat +sqrt(fireplace)+
              ac+baths, data=relevant_data_pos_quarter)

summary(test3)
plot(test3)
anova(test3,test1)
test4 <- lm(log(price)~log(total_sqft)*deviation_quarter+year_built+water_shed+
              year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths, data=relevant_data_pos_quarter)

AIC(test3,test4)
BIC(test3,test4)


lm_k8_pos <- lm(log(price)~log(total_sqft)*percentDeviation_k8+year_built+water_shed+
              year*month+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
              ac+baths, data=relevant_data_pos_k8) 
plot(lm_k8_pos)
leverage_problems <- data.frame(PID=relevant_data[((hatvalues(lm_k8_pos)/mean(hatvalues(lm_k8_pos)))>2),1])
resid_problems <- data.frame(PID =relevant_data[(abs(lm_k8_pos$residuals)>2),1])
cooks_problems <- data.frame(PID=relevant_data[((cooks.distance(lm_k8_pos)/mean(cooks.distance(lm_k8_pos),na.rm=T))>10),1])
outliers <- full_join(leverage_problems,resid_problems)
outliers<-full_join(outliers,cooks_problems)
data_pos_k8 <- anti_join(relevant_data_pos_k8,outliers)

lm_k8_pos_final <- lm(log(price)~log(total_sqft)*percentDeviation_k8+year_built+water_shed+
                  year*month+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
                  ac+baths, data=data_pos_k8) 

leverage_problems <- data.frame(PID=relevant_data[((hatvalues(lm_k8_pos_final)/mean(hatvalues(lm_k8_pos_final)))>2),1])
resid_problems <- data.frame(PID =relevant_data[(abs(lm_k8_pos_final$residuals)>2),1])
cooks_problems <- data.frame(PID=relevant_data[((cooks.distance(lm_k8_pos_final)/mean(cooks.distance(lm_k8_pos_final),na.rm=T))>10),1])
outliers <- full_join(leverage_problems,resid_problems)
outliers<-full_join(outliers,cooks_problems)
data_pos_k8_2 <- anti_join(data_pos_k8,outliers)

lm_k8_pos_final2 <- lm(log(price)~log(total_sqft)*percentDeviation_k8+year_built+water_shed+
                        year*month+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
                        ac+baths, data=data_pos_k8_2) 

summary(lm_k8_pos_final2)
plot(lm_k8_pos_final)

lm_k8_neg <- lm(log(price)~log(total_sqft)*percentDeviation_k8+year_built+water_shed+
                  year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
                  ac+baths, data=relevant_data_neg_k8) 
lm_quarter_pos <- lm(log(price)~log(total_sqft)*percentDeviation_k8+year_built+water_shed+
                  year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
                  ac+baths, data=relevant_data_pos_quarter) 
lm_quarter_neg <- lm(log(price)~log(total_sqft)*percentDeviation_k8+year_built+water_shed+
                  year*month+roof+stories+acres+condition+heat+neighborhood +sqrt(fireplace)+
                  ac+baths, data=relevant_data_neg_quarter) 